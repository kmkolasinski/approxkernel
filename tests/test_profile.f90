PROGRAM example
  USE MKL_VSL
  USE modutils
  USE modio
  USE modapproxkernel2d
  USE modvslconv2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 8

!  CALL benchmark_methods(&
!    "resources/coulomb_kernel_128/kernels_2D_scales=3_size=65_grid=128.txt", 3, 65, 128)
!  CALL benchmark_methods(&
!    "resources/coulomb_kernel_128/kernels_2D_scales=4_size=33_grid=128.txt", 4, 33, 128)
  !CALL benchmark_methods(&
  !  "resources/coulomb_kernel_256/kernels_2D_scales=4_size=65_grid=256.txt", 4, 65, 256)
  !CALL benchmark_methods(&
  !  "resources/coulomb_kernel_256/kernels_2D_scales=5_size=33_grid=256.txt", 5, 33, 256)
  !CALL benchmark_methods(&
  !  "resources/coulomb_kernel_512/kernels_2D_scales=4_size=129_grid=512.txt", 4, 129, 512)
  !CALL benchmark_methods(&
  !  "resources/coulomb_kernel_512/kernels_2D_scales=5_size=65_grid=512.txt", 5, 65, 512)
  CALL benchmark_methods(&
    "resources/coulomb_kernel_512/kernels_2D_scales=6_size=33_grid=512.txt", 6, 33, 512)

CONTAINS


SUBROUTINE benchmark_methods(kernels_file, num_scales, kernel_size, width)
  CHARACTER(*) :: kernels_file
  INTEGER :: width, num_scales, kernel_size

  REAL(KIND=dtype), ALLOCATABLE ,DIMENSION(:, :) :: input_x, output_x, exact_output_x
  REAL(KIND=dtype), ALLOCATABLE ,DIMENSION(:, :) :: fft_kernel
  REAL(KIND=dtype), ALLOCATABLE ,DIMENSION(:, :, :) :: kernels
  TYPE(ApproxDKernel2D) :: approx_kernel
  TYPE(VSLDConv2D) :: conv2d_fft, conv2d_direct
  INTEGER :: i, j, num_samples, height
  REAL(KIND=dtype) :: radius, cx, cy, time, ERROR

  ALLOCATE(input_x(width, width), output_x(width, width), exact_output_x(width, width))
  ALLOCATE(fft_kernel(2 * width + 1, 2 * width + 1))


  num_samples = 20
  height = width

  ! Fill FFT 2d kernel
  cx = width + 1.0
  cy = height + 1.0
  DO i = 1, 2 * width + 1
    DO j = 1, 2 * height + 1
      radius = NORM2([i - cx, j - cy])
      fft_kernel(i, j) = coulomb_radial_function(radius)
    END DO
  END DO

  CALL initvslconv2d(conv2d_fft, fft_kernel, [width, height], mode=VSL_CONV_MODE_FFT)
  CALL initvslconv2d(conv2d_direct, fft_kernel, [width, height], mode=VSL_CONV_MODE_DIRECT)

  CALL RANDOM_NUMBER(input_x)
  input_x = input_x / 100
  input_x(width / 2, 1 * height / 4) = -1.5
  input_x(width / 2, 3 * height / 4) = -1.5
  input_x(1 * width / 4, height / 2) = +1.5
  input_x(3 * width / 4, height / 2) = +1.5

  CALL read_kernels_2d(kernels_file, num_scales, kernel_size, kernels)
  CALL initapproxkernel2d(approx_kernel, kernels=kernels, &
      input_shape=[width, height], use_smoothing=.false.)
  exact_output_x = 0
  CALL reset_clock()
!  CALL execvslconv2d(conv2d_direct, input_x, exact_output_x)
  PRINT"(A, 3i4)"," CONFIGURATION =", num_scales, kernel_size, width
  !PRINT"(A15, f12.4, A)","Time direct", get_clock() * 1000, "[ms]"

  ! first run for initialization (it will take longer, then next one)
  DO i = 1, 5
    CALL execapproxkernel2d(approx_kernel, input_x, output_x)
  ENDDO
  CALL reset_clock()
  DO i = 1, num_samples
    CALL execapproxkernel2d(approx_kernel, input_x, output_x)
  ENDDO
  time  = get_clock() * 1000 / num_samples
  ERROR = abs_error(output_x, exact_output_x)
  PRINT"(A15,f12.4,A,f12.4)"," Time approx", time, "[ms]   Error:", ERROR

  DO i = 1, 5
    CALL execvslconv2d(conv2d_fft, input_x, output_x)
  ENDDO
  CALL reset_clock()
  DO i = 1, num_samples
    CALL execvslconv2d(conv2d_fft, input_x, output_x)
  ENDDO
  time  = get_clock() * 1000 / num_samples
  ERROR = abs_error(output_x, exact_output_x)
  PRINT"(A15,f12.4,A,f12.4)"," Time ML fft", time, "[ms]   Error:", ERROR


  DEALLOCATE(input_x, output_x, exact_output_x, fft_kernel, kernels)
  CALL deleteapproxkernel2d(approx_kernel)
  CALL deletevslconv2d(conv2d_fft)
  CALL deletevslconv2d(conv2d_direct)

END SUBROUTINE

REAL(KIND=dtype) FUNCTION abs_error(a, b) RESULT(eval)
  REAL(KIND=dtype), DIMENSION(:, :) :: a, b
  INTEGER :: width, height
  width = SIZE(a, 1)
  height = SIZE(a, 2)
  eval = SUM(ABS(a - b)) / width / height
END FUNCTION

REAL(KIND=dtype) FUNCTION coulomb_radial_function(r) RESULT(rval)
!    """Example implementation of the coulomb 2D kernel
!
!            U(r) = alpha / |r*dx + epsilon|
!
!    Args:
!        r: distance from center, expressed in terms of grid position
!
!    Returns:
!        value of U(r)
!    """
    REAL(KIND=dtype) :: r, alpha, dx, eps, amplitude, distance
    eps = 1e-6
    alpha = 0.05
    dx = 1.0 / 32
    distance = dx * (r + eps)
    amplitude = 1.0 / distance
    amplitude = MIN(amplitude, 2.0/dx)
    rval = alpha * amplitude
END FUNCTION


SUBROUTINE coulomb_integral(density, potential)
  REAL(KIND=dtype), DIMENSION(:, :) :: density, potential
  INTEGER :: i, j, ii, jj, width, height
  REAL(KIND=dtype) :: current_value, radius
  REAL(KIND=dtype), DIMENSION(2) :: vector_r

  width = SIZE(potential, 1)
  height = SIZE(potential, 2)

  potential = 0.0
  DO i = 1, width
    DO j = 1, height
      current_value = 0
      DO ii = 1, width
        DO jj = 1, height
          vector_r = [i - ii, j - jj]
          radius = NORM2(vector_r)
          current_value = current_value + density(ii, jj) * coulomb_radial_function(radius)
        END DO
      END DO
      potential(i, j) = current_value
    END DO
  END DO

END SUBROUTINE

END PROGRAM

