PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel2d
  USE modimageops2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4, num_samples = 50
  INTEGER, PARAMETER :: width = 128, height = 128

  REAL(KIND=dtype), ALLOCATABLE, DIMENSION(:, :) :: input_x, output_x, exact_output_x
  REAL(KIND=dtype), ALLOCATABLE, DIMENSION(:, :) :: input_highres_x, output_highres_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernel2D) :: coulomb, coulomb_highres
  REAL(KIND=dtype) :: dx2 = 0.001, derror, potential
  INTEGER :: i, j


  ALLOCATE(input_highres_x(2*width, 2*height))
  ALLOCATE(output_highres_x(2*width, 2*height))
  ALLOCATE(exact_output_x(2*width, 2*height))
  ALLOCATE(input_x(width, height))
  ALLOCATE(output_x(width, height))

  DO i = 1 , 2 * width
    DO j = 1 , 2 * height
      potential = 1 / (dx2 * (i - width)**2 + dx2 * (j - height/4)**2 + 1)
      potential = potential -  1 / (dx2 * (i - width)**2 + dx2 * (j - 3*height/4)**2 + 1)
      input_highres_x(i, j) = potential
    END DO
  END DO

  ! Normal grid initialization
  CALL read_kernels_2d(&
        "resources/coulomb_kernel_128/kernels_2D_scales=4_size=33_grid=128.txt", &
        4, 33, kernels &
    )
  CALL initapproxkernel2d(coulomb, kernels=kernels, input_shape=[width, height])

  ! High resolution grid initialization
  CALL read_kernels_2d(&
        "resources/coulomb_kernel_256/kernels_2D_scales=5_size=33_grid=256.txt", &
        5, 33, kernels &
    )
  CALL initapproxkernel2d(coulomb_highres, kernels=kernels, input_shape=[2*width, 2*height])
  CALL coulomb_integral(input_highres_x, exact_output_x)

  ! Benchmark speed
  CALL execapproxkernel2d(coulomb, input_x, output_x)
  CALL reset_clock()
  DO i = 1 , num_samples
    CALL averagepool2x2(input_highres_x, input_x, dynamic=.false.)

    CALL execapproxkernel2d(coulomb, input_x, output_x)
    CALL resizebilinear2d(output_x, output_highres_x)
    output_highres_x = output_highres_x * 2  ! ???
  END DO
  derror =  SUM(ABS(output_highres_x - exact_output_x)) / width / height / 4
  PRINT*, "Coulomb LowRes :", get_clock() / num_samples * 1000, "[ms]", derror

  CALL save_array_2d("outputs/output_coulomb_lowres_scaled.txt", output_highres_x)
  CALL save_array_2d("outputs/output_coulomb_exact.txt", exact_output_x)

  CALL execapproxkernel2d(coulomb_highres, input_highres_x, output_highres_x)
  CALL reset_clock()
  DO i = 1 , num_samples
    CALL execapproxkernel2d(coulomb_highres, input_highres_x, output_highres_x)
  END DO
  derror =  SUM(ABS(output_highres_x - exact_output_x)) / width / height / 4

  PRINT*, "Coulomb HighRes:", get_clock() / num_samples * 1000, "[ms]", derror

  CALL save_array_2d("outputs/output_coulomb_highres.txt", output_highres_x)

  CALL deleteapproxkernel2d(coulomb)
  CALL deleteapproxkernel2d(coulomb_highres)
  DEALLOCATE(kernels, input_x, output_x, &
      input_highres_x, output_highres_x, exact_output_x)

CONTAINS



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
  INTEGER, DIMENSION(2) :: array_shape
  REAL(KIND=dtype) :: current_value, radius
  REAL(KIND=dtype), DIMENSION(2) :: vector_r

  array_shape = SHAPE(density)
  width = array_shape(1)
  height = array_shape(2)

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

