PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel1d

  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4
  INTEGER, PARAMETER :: kernel_size = 33
  INTEGER, PARAMETER :: width = 128

  REAL(KIND=dtype), DIMENSION(width) :: input_x, output_x, exact_output_x
  REAL(KIND=dtype), DIMENSION(:, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernel1D) :: kernel1d
  INTEGER :: i, j, num_samples
  REAL(8) :: abs_error

  num_samples = 100
  input_x = 0
  input_x(width / 2) = 1

  CALL read_kernels_1d(&
    "resources/coulomb1d_kernel/kernels_1D_scales=4_size=33_grid=128.txt", 4, 33, kernels)

  CALL initapproxkernel1d(kernel1d, kernels=kernels, input_shape=[width])

  PRINT*,"Running benchmark"
  DO j = 1, 3
    CALL reset_clock()
    DO i = 1, num_samples
      CALL execapproxkernel1d(kernel1d, input_x, output_x)
    END DO
    PRINT*, j, " => Time Coulomb full:", get_clock() / num_samples * 1000, "[ms]"
  END DO

  call coulomb_integral1d(input_x, exact_output_x)

  abs_error = sum(abs(exact_output_x - output_x)) / width

  if (abs_error > 1e-3) then
      print*, "[ERROR] Kerel approximation error is greater than 1e-3: ", abs_error
  end if

  CALL save_array_1d("outputs/output_coulomb_1d_approx.txt", output_x)
  CALL save_array_1d("outputs/output_coulomb_1d_exact.txt", exact_output_x)
  CALL deleteapproxkernel1d(kernel1d)
  DEALLOCATE(kernels)

CONTAINS


REAL(KIND=dtype) FUNCTION kernel_fn(r) RESULT(rval)
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
    REAL(KIND=dtype) :: r, singularity, eps, amplitude, distance

    eps = 1e-6
    singularity = - 2.0 * log(3.0 - 2.0 * sqrt(2.0))
    distance = r + eps
    amplitude = 1 / distance
    amplitude = MIN(amplitude, singularity)
    rval = amplitude

END FUNCTION


SUBROUTINE coulomb_integral1d(density, potential)
  REAL(KIND=dtype), DIMENSION(width) :: density, potential
  INTEGER :: i, ii
  REAL(KIND=dtype) :: current_value, radius

  potential = 0.0
  DO i = 1, width
    current_value = 0
    DO ii = 1, width
      radius = ABS(i - ii)
      current_value = current_value + density(ii) * kernel_fn(radius)
    END DO
    potential(i) = current_value
  END DO

END SUBROUTINE

END PROGRAM

