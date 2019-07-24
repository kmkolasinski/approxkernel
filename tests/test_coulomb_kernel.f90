PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 8
  INTEGER, PARAMETER :: width = 96, height = 96

  REAL(KIND=dtype), DIMENSION(width, height) :: input_x, output_x, exact_output_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxDKernel2D) :: coulomb
  INTEGER :: i, j

  CALL RANDOM_NUMBER(input_x)
  input_x = input_x / 100
  input_x(width / 2, 1 * height / 4) = -1.5
  input_x(width / 2, 3 * height / 4) = -1.5
  input_x(1 * width / 4, height / 2) = +1.5
  input_x(3 * width / 4, height / 2) = +1.5

  CALL read_kernels_2d("resources/coulomb_kernel/trained_kernels.txt", 4, 33, kernels)
  CALL initapproxkernel2d(coulomb, kernels=kernels, input_shape=[width, height], use_smoothing=.false.)
  PRINT*,"Running approximated integral"
  ! first run for initialization (it will take longer, then next one)
  CALL execapproxkernel2d(coulomb, input_x, output_x)
  CALL reset_clock()
  CALL execapproxkernel2d(coulomb, input_x, output_x)
  PRINT*,"Time approx:", get_clock() * 1000, "[ms]"
  PRINT*,"Running exact integral (naive approach)"
  CALL reset_clock()
  CALL coulomb_integral(input_x, exact_output_x)
  PRINT*,"Time naive          :", get_clock() * 1000, "[ms]"
  PRINT*,"Mean absolute error :", SUM(ABS(output_x - exact_output_x)) / width / height

  CALL save_array_2d("outputs/coulomb_kernel/output_coulomb_approx.txt", output_x)
  CALL save_array_2d("outputs/coulomb_kernel/output_coulomb_exact.txt", exact_output_x)
  CALL save_array_2d("outputs/coulomb_kernel/output_coulomb_diff.txt", output_x - exact_output_x)

  CALL deleteapproxkernel2d(coulomb)
  DEALLOCATE(kernels)

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
  REAL(KIND=dtype), DIMENSION(width, height) :: density, potential
  INTEGER :: i, j, ii, jj
  REAL(KIND=dtype) :: current_value, radius
  REAL(KIND=dtype), DIMENSION(2) :: vector_r

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

