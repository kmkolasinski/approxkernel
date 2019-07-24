PROGRAM demo
  USE modutils
  USE modio
  USE modapproxkernel2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: width = 128, height = 128

  REAL(KIND=8), DIMENSION(width, height) :: rho, potential
  COMPLEX(KIND=8), DIMENSION(width, height) :: cmplx_rho, cmplx_potential
  REAL(KIND=8), DIMENSION(:, :, :), ALLOCATABLE :: kernels
  REAL(KIND=8) :: error_value
  TYPE(ApproxDKernel2D) :: double_coulomb
  TYPE(ApproxZKernel2D) :: cmplx_coulomb

  ! -------------------------------------------------
  ! This kernel was trained for maximum grid size 128
  ! Num scales and kernel size cannot be changed. This is
  ! fixes for each kernels.txt file
  ! -------------------------------------------------
  CALL read_kernels_2d(&
      "resources/coulomb2d_kernel/kernels_scales=4_size=33_grid=128.txt", &
      num_kernels=4, kernel_size=33, kernels=kernels)

  ! Initialize kernel approximator with loaded kernels
  CALL initapproxkernel2d(cmplx_coulomb, kernels=kernels, input_shape=[width, height])
  CALL initapproxkernel2d(double_coulomb, kernels=kernels, input_shape=[width, height])

  ! create complex random matrix
  CALL RANDOM_NUMBER(rho)
  print*, "min/max rho:", minval(rho), maxval(rho)
  cmplx_rho = rho
  CALL RANDOM_NUMBER(rho)
  cmplx_rho = cmplx_rho + CMPLX(0.0, 1.0) * rho

  CALL execapproxkernel2d(cmplx_coulomb, cmplx_rho, cmplx_potential)

  rho = DBLE(cmplx_rho)
  CALL execapproxkernel2d(double_coulomb, rho, potential)
  error_value = SUM(ABS(DBLE(cmplx_potential) - potential)) / width / height
  PRINT*, "real part difference:", error_value
  if (error_value > 1e-12) then
      STOP(-1)
  end if

  rho = IMAG(cmplx_rho)
  CALL execapproxkernel2d(double_coulomb, rho, potential)
  error_value = SUM(ABS(IMAG(cmplx_potential) - potential)) / width / height
  PRINT*, "imag part difference:", error_value
  if (error_value > 1e-12) then
      PRINT*, "ERROR: imag part difference:", error_value
      STOP(-1)
  end if

  CALL deleteapproxkernel2d(double_coulomb)
  CALL deleteapproxkernel2d(cmplx_coulomb)
  DEALLOCATE(kernels)

END PROGRAM
