PROGRAM demo
  USE modutils
  USE modio
  USE modapproxkernel2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: width = 128, height = 64

  COMPLEX(KIND=8), DIMENSION(width, height) :: rho, potential
  REAL(KIND=8), DIMENSION(:, :, :), ALLOCATABLE :: kernels
  TYPE(ApproxZKernel2D) :: coulomb

  ! -------------------------------------------------
  ! This kernel was trained for maximum grid size 128
  ! Num scales and kernel size cannot be changed. This is
  ! fixes for each kernels.txt file
  ! -------------------------------------------------
  CALL read_kernels_2d(&
      "resources/coulomb2d_kernel/kernels_scales=4_size=33_grid=128.txt", &
      num_kernels=4, kernel_size=33, kernels=kernels)

  CALL initapproxkernel2d(coulomb, kernels=kernels, input_shape=[width, height])

  ! set-up potential
  rho = 0
  rho(width / 2, height / 2) = 1
  rho(width / 4, height / 4) = cmplx(0, 1)

  CALL reset_clock()
  CALL execapproxkernel2d(coulomb, rho, potential)
  PRINT*, "First call time:", get_clock() * 1000, "[ms]"
  CALL reset_clock()
  CALL execapproxkernel2d(coulomb, rho, potential)
  PRINT*, "Second call time:", get_clock() * 1000, "[ms]"
  ! this can be plotted with gnuplot: sp "output_coulomb_2d.txt" w pm3d
  CALL save_array_2d("outputs/output_complex_coulomb_2d_real.txt", dble(potential))
  CALL save_array_2d("outputs/output_complex_coulomb_2d_imag.txt", imag(potential))
  CALL deleteapproxkernel2d(coulomb)
  DEALLOCATE(kernels)


END PROGRAM


