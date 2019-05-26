PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4
  INTEGER, PARAMETER :: width = 128, height = 128

  REAL(KIND=dtype), DIMENSION(width, height) :: input_x, output_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernelData) :: coulomb
  INTEGER :: i, j, num_samples

  num_samples = 100
  input_x = 0
  input_x(width / 2, height / 2) = 1

  CALL read_kernels("resources/kernels_scales=4_size=33_grid=128_loss=0.0852.txt", 4, 33, kernels)
  CALL initapproxkernel(coulomb, kernels=kernels, input_shape=[width, height])

  PRINT*,"Running benchmark"
  DO j = 1, 3
    CALL reset_clock()
    DO i = 1, num_samples
      CALL execapproxkernel(coulomb, input_x, output_x)
    END DO
    PRINT*, j, " => Time Coulomb full:", get_clock() / num_samples * 1000, "[ms]"
  END DO

  CALL save_array2d("outputs/output_coulomb.txt", output_x)


  CALL deleteapproxkernel(coulomb)
  DEALLOCATE(kernels)

CONTAINS

END PROGRAM

