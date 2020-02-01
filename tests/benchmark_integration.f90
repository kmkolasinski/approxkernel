PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4,  num_samples = 50

  REAL(KIND=dtype), DIMENSION(:, :), ALLOCATABLE :: input_x, output_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernel2D) :: coulomb


  CALL run_benchmark("outputs/benchmark_w_smoothing.txt", .true.)
  CALL run_benchmark("outputs/benchmark_wo_smoothing.txt", .false.)


CONTAINS

  SUBROUTINE run_benchmark(filename, use_smoothing)
    CHARACTER(*) :: filename
    LOGICAL :: use_smoothing

    INTEGER :: i, j, s
    INTEGER :: grid_size, kernel_size, num_kernels
    REAL(KIND=dtype) :: time_initialization, time_execution

    OPEN(UNIT=1, FILE=filename)
    DO i = 1, 4
      DO num_kernels = 1, 6
        grid_size = 32 * 2**i
        kernel_size = 4 * grid_size / 2**num_kernels + 1
        PRINT*,"Running benchmark"
        PRINT"(A, i5)","         grid_size:", grid_size
        PRINT"(A, i5)","       num_kernels:", num_kernels
        PRINT"(A, i5)","       kernel_size:", kernel_size

        CALL reset_clock()
        CALL allocate_data(grid_size, kernel_size, num_kernels, use_smoothing)
        CALL execapproxkernel2d(coulomb, input_x, output_x)
        time_initialization = get_clock()
        PRINT*, "Initialization time:", time_initialization * 1000, "[ms]"
        ! warmup
        DO s = 1, 10
          CALL execapproxkernel2d(coulomb, input_x, output_x)
        END DO

        CALL reset_clock()
        DO s = 1, num_samples
          CALL execapproxkernel2d(coulomb, input_x, output_x)
        END DO
        time_execution = get_clock() / num_samples
        PRINT*, "Time execution:", time_execution * 1000, "[ms]"
        CALL deallocate_data()
        WRITE(1,"(3i6, 2f12.6)"), grid_size, num_kernels, kernel_size, &
            time_initialization * 1000, time_execution * 1000
      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

  SUBROUTINE allocate_data(grid_size, kernel_size, num_kernels, use_smoothing)
    INTEGER, INTENT(IN) :: grid_size, kernel_size, num_kernels
    LOGICAL :: use_smoothing
    ALLOCATE(input_x(grid_size, grid_size))
    ALLOCATE(output_x(grid_size, grid_size))
    ALLOCATE(kernels(num_kernels, kernel_size, kernel_size))

    kernels = SQRT(3.0)
    input_x = SQRT(2.0)
    output_x = 0.0

    CALL initapproxkernel2d(coulomb, &
        kernels=kernels, &
        input_shape=[grid_size, grid_size], &
        use_smoothing=use_smoothing)

  END SUBROUTINE

  SUBROUTINE deallocate_data()
    CALL deleteapproxkernel2d(coulomb)
    DEALLOCATE(kernels)
    DEALLOCATE(input_x)
    DEALLOCATE(output_x)
  END SUBROUTINE

END PROGRAM

