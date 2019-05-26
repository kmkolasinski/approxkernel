PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4,  num_samples = 200

  REAL(KIND=dtype), DIMENSION(:, :), ALLOCATABLE :: input_x, output_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernelData) :: coulomb


  call run_benchmark("outputs/benchmark_w_smoothing.txt", .true.)
  call run_benchmark("outputs/benchmark_wo_smoothing.txt", .false.)


CONTAINS

  subroutine run_benchmark(filename, use_smoothing)
    character(*) :: filename
    logical :: use_smoothing

    INTEGER :: i, j, s
    INTEGER :: grid_size, kernel_size, num_kernels
    REAL(KIND=dtype) :: time_initialization, time_execution

    open(unit=1, file=filename)
    do i = 1, 3
      do num_kernels = 1, 6
        grid_size = 32 * 2**i
        kernel_size = 4 * grid_size / 2**num_kernels + 1
        print*,"Running benchmark"
        print"(A, i5)","         grid_size:", grid_size
        print"(A, i5)","       num_kernels:", num_kernels
        print"(A, i5)","       kernel_size:", kernel_size

        call reset_clock()
        call allocate_data(grid_size, kernel_size, num_kernels, use_smoothing)
        call execapproxkernel(coulomb, input_x, output_x)
        time_initialization = get_clock()
        print*, "Initialization time:", time_initialization * 1000, "[ms]"

        call reset_clock()
        do s = 1, num_samples
          call execapproxkernel(coulomb, input_x, output_x)
        end do
        time_execution = get_clock() / num_samples
        print*, "Time execution:", time_execution * 1000, "[ms]"
        call deallocate_data()
        write(1,"(3i6, 2f12.6)"), grid_size, num_kernels, kernel_size, &
            time_initialization, time_execution
      end do
    end do
    close(1)

  end subroutine


  SUBROUTINE allocate_data(grid_size, kernel_size, num_kernels, use_smoothing)
    INTEGER, INTENT(IN) :: grid_size, kernel_size, num_kernels
    logical :: use_smoothing
    ALLOCATE(input_x(grid_size, grid_size))
    ALLOCATE(output_x(grid_size, grid_size))
    ALLOCATE(kernels(num_kernels, kernel_size, kernel_size))

    kernels = SQRT(3.0)
    input_x = SQRT(2.0)
    output_x = 0.0

    CALL initapproxkernel(coulomb, &
        kernels=kernels, &
        input_shape=[grid_size, grid_size], &
        use_smoothing=use_smoothing)

  END SUBROUTINE

  SUBROUTINE deallocate_data()
    CALL deleteapproxkernel(coulomb)
    DEALLOCATE(kernels)
    DEALLOCATE(input_x)
    DEALLOCATE(output_x)
  END SUBROUTINE

END PROGRAM

