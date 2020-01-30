PROGRAM example
  USE MKL_VSL
  USE modutils
  USE modio
  USE modapproxkernel2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4
  INTEGER, PARAMETER :: NUM_GRID_SIZES = 4
  INTEGER, PARAMETER :: NUM_SCALES_SCAN = 7
  INTEGER, PARAMETER :: NUM_WARMUP_STEPS = 10
  INTEGER, PARAMETER :: NUM_BENCHMARK_STEPS = 20
  CHARACTER(2) :: MKL_NUM_THREADS, RUN_ID
  CHARACTER(100) :: filename
  REAL(KIND=dtype), DIMENSION(:, :), ALLOCATABLE :: input_x, output_x
  REAL(KIND=dtype), DIMENSION(:, :, :), ALLOCATABLE :: kernels

  TYPE(ApproxSKernel2D) :: coulomb

  CALL GET_ENVIRONMENT_VARIABLE('MKL_NUM_THREADS', MKL_NUM_THREADS)
  CALL GET_ENVIRONMENT_VARIABLE('RUN_ID', RUN_ID)
  print*, "MKL_NUM_THREADS=", MKL_NUM_THREADS
  print*, "         RUN_ID=", RUN_ID

  WRITE(filename, *), "results/ifort-pegasus-cpu-fft-ncpus=", trim(MKL_NUM_THREADS), ".txt"
  call run_benchmark(trim(filename), RUN_ID, "fft", mode=VSL_CONV_MODE_FFT)

!  WRITE(filename, *), "results/ifort-pegasus-cpu-direct-ncpus=", trim(MKL_NUM_THREADS), ".txt"
!  call run_benchmark(trim(filename), RUN_ID, "direct", mode=VSL_CONV_MODE_DIRECT)

CONTAINS

  subroutine run_benchmark(filename, run, tag, mode)
    character(*) :: filename
    character(*) :: run, tag
    INTEGER :: mode

    INTEGER :: i, j, s
    INTEGER :: grid_size, kernel_size, num_scales
    REAL(KIND=dtype) :: delta
    LOGICAL :: file_exists

    INQUIRE(FILE=filename, EXIST=file_exists)
    IF (.not. file_exists) THEN
      OPEN(UNIT=1, FILE=filename)
      WRITE(1,"(A)"), "timestamp,tag,grid_size,num_scales,kernel_size,delta"
    ELSE
      OPEN(UNIT=1, FILE=filename, ACCESS='APPEND')
    ENDIF


    do i = 1, NUM_GRID_SIZES
      do num_scales = 1, NUM_SCALES_SCAN

        grid_size = 32 * 2**i
        kernel_size = 4 * grid_size / 2**num_scales + 1

        if ( 2**num_scales > grid_size ) then
          cycle
        end if

        print*,"Running benchmark"
        print"(A, i5)","         grid_size:", grid_size
        print"(A, i5)","        num_scales:", num_scales
        print"(A, i5)","       kernel_size:", kernel_size

        call allocate_data(grid_size, kernel_size, num_scales, mode=mode)

        ! warmup
        do s = 1, NUM_WARMUP_STEPS
          call reset_clock()
          call random_number(input_x)
          call execapproxkernel2d(coulomb, input_x, output_x)
          print"(3A, f10.3, A)", "Running warmup for ", tag," delta:", get_clock()*1000, " [ms]"
        end do

        call reset_clock()
        do s = 1, NUM_BENCHMARK_STEPS
          call random_number(input_x)
          call execapproxkernel2d(coulomb, input_x, output_x)
        end do
        delta = get_clock() / NUM_BENCHMARK_STEPS

        call deallocate_data()

        ! {timestamp},{tag},{grid_size},{num_scales},{kernel_size},{delta}"
        write(1,"(4A,i12,A,i12,A,i12,A,1e14.6)"), &
            get_date(), ",run_", run ,",", &
            grid_size,",", num_scales,",", kernel_size,",", delta

      end do
    end do
    close(1)

  end subroutine

  SUBROUTINE allocate_data(grid_size, kernel_size, num_scales, mode)
    INTEGER, INTENT(IN) :: grid_size, kernel_size, num_scales
    INTEGER :: mode
    ALLOCATE(input_x(grid_size, grid_size))
    ALLOCATE(output_x(grid_size, grid_size))
    ALLOCATE(kernels(num_scales, kernel_size, kernel_size))

    kernels = SQRT(3.0)
    input_x = SQRT(2.0)
    output_x = 0.0

    CALL initapproxkernel2d(coulomb, &
        kernels=kernels, &
        input_shape=[grid_size, grid_size], &
        mode=mode, use_smoothing=.false.)

  END SUBROUTINE

  SUBROUTINE deallocate_data()
    CALL deleteapproxkernel2d(coulomb)
    DEALLOCATE(kernels)
    DEALLOCATE(input_x)
    DEALLOCATE(output_x)
  END SUBROUTINE

END PROGRAM

