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
  PRINT*, "MKL_NUM_THREADS=", MKL_NUM_THREADS
  PRINT*, "         RUN_ID=", RUN_ID

!  WRITE(filename, *), "results/ifort-pegasus-cpu-fft-ncpus=", trim(MKL_NUM_THREADS), ".txt"
!  call run_benchmark(trim(filename), RUN_ID, "fft", mode=VSL_CONV_MODE_FFT)

  filename = "results/ifort-pegasus-cpu-fft-default.txt"
  CALL run_benchmark(TRIM(filename), RUN_ID, "fft", mode=VSL_CONV_MODE_FFT)

  filename = "results/ifort-pegasus-cpu-direct-default.txt"
  CALL run_benchmark(TRIM(filename), RUN_ID, "direct", mode=VSL_CONV_MODE_DIRECT)
CONTAINS

  SUBROUTINE run_benchmark(filename, run, tag, mode)
    CHARACTER(*) :: filename
    CHARACTER(*) :: run, tag
    INTEGER :: mode

    INTEGER :: i, j, s
    INTEGER :: grid_size, kernel_size, num_scales
    REAL(KIND=dtype) :: delta
    LOGICAL :: file_exists

    INQUIRE(FILE=filename, EXIST=file_exists)
    IF (.NOT. file_exists) THEN
      OPEN(UNIT=1, FILE=filename)
      WRITE(1,"(A)"), "timestamp,tag,grid_size,num_scales,kernel_size,delta"
    ELSE
      OPEN(UNIT=1, FILE=filename, ACCESS='APPEND')
    ENDIF

    DO i = 1, NUM_GRID_SIZES
      DO num_scales = 1, NUM_SCALES_SCAN

        grid_size = 32 * 2**i
        kernel_size = 4 * grid_size / 2**num_scales + 1

        IF ( 2**num_scales > grid_size ) THEN
          CYCLE
        END IF

        IF ( mode == VSL_CONV_MODE_DIRECT .and. kernel_size >= 512 ) THEN
          CYCLE
        END IF

        PRINT*,"Running benchmark"
        PRINT"(A, i5)","         grid_size:", grid_size
        PRINT"(A, i5)","        num_scales:", num_scales
        PRINT"(A, i5)","       kernel_size:", kernel_size

        CALL allocate_data(grid_size, kernel_size, num_scales, mode=mode)

        ! warmup
        DO s = 1, NUM_WARMUP_STEPS
          CALL reset_clock()
          CALL RANDOM_NUMBER(input_x)
          CALL execapproxkernel2d(coulomb, input_x, output_x)
          PRINT"(3A, f10.3, A)", "Running warmup for ", tag," delta:", get_clock()*1000, " [ms]"
        END DO

        CALL reset_clock()
        DO s = 1, NUM_BENCHMARK_STEPS
          CALL execapproxkernel2d(coulomb, input_x, output_x)
        END DO
        delta = get_clock() / NUM_BENCHMARK_STEPS

        CALL deallocate_data()

        ! {timestamp},{tag},{grid_size},{num_scales},{kernel_size},{delta}"
        WRITE(1,"(4A,i12,A,i12,A,i12,A,1e14.6)"), &
            get_date(), ",run_", run ,",", &
            grid_size,",", num_scales,",", kernel_size,",", delta

      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

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

