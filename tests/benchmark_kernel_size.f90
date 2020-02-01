PROGRAM example
  USE MKL_VSL
  USE modutils
  USE modio
  USE modvslconv2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 4
  INTEGER :: i
  INTEGER, DIMENSION(4) :: kernel_sizes_a = (/3, 5, 17, 33/)
  INTEGER, DIMENSION(4) :: kernel_sizes_b = (/65, 129, 257, 513/)

  PRINT"(2A6, A20)", "N", "K", "FFTConv2D [ms]"

  DO i = 1, SIZE(kernel_sizes_a)
    CALL benchmark_conv2d(kernel_sizes_a(i), 512)
  END DO

  DO i = 1, SIZE(kernel_sizes_b)
    CALL benchmark_conv2d(kernel_sizes_b(i), 512)
  END DO

  DO i = 1, SIZE(kernel_sizes_a)
    CALL benchmark_conv2d(kernel_sizes_a(i), 256)
  END DO

  DO i = 1, SIZE(kernel_sizes_b)
    CALL benchmark_conv2d(kernel_sizes_b(i), 256)
  END DO

  DO i = 1, SIZE(kernel_sizes_a)
    CALL benchmark_conv2d(kernel_sizes_a(i), 128)
  END DO

  DO i = 1, SIZE(kernel_sizes_b)
    CALL benchmark_conv2d(kernel_sizes_b(i), 128)
  END DO

CONTAINS


SUBROUTINE benchmark_conv2d(kernel_size, width)

  INTEGER :: width, kernel_size

  REAL(KIND=dtype), ALLOCATABLE ,DIMENSION(:, :) :: input_x, output_x
  REAL(KIND=dtype), ALLOCATABLE ,DIMENSION(:, :) :: kernel

  TYPE(VSLSConv2D) :: conv2d_fft, conv2d_direct
  INTEGER :: i, j, num_samples, height
  REAL :: time_fft, time_direct
  ALLOCATE(input_x(width, width), output_x(width, width))
  ALLOCATE(kernel(kernel_size, kernel_size))
  CALL reset_clock()
  num_samples = 50
  CALL initvslconv2d(conv2d_fft, kernel, [width, width], mode=VSL_CONV_MODE_FFT)
  CALL initvslconv2d(conv2d_direct, kernel, [width, width], mode=VSL_CONV_MODE_DIRECT)

  time_direct = -1


!  ! first run for initialization (it will take longer, then next one)
!  DO i = 1, 10
!    CALL execvslconv2d(conv2d_direct, input_x, output_x)
!  ENDDO
!  CALL reset_clock()
!  DO i = 1, num_samples
!    CALL execvslconv2d(conv2d_direct, input_x, output_x)
!  ENDDO
!  time_direct  = get_clock() * 1000 / num_samples


  ! first run for initialization (it will take longer, then next one)
  DO i = 1, 10
    CALL execvslconv2d(conv2d_fft, input_x, output_x)
  ENDDO

  CALL reset_clock()
  DO i = 1, num_samples
    CALL execvslconv2d(conv2d_fft, input_x, output_x)
  ENDDO

  time_fft  = get_clock() * 1000 / num_samples
  PRINT"(2i6, f12.2)", width, kernel_size , time_fft

  DEALLOCATE(input_x, output_x, kernel)
  CALL deletevslconv2d(conv2d_fft)
  CALL deletevslconv2d(conv2d_direct)

END SUBROUTINE

END PROGRAM

