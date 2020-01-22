PROGRAM example
  USE MKL_VSL
  USE modutils
  USE modimageops2d
  IMPLICIT NONE
  INTEGER, PARAMETER :: dtype = 8
  INTEGER, PARAMETER :: N_from = 256, N_to = 512
  INTEGER :: i, j, K
  REAL(dtype) :: x, time
  REAL(dtype), DIMENSION(:, :), ALLOCATABLE :: input
  REAL(dtype), DIMENSION(:, :), ALLOCATABLE :: output1
  REAL(dtype), DIMENSION(:, :), ALLOCATABLE :: output2

  allocate(input(N_from, N_from))
  allocate(output1(N_to, N_to))
  allocate(output2(N_to, N_to))

  do i = 1, N_from
    do j = 1, N_from
      call random_number(x)
      input(i, j) = x
    end do
  end do

  K = 1000
  CALL reset_clock()
  do i = 1, K
    call resizebilinear2d(input, output1)
  enddo
  time  = get_clock() * 1000

  PRINT"(A15,f12.4,A,f12.4)"," Time old", time, "[ms] "

  CALL reset_clock()
  do i = 1, K
    call upsample2x2(input, output2)
  enddo
  time  = get_clock() * 1000
  x = sum(abs(output1 - output2))
  PRINT"(A15,f12.4,A,f12.4)"," Time new", time, "[ms]   Error:", x


CONTAINS



SUBROUTINE upsample2x2(input, output)
  REAL(dtype), DIMENSION(:, :), INTENT(IN) :: input
  REAL(dtype), DIMENSION(:, :) :: output
  REAL(dtype) :: c0, c1, c2, c3
  INTEGER, DIMENSION(2) :: input_shape, target_shape

  INTEGER :: i, j, ix, iy
  REAL(8) :: x, y, alpha, dx, dy
  INTEGER :: target_width, target_height
  INTEGER :: source_width, source_height

  target_shape = SHAPE(output)
  input_shape = SHAPE(input)
  !CALL check_inputs(input_shape, input_shape, .true.)

  target_width = target_shape(1)
  target_height = target_shape(2)

  source_width = input_shape(1)
  source_height = input_shape(2)


  output = 0
  DO i = 1 , source_width - 1
    DO j = 1, source_height - 1
      c0 = input(i  , j  )
      c1 = input(i+1, j  )
      c2 = input(i+1, j+1)
      c3 = input(i  , j+1)
      ix = 2*i - 1
      iy = 2*j - 1
      ! To jest zle trzeba to dobrze zaimplementowac
      output(ix  , iy  ) = c0
      output(ix+1, iy  ) = (c0 + c1) / 2
      output(ix+1, iy+1) = (c0 + c1 + c2 + c3) / 4
      output(ix  , iy+1) = (c0 + c3) / 2

    END DO
  END DO


END SUBROUTINE
END PROGRAM

