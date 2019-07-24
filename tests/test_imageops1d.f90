PROGRAM example
  USE modimageops1d
  IMPLICIT NONE
  type(DArray1D) :: array1d
  integer :: width, i
  REAL(8) :: abs_error
  width = 16

  allocate(array1d%array(width))
  array1d%array = 1.0

  CALL averagepool2inplace(array1d%array)
  print*, "[INFO] Test #1: averagepool2inplace"
  abs_error = sum(abs(array1d%array - [1, 1, 1, 1, 1, 1, 1, 1]))
  if ( abs_error > 1e-16 ) then
    print*, "[ERROR] Test #1: averagepool2inplace failed with error:", abs_error
    stop(-1)
  end if

  print*, "[INFO] Test #2: averagepool2inplace"
  call allocate_array1d(array1d, [width])
  do i = 1, width
    array1d%array(i) = i
  end do

  CALL averagepool2inplace(array1d%array)
  abs_error = sum(abs(array1d%array - [3./2, 7./2, 11./2, 15./2, 19./2, 23./2, 27./2, 31./2]))
  if ( abs_error > 1e-16 ) then
    print*, "[ERROR] Test #2: averagepool2inplace failed with error:", abs_error
    stop(-1)
  end if

  print*, "[INFO] Test #3: resizebilinearinplace1d"
  call allocate_array1d(array1d, [width])
  array1d%array = 1
  CALL resizebilinearinplace1d(array1d%array, [4])

  abs_error = sum(abs(array1d%array - [1, 1, 1, 1]))
  if ( abs_error > 1e-16 ) then
    print*, "[ERROR] Test #3: resizebilinearinplace1d failed with error:", abs_error
    stop(-1)
  end if

  print*, "[INFO] Test #4: resizebilinearinplace1d"
  call allocate_array1d(array1d, [4])
  array1d%array = [0, 1, 4, 8]
  CALL resizebilinearinplace1d(array1d%array, [8])

  abs_error = sum(abs(array1d%array - &
        [0.0, 0.428571432828903, 0.857142865657806, &
              1.85714289546013, 3.14285719394684, &
              4.57142865657806, 6.28571438789368, 8.0]))

  if ( abs_error > 1e-6 ) then
    print*, "[ERROR] Test #4: resizebilinearinplace1d failed with error:", abs_error
    stop(-1)
  end if

CONTAINS


END PROGRAM

