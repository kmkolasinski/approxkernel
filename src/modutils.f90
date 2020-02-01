MODULE modutils
  IMPLICIT NONE
  PRIVATE
  INTEGER :: CURRENT_CLOCK
  INTERFACE print_matrix
    MODULE PROCEDURE print_zmatrix, print_dmatrix
  END INTERFACE print_matrix

  PUBLIC :: print_matrix
  PUBLIC :: print_date
  PUBLIC :: reset_clock , get_clock, get_date

CONTAINS


  ! Print complex matrix
  SUBROUTINE print_zmatrix(matrix, mformat)
    COMPLEX*16 ,INTENT(IN) :: matrix(:,:)
    CHARACTER(*) :: mformat
    INTEGER :: i,sx,sy

    sx = SIZE(matrix,1)
    sy = SIZE(matrix,2)
    DO i = 1 , sx
      PRINT mformat,DBLE(matrix(i,:))
    ENDDO
  END SUBROUTINE print_zmatrix

  ! Print real valued matrix
  SUBROUTINE print_dmatrix(matrix, mformat)
    REAL*8 ,INTENT(IN) :: matrix(:,:)
    CHARACTER(*) :: mformat
    INTEGER :: i,sx,sy

    sx = SIZE(matrix,1)
    sy = SIZE(matrix,2)
    DO i = 1 , sx
      PRINT mformat,(matrix(i,:))
    ENDDO
  END SUBROUTINE print_dmatrix


  ! Reset current clock - see get clock
  SUBROUTINE reset_clock()
    CALL SYSTEM_CLOCK(COUNT=CURRENT_CLOCK)
  END SUBROUTINE reset_clock


  ! Get clock since last reset (in seconds)
  REAL FUNCTION get_clock() RESULT(c)
    INTEGER :: clock_rate,c_time
    CALL SYSTEM_CLOCK(COUNT_RATE=clock_rate)
    CALL SYSTEM_CLOCK(COUNT=c_time)
    c = (REAL(c_time) - CURRENT_CLOCK)/clock_rate
  END FUNCTION get_clock


  ! Print current date
  SUBROUTINE print_date()
    CHARACTER(8)  :: DATE
    CHARACTER(10) :: time
    CHARACTER(5)  :: zone
    INTEGER,DIMENSION(8) :: values

    ! using keyword arguments
    CALL DATE_AND_TIME(DATE,time,zone,values)
    CALL DATE_AND_TIME(DATE=DATE,ZONE=zone)
    CALL DATE_AND_TIME(TIME=time)
    CALL DATE_AND_TIME(VALUES=values)
    PRINT '(a,2x,a,A,2x,a)'," DATA:", DATE,"   TIME:", time
  END SUBROUTINE print_date


  ! Return string formatted date
  CHARACTER(20) FUNCTION get_date() RESULT(rval)
    CHARACTER(8)  :: DATE
    CHARACTER(10) :: time
    CHARACTER(5)  :: zone
    INTEGER,DIMENSION(8) :: values
    CHARACTER(20)  :: formatted_date
    ! using keyword arguments
    CALL DATE_AND_TIME(DATE,time,zone,values)
    CALL DATE_AND_TIME(DATE=DATE,ZONE=zone)
    CALL DATE_AND_TIME(TIME=time)
    CALL DATE_AND_TIME(VALUES=values)
    WRITE(formatted_date, *), DATE," ",time
    rval = TRIM(formatted_date)

  END FUNCTION get_date

END MODULE modutils
