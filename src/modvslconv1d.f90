MODULE modvslconv1d
  USE MKL_VSL
  IMPLICIT NONE
  PRIVATE

  TYPE VSLConv1DParams
    TYPE(VSL_CONV_TASK) :: task
    INTEGER, DIMENSION(1) :: kernel_stride, xstride, ystride
    INTEGER, DIMENSION(1) :: kernel_shape, xshape, yshape
    INTEGER :: padding, mode
  ENDTYPE

  ! stride (1, 1) padding SAME
  ! output of the convolution with padding
  TYPE VSLSConv1D
    REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: output
    REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: kernel
    TYPE(VSLConv1DParams) :: params
  ENDTYPE

  TYPE VSLDConv1D
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: output
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: kernel
    TYPE(VSLConv1DParams) :: params
  ENDTYPE

  TYPE VSLCConv1D
    COMPLEX(KIND=4), DIMENSION(:), ALLOCATABLE :: output
    COMPLEX(KIND=4), DIMENSION(:), ALLOCATABLE :: kernel
    TYPE(VSLConv1DParams) :: params
  ENDTYPE

  TYPE VSLZConv1D
    COMPLEX(KIND=8), DIMENSION(:), ALLOCATABLE :: output
    COMPLEX(KIND=8), DIMENSION(:), ALLOCATABLE :: kernel
    TYPE(VSLConv1DParams) :: params
  ENDTYPE

  INTERFACE vslconvnewtaskx1d
    MODULE PROCEDURE vslconvnewtaskx1d_s
    MODULE PROCEDURE vslconvnewtaskx1d_d
    MODULE PROCEDURE vslconvnewtaskx1d_c
    MODULE PROCEDURE vslconvnewtaskx1d_z
  END INTERFACE

  INTERFACE vslconvexecx1d
    MODULE PROCEDURE vslconvexecx1d_d
    MODULE PROCEDURE vslconvexecx1d_s
    MODULE PROCEDURE vslconvexecx1d_c
    MODULE PROCEDURE vslconvexecx1d_z
  END INTERFACE

  INTERFACE initvslconv1d
    MODULE PROCEDURE initvslconv1d_s
    MODULE PROCEDURE initvslconv1d_d
    MODULE PROCEDURE initvslconv1d_c
    MODULE PROCEDURE initvslconv1d_z
    MODULE PROCEDURE initvslconv1d_d_z
  END INTERFACE

  INTERFACE deletevslconv1d
    MODULE PROCEDURE deletevslconv1d_s
    MODULE PROCEDURE deletevslconv1d_d
    MODULE PROCEDURE deletevslconv1d_c
    MODULE PROCEDURE deletevslconv1d_z
  END INTERFACE

  INTERFACE execvslconv1d
    MODULE PROCEDURE execvslconv1d_s
    MODULE PROCEDURE execvslconv1d_d
    MODULE PROCEDURE execvslconv1d_c
    MODULE PROCEDURE execvslconv1d_z
  END INTERFACE

  PUBLIC :: VSLSConv1D, VSLDConv1D, VSLCConv1D, VSLZConv1D
  PUBLIC :: initvslconv1d, deletevslconv1d, execvslconv1d

CONTAINS

  ! Initialize Convolution to work in 1D case
  SUBROUTINE init_conv1d(params, kernel_size, input_size, mode)
    CLASS(VSLConv1DParams) :: params
    INTEGER, OPTIONAL :: mode
    INTEGER :: input_size, output_size, kernel_size
    INTEGER :: task_mode

    IF (MOD(kernel_size, 2) == 0) THEN
      PRINT*,"MODVSLCONV1D::Kernel must be of odd size! "&
          "Input kernel size:", kernel_size
      STOP(-1)
    END IF

    task_mode = VSL_CONV_MODE_AUTO
    IF (PRESENT(mode)) task_mode = mode

    output_size = input_size + kernel_size - 1

    params%padding = INT(kernel_size / 2)
    params%kernel_stride = (/ 1 /)
    params%xstride = (/ 1 /)
    params%ystride = (/ 1 /)

    params%kernel_shape = (/ kernel_size /)
    params%xshape = (/ input_size /) ! input shape
    params%yshape = (/ output_size /) ! output shape
    params%mode = task_mode

  END SUBROUTINE init_conv1d


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv1d_s(this, kernel, input_size, mode)
    TYPE(VSLSConv1D), TARGET :: this
    REAL(4), DIMENSION(:), INTENT(IN) :: kernel
    INTEGER :: input_size
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv1d/initvslconv1d.template"
  END SUBROUTINE initvslconv1d_s


  SUBROUTINE deletevslconv1d_s(this)
    TYPE(VSLSConv1D) :: this
#INCLUDE "modvslconv1d/deletevslconv1d.template"
  END SUBROUTINE deletevslconv1d_s


  SUBROUTINE execvslconv1d_s(this, input, output)
    TYPE(VSLSConv1D), TARGET :: this
    REAL(KIND=4), DIMENSION(:) :: input, output
#INCLUDE "modvslconv1d/execvslconv1d.template"
  END SUBROUTINE


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv1d_d(this, kernel, input_size, mode)
    TYPE(VSLDConv1D), TARGET :: this
    REAL(8), DIMENSION(:), INTENT(IN) :: kernel
    INTEGER :: input_size
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv1d/initvslconv1d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv1d_d(this)
    TYPE(VSLDConv1D) :: this
#INCLUDE "modvslconv1d/deletevslconv1d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv1d_d(this, input, output)
    TYPE(VSLDConv1D), TARGET :: this
    REAL(KIND=8), DIMENSION(:) :: input, output
#INCLUDE "modvslconv1d/execvslconv1d.template"
  END SUBROUTINE


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv1d_c(this, kernel, input_size, mode)
    TYPE(VSLCConv1D), TARGET :: this
    COMPLEX(KIND=4), DIMENSION(:), INTENT(IN) :: kernel
    INTEGER :: input_size
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv1d/initvslconv1d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv1d_c(this)
    TYPE(VSLCConv1D) :: this
#INCLUDE "modvslconv1d/deletevslconv1d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv1d_c(this, input, output)
    TYPE(VSLCConv1D), TARGET :: this
    COMPLEX(KIND=4), DIMENSION(:) :: input, output
#INCLUDE "modvslconv1d/execvslconv1d.template"
  END SUBROUTINE

  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv1d_z(this, kernel, input_size, mode)
    TYPE(VSLZConv1D), TARGET :: this
    COMPLEX(KIND=8), DIMENSION(:), INTENT(IN) :: kernel
    INTEGER :: input_size
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv1d/initvslconv1d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv1d_z(this)
    TYPE(VSLZConv1D) :: this
#INCLUDE "modvslconv1d/deletevslconv1d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv1d_z(this, input, output)
    TYPE(VSLZConv1D), TARGET :: this
    COMPLEX(KIND=8), DIMENSION(:) :: input, output
#INCLUDE "modvslconv1d/execvslconv1d.template"
  END SUBROUTINE

  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv1d_d_z(this, kernel, input_size, mode)
    TYPE(VSLZConv1D), TARGET :: this
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: kernel
    INTEGER :: input_size
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv1d/initvslconv1d.template"
  END SUBROUTINE


  SUBROUTINE check_vslconvnewtaskx1d(info)
    INTEGER, INTENT(IN) :: info
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV1D:: creation of job failed, exit with ", info
      STOP 1
    ENDIF
  END SUBROUTINE


  SUBROUTINE check_vslconvexecx1d(info)
    INTEGER, INTENT(IN) :: info
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV1D:: job info bad, exit with", info
      STOP 1
    ENDIF
  END SUBROUTINE


  SUBROUTINE vslconvdeletetask1d(task)
    TYPE(VSL_CONV_TASK) :: task
    INTEGER :: info
    info = vslconvdeletetask(task)
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV1D:: failed to delete task, exit with", info
      STOP 1
    ENDIF
  END SUBROUTINE


  INTEGER FUNCTION vslconvnewtaskx1d_d( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(1),yshape(1)
    INTEGER       :: zshape(1),xstride(1)
    REAL(8),DIMENSION(:) :: x

    info = vsldconvnewtaskx(task,mode,1,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx1d_s( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(1),yshape(1)
    INTEGER       :: zshape(1),xstride(1)
    REAL(4),DIMENSION(:):: x

    info = vslsconvnewtaskx(task,mode,1,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx1d_z( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(1),yshape(1)
    INTEGER       :: zshape(1),xstride(1)
    COMPLEX(8),DIMENSION(:):: x

    info = vslzconvnewtaskx(task,mode,1,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx1d_c( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(1),yshape(1)
    INTEGER       :: zshape(1),xstride(1)
    COMPLEX(4),DIMENSION(:):: x

    info = vslcconvnewtaskx(task,mode,1,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx1d(info)

  END FUNCTION


  INTEGER FUNCTION vslconvexecx1d_d( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)       :: task
    INTEGER,DIMENSION(*)      :: ystride, zstride
    REAL(KIND=8),DIMENSION(:) :: y,z

    info = vsldconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx1d_s( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)       :: task
    INTEGER,DIMENSION(*)      :: ystride,zstride
    REAL(KIND=4),DIMENSION(:) :: y,z

    info = vslsconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx1d_c( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)          :: task
    INTEGER,DIMENSION(*)         :: ystride, zstride
    COMPLEX(KIND=4),DIMENSION(:) :: y, z

    info = vslcconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx1d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx1d_z( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)          :: task
    INTEGER,DIMENSION(*)         :: ystride, zstride
    COMPLEX(KIND=8),DIMENSION(:) :: y,z

    info = vslzconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx1d(info)
  END FUNCTION


END MODULE modvslconv1d
