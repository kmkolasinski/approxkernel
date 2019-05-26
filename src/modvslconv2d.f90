MODULE modvslconv2d
  USE MKL_VSL
  IMPLICIT NONE
  PRIVATE

  TYPE VSLConv2DParams
    TYPE(VSL_CONV_TASK) :: task
    INTEGER, DIMENSION(2) :: kernel_stride, xstride, ystride
    INTEGER, DIMENSION(2) :: kernel_shape, xshape, yshape
    INTEGER :: padding, mode
  ENDTYPE VSLConv2DParams

  ! stride (1, 1) padding SAME
  ! output of the convolution with padding
  TYPE VSLSConv2D
    REAL(KIND=4), DIMENSION(:, :), ALLOCATABLE :: output
    REAL(KIND=4), DIMENSION(:, :), ALLOCATABLE :: kernel
    TYPE(VSLConv2DParams) :: params
  ENDTYPE VSLSConv2D

  TYPE VSLDConv2D
    REAL(KIND=8), DIMENSION(:, :), ALLOCATABLE :: output
    REAL(KIND=8), DIMENSION(:, :), ALLOCATABLE :: kernel
    TYPE(VSLConv2DParams) :: params
  ENDTYPE VSLDConv2D

  TYPE VSLCConv2D
    COMPLEX(KIND=4), DIMENSION(:, :), ALLOCATABLE :: output
    COMPLEX(KIND=4), DIMENSION(:, :), ALLOCATABLE :: kernel
    TYPE(VSLConv2DParams) :: params
  ENDTYPE VSLCConv2D

  TYPE VSLZConv2D
    COMPLEX(KIND=8), DIMENSION(:, :), ALLOCATABLE :: output
    COMPLEX(KIND=8), DIMENSION(:, :), ALLOCATABLE :: kernel
    TYPE(VSLConv2DParams) :: params
  ENDTYPE VSLZConv2D

  INTERFACE vslconvnewtaskx2d
    MODULE PROCEDURE vslconvnewtaskx2d_s
    MODULE PROCEDURE vslconvnewtaskx2d_d
    MODULE PROCEDURE vslconvnewtaskx2d_c
    MODULE PROCEDURE vslconvnewtaskx2d_z
  END INTERFACE vslconvnewtaskx2d

  INTERFACE vslconvexecx2d
    MODULE PROCEDURE vslconvexecx2d_d
    MODULE PROCEDURE vslconvexecx2d_s
    MODULE PROCEDURE vslconvexecx2d_c
    MODULE PROCEDURE vslconvexecx2d_z
  END INTERFACE vslconvexecx2d

  INTERFACE initvslconv2d
    MODULE PROCEDURE initvslconv2d_s
    MODULE PROCEDURE initvslconv2d_d
    MODULE PROCEDURE initvslconv2d_c
    MODULE PROCEDURE initvslconv2d_z
    MODULE PROCEDURE initvslconv2d_d_z
  END INTERFACE initvslconv2d

  INTERFACE deletevslconv2d
    MODULE PROCEDURE deletevslconv2d_s
    MODULE PROCEDURE deletevslconv2d_d
    MODULE PROCEDURE deletevslconv2d_c
    MODULE PROCEDURE deletevslconv2d_z
  END INTERFACE deletevslconv2d

  INTERFACE execvslconv2d
    MODULE PROCEDURE execvslconv2d_s
    MODULE PROCEDURE execvslconv2d_d
    MODULE PROCEDURE execvslconv2d_c
    MODULE PROCEDURE execvslconv2d_z
  END INTERFACE execvslconv2d

  PUBLIC :: VSLSConv2D, VSLDConv2D, VSLCConv2D, VSLZConv2D
  PUBLIC :: initvslconv2d, deletevslconv2d, execvslconv2d

CONTAINS

  SUBROUTINE init_conv2d(params, kernel_shape, input_shape, mode)
    CLASS(VSLConv2DParams) :: params
    INTEGER, OPTIONAL :: mode
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, DIMENSION(2) :: kernel_shape, xshape, yshape
    INTEGER :: kernel_size, task_mode

    IF (.NOT. kernel_shape(1) == kernel_shape(2)) THEN
      PRINT*,"MODVSLDCONV2D::Kernel must be square!"
      STOP(-1)
    END IF

    IF (MOD(kernel_shape(1), 2) == 0) THEN
      PRINT*,"MODVSLDCONV2D::Kernel must be of odd shape!"
      STOP(-1)
    END IF

    task_mode = VSL_CONV_MODE_AUTO
    IF (PRESENT(mode)) task_mode = mode

    kernel_size = kernel_shape(1)
    xshape = input_shape ! input shape
    yshape = xshape + (/kernel_size - 1, kernel_size - 1/) ! output shape

    params%padding = INT(kernel_size / 2)
    params%kernel_stride = (/1, kernel_size/)
    params%xstride = (/1, input_shape(1)/)
    params%ystride = (/1, yshape(1)/)
    params%kernel_shape = kernel_shape
    params%xshape = xshape
    params%yshape = yshape
    params%mode = task_mode

  END SUBROUTINE init_conv2d

  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv2d_s(this, kernel, input_shape, mode)
    TYPE(VSLSConv2D), TARGET :: this
    REAL(4), DIMENSION(:, :), INTENT(IN) :: kernel
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv2d/initvslconv2d.template"
  END SUBROUTINE initvslconv2d_s


  SUBROUTINE deletevslconv2d_s(this)
    TYPE(VSLSConv2D) :: this
#INCLUDE "modvslconv2d/deletevslconv2d.template"
  END SUBROUTINE deletevslconv2d_s


  SUBROUTINE execvslconv2d_s(this, input, output)
    TYPE(VSLSConv2D), TARGET :: this
    REAL(KIND=4), DIMENSION(:, :) :: input, output
#INCLUDE "modvslconv2d/execvslconv2d.template"
  END SUBROUTINE


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv2d_d(this, kernel, input_shape, mode)
    TYPE(VSLDConv2D), TARGET :: this
    REAL(8), DIMENSION(:, :), INTENT(IN) :: kernel
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv2d/initvslconv2d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv2d_d(this)
    TYPE(VSLDConv2D) :: this
#INCLUDE "modvslconv2d/deletevslconv2d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv2d_d(this, input, output)
    TYPE(VSLDConv2D), TARGET :: this
    REAL(KIND=8), DIMENSION(:, :) :: input, output
#INCLUDE "modvslconv2d/execvslconv2d.template"
  END SUBROUTINE


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv2d_c(this, kernel, input_shape, mode)
    TYPE(VSLCConv2D), TARGET :: this
    COMPLEX(KIND=4), DIMENSION(:, :), INTENT(IN) :: kernel
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv2d/initvslconv2d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv2d_c(this)
    TYPE(VSLCConv2D) :: this
#INCLUDE "modvslconv2d/deletevslconv2d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv2d_c(this, input, output)
    TYPE(VSLCConv2D), TARGET :: this
    COMPLEX(KIND=4), DIMENSION(:, :) :: input, output
#INCLUDE "modvslconv2d/execvslconv2d.template"
  END SUBROUTINE

  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv2d_z(this, kernel, input_shape, mode)
    TYPE(VSLZConv2D), TARGET :: this
    COMPLEX(KIND=8), DIMENSION(:, :), INTENT(IN) :: kernel
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv2d/initvslconv2d.template"
  END SUBROUTINE


  SUBROUTINE deletevslconv2d_z(this)
    TYPE(VSLZConv2D) :: this
#INCLUDE "modvslconv2d/deletevslconv2d.template"
  END SUBROUTINE


  SUBROUTINE execvslconv2d_z(this, input, output)
    TYPE(VSLZConv2D), TARGET :: this
    COMPLEX(KIND=8), DIMENSION(:, :) :: input, output
#INCLUDE "modvslconv2d/execvslconv2d.template"
  END SUBROUTINE


  ! Initialize convolution
  ! mode can be VSL_CONV_MODE_AUTO, VSL_CONV_MODE_DIRECT, VSL_CONV_MODE_FFT
  SUBROUTINE initvslconv2d_d_z(this, kernel, input_shape, mode)
    TYPE(VSLZConv2D), TARGET :: this
    REAL(KIND=8), DIMENSION(:, :), INTENT(IN) :: kernel
    INTEGER, DIMENSION(2) :: input_shape
    INTEGER, OPTIONAL :: mode
#INCLUDE "modvslconv2d/initvslconv2d.template"
  END SUBROUTINE


  SUBROUTINE check_vslconvnewtaskx2d(info)
    INTEGER, INTENT(IN) :: info
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV2D:: creation of job failed, exit with ", info
      STOP 1
    ENDIF
  END SUBROUTINE


  SUBROUTINE check_vslconvexecx2d(info)
    INTEGER, INTENT(IN) :: info
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV2D:: job info bad, exit with", info
      STOP 1
    ENDIF
  END SUBROUTINE


  SUBROUTINE vslconvdeletetask2d(task)
    TYPE(VSL_CONV_TASK) :: task
    INTEGER :: info
    info = vslconvdeletetask(task)
    IF (info .NE. VSL_STATUS_OK) THEN
      PRINT *,"ERROR::MODVSLDCONV2D:: failed to delete task, exit with", info
      STOP 1
    ENDIF
  END SUBROUTINE


  INTEGER FUNCTION vslconvnewtaskx2d_d( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(2),yshape(2)
    INTEGER       :: zshape(2),xstride(2)
    REAL(8),DIMENSION(:, :):: x

    info = vsldconvnewtaskx(task,mode,2,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx2d_s( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(2),yshape(2)
    INTEGER       :: zshape(2),xstride(2)
    REAL(4),DIMENSION(:, :):: x

    info = vslsconvnewtaskx(task,mode,2,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx2d_z( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(2),yshape(2)
    INTEGER       :: zshape(2),xstride(2)
    COMPLEX(8),DIMENSION(:, :):: x

    info = vslzconvnewtaskx(task,mode,2,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvnewtaskx2d_c( task, mode, xshape,  &
      & yshape, zshape, x, xstride ) RESULT(info)

    TYPE(VSL_CONV_TASK) :: task
    INTEGER       :: mode
    INTEGER       :: xshape(2),yshape(2)
    INTEGER       :: zshape(2),xstride(2)
    COMPLEX(4),DIMENSION(:, :):: x

    info = vslcconvnewtaskx(task,mode,2,xshape,yshape,zshape,x,xstride)
    CALL check_vslconvnewtaskx2d(info)

  END FUNCTION


  INTEGER FUNCTION vslconvexecx2d_d( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)   :: task
    INTEGER,DIMENSION(*)   :: ystride,zstride
    REAL(KIND=8),DIMENSION(:, :):: y,z

    info = vsldconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx2d_s( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)   :: task
    INTEGER,DIMENSION(*)   :: ystride,zstride
    REAL(KIND=4),DIMENSION(:, :):: y,z

    info = vslsconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx2d_c( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)   :: task
    INTEGER,DIMENSION(*)   :: ystride,zstride
    COMPLEX(KIND=4),DIMENSION(:, :):: y,z

    info = vslcconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx2d(info)
  END FUNCTION


  INTEGER FUNCTION vslconvexecx2d_z( task, y, ystride, z, zstride ) RESULT(info)
    USE MKL_VSL
    TYPE(VSL_CONV_TASK)   :: task
    INTEGER,DIMENSION(*)   :: ystride, zstride
    COMPLEX(KIND=8),DIMENSION(:, :):: y,z

    info = vslzconvexecx(task, y, ystride, z, zstride)
    CALL check_vslconvexecx2d(info)
  END FUNCTION
END MODULE modvslconv2d
