MODULE modimageops2d
  IMPLICIT NONE
  PRIVATE

  TYPE SArray2D
    REAL(KIND=4),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE DArray2D
    REAL(KIND=8),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE CArray2D
    COMPLEX(KIND=4),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE ZArray2D
    COMPLEX(KIND=8),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  INTERFACE allocate_array2d
    MODULE PROCEDURE allocate_array2d_s
    MODULE PROCEDURE allocate_array2d_d
    MODULE PROCEDURE allocate_array2d_c
    MODULE PROCEDURE allocate_array2d_z
  END INTERFACE

  INTERFACE deallocate_array2d
    MODULE PROCEDURE deallocate_array2d_s
    MODULE PROCEDURE deallocate_array2d_d
    MODULE PROCEDURE deallocate_array2d_c
    MODULE PROCEDURE deallocate_array2d_z
  END INTERFACE

  INTERFACE averagepool2x2
    MODULE PROCEDURE averagepool2x2_s
    MODULE PROCEDURE averagepool2x2_d
    MODULE PROCEDURE averagepool2x2_c
    MODULE PROCEDURE averagepool2x2_z
  END INTERFACE

  INTERFACE averagepool2x2inplace
    MODULE PROCEDURE averagepool2x2inplace_s
    MODULE PROCEDURE averagepool2x2inplace_d
    MODULE PROCEDURE averagepool2x2inplace_c
    MODULE PROCEDURE averagepool2x2inplace_z
  END INTERFACE

  INTERFACE resizebilinearinplace2d
    MODULE PROCEDURE resizebilinearinplace2d_s
    MODULE PROCEDURE resizebilinearinplace2d_d
    MODULE PROCEDURE resizebilinearinplace2d_c
    MODULE PROCEDURE resizebilinearinplace2d_z
  END INTERFACE

  INTERFACE resizebilinear2d
    MODULE PROCEDURE resizebilinear2d_s
    MODULE PROCEDURE resizebilinear2d_d
    MODULE PROCEDURE resizebilinear2d_c
    MODULE PROCEDURE resizebilinear2d_z
  END INTERFACE

  PUBLIC :: SArray2D, DArray2D, CArray2D, ZArray2D
  PUBLIC :: allocate_array2d, deallocate_array2d
  PUBLIC :: averagepool2x2, averagepool2x2inplace
  PUBLIC :: resizebilinearinplace2d, resizebilinear2d

CONTAINS

  SUBROUTINE allocate_array2d_s(array, array_shape)
    TYPE(SArray2D) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array2d(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array2d_s(array)
    TYPE(SArray2D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array2d_d(array, array_shape)
    TYPE(DArray2D) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array2d(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array2d_d(array)
    TYPE(DArray2D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array2d_c(array, array_shape)
    TYPE(CArray2D) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array2d(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array2d_c(array)
    TYPE(CArray2D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array2d_z(array, array_shape)
    TYPE(ZArray2D) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array2d(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array2d_z(array)
    TYPE(ZArray2D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE check_inputs(input_shape, output_shape, dynamic)
    INTEGER, DIMENSION(2), INTENT(IN) :: input_shape, output_shape
    LOGICAL :: dynamic

    IF (ANY(MOD(input_shape, 2) == 1)) THEN
      PRINT*,"modimageops2d::Input array must be of even shape:", input_shape
      STOP(-1)
    END IF

    IF  (.NOT. dynamic) THEN
      IF (ALL(input_shape/2 /= output_shape)) THEN
        PRINT*, "modimageops2d::In static mode, output_shape must be of half of input:"
        PRINT*, "   input_shape : ", input_shape
        PRINT*, "   output_shape: ", output_shape
        STOP(-1)
      ENDIF
    END IF

  END SUBROUTINE


  SUBROUTINE averagepool2x2inplace_s(input)
    REAL(4), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops2d/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_d(input)
    REAL(8), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops2d/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_c(input)
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops2d/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_z(input)
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops2d/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE averagepool2x2_s(input, output, dynamic)
    REAL(4), DIMENSION(:, :), INTENT(IN) :: input
    REAL(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops2d/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_d(input, output, dynamic)
    REAL(8), DIMENSION(:, :), INTENT(IN) :: input
    REAL(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops2d/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_c(input, output, dynamic)
    COMPLEX(4), DIMENSION(:, :), INTENT(IN) :: input
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops2d/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_z(input, output, dynamic)
    COMPLEX(8), DIMENSION(:, :), INTENT(IN) :: input
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops2d/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinear2d_s(input, output)
    REAL(4), DIMENSION(:, :) :: input
    REAL(4), DIMENSION(:, :) :: output
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops2d/resizebilinear2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear2d_d(input, output)
    REAL(8), DIMENSION(:, :) :: input
    REAL(8), DIMENSION(:, :) :: output
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops2d/resizebilinear2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear2d_c(input, output)
    COMPLEX(4), DIMENSION(:, :) :: input
    COMPLEX(4), DIMENSION(:, :) :: output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops2d/resizebilinear2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear2d_z(input, output)
    COMPLEX(8), DIMENSION(:, :) :: input
    COMPLEX(8), DIMENSION(:, :) :: output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops2d/resizebilinear2d.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinearinplace2d_s(input, target_shape)
    REAL(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops2d/resizebilinearinplace2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace2d_d(input, target_shape)
    REAL(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops2d/resizebilinearinplace2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace2d_c(input, target_shape)
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops2d/resizebilinearinplace2d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace2d_z(input, target_shape)
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops2d/resizebilinearinplace2d.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE modimageops2d

