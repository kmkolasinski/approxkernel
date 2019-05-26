MODULE modimageops
  IMPLICIT NONE
  PRIVATE

  TYPE SArray
    REAL(KIND=4),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE DArray
    REAL(KIND=8),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE CArray
    COMPLEX(KIND=4),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  TYPE ZArray
    COMPLEX(KIND=8),ALLOCATABLE, DIMENSION(:, :) :: array
  ENDTYPE

  INTERFACE allocate_array
    MODULE PROCEDURE allocate_array_s
    MODULE PROCEDURE allocate_array_d
    MODULE PROCEDURE allocate_array_c
    MODULE PROCEDURE allocate_array_z
  END INTERFACE

  INTERFACE deallocate_array
    MODULE PROCEDURE deallocate_array_s
    MODULE PROCEDURE deallocate_array_d
    MODULE PROCEDURE deallocate_array_c
    MODULE PROCEDURE deallocate_array_z
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

  INTERFACE resizebilinearinplace
    MODULE PROCEDURE resizebilinearinplace_s
    MODULE PROCEDURE resizebilinearinplace_d
    MODULE PROCEDURE resizebilinearinplace_c
    MODULE PROCEDURE resizebilinearinplace_z
  END INTERFACE

  INTERFACE resizebilinear
    MODULE PROCEDURE resizebilinear_s
    MODULE PROCEDURE resizebilinear_d
    MODULE PROCEDURE resizebilinear_c
    MODULE PROCEDURE resizebilinear_z
  END INTERFACE

  PUBLIC :: SArray, DArray, CArray, ZArray
  PUBLIC :: allocate_array, deallocate_array
  PUBLIC :: averagepool2x2, averagepool2x2inplace
  PUBLIC :: resizebilinearinplace, resizebilinear
CONTAINS

  SUBROUTINE allocate_array_s(array, array_shape)
    TYPE(SArray) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array_s(array)
    TYPE(SArray) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array_d(array, array_shape)
    TYPE(DArray) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array_d(array)
    TYPE(DArray) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array_c(array, array_shape)
    TYPE(CArray) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array_c(array)
    TYPE(CArray) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array_z(array, array_shape)
    TYPE(ZArray) :: array
    INTEGER, DIMENSION(2) :: array_shape

    CALL deallocate_array(array)
    ALLOCATE(array%array(array_shape(1), array_shape(2)))
  END SUBROUTINE

  SUBROUTINE deallocate_array_z(array)
    TYPE(ZArray) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE check_inputs(input_shape, output_shape, dynamic)
    INTEGER, DIMENSION(2), INTENT(IN) :: input_shape, output_shape
    LOGICAL :: dynamic

    IF (ANY(MOD(input_shape, 2) == 1)) THEN
      PRINT*,"modimageops::Input array must be of even shape:", input_shape
      STOP(-1)
    END IF

    IF  (.NOT. dynamic) THEN
      IF (ALL(input_shape/2 /= output_shape)) THEN
        PRINT*, "modimageops::In static mode, output_shape must be of half of input:"
        PRINT*, "   input_shape : ", input_shape
        PRINT*, "   output_shape: ", output_shape
        STOP(-1)
      ENDIF
    END IF

  END SUBROUTINE


  SUBROUTINE averagepool2x2inplace_s(input)
    REAL(4), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_d(input)
    REAL(8), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_c(input)
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2inplace_z(input)
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops/averagepool2x2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE averagepool2x2_s(input, output, dynamic)
    REAL(4), DIMENSION(:, :), INTENT(IN) :: input
    REAL(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_d(input, output, dynamic)
    REAL(8), DIMENSION(:, :), INTENT(IN) :: input
    REAL(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_c(input, output, dynamic)
    COMPLEX(4), DIMENSION(:, :), INTENT(IN) :: input
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2x2_z(input, output, dynamic)
    COMPLEX(8), DIMENSION(:, :), INTENT(IN) :: input
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops/averagepool2x2.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinear_s(input, output)
    REAL(4), DIMENSION(:, :) :: input
    REAL(4), DIMENSION(:, :) :: output
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops/resizebilinear.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear_d(input, output)
    REAL(8), DIMENSION(:, :) :: input
    REAL(8), DIMENSION(:, :) :: output
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops/resizebilinear.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear_c(input, output)
    COMPLEX(4), DIMENSION(:, :) :: input
    COMPLEX(4), DIMENSION(:, :) :: output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops/resizebilinear.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear_z(input, output)
    COMPLEX(8), DIMENSION(:, :) :: input
    COMPLEX(8), DIMENSION(:, :) :: output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops/resizebilinear.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinearinplace_s(input, target_shape)
    REAL(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops/resizebilinearinplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace_d(input, target_shape)
    REAL(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops/resizebilinearinplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace_c(input, target_shape)
    COMPLEX(4), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops/resizebilinearinplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace_z(input, target_shape)
    COMPLEX(8), DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(2) :: target_shape
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops/resizebilinearinplace.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE modimageops

