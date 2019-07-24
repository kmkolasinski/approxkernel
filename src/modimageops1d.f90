MODULE modimageops1d
  IMPLICIT NONE
  PRIVATE

  TYPE SArray1D
    REAL(KIND=4),ALLOCATABLE, DIMENSION(:) :: array
  ENDTYPE

  TYPE DArray1D
    REAL(KIND=8),ALLOCATABLE, DIMENSION(:) :: array
  ENDTYPE

  TYPE CArray1D
    COMPLEX(KIND=4),ALLOCATABLE, DIMENSION(:) :: array
  ENDTYPE

  TYPE ZArray1D
    COMPLEX(KIND=8),ALLOCATABLE, DIMENSION(:) :: array
  ENDTYPE

  INTERFACE allocate_array1d
    MODULE PROCEDURE allocate_array1d_s
    MODULE PROCEDURE allocate_array1d_d
    MODULE PROCEDURE allocate_array1d_c
    MODULE PROCEDURE allocate_array1d_z
  END INTERFACE

  INTERFACE deallocate_array1d
    MODULE PROCEDURE deallocate_array1d_s
    MODULE PROCEDURE deallocate_array1d_d
    MODULE PROCEDURE deallocate_array1d_c
    MODULE PROCEDURE deallocate_array1d_z
  END INTERFACE

  INTERFACE averagepool2
    MODULE PROCEDURE averagepool2_s
    MODULE PROCEDURE averagepool2_d
    MODULE PROCEDURE averagepool2_c
    MODULE PROCEDURE averagepool2_z
  END INTERFACE

  INTERFACE averagepool2inplace
    MODULE PROCEDURE averagepool2inplace_s
    MODULE PROCEDURE averagepool2inplace_d
    MODULE PROCEDURE averagepool2inplace_c
    MODULE PROCEDURE averagepool2inplace_z
  END INTERFACE

  INTERFACE resizebilinearinplace1d
    MODULE PROCEDURE resizebilinearinplace1d_s
    MODULE PROCEDURE resizebilinearinplace1d_d
    MODULE PROCEDURE resizebilinearinplace1d_c
    MODULE PROCEDURE resizebilinearinplace1d_z
  END INTERFACE

  INTERFACE resizebilinear1d
    MODULE PROCEDURE resizebilinear1d_s
    MODULE PROCEDURE resizebilinear1d_d
    MODULE PROCEDURE resizebilinear1d_c
    MODULE PROCEDURE resizebilinear1d_z
  END INTERFACE

  PUBLIC :: SArray1D, DArray1D, CArray1D, ZArray1D
  PUBLIC :: allocate_array1d, deallocate_array1d
  PUBLIC :: averagepool2, averagepool2inplace
  PUBLIC :: resizebilinearinplace1d, resizebilinear1d
CONTAINS

  SUBROUTINE allocate_array1d_s(array, array_shape)
    TYPE(SArray1D) :: array
    INTEGER, DIMENSION(1) :: array_shape

    CALL deallocate_array1d(array)
    ALLOCATE(array%array(array_shape(1)))
  END SUBROUTINE

  SUBROUTINE deallocate_array1d_s(array)
    TYPE(SArray1D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array1d_d(array, array_shape)
    TYPE(DArray1D) :: array
    INTEGER, DIMENSION(1) :: array_shape

    CALL deallocate_array1d(array)
    ALLOCATE(array%array(array_shape(1)))
  END SUBROUTINE

  SUBROUTINE deallocate_array1d_d(array)
    TYPE(DArray1D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array1d_c(array, array_shape)
    TYPE(CArray1D) :: array
    INTEGER, DIMENSION(1) :: array_shape

    CALL deallocate_array1d(array)
    ALLOCATE(array%array(array_shape(1)))
  END SUBROUTINE

  SUBROUTINE deallocate_array1d_c(array)
    TYPE(CArray1D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE allocate_array1d_z(array, array_shape)
    TYPE(ZArray1D) :: array
    INTEGER, DIMENSION(1) :: array_shape

    CALL deallocate_array1d(array)
    ALLOCATE(array%array(array_shape(1)))
  END SUBROUTINE

  SUBROUTINE deallocate_array1d_z(array)
    TYPE(ZArray1D) :: array
    IF (ALLOCATED(array%array)) DEALLOCATE(array%array)
  END SUBROUTINE


  SUBROUTINE check_inputs(input_shape, output_shape, dynamic)
    INTEGER, DIMENSION(1), INTENT(IN) :: input_shape, output_shape
    LOGICAL :: dynamic

    IF (ANY(MOD(input_shape, 2) == 1)) THEN
      PRINT*,"modimageops1d::Input array must be of even shape:", input_shape
      STOP(-1)
    END IF

    IF  (.NOT. dynamic) THEN
      IF (ALL(input_shape/2 /= output_shape)) THEN
        PRINT*, "modimageops1d::In static mode, output_shape must be of half of input:"
        PRINT*, "   input_shape : ", input_shape
        PRINT*, "   output_shape: ", output_shape
        STOP(-1)
      ENDIF
    END IF

  END SUBROUTINE


  SUBROUTINE averagepool2inplace_s(input)
    REAL(4), DIMENSION(:), ALLOCATABLE :: input
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops1d/averagepool2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2inplace_d(input)
    REAL(8), DIMENSION(:), ALLOCATABLE :: input
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops1d/averagepool2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2inplace_c(input)
    COMPLEX(4), DIMENSION(:), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops1d/averagepool2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2inplace_z(input)
    COMPLEX(8), DIMENSION(:), ALLOCATABLE :: input
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops1d/averagepool2inplace.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE averagepool2_s(input, output, dynamic)
    REAL(4), DIMENSION(:), INTENT(IN) :: input
    REAL(4), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops1d/averagepool2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2_d(input, output, dynamic)
    REAL(8), DIMENSION(:), INTENT(IN) :: input
    REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops1d/averagepool2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2_c(input, output, dynamic)
    COMPLEX(4), DIMENSION(:), INTENT(IN) :: input
    COMPLEX(4), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops1d/averagepool2.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE averagepool2_z(input, output, dynamic)
    COMPLEX(8), DIMENSION(:), INTENT(IN) :: input
    COMPLEX(8), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: output
    LOGICAL, OPTIONAL :: dynamic
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops1d/averagepool2.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinear1d_s(input, output)
    REAL(4), DIMENSION(:) :: input
    REAL(4), DIMENSION(:) :: output
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops1d/resizebilinear1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear1d_d(input, output)
    REAL(8), DIMENSION(:) :: input
    REAL(8), DIMENSION(:) :: output
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops1d/resizebilinear1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear1d_c(input, output)
    COMPLEX(4), DIMENSION(:) :: input
    COMPLEX(4), DIMENSION(:) :: output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops1d/resizebilinear1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinear1d_z(input, output)
    COMPLEX(8), DIMENSION(:) :: input
    COMPLEX(8), DIMENSION(:) :: output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops1d/resizebilinear1d.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE resizebilinearinplace1d_s(input, target_shape)
    REAL(4), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(1) :: target_shape
#DEFINE DTYPE REAL(4)
#INCLUDE "modimageops1d/resizebilinearinplace1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace1d_d(input, target_shape)
    REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(1) :: target_shape
#DEFINE DTYPE REAL(8)
#INCLUDE "modimageops1d/resizebilinearinplace1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace1d_c(input, target_shape)
    COMPLEX(4), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(1) :: target_shape
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modimageops1d/resizebilinearinplace1d.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE resizebilinearinplace1d_z(input, target_shape)
    COMPLEX(8), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: input
    INTEGER, DIMENSION(1) :: target_shape
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modimageops1d/resizebilinearinplace1d.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE modimageops1d

