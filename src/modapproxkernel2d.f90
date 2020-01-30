MODULE modapproxkernel2d
  USE modio
  USE modimageops2d
  USE modvslconv2d
  USE MKL_VSL
  IMPLICIT NONE

  PRIVATE

  TYPE ApproxSKernel2D
    TYPE(VSLSConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLSConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(SArray2D), ALLOCATABLE, DIMENSION(:) :: inputs
    TYPE(SArray2D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxDKernel2D
    TYPE(VSLDConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLDConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(DArray2D), ALLOCATABLE, DIMENSION(:) :: inputs
    TYPE(DArray2D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxCKernel2D
    TYPE(VSLCConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLCConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(CArray2D), ALLOCATABLE, DIMENSION(:) :: inputs
    TYPE(CArray2D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxZKernel2D
    TYPE(VSLZConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLZConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(ZArray2D), ALLOCATABLE, DIMENSION(:) :: inputs
    TYPE(ZArray2D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  INTERFACE deleteapproxkernel2d
    MODULE PROCEDURE deleteapproxkernel_s
    MODULE PROCEDURE deleteapproxkernel_d
    MODULE PROCEDURE deleteapproxkernel_c
    MODULE PROCEDURE deleteapproxkernel_z
  END INTERFACE

  INTERFACE initapproxkernel2d
    MODULE PROCEDURE initapproxkernel_s
    MODULE PROCEDURE initapproxkernel_d
    MODULE PROCEDURE initapproxkernel_c
    MODULE PROCEDURE initapproxkernel_z
    MODULE PROCEDURE initapproxkernel_dz
  END INTERFACE

  INTERFACE execapproxkernel2d
    MODULE PROCEDURE execapproxkernel_s
    MODULE PROCEDURE execapproxkernel_d
    MODULE PROCEDURE execapproxkernel_c
    MODULE PROCEDURE execapproxkernel_z
  END INTERFACE

  PUBLIC :: ApproxSKernel2D, ApproxDKernel2D
  PUBLIC :: ApproxCKernel2D, ApproxZKernel2D
  PUBLIC :: initapproxkernel2d, deleteapproxkernel2d, execapproxkernel2d

CONTAINS


  SUBROUTINE deleteapproxkernel_s(kernel_obj)
    TYPE(ApproxSKernel2D) :: kernel_obj
#INCLUDE "modapproxkernel2d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_d(kernel_obj)
    TYPE(ApproxDKernel2D) :: kernel_obj
#INCLUDE "modapproxkernel2d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_c(kernel_obj)
    TYPE(ApproxCKernel2D) :: kernel_obj
#INCLUDE "modapproxkernel2d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_z(kernel_obj)
    TYPE(ApproxZKernel2D) :: kernel_obj
#INCLUDE "modapproxkernel2d/deleteapproxkernel.template"
  END SUBROUTINE

  ! Initialize approximate-kernel_obj integral
  SUBROUTINE initapproxkernel_s(kernel_obj, kernels, input_shape, use_smoothing, mode)
    TYPE(ApproxSKernel2D) :: kernel_obj
    REAL(KIND=4), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
    INTEGER, OPTIONAL :: mode
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel2d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_d(kernel_obj, kernels, input_shape, use_smoothing, mode)
    TYPE(ApproxDKernel2D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
    INTEGER, OPTIONAL :: mode
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel2d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_c(kernel_obj, kernels, input_shape, use_smoothing, mode)
    TYPE(ApproxCKernel2D) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
    INTEGER, OPTIONAL :: mode
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel2d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_z(kernel_obj, kernels, input_shape, use_smoothing, mode)
    TYPE(ApproxZKernel2D) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
    INTEGER, OPTIONAL :: mode
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel2d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_dz(kernel_obj, kernels, input_shape, use_smoothing, mode)
    TYPE(ApproxZKernel2D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
    INTEGER, OPTIONAL :: mode
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel2d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE execapproxkernel_s(kernel_obj, input, output)
    TYPE(ApproxSKernel2D) :: kernel_obj
    REAL(KIND=4), DIMENSION(:, :) :: input, output
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel2d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_d(kernel_obj, input, output)
    TYPE(ApproxDKernel2D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :) :: input, output
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel2d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_c(kernel_obj, input, output)
    TYPE(ApproxCKernel2D) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:, :) :: input, output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel2d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_z(kernel_obj, input, output)
    TYPE(ApproxZKernel2D) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:, :) :: input, output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel2d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE modapproxkernel2d
