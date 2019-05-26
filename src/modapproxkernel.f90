MODULE modapproxkernel
  USE modio
  USE modimageops
  USE modvslconv2d
  USE MKL_VSL
  IMPLICIT NONE

  PRIVATE

  TYPE ApproxSKernelData
    TYPE(VSLSConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLSConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(SArray), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxDKernelData
    TYPE(VSLDConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLDConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(DArray), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxCKernelData
    TYPE(VSLCConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLCConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(CArray), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  TYPE ApproxZKernelData
    TYPE(VSLZConv2D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLZConv2D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(ZArray), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(2) :: input_shape
  ENDTYPE

  INTERFACE deleteapproxkernel
    MODULE PROCEDURE deleteapproxkernel_s
    MODULE PROCEDURE deleteapproxkernel_d
    MODULE PROCEDURE deleteapproxkernel_c
    MODULE PROCEDURE deleteapproxkernel_z
  END INTERFACE

  INTERFACE initapproxkernel
    MODULE PROCEDURE initapproxkernel_s
    MODULE PROCEDURE initapproxkernel_d
    MODULE PROCEDURE initapproxkernel_c
    MODULE PROCEDURE initapproxkernel_z
    MODULE PROCEDURE initapproxkernel_dz
  END INTERFACE

  INTERFACE execapproxkernel
    MODULE PROCEDURE execapproxkernel_s
    MODULE PROCEDURE execapproxkernel_d
    MODULE PROCEDURE execapproxkernel_c
    MODULE PROCEDURE execapproxkernel_z
  END INTERFACE

  public :: initapproxkernel_dz
  PUBLIC :: ApproxSKernelData, ApproxDKernelData
  PUBLIC :: ApproxCKernelData, ApproxZKernelData
  PUBLIC :: initapproxkernel, deleteapproxkernel, execapproxkernel

CONTAINS


  SUBROUTINE deleteapproxkernel_s(kernel_obj)
    TYPE(ApproxSKernelData) :: kernel_obj
#INCLUDE "modapproxkernel/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_d(kernel_obj)
    TYPE(ApproxDKernelData) :: kernel_obj
#INCLUDE "modapproxkernel/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_c(kernel_obj)
    TYPE(ApproxCKernelData) :: kernel_obj
#INCLUDE "modapproxkernel/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_z(kernel_obj)
    TYPE(ApproxZKernelData) :: kernel_obj
#INCLUDE "modapproxkernel/deleteapproxkernel.template"
  END SUBROUTINE

  ! Initialize approximate-kernel_obj integral
  SUBROUTINE initapproxkernel_s(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxSKernelData) :: kernel_obj
    REAL(KIND=4), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_d(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxDKernelData) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_c(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxCKernelData) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_z(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxZKernelData) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_dz(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxZKernelData) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :, :) :: kernels
    INTEGER, DIMENSION(2) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE execapproxkernel_s(kernel_obj, input, output)
    TYPE(ApproxSKernelData) :: kernel_obj
    REAL(KIND=4), DIMENSION(:, :) :: input, output
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_d(kernel_obj, input, output)
    TYPE(ApproxDKernelData) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :) :: input, output
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_c(kernel_obj, input, output)
    TYPE(ApproxCKernelData) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:, :) :: input, output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_z(kernel_obj, input, output)
    TYPE(ApproxZKernelData) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:, :) :: input, output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE modapproxkernel
