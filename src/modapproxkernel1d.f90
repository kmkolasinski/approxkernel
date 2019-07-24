MODULE modapproxkernel1d
  USE modio
  USE modimageops1d
  USE modvslconv1d
  USE MKL_VSL
  IMPLICIT NONE

  PRIVATE

  TYPE ApproxSKernel1D
    TYPE(VSLSConv1D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLSConv1D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(SArray1D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(1) :: input_shape
  ENDTYPE

  TYPE ApproxDKernel1D
    TYPE(VSLDConv1D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLDConv1D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(DArray1D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(1) :: input_shape
  ENDTYPE

  TYPE ApproxCKernel1D
    TYPE(VSLCConv1D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLCConv1D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(CArray1D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(1) :: input_shape
  ENDTYPE

  TYPE ApproxZKernel1D
    TYPE(VSLZConv1D), ALLOCATABLE, DIMENSION(:) :: kernels_convs
    TYPE(VSLZConv1D), ALLOCATABLE, DIMENSION(:) :: smoothing_convs
    TYPE(ZArray1D), ALLOCATABLE, DIMENSION(:) :: outputs
    INTEGER :: num_scales
    LOGICAL :: use_smoothing
    INTEGER, DIMENSION(1) :: input_shape
  ENDTYPE

  INTERFACE deleteapproxkernel1d
    MODULE PROCEDURE deleteapproxkernel_s
    MODULE PROCEDURE deleteapproxkernel_d
    MODULE PROCEDURE deleteapproxkernel_c
    MODULE PROCEDURE deleteapproxkernel_z
  END INTERFACE

  INTERFACE initapproxkernel1d
    MODULE PROCEDURE initapproxkernel_s
    MODULE PROCEDURE initapproxkernel_d
    MODULE PROCEDURE initapproxkernel_c
    MODULE PROCEDURE initapproxkernel_z
    MODULE PROCEDURE initapproxkernel_dz
  END INTERFACE

  INTERFACE execapproxkernel1d
    MODULE PROCEDURE execapproxkernel_s
    MODULE PROCEDURE execapproxkernel_d
    MODULE PROCEDURE execapproxkernel_c
    MODULE PROCEDURE execapproxkernel_z
  END INTERFACE

  PUBLIC :: ApproxSKernel1D, ApproxDKernel1D
  PUBLIC :: ApproxCKernel1D, ApproxZKernel1D
  PUBLIC :: initapproxkernel1d, deleteapproxkernel1d, execapproxkernel1d

CONTAINS


  SUBROUTINE deleteapproxkernel_s(kernel_obj)
    TYPE(ApproxSKernel1D) :: kernel_obj
#INCLUDE "modapproxkernel1d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_d(kernel_obj)
    TYPE(ApproxDKernel1D) :: kernel_obj
#INCLUDE "modapproxkernel1d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_c(kernel_obj)
    TYPE(ApproxCKernel1D) :: kernel_obj
#INCLUDE "modapproxkernel1d/deleteapproxkernel.template"
  END SUBROUTINE

  SUBROUTINE deleteapproxkernel_z(kernel_obj)
    TYPE(ApproxZKernel1D) :: kernel_obj
#INCLUDE "modapproxkernel1d/deleteapproxkernel.template"
  END SUBROUTINE

  ! Initialize approximate-kernel_obj integral
  SUBROUTINE initapproxkernel_s(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxSKernel1D) :: kernel_obj
    REAL(KIND=4), DIMENSION(:, :) :: kernels
    INTEGER, DIMENSION(1) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel1d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_d(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxDKernel1D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :) :: kernels
    INTEGER, DIMENSION(1) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel1d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_c(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxCKernel1D) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:, :) :: kernels
    INTEGER, DIMENSION(1) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel1d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_z(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxZKernel1D) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:, :) :: kernels
    INTEGER, DIMENSION(1) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel1d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE initapproxkernel_dz(kernel_obj, kernels, input_shape, use_smoothing)
    TYPE(ApproxZKernel1D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:, :) :: kernels
    INTEGER, DIMENSION(1) :: input_shape
    LOGICAL, OPTIONAL :: use_smoothing
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel1d/initapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE


  SUBROUTINE execapproxkernel_s(kernel_obj, input, output)
    TYPE(ApproxSKernel1D) :: kernel_obj
    REAL(KIND=4), DIMENSION(:) :: input, output
#DEFINE DTYPE REAL(4)
#INCLUDE "modapproxkernel1d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_d(kernel_obj, input, output)
    TYPE(ApproxDKernel1D) :: kernel_obj
    REAL(KIND=8), DIMENSION(:) :: input, output
#DEFINE DTYPE REAL(8)
#INCLUDE "modapproxkernel1d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_c(kernel_obj, input, output)
    TYPE(ApproxCKernel1D) :: kernel_obj
    COMPLEX(KIND=4), DIMENSION(:) :: input, output
#DEFINE DTYPE COMPLEX(4)
#INCLUDE "modapproxkernel1d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

  SUBROUTINE execapproxkernel_z(kernel_obj, input, output)
    TYPE(ApproxZKernel1D) :: kernel_obj
    COMPLEX(KIND=8), DIMENSION(:) :: input, output
#DEFINE DTYPE COMPLEX(8)
#INCLUDE "modapproxkernel1d/execapproxkernel.template"
#UNDEF DTYPE
  END SUBROUTINE

END MODULE
