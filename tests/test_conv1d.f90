PROGRAM example
  USE modutils
  USE modio
  USE modvslconv1d

  IMPLICIT NONE

  call test_conv1d_ss()
  call test_conv1d_dd()
  call test_conv1d_cc()
  call test_conv1d_zz()
  call test_conv1d_zd()

CONTAINS


  SUBROUTINE test_conv1d_ss()
    TYPE(VSLSConv1D) :: conv1d
    character(*), parameter :: conv_type = "VSLSConv1D"
#DEFINE DTYPE REAL(4)
#DEFINE KTYPE REAL(4)
#INCLUDE "test_conv1d.template"
#UNDEF DTYPE
#UNDEF KTYPE
  END SUBROUTINE


  SUBROUTINE test_conv1d_dd()
    TYPE(VSLDConv1D) :: conv1d
    character(*), parameter :: conv_type = "VSLDConv1D"
#DEFINE DTYPE REAL(8)
#DEFINE KTYPE REAL(8)
#INCLUDE "test_conv1d.template"
#UNDEF DTYPE
#UNDEF KTYPE
  END SUBROUTINE


  SUBROUTINE test_conv1d_cc()
    TYPE(VSLCConv1D) :: conv1d
    character(*), parameter :: conv_type = "VSLCConv1D"
#DEFINE DTYPE COMPLEX(4)
#DEFINE KTYPE COMPLEX(4)
#INCLUDE "test_conv1d.template"
#UNDEF DTYPE
#UNDEF KTYPE
  END SUBROUTINE


  SUBROUTINE test_conv1d_zz()
    TYPE(VSLZConv1D) :: conv1d
    character(*), parameter :: conv_type = "VSLZConv1D"
#DEFINE DTYPE COMPLEX(8)
#DEFINE KTYPE COMPLEX(8)
#INCLUDE "test_conv1d.template"
#UNDEF DTYPE
#UNDEF KTYPE
  END SUBROUTINE


  SUBROUTINE test_conv1d_zd()
    TYPE(VSLZConv1D) :: conv1d
    character(*), parameter :: conv_type = "VSLZConv1D (REAL KERNEL)"
#DEFINE DTYPE COMPLEX(8)
#DEFINE KTYPE REAL(8)
#INCLUDE "test_conv1d.template"
#UNDEF DTYPE
#UNDEF KTYPE
  END SUBROUTINE

END PROGRAM

