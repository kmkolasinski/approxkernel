MODULE modio
  IMPLICIT NONE
  PRIVATE

  ! Saves 2-D array to file
  INTERFACE save_array2d
    MODULE PROCEDURE I_save_array2d
    MODULE PROCEDURE D_save_array2d
    MODULE PROCEDURE S_save_array2d
  END INTERFACE save_array2d

  ! Read 2-d matrix from file
  INTERFACE read_array2d
    MODULE PROCEDURE I_read_array2d
    MODULE PROCEDURE D_read_array2d
    MODULE PROCEDURE S_read_array2d
  END INTERFACE read_array2d

  ! Reads Square matrix from file
  INTERFACE read_kernel
    MODULE PROCEDURE S_read_kernel
    MODULE PROCEDURE D_read_kernel
  END INTERFACE read_kernel

  ! Reads Number of matrixes of the same size.
  ! This function returns matrix [num_kernels, kernel_size, kernel_size]
  INTERFACE read_kernels
    MODULE PROCEDURE S_read_kernels
    MODULE PROCEDURE D_read_kernels
  END INTERFACE read_kernels

  PUBLIC :: read_array2d, save_array2d, read_kernel, read_kernels

CONTAINS

  SUBROUTINE S_read_kernels(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_kernels.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_kernels


  SUBROUTINE D_read_kernels(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_kernels.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_kernels


  ! Read square matrix from file
  SUBROUTINE D_read_kernel(filename, n, kernel)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_kernel.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_kernel


  SUBROUTINE S_read_kernel(filename, n, kernel)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_kernel.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_kernel


  ! Read matrix from file
  SUBROUTINE D_read_array2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_array2d


  SUBROUTINE S_read_array2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_array2d


  ! Read integer matrix from file
  SUBROUTINE I_read_array2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE INTEGER
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE I_read_array2d


  ! Save 2D array to file (integer)
  SUBROUTINE I_save_array2d(filename,array,asMatrix)
#DEFINE DTYPE INTEGER
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE I_save_array2d


  ! Save 2D array to file (double)
  SUBROUTINE D_save_array2d(filename,array,asMatrix)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE D_save_array2d


  SUBROUTINE S_save_array2d(filename,array,asMatrix)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE S_save_array2d

END MODULE modio
