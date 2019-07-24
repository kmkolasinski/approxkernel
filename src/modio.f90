MODULE modio
  IMPLICIT NONE
  PRIVATE

  ! Saves 2-D array to file
  INTERFACE save_array_2d
    MODULE PROCEDURE I_save_array_2d
    MODULE PROCEDURE D_save_array_2d
    MODULE PROCEDURE S_save_array_2d
  END INTERFACE save_array_2d

  ! Read 2-d matrix from file
  INTERFACE read_array_2d
    MODULE PROCEDURE I_read_array_2d
    MODULE PROCEDURE D_read_array_2d
    MODULE PROCEDURE S_read_array_2d
  END INTERFACE read_array_2d

  ! Reads Square matrix from file
  INTERFACE read_kernel_2d
    MODULE PROCEDURE S_read_kernel_2d
    MODULE PROCEDURE D_read_kernel_2d
  END INTERFACE read_kernel_2d

  ! Reads Number of matrixes of the same size.
  ! This function returns matrix [num_kernels, kernel_size, kernel_size]
  INTERFACE read_kernels_2d
    MODULE PROCEDURE S_read_kernels_2d
    MODULE PROCEDURE D_read_kernels_2d
  END INTERFACE read_kernels_2d

  INTERFACE read_kernels_1d
    MODULE PROCEDURE S_read_kernels_1d
    MODULE PROCEDURE D_read_kernels_1d
  END INTERFACE read_kernels_1d

  ! Saves 1-D array to file
  INTERFACE save_array_1d
    MODULE PROCEDURE I_save_array_1d
    MODULE PROCEDURE D_save_array_1d
    MODULE PROCEDURE S_save_array_1d
  END INTERFACE save_array_1d

  PUBLIC :: read_array_2d, save_array_2d
  PUBLIC :: read_kernel_2d, read_kernels_2d
  PUBLIC :: save_array_1d, read_kernels_1d

CONTAINS

  SUBROUTINE S_read_kernels_2d(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_kernels.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_kernels_2d


  SUBROUTINE D_read_kernels_2d(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_kernels.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_kernels_2d


  ! Read square matrix from file
  SUBROUTINE D_read_kernel_2d(filename, n, kernel)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_kernel.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_kernel_2d


  SUBROUTINE S_read_kernel_2d(filename, n, kernel)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_kernel.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_kernel_2d


  ! Read matrix from file
  SUBROUTINE D_read_array_2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_array_2d


  SUBROUTINE S_read_array_2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_array_2d


  ! Read integer matrix from file
  SUBROUTINE I_read_array_2d(FileName, NumRows, NumCols, Array )
#DEFINE DTYPE INTEGER
#INCLUDE "modio/read_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE I_read_array_2d


  ! Save 2D array to file (integer)
  SUBROUTINE I_save_array_2d(filename,array,asMatrix)
#DEFINE DTYPE INTEGER
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE I_save_array_2d


  ! Save 2D array to file (double)
  SUBROUTINE D_save_array_2d(filename,array,asMatrix)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE D_save_array_2d


  SUBROUTINE S_save_array_2d(filename,array,asMatrix)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/save_array2d.template"
#UNDEF DTYPE
  END SUBROUTINE S_save_array_2d

  ! ----------------------------------------------------
  !                 Reading 1D Kernels
  ! ----------------------------------------------------
  SUBROUTINE S_read_kernels_1d(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/read_kernels_1d.template"
#UNDEF DTYPE
  END SUBROUTINE S_read_kernels_1d


  SUBROUTINE D_read_kernels_1d(filename, num_kernels, kernel_size, kernels)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/read_kernels_1d.template"
#UNDEF DTYPE
  END SUBROUTINE D_read_kernels_1d

  ! ----------------------------------------------------
  !                 Save 1D Array
  ! ----------------------------------------------------
  ! Save 1D array to file (integer)
  SUBROUTINE I_save_array_1d(filename,array)
#DEFINE DTYPE INTEGER
#INCLUDE "modio/save_array1d.template"
#UNDEF DTYPE
  END SUBROUTINE I_save_array_1d


  ! Save 2D array to file (double)
  SUBROUTINE D_save_array_1d(filename,array)
#DEFINE DTYPE REAL(8)
#INCLUDE "modio/save_array1d.template"
#UNDEF DTYPE
  END SUBROUTINE D_save_array_1d


  SUBROUTINE S_save_array_1d(filename,array)
#DEFINE DTYPE REAL(4)
#INCLUDE "modio/save_array1d.template"
#UNDEF DTYPE
  END SUBROUTINE S_save_array_1d

END MODULE modio
