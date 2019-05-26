PROGRAM example
  USE modutils
  USE modio
  USE modapproxkernel
  IMPLICIT NONE
  INTEGER :: num_samples = 500
  INTEGER, PARAMETER :: grid_size = 128, kernel_size = 31, num_kernels = 6

  CALL run_benchmark_ss()
  CALL run_benchmark_dd()
  CALL run_benchmark_cc()
  CALL run_benchmark_zz()
  CALL run_benchmark_dz()

CONTAINS

  SUBROUTINE run_benchmark_ss()
    INTEGER, PARAMETER :: dtype = 4
    REAL(KIND=dtype), DIMENSION(grid_size, grid_size) :: input_x, output_x
    REAL(KIND=dtype), DIMENSION(num_kernels, kernel_size, kernel_size) :: kernels
    TYPE(ApproxSKernelData) :: coulomb
    INTEGER :: s

    input_x = SQRT(2.0)
#INCLUDE "benchmark_dtype.template"
    PRINT*, "[SS]:", get_clock() / num_samples * 1000, "[ms]"
  END SUBROUTINE


  SUBROUTINE run_benchmark_dd()
    INTEGER, PARAMETER :: dtype = 8
    REAL(KIND=dtype), DIMENSION(grid_size, grid_size) :: input_x, output_x
    REAL(KIND=dtype), DIMENSION(num_kernels, kernel_size, kernel_size) :: kernels
    TYPE(ApproxDKernelData) :: coulomb
    INTEGER :: s

    input_x = SQRT(2.0)
#INCLUDE "benchmark_dtype.template"
    PRINT*, "[DD]:", get_clock() / num_samples * 1000, "[ms]"
  END SUBROUTINE

  SUBROUTINE run_benchmark_cc()
    INTEGER, PARAMETER :: dtype = 4
    COMPLEX(KIND=dtype), DIMENSION(grid_size, grid_size) :: input_x, output_x
    COMPLEX(KIND=dtype), DIMENSION(num_kernels, kernel_size, kernel_size) :: kernels
    TYPE(ApproxCKernelData) :: coulomb
    INTEGER :: s

    input_x = SQRT(2.0) * CMPLX(1.0, 1.0)
#INCLUDE "benchmark_dtype.template"
    PRINT*, "[CC]:", get_clock() / num_samples * 1000, "[ms]"
  END SUBROUTINE

  SUBROUTINE run_benchmark_zz()
    INTEGER, PARAMETER :: dtype = 8
    COMPLEX(KIND=dtype), DIMENSION(grid_size, grid_size) :: input_x, output_x
    COMPLEX(KIND=dtype), DIMENSION(num_kernels, kernel_size, kernel_size) :: kernels
    TYPE(ApproxZKernelData) :: coulomb
    INTEGER :: s

    input_x = SQRT(2.0) * CMPLX(1.0, 1.0)
#INCLUDE "benchmark_dtype.template"
    PRINT*, "[ZZ]:", get_clock() / num_samples * 1000, "[ms]"
  END SUBROUTINE

  SUBROUTINE run_benchmark_dz()
    INTEGER, PARAMETER :: dtype = 8
    COMPLEX(KIND=dtype), DIMENSION(grid_size, grid_size) :: input_x, output_x
    REAL(KIND=dtype), DIMENSION(num_kernels, kernel_size, kernel_size) :: kernels
    TYPE(ApproxZKernelData) :: coulomb
    INTEGER :: s

    input_x = SQRT(2.0) * CMPLX(1.0, 1.0)
#INCLUDE "benchmark_dtype.template"
    PRINT*, "[DZ]:", get_clock() / num_samples * 1000, "[ms]"
  END SUBROUTINE

END PROGRAM

