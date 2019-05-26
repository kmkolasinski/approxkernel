PROGRAM intTest
  USE modutils
  USE modio
  USE modapproxkernel
  USE omp_lib
  IMPLICIT NONE

  INTEGER,PARAMETER :: width = 128, height = 128
  DOUBLE PRECISION, PARAMETER :: M_PI = 4*ATAN(1.0d0)
  TYPE(approxzkerneldata), SAVE :: coulomb
  DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: kernels
  INTEGER::num_threads,thread_id,i
  COMPLEX*16::expected_value, current_value

  !$OMP PARALLEL
  IF(omp_get_thread_num().eq.0) THEN
    num_threads=omp_get_num_threads()
  ENDIF
  !$OMP END PARALLEL

  WRITE(*,*) "Dostepnych watkow: ",num_threads
  !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(num_threads) copyin(coulomb)
  !$OMP CRITICAL
  thread_id=omp_get_thread_num()

  CALL read_kernels(&
    "resources/coulomb2d_kernel/kernels_scales=4_size=33_grid=128.txt", &
    num_kernels=4, kernel_size=33, kernels=kernels)

  ! scale kernel by thread id, this will change value of the potential
  ! of eatch thread
  kernels = kernels * (thread_id + 1)

  CALL initapproxkernel(coulomb, kernels=kernels, input_shape=[width, height])
  expected_value = compute_rho(width, height, coulomb, thread_id)

  !$OMP ENDCRITICAL
  !$OMP BARRIER
  WRITE(*,"(A, i4, 2f12.6)") " Expected value of integral for thread:", thread_id + 1, expected_value
  !$OMP BARRIER

  DO i=1, 200 * 8 / num_threads
    current_value = compute_rho(width,height,coulomb, thread_id)
    IF(ABS(current_value - expected_value) > 1e-6) THEN
      WRITE(*,*) ,"[ERROR] :", i, current_value, ABS(current_value - expected_value)
    ENDIF
  ENDDO

  !$OMP BARRIER
  WRITE(*,*) "Memory deallocation, thread:",thread_id
  CALL deleteapproxkernel(coulomb)
  !$OMP END PARALLEL

CONTAINS

  FUNCTION compute_rho(width,height,coulomb, thread_id) RESULT(res)
    INTEGER::width,height
    TYPE(approxzkerneldata) :: coulomb
    DOUBLE PRECISION:: dx
    INTEGER::i,j

    COMPLEX*16,DIMENSION(:,:), ALLOCATABLE::rho, potential
    COMPLEX*16::res
    INTEGER::thread_id

    ALLOCATE(rho(width,height),potential(width,height))
    rho=0
    potential=0

    dx=1.0d0/width
    DO i=1,width
      DO j=1,height
        rho(i,j) = SIN(M_PI*REAL(i-1)/REAL(width-1))*SIN(M_PI*REAL(j-1)/REAL(height-1))
        rho(i,j) = rho(i,j) + (0.0d0,1.0d0)*COS(M_PI*REAL(i-1)/REAL(width-1))*COS(M_PI*REAL(j-1)/REAL(height-1))
      ENDDO
    ENDDO
    rho = rho/(NORM2(ABS(rho))*dx)

    CALL execapproxkernel(coulomb, rho, potential)

    res = SUM(rho * potential) * (dx**3) * (thread_id + 1)
    DEALLOCATE(rho, potential)

  END FUNCTION
END PROGRAM
