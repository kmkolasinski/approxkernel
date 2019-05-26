all: example_run \
		test_coulomb_kernel \
		benchmark_integration \
		coulomb_kernel2d_demo \
		test_complex_demo \
		benchmark_dtype \
		complex_coulomb_kernel2d_demo \
		test_integral_openmp


run:
	./example_run
	./test_coulomb_kernel
	./benchmark_integration
	./coulomb_kernel2d_demo
	./test_complex_demo
	./benchmark_dtype
	./complex_coulomb_kernel2d_demo
	./test_integral_openmp


.PHONY: run clean
# Default compiler
FOPT= -O2 -132 -parallel -xHost -align -cpp
DEBUG_LIB_NAME=
FF=ifort
LDFLAGS=-mkl -liomp5 ../lib/libcoulomb$(DEBUG_LIB_NAME).a

# set compilator flags for debug mode
ifeq ($(C),DEBUG)
FOPT= -132 -heap-arrays 10 -O0 -check all,noarg_temp_created  \
			-assume realloc_lhs -ftrapuv -fpe0 -warn -g -traceback \
			-debug extended -check bounds -fp-stack-check -gen-interfaces\
			-warn interfaces -fstack-protector -fPIC  -cpp -check pointer
DEBUG_LIB_NAME=d
endif


F90_OBJECTS=$(F90_SOURCES:%.f90=%.o)
BUILD_F90_OBJECTS=$(patsubst %.o,$(BUILD_DIR)%.o,$(F90_OBJECTS))


example_run: example_run.f90
	$(FF) $(FOPT) -I../include/  example_run.f90 $(LDFLAGS) -o $@

test_coulomb_kernel: test_coulomb_kernel.f90
	$(FF) $(FOPT) -I../include/  test_coulomb_kernel.f90 $(LDFLAGS) -o $@

benchmark_integration: benchmark_integration.f90
	$(FF) $(FOPT) -I../include/  benchmark_integration.f90 $(LDFLAGS) -o $@

coulomb_kernel2d_demo: coulomb_kernel2d_demo.f90
	$(FF) $(FOPT) -I../include/  coulomb_kernel2d_demo.f90 $(LDFLAGS) -o $@

test_complex_demo: test_complex_demo.f90
	$(FF) $(FOPT) -I../include/  test_complex_demo.f90 $(LDFLAGS) -o $@

benchmark_dtype: benchmark_dtype.f90
	$(FF) $(FOPT) -I../include/  benchmark_dtype.f90 $(LDFLAGS) -o $@

complex_coulomb_kernel2d_demo: complex_coulomb_kernel2d_demo.f90
	$(FF) $(FOPT) -I../include/  complex_coulomb_kernel2d_demo.f90 $(LDFLAGS) -o $@

test_integral_openmp: test_integral_openmp.f90
	$(FF) $(FOPT) -I../include/  test_integral_openmp.f90  -mkl ../lib/libcoulomb.a  -warn all -openmp  -o $@



clean:
	rm -f *.txt *.dat  *.xml *.png *.pdf fort.*  2> /dev/null
	rm -f outputs/*.txt outputs/coulomb_kernel/*.txt 2> /dev/null
	rm -f example_run test_coulomb_kernel benchmark_integration coulomb_kernel2d_demo \
			test_complex_demo benchmark_dtype complex_coulomb_kernel2d_demo \
			test_integral_openmp 2> /dev/null
