all: example_run \
		test_coulomb_kernel \
		benchmark_integration \
		coulomb_kernel2d_demo \
		test_complex_demo \
		benchmark_dtype \
		complex_coulomb_kernel2d_demo \
		test_integral_openmp \
		example_run_1d \
		example_run_1d_complex \
		test_conv1d test_imageops1d \
		benchmark_baseline_fft \
		test_profile_resize_bilinear \
		test_profile


run:
	./example_run
	./example_run_1d
	./test_coulomb_kernel
	./benchmark_integration
	./coulomb_kernel2d_demo
	./test_complex_demo
	./benchmark_dtype
	./complex_coulomb_kernel2d_demo
	./test_integral_openmp
	./test_conv1d
	./example_run_1d
	./example_run_1d_complex
	./test_imageops1d
	./benchmark_baseline_fft
	./test_profile
	./test_profile_resize_bilinear


.PHONY: run clean
# Default compiler
FOPT= -O3 -132 -parallel -qopenmp -xHost -align -cpp
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

# set compilator flags for debug mode
ifeq ($(C),PROFILE)
FOPT= -132 -xHost -align  -fPIC -cpp -profile-functions \
	-profile-loops=all -profile-loops-report=2
DEBUG_LIB_NAME=profile
endif

F90_OBJECTS=$(F90_SOURCES:%.f90=%.o)
BUILD_F90_OBJECTS=$(patsubst %.o,$(BUILD_DIR)%.o,$(F90_OBJECTS))


example_run: example_run.f90
	$(FF) $(FOPT) -I../include/  example_run.f90 $(LDFLAGS) -o $@

example_run_1d: example_run_1d.f90
	$(FF) $(FOPT) -I../include/  example_run_1d.f90 $(LDFLAGS) -o $@

example_run_1d_complex: example_run_1d_complex.f90
	$(FF) $(FOPT) -I../include/  example_run_1d_complex.f90 $(LDFLAGS) -o $@

test_conv1d: test_conv1d.f90
	$(FF) $(FOPT) -I../include/  test_conv1d.f90 $(LDFLAGS) -o $@

test_imageops1d: test_imageops1d.f90
	$(FF) $(FOPT) -I../include/  test_imageops1d.f90 $(LDFLAGS) -o $@

test_coulomb_kernel: test_coulomb_kernel.f90
	$(FF) $(FOPT) -I../include/  test_coulomb_kernel.f90 $(LDFLAGS) -o $@

benchmark_baseline_fft: benchmark_baseline_fft.f90
	$(FF) $(FOPT) -I../include/  benchmark_baseline_fft.f90 $(LDFLAGS) -o $@

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
	$(FF) $(FOPT) -I../include/  test_integral_openmp.f90  -mkl ../lib/libcoulomb.a  -warn all -qopenmp  -o $@

test_profile: test_profile.f90
	$(FF) $(FOPT) -I../include/  test_profile.f90 $(LDFLAGS) -o $@

test_profile_resize_bilinear: test_profile_resize_bilinear.f90
	$(FF) $(FOPT) -I../include/  test_profile_resize_bilinear.f90 $(LDFLAGS) -o $@


clean:
	rm -f *.txt *.dat  *.xml *.png *.pdf fort.*  2> /dev/null
	rm -f loop_prof_*  2> /dev/null
	rm -f outputs/*.txt outputs/coulomb_kernel/*.txt outputs/benchmark_baseline_fft/*.txt 2> /dev/null
	rm -f example_run example_run_1d example_run_1d_complex test_coulomb_kernel benchmark_integration coulomb_kernel2d_demo \
			test_complex_demo benchmark_dtype complex_coulomb_kernel2d_demo \
			test_integral_openmp test_conv1d test_imageops1d benchmark_baseline_fft \
			test_profile test_profile_resize_bilinear 2> /dev/null
