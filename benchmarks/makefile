all: run_ifort_benchmark

run:
	./run_ifort_benchmark


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


run_ifort_benchmark: run_ifort_benchmark.f90
	$(FF) $(FOPT) -I../include/  run_ifort_benchmark.f90 $(LDFLAGS) -o $@


clean:
	rm -f *.txt *.dat  *.xml *.png *.pdf fort.*  2> /dev/null
	rm -f loop_prof_*  2> /dev/null
	rm -f run_ifort_benchmark 2> /dev/null
