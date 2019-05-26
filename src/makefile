all: static_lib dynamic_lib copy_includes

# Default compiler
FOPT= -O2 -132 -parallel -xHost -align -fPIC -cpp -openmp

DEBUG_LIB_NAME=
FF=ifort
FCFLAGS=-c $(FOPT)
BUILD_DIR=../build/release/
LIB_DIR=../lib/
INCLUDE_DIR=../include/

# set compilator flags for debug mode
ifeq ($(C),DEBUG)
FOPT= -132 -heap-arrays 10 -O0 -check all,noarg_temp_created  \
			-assume realloc_lhs -ftrapuv -fpe0 -warn -g -traceback \
			-debug extended -check bounds -fp-stack-check -gen-interfaces\
			-warn interfaces -fstack-protector -fPIC  -cpp -check pointer
DEBUG_LIB_NAME=d
BUILD_DIR=../build/debug/
endif


F90_SOURCES= \
		mkl_vsl.f90 \
		modutils.f90 \
		modio.f90 \
		modimageops.f90 \
		modvslconv2d.f90 \
		modapproxkernel.f90


F90_OBJECTS=$(F90_SOURCES:%.f90=%.o)
BUILD_F90_OBJECTS=$(patsubst %.o,$(BUILD_DIR)%.o,$(F90_OBJECTS))


modutils.o: modutils.f90
	$(FF) $(FCFLAGS) modutils.f90 -o $(BUILD_DIR)$@

modvslconv2d.o: modvslconv2d.f90
	$(FF) $(FCFLAGS) modvslconv2d.f90 -o $(BUILD_DIR)$@

modio.o: modio.f90
	$(FF) $(FCFLAGS) modio.f90 -o $(BUILD_DIR)$@

modimageops.o: modimageops.f90
	$(FF) $(FCFLAGS) modimageops.f90 -o $(BUILD_DIR)$@

modapproxkernel.o: modapproxkernel.f90
	$(FF) $(FCFLAGS) modapproxkernel.f90 -o $(BUILD_DIR)$@

mkl_vsl.o: mkl_vsl.f90
	$(FF) $(FCFLAGS) mkl_vsl.f90 -o $(BUILD_DIR)$@

dynamic_lib: $(F90_OBJECTS)
	@echo "# Building DYNAMIC LIBRARY"
	ifort -shared $(BUILD_F90_OBJECTS) -o $(LIB_DIR)libcoulomb$(DEBUG_LIB_NAME).so


static_lib: $(F90_OBJECTS)
	@echo "# Building STATIC LIBRARY"
	ar rcs  $(LIB_DIR)libcoulomb$(DEBUG_LIB_NAME).a $(BUILD_DIR)*.o


copy_includes:
	@echo "# Copying include files"
	cp *.mod $(INCLUDE_DIR)


clean:
		rm -f $(BUILD_DIR)*.o $(BUILD_DIR)*.mod *.txt *.dat *.xml *.png *.pdf fort.* *.mod 2> /dev/null
		rm -f ../plots/*.txt ../plots/*.dat ../plots/*.xml ../plots/*.png ../plots/*.pdf ../plots/fort.* 2> /dev/null

clean_lib:
		rm $(LIB_DIR)*.a $(LIB_DIR)*.so $(INCLUDE_DIR)* 2> /dev/null

