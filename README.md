# ApproxKernels

## Non Conda way of installation:

* Install python3.6 and pip3, this can be done via:

```bash
sudo apt-get install python3.6
sudo apt install python3-pip
sudo apt-get install python3.6-tk
```

* Then install python lib with `install_python_lib.sh`

## Install with conda envrionment

* Using conda enviroments is more safe approach
* Create or use existing conda envionment. 
* Enable created conda environment e.g. `source activate kernels`
* Enter python_lib folder and run: `pip install .`


## Training custom kernel

This example shows how to train kernel from exising predefined configuration

* Enter `tests/resources/coulomb1d_kernel` folder
* Check content of config.yml and kernel.py files
* `config.yml` - contains grid definition, number of scales and training schedule
* `kernel.py` - contains `def kernel_fn(r: float) -> float:` which contains definition of your kernel function. This function must accept scalar float value and return scalar
value of kernel function. For example f(r) = 1/r. Make sure that there are no singularities.
* Open terminal in this folder. Make sure you are in correct conda environment.
* Just in case run: `source activate kernels` 
* Run `fit_kernel` command, wait ...
* Use the created kernel files in your fortran code:

```fortran
USE modutils
USE modio
USE modapproxkernel2d

INTEGER, PARAMETER :: width = 128, height = 128
REAL(KIND=8), DIMENSION(width, height) :: input_x, output_x
REAL(KIND=8), DIMENSION(:, :, :), ALLOCATABLE :: kernels
TYPE(ApproxDKernel2D) :: kernelOp

CALL read_kernels_2d(&
    "path/kernels_1D_scales=4_size=33_grid=128_loss=X.XXXX.txt", 4, 33, kernels)
CALL initapproxkernel2d(kernelOp, kernels=kernels, input_shape=[width, height])
CALL execapproxkernel2d(kernelOp, input_x, output_x)
CALL deleteapproxkernel2d(kernelOp)
```


## Running fortran examples with multiple threads using MKL threading:

* Set enviromental variable:
```
export MKL_NUM_THREADS=8
export OMP_NUM_THREADS=8 
```

## Profiling code

* Compile main code with C=PROFILE option
```
cd src
make clean
make C=PROFILE
cd -
```
* Compile test with the same option
```
cd tests
make clean && make test_profile C=PROFILE
./test_profile
loopprofileviewer.sh loop_prof_1579625645.xml
```
