# 1D heat equation solver
Numerical solution to the heat equation using the finite difference method, written in Fortran.
### Requirements:
Fairly new version of `gfortran`
### Build:
In order to build the application, just type:
```sh
make
```
### Usage:
In order to run the application, open your terminal in main folder of the project and run the following line of code. 
```sh
./main RANGE STEP
```
where RANGE is range of error sampling and STEP is its step. Subsequent error values are redirected to `errors.txt` file.

### Plots:
Plots have been generated in `jupyter notebook` using Julia language and `Plots` package.
