OPT = -ffree-form -std=f2008 -fimplicit-none -Wall -pedantic

main:
	gfortran $(OPT) -c gauss.f90
	gfortran $(OPT) -c fdm.f90
	gfortran $(OPT) -c main.f90 

clean:
	rm *.o *.mod main
