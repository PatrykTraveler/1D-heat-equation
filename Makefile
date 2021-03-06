OPT = -ffree-form -std=f2008 -fimplicit-none -Wall -pedantic -cpp

main:
	gfortran $(OPT) -c gauss.f90
	gfortran $(OPT) -c fdm.f90
	gfortran $(OPT) -c main.f90 
	gfortran $(OPT) main.o fdm.o gauss.o -o main

clean:
	rm *.o *.mod main
