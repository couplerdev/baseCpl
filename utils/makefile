FC = gfortran
EXE = main
OBJ = vector.o main.o 

EXE : $(OBJ)
	$(FC) -o $(EXE) $(OBJ)

vector.o : vector.F90
	$(FC) -c vector.F90

main.o : main.F90
	$(FC) -c main.F90

PHONY.= clean
clean:
	rm -rf *o
