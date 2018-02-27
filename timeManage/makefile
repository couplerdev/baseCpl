FC = gfortran 
EXE = test
OBJ = test.o testF.o  timeM.o

EXE : $(OBJ)
	$(FC) -o $(EXE) $(OBJ)

test.o : test.F90
	$(FC) -c test.F90

testF.o : testF.F90
	$(FC) -c testF.F90

timeM.o : timeM.F90
	$(FC) -c timeM.F90

.PHONY:clean

clean: 
	rm -rf *.o
