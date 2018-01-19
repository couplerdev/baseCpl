FC = mpif90
EXE = main
OBJ =  main.o manage.o

EXE : $(OBJ)
	$(FC) -o $(EXE) $(OBJ)

main.o :  main.F90  
	$(FC) -c main.F90
 
manage.o : manage.F90
	$(FC) -c manage.F90

.PHONY:clean

clean:
	rm -rf *.o
