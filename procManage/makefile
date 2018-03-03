FC = mpif90
#EXE = main
OBJ =  manage.o  main.o 
LIB = -lmct -lmpeu
FLAG = -I../MCTWrapper -I/usr/local/include

#EXE : $(OBJ)
#	$(FC) -o $(EXE) $(OBJ)

 
manage.o : manage.F90 ../MCTWrapper/mct_mod.o
	$(FC) $(FLAG) -c manage.F90 $(LIB)

#main.o : main.F90
#	$(FC)  -c main.F90

.PHONY:clean

clean:
	rm -rf *.o
