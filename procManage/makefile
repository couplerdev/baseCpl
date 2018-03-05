FC = mpif90
#EXE = main
OBJ =  manage.o  main.o 
LIB = -lmct -lmpeu
FLAG = -I../MCTWrapper -I/usr/local/include


 
manage.o : manage.F90 ../MCTWrapper/mct_mod.o
	$(FC) $(FLAG) -c manage.F90 $(LIB)


.PHONY:clean

clean:
	rm -rf *.o
