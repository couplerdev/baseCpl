FC = mpif90
#EXE = main
OBJ = manage.o mpi_comm.o deploy_mod.o
SRC = manage.F90 mpi_comm.F90 deploy_mod.F90
LIB = -lmct -lmpeu
FLAG = -I../MCTWrapper -I/usr/local/include -I../data_def -I../transManage


 
$(OBJ) : $(SRC)
	$(FC) $(FLAG) -c $(SRC) $(LIB)


.PHONY:clean

clean:
	rm -rf *.o
