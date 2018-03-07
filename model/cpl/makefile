FC = mpif90
OBJ = test.o comms.o mct_mod.o proc_def.o
SRC = test.F90
EXE = test
LIB = -I/usr/local/lib -I/usr/local/include -I../../MCTWrapper \
	  -I../../transManage -I ../../data_def
#FFLAG = -I/usr/local/include -I../MCTWrapper -I../data_def
FFLAG = -l/usr/local/lib -l/usr/local/include -l../../MCTWrapper \
	  -l../../transManage -l../../data_def


$(EXE) : $(OBJ)
	$(FC)  -lmct -lmpeu $(OBJ) -o $(EXE)  $(LIB)

$(OBJ) : $(SRC)
	$(FC)  -c $(SRC) $(LIB)

 
clean:
	rm -rf *.mod *.o
