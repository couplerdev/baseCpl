FC = mpif90
OBJ = proc_def.o
SRC = proc_def.F90
LIB = -lmct -lmpeu
FFLAG = -I/usr/local/include -I/usr/local/lib -I../MCTWrapper

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)

comms_def.o : comms_def.F90
	$(FC) $(FFLAG) -c comms_def.F90 $(LIB)


