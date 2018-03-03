FC = mpif90
OBJ = comms_def.o comms.o
SRC = comms_def.F90 comms.F90
LIB = -lmct -lmpeu
FFLAG = -I/usr/local/include -I../MCTWrapper

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)

 
