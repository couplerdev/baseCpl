FC = mpif90
OBJ = comms.o extend.o
SRC = comms.F90 extend.F90
LIB = -lmct -lmpeu
FFLAG = -I/usr/local/include -I../MCTWrapper -I../data_def

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)

 
clean:
	rm -rf *.mod *.o
