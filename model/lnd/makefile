FC = mpif90
OBJ = comp_lnd.o
SRC = comp_lnd.F90
LIB = -I../../include

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)
	mv *.o *.mod ../cpl

.PHONY : clean
clean:
	rm *.mod *.o 

