FC = mpif90
OBJ = comp_b.o
SRC = comp_b.F90
LIB = -I../../include

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)

.PHONY : clean
clean:
	rm *.mod *.o 

