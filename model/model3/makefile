FC = mpif90
OBJ = comp_c.o
SRC = comp_c.F90
LIB = -I../../include

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)
	mv *.o *.mod ../cpl

.PHONY : clean
clean:
	rm *.mod  *.o

