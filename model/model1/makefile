FC = mpif90
OBJ = comp_a.o
SRC = comp_a.F90
LIB = -I../../include

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)

.PHONY : clean
clean:
	rm *.mod *.o 

