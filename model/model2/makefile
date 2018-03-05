FC = mpif90
OBJ = comp_b.o
SRC = comp_b.F90
LIB = -I../MCTWrapper -I../timeManange -I../procManage

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)

.PHONY : clean
clean:
	rm *.mod  

