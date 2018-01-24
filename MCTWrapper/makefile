FC = mpif90
OBJ = mct_mod.o
FLAGS = -I../MCT/mct

$(OBJ) : mct_mod.F90
	$(FC) $(FLAGS) -c mct_mod.F90

.PHONY:clean

clean:
	rm -rf *.mod *.o
