FC = mpif90
OBJ = mct_mod.o
LIB = -lmct -lmpeu
FLAGS = -I/usr/local/include

$(OBJ) : mct_mod.F90
	$(FC) $(FLAGS) -c mct_mod.F90 $(LIB)

.PHONY:clean

clean:
	rm -rf *.mod *.o
