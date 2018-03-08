FC = mpif90
AR = ar
OBJ = mct_mod.o
LIB = -lmct -lmpeu
FLAGS = -I/usr/local/include
LIB_A = libmctm.a
INCLDIR = ../include
LIBDIR = ../lib

#$(LIB) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(OBJ)
#	mv *.mod $(INCLDIR)	
#	mv *.a $(LIBDIR)

$(OBJ) : mct_mod.F90
	$(FC) $(FLAGS) -c mct_mod.F90 $(LIB)
	mv *.mod *.o $(INCLDIR)

.PHONY:clean

clean:
	rm -rf *.mod *.o
