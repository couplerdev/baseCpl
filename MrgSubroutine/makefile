FC = mpif90
AR = ar
LIB_A = libprocm.a
OBJ =   fraction_mod.o mrg_mod.o
SRC  = ./mrg_mod.F90 ./fraction_mod.F90
INCLDIR = ../include
LIBDIR = ../lib
LIB = -lmct -lmpeu -lmctm -lcomm -ldata_def
FLAG = -I../include -I/usr/local/include

$(OBJ) : $(SRC)
	$(FC) $(FLAG) -c $(SRC) $(LIB)
	 mv *.mod *.o $(INCLDIR)


.PHONY: clean
clean:
	rm -rf *.o *.mod
