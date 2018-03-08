FC = mpif90
AR = ar
OBJ = proc_def.o comms_def.o
SRC = comms_def.F90 proc_def.F90
LIB_A = libdata_def.a
LIB = -lmct -lmpeu -lmctm
INCLDIR = ../include
LIBDIR = ../lib
FFLAG = -I/usr/local/include -I/usr/local/lib -I../include

#$(LIB_A) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(OBJ)
#	mv *.mod $(INCLDIR)
#	mv *.a $(LIBDIR)

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)
	mv *.mod *.o $(INCLDIR)

.PHONY : clean
clean :
	rm *.o *.mod
