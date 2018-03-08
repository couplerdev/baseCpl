FC = mpif90
AR = ar
OBJ = comms.o extend.o
SRC = comms.F90 extend.F90
LIB_A = libcomm.a
LIBDIR = ../lib
INCLDIR = ../include
LIB = -lmct -lmpeu -ldata_def
FFLAG = -I/usr/local/include -I../MCTWrapper -I../include

#$(LIB_A) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(OBJ)
#	mv *.mod $(INCLDIR)
#	mv *.a $(LIBDIR)

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)
	mv *.mod *.o $(INCLDIR)

.PHONY : clean 
clean:
	rm -rf *.mod *.o
