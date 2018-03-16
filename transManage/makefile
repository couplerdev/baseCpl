FC = mpif90
AR = ar
OBJ =  extend.o comms.o
SRC =  extend.F90 comms.F90
LIB_A = libcomm.a
LIBDIR = ../lib
INCLDIR = ../include
LIB = -lmct -lmpeu 
FFLAG = -I/usr/local/include  -I../include -I./

#$(LIB_A) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(OBJ)
#	mv *.mod $(INCLDIR)
#	mv *.a $(LIBDIR)

#comms.o : comms.F90
#	$(FC) $(FFLAG) -c comms.F90 $(LIB)
#	mv *.mod *.o $(INCLDIR)

$(OBJ) : $(SRC)
	$(FC) $(FFLAG) -c $(SRC) $(LIB)
	mv *.mod *.o $(INCLDIR)

.PHONY : clean 
clean:
	rm -rf *.mod *.o
