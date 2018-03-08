FC = mpif90 
OBJ = timeM.o
LIB_A = libtime.a
INCLDIR = ../include
LIBDIR = ../lib
AR = ar

#$(LIB_A) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(OBJ)
#	mv *.mod $(INCLDIR)
#	mv *.a $(LIBDIR)

timeM.o : timeM.F90
	$(FC) -c timeM.F90
	mv *.mod *.o $(INCLDIR)

.PHONY:clean

clean: 
	rm -rf *.o
