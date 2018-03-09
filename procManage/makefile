FC = mpif90
AR = ar
LIB_A = libprocm.a
OBJ = manage.o mpi_comm.o deploy_mod.o
SRC = deploy_mod.F90 mpi_comm.F90  manage.F90
INCLDIR = ../include
LIBDIR = ../lib
LIB = -lmct -lmpeu -lmctm -lcomm -ldata_def 
FLAG = -I../include -I/usr/local/include

#$(LIB_A) : $(OBJ)
#	$(AR) rcs $(LIB_A) $(SRC)
#	mv *.mod $(INCLDIR)
#	mv *.a $(LIBDIR)
 
$(OBJ) : $(SRC)
	$(FC) $(FLAG) -c $(SRC) $(LIB)
	mv *.mod *.o $(INCLDIR)


.PHONY:clean
clean:
	rm -rf *.o *.mod
