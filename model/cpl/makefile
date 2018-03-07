FC = mpif90
OBJ = test.o
OBJS = ../../MCTWrapper/mct_mod.o \
	   ../../data_def/proc_def.o \
	   ../../transManage/extend.o 
SRC = test.F90 
EXE = test
LIB = -I/usr/local/lib -I/usr/local/include -I../../MCTWrapper \
	  -I../../transManage -I../../data_def
FFLAG = -I/usr/local/include -I/usr/local/lib


$(EXE) : $(OBJ)
	$(FC)  $(OBJS) $(OBJ) -o $(EXE) $(LIB) -lmct -lmpeu

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)

 
clean:
	rm -rf *.mod *.o
