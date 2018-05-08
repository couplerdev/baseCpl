FC = mpif90
OBJ_M = main.o
OBJ = baseCpl.o 
MODELS =$(shell ls -al| grep '\.o'| grep -v 'main'| grep -v 'baseCpl'| awk '{print $$9}') 
#OBJS = ../../MCTWrapper/mct_mod.o \
	   ../../data_def/proc_def.o \
           ../../data_def/comms_def.o \
	   ../../transManage/comms.o \
	   ../../transManage/extend.o \
	   ../../timeManage/timeM.o \
	   ../../procManage/manage.o
SRC = baseCpl.F90
EXE = main
LIB =  -I../../include -I./ -I/usr/local/netcdf4/include
#      -I../model1 -I../model2 -I../model3 
LIBDIR = ../../lib
SLIB = -lbcpl 
FFLAG = -L$(LIBDIR) -L/usr/local/netcdf4/lib -I/usr/local/netcdf4/include

$(EXE) : $(OBJ) $(OBJ_M) $(MODELS)
	$(FC) $(MODELS) $(OBJ_M) $(OBJ)  -o $(EXE) \
	$(FFLAG) $(SLIB) -lmct -lmpeu  -lnetcdff

$(OBJ) : $(SRC)
	$(FC) -c  $(SRC) $(LIB)

$(OBJ_M) : main.F90 
	$(FC) -c main.F90 $(LIB)

.PHONY : clean  
clean:
	rm -rf *.mod *.o
