FC = mpif90
OBJ_M = main.o
OBJ = baseCpl.o 
MODELS = ./comp_a.o ./comp_b.o ./comp_c.o  
#OBJS = ../../MCTWrapper/mct_mod.o \
	   ../../data_def/proc_def.o \
           ../../data_def/comms_def.o \
	   ../../transManage/comms.o \
	   ../../transManage/extend.o \
	   ../../timeManage/timeM.o \
	   ../../procManage/manage.o
SRC = baseCpl.F90
EXE = main
LIB =  -I../../include -I./ 
#      -I../model1 -I../model2 -I../model3 
LIBDIR = ../../lib
SLIB = -lbcpl
FFLAG = -L$(LIBDIR)

$(EXE) : $(OBJ) $(OBJ_M) $(MODELS)
	$(FC) $(MODELS) $(OBJ_M) $(OBJ)  -o $(EXE) \
	$(FFLAG) $(SLIB) -lmct -lmpeu 

$(OBJ) : $(SRC)
	$(FC) -c  $(SRC) $(LIB)

$(OBJ_M) : main.F90 
	$(FC) -c main.F90 $(LIB)

.PHONY : clean  
clean:
	rm -rf *.mod *.o
