FC = mpif90
OBJ_M = main.o
OBJ = baseCpl.o 
MODELS = ../model1/comp_a.o ../model2/comp_b.o ../model3/comp_c.o
#OBJS = ../../MCTWrapper/mct_mod.o \
	   ../../data_def/proc_def.o \
           ../../data_def/comms_def.o \
	   ../../transManage/comms.o \
	   ../../transManage/extend.o \
	   ../../timeManage/timeM.o \
	   ../../procManage/manage.o
SRC = baseCpl.F90
EXE = main
LIB = -I/usr/local/lib -I/usr/local/include -I../../include \
	-I../model1 -I../model2 -I../model3 
LIBDIR = /home/adalos/tmp2/baseCpl/lib
SLIB = -lbcpl
FFLAG = -L$(LIBDIR)

$(EXE) : $(OBJ) $(OBJ_M) $(MODELS)
	$(FC) $(MODELS) $(OBJ_M) $(OBJ)  -o $(EXE) \
	$(FFLAG) $(SLIB) -lmct -lmpeu 

$(OBJ) : $(SRC)
	$(FC) -c $(SRC) $(LIB)

$(OBJ_M) : main.F90 
	$(FC) -c main.F90 $(LIB)

.PHONY : clean  
clean:
	rm -rf *.mod *.o
