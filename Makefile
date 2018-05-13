FC = mpif90
AR = ar
LIB_A = libbcpl.a
SRC  = ./include/*.o
LIBDIR = ./lib
path0 = MCTWrapper
path1 = data_def
path2 = timeManage
path3 = transManage
path4 = procManage
model1 = ./model/model1
model2 = ./model/model2
model3 = ./model/model3
model4 = ./model/atm
model5 = ./model/ocn
model6 = ./model/lnd
MAIN = ./model/cpl


.PHONY : all
all :
	make -C $(path0)
	make -C $(path1)
	make -C $(path2)
	make -C	$(path3) 
	make -C $(path4)
	make -C $(model1)
	make -C $(model2)
	make -C $(model3)
	make -C $(model4)
	make -C $(model5)
	make -C $(model6)
	$(AR) rcs $(LIB_A) $(SRC)
	rm ./include/*.o
	mv $(LIB_A) $(LIBDIR) 	
	make -C $(MAIN)
	mv $(MAIN)/main ./

.PHONY : clean
clean :
	make clean -C $(path0)
	make clean -C $(path1)
	make clean -C $(path2)
	make clean -C $(path3)
	make clean -C $(path4)
	make clean -C $(model1)
	make clean -C $(model2)
	make clean -C $(model3)
	make clean -C $(model4)
	make clean -C $(model5)
	make clean -C $(model6)
	make clean -C $(MAIN)
	rm ./main
