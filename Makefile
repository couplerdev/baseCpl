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
MAIN = ./model/cpl


.PHONY : all
all :
	make -C $(path0)
	make -C $(path1)
	make -C $(path2)
	make -C	$(path3) 
	make -C $(path4)
	$(AR) rcs $(LIB_A) $(SRC)
	rm ./include/*.o
	mv $(LIB_A) $(LIBDIR) 	
	make -C $(MAIN)
	mv $(MAIN)/main ./
