FC = mpif90
AR = ar
LIB_A = libbcpl.a
SRC  = ./include/*.o
LIBDIR = ./lib
path0 = data_def
path1 = MCTWrapper
path2 = timeManage
path3 = procManage
path4 = transManage



.PHONY : all
all :
	make -C $(path0)
	make -C $(path1)
	make -C $(path2)
	make -C	$(path3) 
	make -C $(path4)
	$(AR) rcs $(LIB_A) $(SRC)
	mv $(LIB_A) $(LIBDIR) 	
