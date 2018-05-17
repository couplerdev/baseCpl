FC = mpif90
AR = ar
LIB_A = libbcpl.a
ABSDIR = $(shell pwd)
SRC  = ./include/*.o
LIBDIR = ./lib
path0 = MCTWrapper
path1 = data_def
path2 = timeManage
path3 = transManage
path4 = procManage
models=$(shell ls -l ./model|grep ^d|grep -v 'cpl'|awk '{print $$9}')
MAIN = ./model/cpl


.PHONY : all
all :
	@echo $(ABSDIR)
	@echo $(models)
	make -C $(path0)
	make -C $(path1)
	make -C $(path2)
	make -C	$(path3) 
	make -C $(path4)
	@echo make models
	@for dir in $(models);\
	do\
		cd ./model/$$dir;\
		make;\
		cd -;\
	done
	@echo end make models
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
