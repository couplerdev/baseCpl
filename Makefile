FC = mpif90
OBJ = baseCpl.o main.o
EXE = main
path1 = MCTWrapper
path2 = timeManage
path3 = procManage
path4 = transManage
MAIN = ./model
LIB = -I/usr/local/lib -I/usr/local/include -I./procManage \
      -I./timeManage -I./transManage -I./MCTWrapper

.PHONY : all
all :
	make -C $(path1)
	make -C $(path2)
	make -C	$(path3) 
	make -C $(path4)
	cd $(MAIN)
	make -C $(MAIN)/model1
	make -C $(MAIN)/model2
	make -C $(MAIN)/model3
	cd $(MAIN)/baseCpl
	make
	mv main ../../main
