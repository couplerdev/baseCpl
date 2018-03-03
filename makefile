FC =mpif90
OBJ = ./baseCpl.o 
SRC =
EXE = baseCpl
LIB =
SUBDIR = ./procManage ./

for @dir in SUBDIR: \
	cd dir
	make

$(EXE)
