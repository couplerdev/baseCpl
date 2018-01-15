CC = gfortran
EXE = main
OBJ =  main.o baseCpl.o
SRC = main.F90 baseCpl.F90
main.o : main.F90
baseCpl.o : baseCpl.F90

EXE : ${OBJ} 
	${CC} -o ${EXE} ${OBJ} 

baseCpl.o : baseCpl.F90
main.o : main.F90



