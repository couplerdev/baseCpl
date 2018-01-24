FC = mpif90
EXE = main
MCT_P = ../MCT/mct
MPEU_P = ../MCT/mpeu
OBJ =  manage.o  main.o $(MCT_P)/m_AttrVect.o  $(MPEU_P)/*.o \
       ../MCTWrapper/mct_mod.o
FLAG = -I../MCTWrapper

EXE : $(OBJ)
	$(FC) -o $(EXE) $(OBJ)

 
manage.o : manage.F90
	$(FC) $(FLAG) -c manage.F90

main.o : main.F90
	$(FC)  -c main.F90

.PHONY:clean

clean:
	rm -rf *.o
