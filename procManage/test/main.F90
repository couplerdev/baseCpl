program main
include "mpif.h"

    integer ierr, num_proc, num_size
    integer my_comm
    type def
        character(len=10) :: defName
        integer :: val
    end type def
    type(def), dimension(:), pointer:: defP 

    allocate(defP(10))
    call MPI_Init(ierr)
    my_comm = MPI_COMM_WORLD
    call MPI_Comm_rank(my_comm, num_proc,ierr)
    write(*,*) 'hello, world', num_proc
    if(num_proc==1)then
        defP(2)%defName = "haha" 
        defP(2)%val = 1
    end if
    call MPI_Finalize(ierr)
     
end program main
