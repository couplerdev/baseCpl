program main
use procm
    
    implicit none
   
    type(proc) :: my_proc
    integer :: ierr


    call init(my_proc)
    call printProc(my_proc, ierr)    
    call clean(my_proc)

end program main
