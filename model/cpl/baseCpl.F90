module baseCpl
use procm ,only: pm_init => init 
use cpl_vect
    implicit none

    type(proc) :: my_proc

    public :: cpl_init
    public :: cpl_run
    public :: cpl_final
     
contains 

subroutine cpl_init()
    
    implicit none
    call init(my_proc)
    
    

end subroutine cpl_init

subroutine cpl_run()

end subroutine cpl_run


subroutine cpl_final()

end subroutine cpl_final


end module baseCpl

