module sys

    implicit none
    private 
    
    public :: base_sys_abort

contains 

subroutine base_sys_abort()

    call MPI_Abort(MPI_COMM_WORLD, rc, ierr)


end base_sys_abort




end module sys
