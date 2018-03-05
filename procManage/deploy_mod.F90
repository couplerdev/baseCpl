module deploy_mod
!----------------------------------------------------------
! This module used for deploy comp to proper process
!----------------------------------------------------------
    implicit none
include 'mpif.h'
    integer :: defaulToAll = 1
    integer :: comp(4, 3)
    data comp / & !--- comp_first, comp_last, stride
            0, 2, 2, &
            1, 2, 2, &
            2, 2, 2, &
            3, 2, 2/ 
    public  :: deploy
    public  :: deploy_setRange

contains 

subroutine deploy(glo_comm, deploy_comm, comp_id, pattern, ierr)

    implicit none
    integer, intent(in)              :: glo_comm
    integer, intent(inout)           :: deploy_comm
    integer, intent(in)              :: comp_id  
    integer, optional, intent(in)    :: pattern
    integer, optional, intent(inout) :: ierr    

    integer    :: mpi_grp
    integer    :: new_grp
    integer    :: n
    integer    :: peRange(3,1)
    integer    :: rank  
    integer    :: comp_first
    integer    :: comp_last
    integer    :: stride
    integer    :: ier   

    if(.not. present(pattern) .or. pattern == 1)then
        deploy_comm = glo_comm
    else 
        call mpi_comm_rank(glo_comm, rank, ierr)
        if(rank == 0) then
            call mpi_comm_group(glo_comm, mpi_grp)
            call deploy_readFile(comp ,comp_id, comp_first, comp_last, stride, ier)
            peRange(1,1) = comp_first 
            peRange(2,1) = comp_last
            peRange(3,1) = stride
            !--- set up n and peRange
            call mpi_group_range_incl(mpi_grp, n, peRange, new_grp)
        end if
        call mpi_bcast(new_grp, 1, MPI_INTEGER, comp_id, glo_comm, ier)
        call mpi_comm_create(glo_comm, new_grp, deploy_comm)
    end if

end subroutine deploy

subroutine deploy_setRange(comp_id, comp_first, comp_last, stride, ierr)

    implicit none
    integer,           intent(in)   :: comp_id
    integer,           intent(inout)   :: comp_first
    integer,           intent(inout)   :: comp_last
    integer,           intent(inout)   :: stride
    integer, optional, intent(inout)   :: ierr

    comp_first = comp(comp_id-1, 1)
    comp_last  = comp(comp_id-1, 2)
    stride     = comp(comp_id-1, 3)
    if(present(ierr))ierr = 0

end subroutine deploy_setRange

end module deploy_mod
