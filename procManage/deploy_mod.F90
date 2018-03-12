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
    public  :: deploy_cpl
    public  :: deploy_readFile
    public  :: deploy_setRange

contains 

subroutine deploy_cpl(glo_comm, cpl_comm, cpl_id, iam_in, pattern, ierr)

    implicit none
    integer, intent(in)                            :: glo_comm
    integer, intent(inout)                         :: cpl_comm
    integer, intent(in)                            :: cpl_id
    logical, dimension(:), pointer, intent(inout)  :: iam_in
    integer, optional, intent(in)                  :: pattern
    integer, optional, intent(in)                  :: ierr

    integer :: mpi_grp
    integer :: new_grp
    integer :: peRange(1,3)
    integer :: comp_first
    integer :: comp_last
    integer :: stride
    integer :: me
    integer :: comm_size
    integer :: ier

    if(.not. present(pattern) .or. pattern .eq. 1)then
        cpl_comm = glo_comm
        iam_in(cpl_id) = .true.
    else
        call mpi_comm_group(glo_comm, mpi_grp, ier)
        call mpi_comm_size(glo_comm, comm_size, ier)
        call deploy_readFile(cpl_id, comp_first, comp_last, stride, ier)
        peRange(1,1) = comp_first
        peRange(1,2) = comp_last - 1
        peRange(1,3) = stride
        call mpi_group_range_incl(mpi_grp, 1, peRange, new_grp, ier)
        call mpi_comm_create(glo_comm, new_grp, cpl_comm, ier)
        call mpi_group_rank(new_grp, me, ier)
        if(me .ne. MPI_UNDEFINED)then
            iam_in(cpl_id) = .true.
        end if
    end if 

end subroutine deploy_cpl

subroutine deploy(glo_comm, deploy_comm, deploy_join_comm, &
                  comp_id, cpl_id, id_join, iam_in, pattern, ierr)

    implicit none
    integer, intent(in)              :: glo_comm
    integer, intent(inout)           :: deploy_comm
    integer, intent(inout)           :: deploy_join_comm
    integer, intent(in)              :: comp_id  
    integer, intent(in)              :: cpl_id
    integer, intent(in)              :: id_join
    logical, dimension(:), pointer, intent(inout) :: iam_in
    integer, optional, intent(in)    :: pattern
    integer, optional, intent(inout) :: ierr    

    integer    :: mpi_grp
    integer    :: new_grp
    !integer    :: n
    integer    :: peRange(2,3)
    integer    :: rank  
    integer    :: comp_first
    integer    :: comp_last
    integer    :: stride
    integer    :: me
    integer    :: comm_size
    integer    :: ier   

    if(.not. present(pattern) .or. pattern == 1)then
        deploy_comm = glo_comm
        deploy_join_comm = glo_comm
        iam_in(comp_id) = .true.
    else
        call mpi_comm_group(glo_comm, mpi_grp, ier)
        call mpi_comm_size(glo_comm, comm_size, ier)
        call deploy_readFile(comp_id, comp_first, comp_last, stride, ier)
        peRange(1,1) = min(comp_first , comm_size-1)
        peRange(1,2) = min(comp_last, comm_size-1)
        peRange(1,3) = stride
            !--- set up n and peRange
        call mpi_group_range_incl(mpi_grp, 2, peRange, new_grp)
        call mpi_comm_create(glo_comm, new_grp, deploy_comm, ier)
        call mpi_group_rank(new_grp, me, ier)
        if(me .ne. MPI_UNDEFINED)then
            iam_in(comp_id) = .true.
        end if

        call deploy_readFile(cpl_id, comp_first, comp_last, stride, ier)
        peRange(2,1) = min(comp_first, comm_size-1)
        peRange(2,2) = min(comp_last, comm_size-1)
        peRange(2,3) = stride
        call mpi_group_range_incl(mpi_grp, 2, peRange, new_grp, ier)
        call mpi_comm_create(glo_comm, new_grp, deploy_comm)
        call mpi_group_rank(new_grp, me, ier)
        if(me .ne. MPI_UNDEFINED)then
            iam_in(id_join) = .true.
        end if
    end if

end subroutine deploy

subroutine deploy_readFile(comp_id, comp_first, comp_last, stride, ierr)

    integer, intent(in)     :: comp_id
    integer, intent(inout)  :: comp_first
    integer, intent(inout)  :: comp_last
    integer, intent(inout)  :: stride
    integer, optional, intent(inout)  :: ierr

    comp(comp_id-1,1) = comp_first
    comp(comp_id-1,2) = comp_last
    comp(comp_id-1,3) = stride

end subroutine deploy_readFile

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
