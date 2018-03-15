module mpi_comm

    implicit none
include "mpif.h"
    public :: union_comm
    public :: iamin_comm_root
    public :: iam_comm_root

contains

subroutine union_comm(comm_x, comm_y, comm_union, ierr)

    implicit none
    integer, intent(in) :: comm_x
    integer, intent(in) :: comm_y
    integer, intent(inout) :: comm_union
    integer, intent(inout) :: ierr
    integer :: x_grp
    integer :: y_grp
    integer :: union_grp

    call MPI_Comm_group(comm_x, x_grp, ierr)
    call MPI_Comm_group(comm_y, y_grp, ierr)
    call MPI_Group_union(x_grp, y_grp, union_grp, ierr)
    call MPI_Comm_create(MPI_COMM_WORLD, union_grp, comm_union, ierr)

end subroutine union_comm

subroutine iamin_comm_root(comm_x, iamin, iamroot, ierr)

    implicit none
    integer, intent(in) :: comm_x
    logical, intent(inout) :: iamin
    logical, intent(inout) :: iamroot
    integer, intent(inout) :: ierr
    integer :: x_grp
    integer :: me

    call MPI_Comm_group(comm_x, x_grp, ierr)
    call MPI_Group_rank(x_grp, me, ierr)
   
    if( me .ne. MPI_UNDEFINED) then
        iamin = .true.
        if( me .eq. 0) then
            iamroot = .true.
        else
            iamroot = .false.
        end if
    else
        iamin = .false.
        iamroot = .false.
    end if

end subroutine iamin_comm_root

subroutine iam_comm_root(comm, iam_root, ierr)

    integer, intent(in)           :: comm
    logical, intent(inout)        :: iam_root
    integer, optional, intent(in) :: ierr

    integer :: mpi_grp
    integer :: rank
    integer :: ier

    call mpi_comm_group(comm, mpi_grp, ier)
    call mpi_group_rank(mpi_grp, rank, ier)
    if(rank .eq. 0)iam_root = .true.

end subroutine iam_comm_root

end module mpi_comm
