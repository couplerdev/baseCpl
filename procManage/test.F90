program test
    implicit none
include"mpif.h"
    integer :: me
    integer :: glo_grp
    integer :: sub_grp
    integer :: ierr
    integer ,dimension(2) :: rank
    integer :: new_comm

    rank(1)=1
    rank(2)=2
    call MPI_Init(ierr)
    call MPI_Comm_group(MPI_COMM_WORLD, glo_grp, ierr)
    call MPI_Group_excl(glo_grp, 2, rank, sub_grp, ierr)
    call MPI_Comm_create(MPI_COMM_WORLD,sub_grp,new_comm, ierr)
    call MPI_Group_rank(sub_grp, me, ierr)
    if( me .NE. MPI_UNDEFINED ) then
        write(*,*) 'good',me
    end if
    call MPI_Finalize(ierr)

end program test
