module procM
    implicit none
include"mpif.h"

#define MAX_SIZE 100
    type commInfo
        character(len=20) :: commName
        integer :: num_proc
        integer :: num_size   
        integer  :: my_comm
    end type commInfo
    
    type IFlag
        character(len=20) :: flag_name
        integer :: val
    end type IFlag

    type cplVariable
        character(len=20) :: var_name
        integer, dimension(:) :: array
    end type cplVariable   

    type cplMapper
        character(len=20) :: mapper_name
    end type cplMapper

    type model
        character(len=20) :: model_name
        type(cplVariable), dimension(:) :: cplVars
        type(cplManager), dimension(:) :: mappers
    end type model    

    type proc
        type(commInfo), dimension(:), pointer :: comms
        type(IFlag), dimension(:), pointer :: flags        
        type(model), dimension(:), pointer :: models
        integer :: num_comms
        integer :: num_flags
        integer :: num_models
        integer :: max_size
    end type proc

    public :: init
    public :: add_model
    public :: add_flag
    public :: printProc
    public :: clean

    interface init; module procedure &
        initv_, &
        initf_
    end interface

contains

subroutine initv_(my_proc)
    
    implicit none 
    type(proc), intent(inout) :: my_proc 
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 

    allocate(my_proc%comms(MAX_SIZE))
    allocate(my_proc%flags(MAX_SIZE))
    allocate(my_proc%models(MAX_SIZE))
   
    my_proc%num_comms = 1
    my_proc%num_flags = 0
    my_proc%num_models = 0
    my_proc%max_size = MAX_SIZE

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
   
    my_proc%comms(my_proc%num_comms)%my_comm = MPI_COMM_WORLD
    my_proc%comms(my_proc%num_comms)%num_proc = num_rank
    my_proc%comms(my_proc%num_comms)%num_size = num_size
    my_proc%comms(my_proc%num_comms)%commName = "MPI_COMM_WORLD"

end subroutine initv_

subroutine initf_(my_proc, filename)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    character(len=20), intent(in):: filename
  
    

end subroutine initf_

subroutine clean(my_proc)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: ierr

    deallocate(my_proc%comms)
    deallocate(my_proc%flags)
    deallocate(my_proc%models)
    
    call MPI_Finalize(ierr) 

end subroutine clean

subroutine printProc(my_proc, ierr)
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: iter
    integer :: world_rank    
    integer, intent(inout) :: ierr

    ierr = 0
    call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)
    write(*,*) "my_proc:"
    if(my_proc%num_comms>=1) then
        do iter = 1, my_proc%num_comms
            write(*,*) "the comm: ",my_proc%comms(iter)%commName, "comm size:",&
                       my_proc%comms(iter)%num_size, "my rank in comm:", &
                       my_proc%comms(iter)%num_proc
        end do
    end if
    call MPI_Barrier(MPI_COMM_WORLD,ierr)
    
    if(world_rank==0) then
        if(my_proc%num_flags>=1) then
            do iter = 1, my_proc%num_flags
                write(*,*) "the flags name:", my_proc%flags(iter)%flag_name, &
                           "relative val:", my_proc%flags(iter)%val  
            end do 
        end if
    end if

    call MPI_Barrier(MPI_COMM_WORLD,ierr)
    if(world_rank==0) then
        if(my_proc%num_models>=1) then
            do iter = 1, my_proc%num_models
                write(*,*) "the model name:", my_proc%models(iter)%model_name
            end do
        end if
     end if

end subroutine printProc

subroutine add_flag(my_proc, flag_name, val, ierr)
    ! a bug!!! need to be grp to manage this
    implicit none
    type(proc), intent(inout) :: my_proc
    character(len=20), intent(in) :: flag_name
    integer, intent(in) :: val
    integer, intent(inout) :: ierr

    ierr = 0
    my_proc%num_flags = my_proc%num_flags+1
    if(my_proc%num_flags>MAX_SIZE)then
        ierr = 1
    else
        my_proc%flags(my_proc%num_flags)%flag_name = flag_name
        my_proc%flags(my_proc%num_flags)%val = val
    end if

end subroutine add_flag

subroutine add_model(my_proc, model_name, ierr)

    implicit none
    type(proc), intent(inout) :: my_proc
    character(len=20), intent(in) :: model_name
    integer, intent(inout) :: ierr

    ierr = 0
    my_proc%num_models = my_proc%num_models+1
    if(my_proc%num_models>MAX_SIZE)then
        ierr = 1
    else
        my_proc%models(my_proc%num_models)%model_name = model_name
    end if

end subroutine add_model

subroutine comm_union(my_proc, comm_x, comm_y, ierr)

    implicit none
    type(proc), intent(inout) :: my_proc
    integer, intent(in) :: comm_x
    integer, intent(in) :: comm_y
    integer, intent(inout) :: ierr
    integer :: mpi_grp
    integer :: x_grp
    integer :: y_grp
    integer :: x_comm
    integer :: y_comm
    integer :: mpi_comm
    type(commInfo) :: new_comm

    ierr = 0
    if(my_proc%num_comms<comm_x .or. my_proc%num_comms<comm_y .or. comm_x == comm_y)then
        ierr = 1
    else
        call MPI_Comm_group(my_proc%comms(comm_x)%my_comm, x_grp, ierr)
        call MPI_Comm_group(my_proc%comms(comm_y)%my_comm, y_grp, ierr)
        call MPI_Group_union(x_grp, y_grp, mpi_grp, ierr)
        call MPI_Comm_create(MPI_COMM_WORLD,mpi_grp, mpi_comm, ierr)
        new_comm%commName = my_proc%comms(comm_x)%commName + my_proc%comms(comm_y)%commName
        call MPI_Comm_rank(mpi_comm, new_comm%num_proc, ierr) 
        call MPI_Comm_size(mpi_comm, new_comm%num_size, ierr)
        new_comm%my_comm = mpi_comm
        my_proc%num_comms = my_proc%num_comms + 1
        my_proc%comms(my_proc%num_comms) = new_comm
    end if

end subroutine comm_union

end module procM
