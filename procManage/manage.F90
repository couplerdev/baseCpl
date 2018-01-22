module procM
    implicit none
include"mpif.h"

    type proc
        !type(commInfo), dimension(:), pointer :: comms     
        !type(model), dimension(:), pointer :: models
        !-------------------------------------------------
        ! Meta desc of proc
        !-------------------------------------------------
        integer :: num_comms
        integer :: num_flags
        integer :: num_models
        integer :: my_rank
        integer :: my_size
        !-------------------------------------------------
        ! define flags
        !-------------------------------------------------
        integer :: nothing




        !-------------------------------------------------
        ! define model variables
        !-------------------------------------------------
        character(len=20) :: modela
        character(len=20) :: modelb
        character(len=20) :: modelc
        integer :: a_size
        integer :: b_size
        integer :: c_size
        integer, dimension(:), pointer :: a2x_aa ! ? whether the _a in aa important
        integer, dimension(:), pointer :: x2a_aa
        integer, dimension(:), pointer :: a2x_ax
        integer, dimension(:), pointer :: x2a_ax
        integer, dimension(:), pointer :: b2x_bb
        integer, dimension(:), pointer :: x2b_bb
        integer, dimension(:), pointer :: b2x_bx
        integer, dimension(:), pointer :: x2b_bx
        integer, dimension(:), pointer :: c2x_cc
        integer, dimension(:), pointer :: x2c_cc
        integer, dimension(:), pointer :: c2x_cx
        integer, dimension(:), pointer :: x2c_cx
        character(len=20) :: a_mapper
        character(len=20) :: b_mapper
        character(len=20) :: c_mapper
        !-------------------------------------------------
        ! define relative comm variables
        !-------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
        integer :: mpi_modela
        integer :: mpi_modelb
        integer :: mpi_modelc
        integer :: mpi_modela2cpl
        integer :: mpi_modelb2cpl
        integer :: mpi_modelc2cpl
        !-------------------------------------------------
        ! define comm control variables and run control
        !-------------------------------------------------
        logical :: iam_root
        logical :: iamin_modela
        logical :: iamin_modelb
        logical :: iamin_modelc
        logical :: iamin_modela2cpl
        logical :: iamin_modelb2cpl
        logical :: iamin_modelc2cpl
        logical :: iamroot_modela
        logical :: iamroot_modelb
        logical :: iamroot_modelc
        logical :: iamroot_modela2cpl
        logical :: iamroot_modelb2cpl
        logical :: iamroot_modelc2cpl
        logical :: a_run
        logical :: b_run
        logical :: c_run
    end type proc

    public :: init
    public :: union_comm
    public :: clean


contains

subroutine init(my_proc)
    
    implicit none 
    type(proc), intent(inout) :: my_proc 
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 

    my_proc%num_models = 3
    my_proc%num_comms = my_proc%models*2+2
    my_proc%num_flags = -1
    
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
   
    my_proc%modela = "modela"
    my_proc%modelb = "modelb"
    my_proc%modelc = "modelc"
    my_proc%a_size = 100
    my_proc%b_size = 100
    my_proc%c_size = 100
    
    allocate(my_proc%a2x_aa(my_proc%a_size))
    allocate(my_proc%x2a_aa(my_proc%a_size))
    allocate(my_proc%a2x_ax(my_proc%a_size))
    allocate(my_proc%x2a_ax(my_proc%a_size))
    allocate(my_proc%b2x_bb(my_proc%b_size))
    allocate(my_proc%x2b_bb(my_proc%b_size))
    allocate(my_proc%b2x_bx(my_proc%b_size))
    allocate(my_proc%x2b_bx(my_proc%b_size))
    allocate(my_proc%c2x_cc(my_proc%c_size))
    allocate(my_proc%x2c_cc(my_proc%c_size))
    allocate(my_proc%c2x_cx(my_proc%c_size))
    allocate(my_proc%x2c_cx(my_proc%c_size))

    my_proc%a_mapper = "a_mapper"
    my_proc%b_mapper = "b_mapper"
    my_proc%c_mapper = "c_mapper"

    my_proc%mpi_glocom = MPI_COMM_WORLD
    my_proc%mpi_cpl = my_proc%mpi_glocom
    my_proc%mpi_modela = my_proc%mpi_glocom
    my_proc%mpi_modelb = my_proc%mpi_glocom
    my_proc%mpi_modelc = my_proc%mpi_glocom
    my_proc%mpi_modela2cpl = my_proc%mpi_glocom
    my_proc%mpi_modelb2cpl = my_proc%mpi_glocom
    my_proc%mpi_modelc2cpl = my_proc%mpi_glocom

    if(num_rank==0) then
        my_proc%iam_root = .true.
    else
        my_proc%iam_root = .false.
    end if
    my_proc%iamin_modela = .true.
    my_proc%iamin_modelb = .true.
    my_proc%iamin_modelc = .true.
    my_proc%iamin_modela2cpl = .true.
    my_proc%iamin_modelb2cpl = .true.
    my_proc%iamin_modelc2cpl = .true.
    my_proc%iamroot_modela = .false.
    my_proc%iamroot_modelb = .false.
    my_proc%iamroot_modelc = .false.
    my_proc%iamroot_modela2cpl = .false.
    my_proc%iamroot_modelb2cpl = .false.
    my_proc%iamroot_modelc2cpl = .false.
    my_proc%a_run = .true.
    my_proc%b_run = .true.
    my_proc%c_run = .true.

    my_proc%nothing = .false.

end subroutine initv

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
