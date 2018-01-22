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
    
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modela, my_proc%mpi_modela2cpl)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelb, my_proc%mpi_modelb2cpl)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelc, my_proc%mpi_modelc2cpl)

    if(num_rank==0) then
        my_proc%iam_root = .true.
    else
        my_proc%iam_root = .false.
    end if
    call iamin_comm_root(my_proc%mpi_modela, my_proc%iamin_modela, &
                         my_proc%iamroot_modela, ierr)
    call iamin_comm_root(my_proc%mpi_modelb, my_proc%iamin_modelb, &
                         my_proc%iamroot_modelb, ierr)
    call iamin_comm_root(my_proc%mpi_modelc, my_proc%iamin_modelc, &
                         my_proc%iamroot_modelc, ierr)
    call iamin_comm_root(my_proc%mpi_modela2cpl, my_proc%iamin_modela2cpl, &
                         my_proc%iamroot_modela2cpl, ierr)
    call iamin_comm_root(my_proc%mpi_modelb2cpl, my_proc%iamin_modelb2cpl, &
                         my_proc%iamroot_modelb2cpl, ierr)
    call iamin_comm_root(my_proc%mpi_modelc2cpl, my_proc%iamin_modelc2cpl, &
                         my_proc%iamroot_modelc2cpl, ierr)

    my_proc%nothing = .false.

end subroutine initv

subroutine clean(my_proc)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: ierr

    deallocate(my_proc%a2x_aa)
    deallocate(my_proc%x2a_aa)
    deallocate(my_proc%a2x_ax)
    deallocate(my_proc%x2a_ax)
    deallocate(my_proc%b2x_bb)
    deallocate(my_proc%x2b_bb)
    deallocate(my_proc%b2x_bx)
    deallocate(my_proc%x2b_bx)
    deallocate(my_proc%c2x_cc)
    deallocate(my_proc%x2c_cc)
    deallocate(my_proc%c2x_cx)
    deallocate(my_proc%x2c_cx)
 
    
    call MPI_Finalize(ierr) 

end subroutine clean

subroutine union_comm(comm_x, comm_y, comm_union, ierr)

    implicit none
    integer, intent(in) :: comm_x
    integer, intent(in) :: comm_y
    integer, intent(inout) :: comm_union
    integer, intent(inout) :: ierr
    integer :: x_grp
    integer :: y_grp
    integer :: union_grp    
 
    call MPI_Comm_group(comm_x, x_grp)
    call MPI_Comm_group(comm_y, y_grp)
    call MPI_Group_union(x_grp, y_grp, union_grp)
    call MPI_Comm_create(MPI_COMM_WORLD, union_grp, comm_union)

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
    call MPI_Group(x_grp, me, ierr)
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

end module procM
