module procM
use mct_mod
!use deploy_mod
!use m_attrvect, only: AttrVect, mct_init => init, mct_clean => clean
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
        integer :: ncomps = 8
        !-------------------------------------------------
        ! define flags
        !-------------------------------------------------
        logical :: nothing

        !-------------------------------------------------
        ! define model variables
        !-------------------------------------------------
        character(len=20) :: modela
        character(len=20) :: modelb
        character(len=20) :: modelc
        integer :: a_size
        integer :: b_size
        integer :: c_size
        character(len=20) :: iList = "fieldi"
        character(len=20) :: rList = "fieldr"

        type(AttrVect)  :: a2x_aa
        type(AttrVect)  :: a2x_ax
        type(AttrVect)  :: x2a_aa
        type(AttrVect)  :: x2a_ax
        type(AttrVect)  :: b2x_bb
        type(AttrVect)  :: b2x_bx
        type(AttrVect)  :: x2b_bb
        type(AttrVect)  :: x2b_bx
        type(AttrVect)  :: c2x_cc
        type(AttrVect)  :: c2x_cx
        type(AttrVect)  :: x2c_cc
        type(AttrVect)  :: x2c_cx
    
        type(map_mod)   :: mapper_Ca2x
        type(map_mod)   :: mapper_Cb2x
        type(map_mod)   :: mapper_Cc2x
        type(map_mod)   :: mapper_Cx2a
        type(map_mod)   :: mapper_Cx2b
        type(map_mod)   :: mapper_Cx2c 

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

        !------------------------------------------------
        ! to support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from
        ! ID
        !------------------------------------------------
        integer :: gloid          = 1
        integer :: cplid          = 2
        integer :: modela_id      = 3
        integer :: modelb_id      = 4
        integer :: modelc_id      = 5
        integer :: modela2cpl_id  = 6
        integer :: modelb2cpl_id  = 7
        integer :: modelc2cpl_id  = 8
        integer, dimension(:), pointer :: comp_comm
        integer, dimension(:), pointer :: comp_id       

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
    public :: deploy

contains

subroutine init(my_proc)
    
    implicit none 
    type(proc), intent(inout) :: my_proc 
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 
    integer :: iter

    my_proc%num_models = 3
    my_proc%num_comms = my_proc%num_models*2+2
    my_proc%num_flags = -1
    
    

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
   
    !call mct_world_init(ncomps, MPI_COMM_WORLD, comms, comps) ! comms? comps?

    my_proc%modela = "modela"
    my_proc%modelb = "modelb"
    my_proc%modelc = "modelc"
    my_proc%a_size = 100
    my_proc%b_size = 100
    my_proc%c_size = 100
    

    my_proc%mpi_glocomm = MPI_COMM_WORLD
    my_proc%mpi_cpl = my_proc%mpi_glocomm
    my_proc%mpi_modela = my_proc%mpi_glocomm
    my_proc%mpi_modelb = my_proc%mpi_glocomm
    my_proc%mpi_modelc = my_proc%mpi_glocomm
    
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modela, my_proc%mpi_modela2cpl, ierr)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelb, my_proc%mpi_modelb2cpl, ierr)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelc, my_proc%mpi_modelc2cpl, ierr)

    allocate(my_proc%comp_comm(my_proc%ncomps))
    my_proc%comp_comm(my_proc%gloid)         = my_proc%mpi_glocomm
    my_proc%comp_comm(my_proc%cplid)         = my_proc%mpi_cpl
    my_proc%comp_comm(my_proc%modela_id)     = my_proc%mpi_modela
    my_proc%comp_comm(my_proc%modelb_id)     = my_proc%mpi_modelb
    my_proc%comp_comm(my_proc%modelc_id)     = my_proc%mpi_modelc
    my_proc%comp_comm(my_proc%modela2cpl_id) = my_proc%mpi_modela2cpl  
    my_proc%comp_comm(my_proc%modelb2cpl_id) = my_proc%mpi_modelb2cpl
    my_proc%comp_comm(my_proc%modelc2cpl_id) = my_proc%mpi_modelc2cpl

    allocate(my_proc%comp_id(my_proc%ncomps))
    do iter = 1, my_proc%ncomps
        my_proc%comp_id(iter) = iter
    end do

    call mct_world_init(my_proc%ncomps, MPI_COMM_WORLD, my_proc%comp_comm, my_proc%comp_id)

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

    call mapper_init(my_proc%mapper_Ca2x, ierr)
    call mapper_init(my_proc%mapper_Cx2a, ierr)
    call mapper_init(my_proc%mapper_Cb2x, ierr)
    call mapper_init(my_proc%mapper_Cx2b, ierr)
    call mapper_init(my_proc%mapper_Cc2x, ierr)
    call mapper_init(my_proc%mapper_Cx2c, ierr) 

    my_proc%nothing = .false.

end subroutine init

subroutine clean(my_proc)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: ierr

    call avect_clean(my_proc%a2x_aa)
    call avect_clean(my_proc%x2a_aa)
    call avect_clean(my_proc%a2x_ax)
    call avect_clean(my_proc%x2a_ax)
    call avect_clean(my_proc%b2x_bb)
    call avect_clean(my_proc%x2b_bb)
    call avect_clean(my_proc%b2x_bx)
    call avect_clean(my_proc%x2b_bx)
    call avect_clean(my_proc%c2x_cc)
    call avect_clean(my_proc%x2c_cc)
    call avect_clean(my_proc%c2x_cx)
    call avect_clean(my_proc%x2c_cx)
 
    
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


end module procM
