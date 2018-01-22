module procM
use cpl_vect ,only: c_vect, cv_init => init, cv_zero => zero, &
                  vect_size, cv_del => del
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
        type(c_vect) :: a2x_aa ! ? whether the _a in aa important
        type(c_vect) :: x2a_aa
        type(c_vect) :: a2x_ax
        type(c_vect) :: x2a_ax
        type(c_vect) :: b2x_bb
        type(c_vect) :: x2b_bb
        type(c_vect) :: b2x_bx
        type(c_vect) :: x2b_bx
        type(c_vect) :: c2x_cc
        type(c_vect) :: x2c_cc
        type(c_vect) :: c2x_cx
        type(c_vect) :: x2c_cx
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
    my_proc%num_comms = my_proc%num_models*2+2
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
    
    call cv_init(my_proc%a2x_aa, my_proc%a_size)
    call cv_init(my_proc%x2a_aa, my_proc%a_size)
    call cv_init(my_proc%a2x_ax, my_proc%a_size)
    call cv_init(my_proc%x2a_ax, my_proc%a_size)
    call cv_init(my_proc%b2x_bb, my_proc%b_size)
    call cv_init(my_proc%x2b_bb, my_proc%b_size)
    call cv_init(my_proc%b2x_bx, my_proc%b_size)
    call cv_init(my_proc%x2b_bx, my_proc%b_size)
    call cv_init(my_proc%c2x_cc, my_proc%c_size)
    call cv_init(my_proc%x2c_cc, my_proc%c_size)
    call cv_init(my_proc%c2x_cx, my_proc%c_size)
    call cv_init(my_proc%x2c_cx, my_proc%c_size)

    my_proc%a_mapper = "a_mapper"
    my_proc%b_mapper = "b_mapper"
    my_proc%c_mapper = "c_mapper"

    my_proc%mpi_glocomm = MPI_COMM_WORLD
    my_proc%mpi_cpl = my_proc%mpi_glocomm
    my_proc%mpi_modela = my_proc%mpi_glocomm
    my_proc%mpi_modelb = my_proc%mpi_glocomm
    my_proc%mpi_modelc = my_proc%mpi_glocomm
    
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modela, my_proc%mpi_modela2cpl, ierr)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelb, my_proc%mpi_modelb2cpl, ierr)
    call union_comm(my_proc%mpi_cpl, my_proc%mpi_modelc, my_proc%mpi_modelc2cpl, ierr)

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

end subroutine init

subroutine clean(my_proc)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: ierr

    call cv_del(my_proc%a2x_aa)
    call cv_del(my_proc%x2a_aa)
    call cv_del(my_proc%a2x_ax)
    call cv_del(my_proc%x2a_ax)
    call cv_del(my_proc%b2x_bb)
    call cv_del(my_proc%x2b_bb)
    call cv_del(my_proc%b2x_bx)
    call cv_del(my_proc%x2b_bx)
    call cv_del(my_proc%c2x_cc)
    call cv_del(my_proc%x2c_cc)
    call cv_del(my_proc%c2x_cx)
    call cv_del(my_proc%x2c_cx)
 
    
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
