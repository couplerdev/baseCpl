module procM

use mct_mod
use comms_def, only: mct_mod
use proc_def, only: proc
use comms, only: mapper_init
use deploy_mod, only: deploy, deploy_cpl
use mpi_comm, only:  iam_comm_root
!use deploy_mod
!use m_attrvect, only: AttrVect, mct_init => init, mct_clean => clean
    implicit none
include"mpif.h"

    public :: init
    public :: clean

contains

subroutine init(my_proc)
    
    implicit none 
    type(proc), intent(inout) :: my_proc 
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 
    integer :: iter


    my_proc%num_models = 2
    my_proc%num_comms = 6
    my_proc%num_flags = -1
    
    

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
   
    !call mct_world_init(ncomps, MPI_COMM_WORLD, comms, comps) ! comms? comps?

    my_proc%modela = "modela"
    ! todo
    my_proc%a_size = 10

    my_proc%modelb = "modelb"
    ! todo
    my_proc%b_size = 10
    ! todo
    my_proc%a_gsize = 12
    my_proc%b_gsize = 12
    
    !----------------------------------------------------------
    ! set up every comp's comm
    !----------------------------------------------------------
    my_proc%mpi_glocomm = MPI_COMM_WORLD

    allocate(my_proc%iamin_model(my_proc%ncomps))
    do iter = 1, my_proc%ncomps
        my_proc%iamin_model(iter) = .false.
    end do
    my_proc%iamin_model(1) = .true.

    ! deploy_cpl
    call deploy_cpl(my_proc%mpi_glocomm, my_proc%mpi_cpl, &
                  my_proc%cplid, my_proc%iamin_model, 0, ierr)

    call deploy(my_proc%mpi_glocomm, my_proc%mpi_modela,&
                my_proc%mpi_modela2cpl, &
                my_proc%modela_id, my_proc%cplid, &
                my_proc%modela2cpl_id, my_proc%iamin_model, 0, ierr)
    call deploy(my_proc%mpi_glocomm, my_proc%mpi_modelb,&
                my_proc%mpi_modelb2cpl, &
                my_proc%modelb_id, my_proc%cplid, &
                my_proc%modelb2cpl_id, my_proc%iamin_model, 0, ierr)



    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*), "my_world_rank:", num_rank, " my_in_model", my_proc%iamin_model
    call MPI_Barrier(MPI_COMM_WORLD, ierr)


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'comm initiated'
    allocate(my_proc%comp_comm(my_proc%ncomps))
    my_proc%comp_comm(my_proc%gloid)         = my_proc%mpi_glocomm
    my_proc%comp_comm(my_proc%cplid)         = my_proc%mpi_cpl


    my_proc%comp_comm(my_proc%modela_id)     = my_proc%mpi_modela
    my_proc%comp_comm(my_proc%modela2cpl_id) = my_proc%mpi_modela2cpl  

    my_proc%comp_comm(my_proc%modelb_id)     = my_proc%mpi_modelb
    my_proc%comp_comm(my_proc%modelb2cpl_id) = my_proc%mpi_modelb2cpl  

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*)'comp_comm initiated'
    allocate(my_proc%comp_id(my_proc%ncomps))
    do iter = 1, my_proc%ncomps
        my_proc%comp_id(iter) = iter
    end do

    call mct_world_init(my_proc%ncomps, MPI_COMM_WORLD, my_proc%comp_comm, my_proc%comp_id)

    write(*,*)'mct_world_init initiated'
    if(num_rank==0) then
        my_proc%iam_root = .true.
    else
        my_proc%iam_root = .false.
    end if

    my_proc%iamin_cpl = .false.
    if(my_proc%iamin_model(my_proc%cplid))then
        write(*,*)'Im cpl',num_rank
        call iam_comm_root(my_proc%mpi_cpl, my_proc%iamroot_cpl, ierr)
        my_proc%iamin_cpl = .true.
    end if

    my_proc%iamin_modela = .false.
    if(my_proc%iamin_model(my_proc%modela_id))then
        call iam_comm_root(my_proc%mpi_modela, my_proc%iamroot_modela, ierr)
        my_proc%iamin_modela = .true.
    end if

    my_proc%iamin_modela2cpl = .false.
    if(my_proc%iamin_model(my_proc%modela2cpl_id))then
        call iam_comm_root(my_proc%mpi_modela2cpl, &
            my_proc%iamroot_modela2cpl, ierr)
        my_proc%iamin_modela2cpl = .true.
    end if

    my_proc%iamin_modelb = .false.
    if(my_proc%iamin_model(my_proc%modelb_id))then
        call iam_comm_root(my_proc%mpi_modelb, my_proc%iamroot_modelb, ierr)
        my_proc%iamin_modelb = .true.
    end if

    my_proc%iamin_modelb2cpl = .false.
    if(my_proc%iamin_model(my_proc%modelb2cpl_id))then
        call iam_comm_root(my_proc%mpi_modelb2cpl, &
            my_proc%iamroot_modelb2cpl, ierr)
        my_proc%iamin_modelb2cpl = .true.
    end if



    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'before mapper_init'


    call mapper_init(my_proc%mapper_Ca2x, ierr)
    call mapper_init(my_proc%mapper_Cx2a, ierr)

    call mapper_init(my_proc%mapper_Cb2x, ierr)
    call mapper_init(my_proc%mapper_Cx2b, ierr)

    my_proc%nothing = .false.

end subroutine init

subroutine clean(my_proc)
    
    implicit none
    type(proc), intent(inout) :: my_proc
    integer :: ierr

    call avect_clean(my_proc%a2x_aa)
    call avect_clean(my_proc%x2a_aa)
    call avect_clean(my_proc%b2x_bb)
    call avect_clean(my_proc%x2b_bb)

    call MPI_Finalize(ierr) 

end subroutine clean

end module procM

