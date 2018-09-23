module proc_def
use mct_mod
use comms_def
    implicit none
!include "mpif.h"

    type proc
        !-----------------------------------------------
        ! Meta desc of proc
        !-----------------------------------------------
        integer :: num_comms
        integer :: num_flags
        integer :: num_models
        integer :: my_rank
        integer :: my_size
        integer :: ncomps = 6
        !-----------------------------------------------
        ! define flags
        !-----------------------------------------------
        logical :: nothing

        !-----------------------------------------------
        ! define model variables
        !-----------------------------------------------
        character(len=20) :: modela
        integer :: a_size
        integer :: a_gsize
        type(AttrVect) :: a2x_aa
        type(AttrVect) :: a2x_ax
        type(AttrVect) :: x2a_aa
        type(AttrVect) :: x2a_ax
        type(gGrid) :: a_grid_domain
        type(map_mod)  :: Mapper_Ca2x
        type(map_mod)  :: Mapper_Cx2a
        character(len=20) :: modelb
        integer :: b_size
        integer :: b_gsize
        type(AttrVect) :: b2x_bb
        type(AttrVect) :: b2x_bx
        type(AttrVect) :: x2b_bb
        type(AttrVect) :: x2b_bx
        type(gGrid) :: b_grid_domain
        type(map_mod)  :: Mapper_Cb2x
        type(map_mod)  :: Mapper_Cx2b

        character(len=20) :: a_domain_fld = "lat:lon:area:mask:frac"
        character(len=20) :: b_domain_fld = "lat:lon:area:mask:frac"

        
        type(map_mod)  :: a
        type(map_mod)  :: b
        !sparse mat   emmmm 
        type(map_mod)   :: mapper_SMata2b




        !------------------------------------------------------
        ! define relative comm variables
        !------------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
        integer :: mpi_modela
        integer :: mpi_modela2cpl
        integer :: mpi_modelb
        integer :: mpi_modelb2cpl

        !-------------------------------------------------------
        ! To support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from 
        ! ID
        !-------------------------------------------------------
        integer :: gloid         = 1
        integer :: cplid         = 2
        integer :: modela_id = 3
        integer :: modela2cpl_id = 5
        integer :: modelb_id = 4
        integer :: modelb2cpl_id = 6

        integer, dimension(:), pointer :: comp_comm
        integer, dimension(:), pointer :: comp_id
        ! judge if in model_a/b/c
        logical, dimension(:), pointer :: iamin_model

        !-------------------------------------------------------
        ! define comm control variables and run control 
        !-------------------------------------------------------

        logical :: iam_root
        logical :: iamin_cpl
        logical :: iamroot_cpl

        logical :: iamin_modela
        logical :: iamin_modela2cpl
        logical :: iamroot_modela
        logical :: iamroot_modela2cpl
        logical :: a_run
        logical :: iamin_modelb
        logical :: iamin_modelb2cpl
        logical :: iamroot_modelb
        logical :: iamroot_modelb2cpl
        logical :: b_run


    end type proc




end module proc_def
