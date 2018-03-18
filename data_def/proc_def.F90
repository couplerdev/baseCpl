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
        integer :: ncomps = 8
        !-----------------------------------------------
        ! define flags
        !-----------------------------------------------
        logical :: nothing

        !-----------------------------------------------
        ! define model variables
        !-----------------------------------------------
        character(len=20) :: modela
        character(len=20) :: modelb
        character(len=20) :: modelc
        integer :: a_size
        integer :: b_size
        integer :: c_size
        ! todo dalaoshe set
        integer :: a_gsize
        integer :: b_gsize
        integer :: c_gsize
        character(len=20) :: iList = "fieldi"
        character(len=20) :: rList = "fieldr"

        type(AttrVect) :: a2x_aa
        type(AttrVect) :: a2x_ax
        type(AttrVect) :: x2a_aa
        type(AttrVect) :: x2a_ax
        type(AttrVect) :: b2x_bb
        type(AttrVect) :: b2x_bx
        type(AttrVect) :: x2b_bb
        type(AttrVect) :: x2b_bx
        type(AttrVect) :: c2x_cc
        type(AttrVect) :: c2x_cx
        type(AttrVect) :: x2c_cc
        type(AttrVect) :: x2c_cx

        type(map_mod)  :: mapper_Ca2x
        type(map_mod)  :: mapper_Cb2x
        type(map_mod)  :: mapper_Cc2x
        type(map_mod)  :: mapper_Cx2a
        type(map_mod)  :: mapper_Cx2b
        type(map_mod)  :: mapper_Cx2c
        
        !sparse mat 
        type(map_mod)  :: mapper_SMata2b
        type(map_mod)  :: mapper_SMata2c

        type(map_mod)  :: mapper_SMatb2a
        type(map_mod)  :: mapper_SMatb2c

        type(map_mod)  :: mapper_SMatc2a
        type(map_mod)  :: mapper_SMatc2b



        type(map_mod)  :: mapper_SMatx2a
        type(map_mod)  :: mapper_SMata2x

        !------------------------------------------------------
        ! define relative comm variables
        !------------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
        integer :: mpi_modela
        integer :: mpi_modelb
        integer :: mpi_modelc
        integer :: mpi_modela2cpl
        integer :: mpi_modelb2cpl
        integer :: mpi_modelc2cpl

        !-------------------------------------------------------
        ! To support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from 
        ! ID
        !-------------------------------------------------------
        integer :: gloid         = 1
        integer :: cplid         = 2
        integer :: modela_id     = 3
        integer :: modelb_id     = 4
        integer :: modelc_id     = 5
        integer :: modela2cpl_id = 6
        integer :: modelb2cpl_id = 7
        integer :: modelc2cpl_id = 8
        integer, dimension(:), pointer :: comp_comm
        integer, dimension(:), pointer :: comp_id
        ! judge if in model_a/b/c
        logical, dimension(:), pointer :: iamin_model

        !-------------------------------------------------------
        ! define comm control variables and run control 
        !-------------------------------------------------------

        logical :: iam_root


        logical :: iamin_cpl
        logical :: iamin_modela
        logical :: iamin_modelb
        logical :: iamin_modelc
        logical :: iamin_modela2cpl
        logical :: iamin_modelb2cpl
        logical :: iamin_modelc2cpl
        logical :: iamroot_cpl
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




end module proc_def
