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
        integer :: ncomps = 14
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
        type(map_mod)  :: Mapper_Ca2x
        type(map_mod)  :: Mapper_Cx2a
        character(len=20) :: modelc
        integer :: c_size
        integer :: c_gsize
        type(AttrVect) :: c2x_cc
        type(AttrVect) :: c2x_cx
        type(AttrVect) :: x2c_cc
        type(AttrVect) :: x2c_cx
        type(map_mod)  :: Mapper_Cc2x
        type(map_mod)  :: Mapper_Cx2c
        character(len=20) :: modelb
        integer :: b_size
        integer :: b_gsize
        type(AttrVect) :: b2x_bb
        type(AttrVect) :: b2x_bx
        type(AttrVect) :: x2b_bb
        type(AttrVect) :: x2b_bx
        type(map_mod)  :: Mapper_Cb2x
        type(map_mod)  :: Mapper_Cx2b
        character(len=20) :: modelocn
        integer :: ocn_size
        integer :: ocn_gsize
        type(AttrVect) :: ocn2x_ocnocn
        type(AttrVect) :: ocn2x_ocnx
        type(AttrVect) :: x2ocn_ocnocn
        type(AttrVect) :: x2ocn_ocnx
        type(map_mod)  :: Mapper_Cocn2x
        type(map_mod)  :: Mapper_Cx2ocn
        character(len=20) :: modelatm
        integer :: atm_size
        integer :: atm_gsize
        type(AttrVect) :: atm2x_atmatm
        type(AttrVect) :: atm2x_atmx
        type(AttrVect) :: x2atm_atmatm
        type(AttrVect) :: x2atm_atmx
        type(map_mod)  :: Mapper_Catm2x
        type(map_mod)  :: Mapper_Cx2atm
        character(len=20) :: modellnd
        integer :: lnd_size
        integer :: lnd_gsize
        type(AttrVect) :: lnd2x_lndlnd
        type(AttrVect) :: lnd2x_lndx
        type(AttrVect) :: x2lnd_lndlnd
        type(AttrVect) :: x2lnd_lndx
        type(map_mod)  :: Mapper_Clnd2x
        type(map_mod)  :: Mapper_Cx2lnd

        character(len=20) :: iList = "fieldi"
        character(len=20) :: rList = "fieldr"

        
        type(map_mod)  :: a
        type(map_mod)  :: c
        type(map_mod)  :: b
        type(map_mod)  :: ocn
        type(map_mod)  :: atm
        type(map_mod)  :: lnd
        !sparse mat 
        type(map_mod)  :: mapper_SMata2b
        type(map_mod)  :: mapper_SMata2c

        type(map_mod)  :: mapper_SMatb2a
        type(map_mod)  :: mapper_SMatb2c

        type(map_mod)  :: mapper_SMatc2a
        type(map_mod)  :: mapper_SMatc2b




        !------------------------------------------------------
        ! define relative comm variables
        !------------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
        integer :: mpi_modela
        integer :: mpi_modela2cpl
        integer :: mpi_modelc
        integer :: mpi_modelc2cpl
        integer :: mpi_modelb
        integer :: mpi_modelb2cpl
        integer :: mpi_modelocn
        integer :: mpi_modelocn2cpl
        integer :: mpi_modelatm
        integer :: mpi_modelatm2cpl
        integer :: mpi_modellnd
        integer :: mpi_modellnd2cpl

        !-------------------------------------------------------
        ! To support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from 
        ! ID
        !-------------------------------------------------------
        integer :: gloid         = 1
        integer :: cplid         = 2
        integer :: modela_id = 3
        integer :: modela2cpl_id = 9
        integer :: modelc_id = 4
        integer :: modelc2cpl_id = 10
        integer :: modelb_id = 5
        integer :: modelb2cpl_id = 11
        integer :: modelocn_id = 6
        integer :: modelocn2cpl_id = 12
        integer :: modelatm_id = 7
        integer :: modelatm2cpl_id = 13
        integer :: modellnd_id = 8
        integer :: modellnd2cpl_id = 14

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
        logical :: iamin_modelc
        logical :: iamin_modelc2cpl
        logical :: iamroot_modelc
        logical :: iamroot_modelc2cpl
        logical :: c_run
        logical :: iamin_modelb
        logical :: iamin_modelb2cpl
        logical :: iamroot_modelb
        logical :: iamroot_modelb2cpl
        logical :: b_run
        logical :: iamin_modelocn
        logical :: iamin_modelocn2cpl
        logical :: iamroot_modelocn
        logical :: iamroot_modelocn2cpl
        logical :: ocn_run
        logical :: iamin_modelatm
        logical :: iamin_modelatm2cpl
        logical :: iamroot_modelatm
        logical :: iamroot_modelatm2cpl
        logical :: atm_run
        logical :: iamin_modellnd
        logical :: iamin_modellnd2cpl
        logical :: iamroot_modellnd
        logical :: iamroot_modellnd2cpl
        logical :: lnd_run


    end type proc




end module proc_def
