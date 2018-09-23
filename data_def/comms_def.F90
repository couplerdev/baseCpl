module comms_def
use mct_mod
    implicit none
    type map_mod
        character(len=20) :: map_type  ! copy::rearr::spmat::online
        character(len=20) :: coord
        type(gRearr) :: rearr
        type(SparseMatrix) :: sMat
        type(SparseMatrixPlus) :: sMatPlus
        type(SparseMatrix) :: sMatX2A
        type(SparseMatrixPlus) :: sMatX2APlus
        character(len=20) :: sparseMat 
        type(gGrid), pointer :: dom_s  ! using pointer to save mem
        type(gGrid), pointer :: dom_d
        type(gsMap), pointer :: gsmap_s
        type(gsMap), pointer :: gsmap_d
    end type map_mod


end module comms_def
