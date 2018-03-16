module comms_def
use mct_mod
    implicit none
    type map_mod
        character(len=20) :: map_type  ! copy::rearr::spmat
        character(len=20) :: coord
        type(gRearr) :: rearr
        type(SparseMatrix) :: sMat
        type(SparseMatrixPlus) :: sMatPlus
        type(SparseMatrix) :: sMatX2A
        type(SparseMatrixPlus) :: sMatX2APlus
        character(len=20) :: sparseMat 
        type(gGrid) :: dom_s
        type(gGrid) :: dom_d
    end type map_mod


end module comms_def
