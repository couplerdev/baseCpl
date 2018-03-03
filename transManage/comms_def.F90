module comms_def
use mct_mod
    implicit none
    type map_mod
        character(len=20) :: map_type  ! copy::rearr::spmat
        character(len=20) :: coord
        type(gRearr) :: rearr
        character(len=20) :: sparseMat 
        type(gGrid) :: dom_s
        type(gGrid) :: dom_d
    end type map_mod


end module comms_def
