module comms_def

    implicit none
    type map_mod
        character(len=20) :: map_type  ! copy::rearr::spmat
        character(len=20) :: coord
        character(len=20) :: rearr
        character(len=20) :: sparseMat 
    end type map_def

    type gsMap
        character(len=20) :: maps
    end type gsMap

    public :: mapper_init   

contains 

subroutine mapper_init(mapper, ierr)
 
    implicit none
    type(map_mod), intent(inout) :: mapper
    integer, intent(inout) :: ierr

    mapper%map_type = "copy"
    mapper%coord = "coord"
    mapper%rearr = "rearr"
    mapper%sparseMat = "spmat"

end subroutine mapper_init

end module comms_def
