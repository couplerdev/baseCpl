module comms
use procM
use mct_mod
use comms_def
use proc_def
    implicit none
    
    public :: mapper_init
    public :: mapper_rearrsplit_init
    public :: mapper_spmat_init
    public :: comp_comm
    private :: comp_interpolation  

interface mapper_init ; module procedure &
    mapper_init_nil, &
    mapper_init_func
end interface mapper_init

contains

subroutine mapper_init_nil(mapper, ierr)

    implicit none
    type(map_mod), intent(inout)   :: mapper
    integer, optional, intent(in)  :: ierr
   
    mapper%map_type = "nil"

end subroutine mapper_init_nil

subroutine mapper_init_func(mapper, gsmap_s, )

end subroutine mapper_init_func

subroutine mapper_rearrsplit_init()

end subroutine mapper_rearrsplit_init

subroutine mapper_spmat_init()

end subroutine mapper_spmat_init

subroutine comp_comm(mapper, gsMap_s, src, gsMap_d, dst, msgtag, ierr)
    
    implicit none
    type(map_mod),  intent(in)     :: mapper
    type(gsMap),    intent(in)     :: gsMap_s
    type(AttrVect), intent(inout)  :: src
    type(gsMap),    intent(in)     :: gsMap_d
    type(AttrVect), intent(inout)  :: dst
    integer,        optional,      intent(inout) :: ierr

    if(mapper%map_type=="copy")then
        call avect_copy()
    else if(mapper%map_type=="rearrange")then
        call rearr_rearrange(src, dst, mapper%rearr)
    else
        call comp_interpolation()
    end if

end subroutine comp_comm



!------------------------------------------------------
!   interpolation only based sparse matrix muliplation
!------------------------------------------------------
subroutine comp_interpolation()
 
    write(*,*)'haha'

end subroutine comp_interpolation

end module comms
