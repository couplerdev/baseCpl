module comms
use procM
use mct_mod
use comms_def
    implicit none
    
    public :: comp_comm
  
contains

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
        call rearr_rearrange()
    else
        call comp_interpolation()
    end if

end subroutine comp_comm


!------------------------------------------------------
!   interpolation only based sparse matrix muliplation
!------------------------------------------------------
subroutine comp_interpolation()


end subroutine comp_interpolation

end module comms
