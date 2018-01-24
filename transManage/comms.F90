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


end subroutine comp_comm



end module comms
