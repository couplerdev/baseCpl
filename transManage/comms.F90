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
    private :: gsmap_check    

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

subroutine mapper_init_func()

end subroutine mapper_init_func

subroutine mapper_rearrsplit_init(mapper, my_proc, gsmap_s, ID_s, gsmap_d, ID_d, ID_join)

    implicit none
    type(map_mod), intent(inout)   :: mapper
    type(proc),    intent(in)      :: my_proc
    type(gsMap),   intent(in)      :: gsmap_s
    integer,       intent(in)      :: ID_s
    type(gsMap),   intent(in)      :: gsmap_d
    integer,       intent(in)      :: ID_d
    integer,       intent(in)      :: ID_join

    integer    :: mpicom_s, mpicom_d, mpicom_join
    type(gsMap) :: gsmap_s_join
    type(gsMap) :: gsmap_d_join

    mpicom_s = my_proc%comp_comm(ID_s)
    mpicom_d = my_proc%comp_comm(ID_d)
    mpicom_join = my_proc%comp_comm(ID_join)

    if(gsmap_Identical(gsmap_s, gsmap_d))then
        mapper%map_type = "copy"
    else 
        mapper%map_type = "rearr"
        call gsmap_extend(gsmap_s, mpicom_s, gsmap_s_join, mpicom_join, ID_join)
        call gsmap_extend(gsmap_d, mpicom_d, gsmap_d_join, mpicom_join, ID_join)

        call gsmap_check(gsmap_s_join, gsmap_d_join)
        call rearr_init(gsmap_s_join, gsmap_d_join, mpicom_join, mapper%rearr)

        call gsMap_clean(gsmap_s_join)
        call gsMap_clean(gsmap_d_join)
   end if

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

!-------------------------------------------------
! check two gsmap whether they have the same gsize
!-------------------------------------------------
subroutine gsmap_check(gsmap1, gsmap2)

    implicit none
    type(gsMap), intent(in)   :: gsmap1
    type(gsMap), intent(in)   :: gsmap2

    integer :: gsize1, gsize2
     
    gsize1 = gsMap_gsize(gsmap1)
    gsize2 = gsMap_gsize(gsmap2)

    if(gsize1 /= gsize2)then
        !--------------------------------------
        ! need modify when sys implemented
        !--------------------------------------
        write(*,*)'gsize not same'
    end if

end subroutine gsmap_check

!------------------------------------------------------
!   interpolation only based sparse matrix muliplation
!------------------------------------------------------
subroutine comp_interpolation()
 
    write(*,*)'haha'

end subroutine comp_interpolation

end module comms
