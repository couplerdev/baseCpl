module comms_def

    implicit none
    type map_mod
        character(len=20) :: map_type  ! copy::rearr::spmat
        character(len=20) :: coord
        type(gRearr) :: rearr
        character(len=20) :: sparseMat 
        integer :: mpicom
        type(gGrid) :: dom_s
        type(gGrid) :: dom_d
    end type map_mod

    type gsMap
        character(len=20) :: maps
    end type gsMap

    public :: mapper_init   
    public :: mapper_rearr_init
    public :: mapper_rearrsplit_init
    public :: mapper_spmat_init
contains 

subroutine mapper_init(mapper, gsmap_s,comm_s, gsmap_d, comm_d, mapper_type, filename, ierr)
 
    implicit none
    type(map_mod), intent(inout)         :: mapper
    type(gsMap), intent(in)              :: gsmap_s
    type(gsMap), intent(in)              :: gsmap_d
    type(mapper_type), intent(in)        :: mapper_type
    integer, intent(in)                  :: comm_s
    integer, intent(in)                  :: comm_d
    type(filename), optional ,intent(in) :: filename
    integer, intent(inout)               :: ierr

    mapper%map_type = mapper_type
    mapper%comm_s = comm_s
    mapper%comm_d = comm_d
    if(mapper%comm_s == mapper%comm_d)then
        mapper%mpicom = comm_s
    else
        call 
    end if
    if(mapper%map_type=="copy")then
        print 'mapper copy init'
    else if(mapper%map_type=='rearr')then
        call mapper_rearr_init(mapper, gsmap_s, gsmap_d, mapper%mpicom)
    else
        call mapper_spmat_init(mapper, filename, ierr)
    end if

end subroutine mapper_init

subroutine mapper_rearr_init(mapper, gsmap_s, gsmap_d, mpicom)

    implicit none
    type(map_mod), intent(inout) :: mapper
    type(gsMap),   intent(in)    :: gsmap_s
    type(gsMap),   intent(in)    :: gsmap_d
    integer,       intent(in)    :: mpicom
   
    if (mct_gsmap_Identical(gsmap_s, gsmap_d)) then
        mapper%map_type = "copy"
    else
        mapper%map_type = "rearr"
        call mapper_rearrsplit_init(mapper, gsmap_s, mapper%comm_s, gsmap_d, mapper%comm_d, mpicom)
    end if

end subroutine mapper_rearr_init

subroutine mapper_rearrsplit_init(mapper, gsmap_s, comm_s, gsmap_d, comm_d, comm_join)

    type(map_mod), intent(inout) :: mapper
    type(gsMap),   intent(in)    :: gsmap_s
    integer,       intent(in)    :: comm_s
    type(gsMap),   intent(in)    :: gsmap_d
    integer,       intent(in)    :: comm_d
    integer,       intent(in)    :: comm_join

    type(gsMap),   intent(in)    :: gsmap_s_join
    type(gsMap),   intent(in)    :: gsmap_d_join

    if(gsmap_Identical(gsmap_s, gsmap_d))then
        mapper%map_type = "copy"
    else
        mapper%map_type = "rearr"
        call gsmap_extend(gsmap_s, comm_s, gsmap_s_join, comm_join)
        call gsmap_extend(gsmap_d, comm_d, gsmap_d_join, comm_join)
        call rearr_init(gsmap_s_join, gsmap_d_join, comm_join, mapper%rearr)
       
        call gsmap_clean(gsmap_s_join)
        call gsmap_clean(gsmap_d_join)

end subroutine mapper_rearrsplit_init

subroutine mapper_spmat_init(mapper, filename, ierr)

end subroutine mapper_spmat_init

end module comms_def
