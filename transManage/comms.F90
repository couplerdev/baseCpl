module comms
use mct_mod
use comms_def
use extend
use proc_def
    implicit none
    
    public :: mapper_init
    public :: mapper_rearrsplit_init
    public :: mapper_spmat_init
    public :: mapper_comp_map
    public :: mapper_comp_interpolation  
    public :: mapper_comp_avMerge
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

subroutine mapper_rearrsplit_init(mapper, my_proc, gsmap_s, ID_s, gsmap_d, ID_d, ID_join, ierr)

    implicit none
    type(map_mod), intent(inout)   :: mapper
    type(proc),    intent(in)      :: my_proc
    type(gsMap),   intent(in)      :: gsmap_s
    integer,       intent(in)      :: ID_s
    type(gsMap),   intent(in)      :: gsmap_d
    integer,       intent(in)      :: ID_d
    integer,       intent(in)      :: ID_join
    integer,  optional,  intent(in):: ierr

    integer    :: mpicom_s, mpicom_d, mpicom_join
    type(gsMap) :: gsmap_s_join
    type(gsMap) :: gsmap_d_join

    mpicom_s = my_proc%comp_comm(ID_s)
    mpicom_d = my_proc%comp_comm(ID_d)
    mpicom_join = my_proc%comp_comm(ID_join)

    !--if(gsmap_Identical(gsmap_s, gsmap_d))then
    if(1 == 0)then
        mapper%map_type = "copy"
    else if(1 == 1) then 
        mapper%map_type = "rearr"
        call gsmap_extend(gsmap_s, gsmap_s_join, mpicom_s, mpicom_join, ID_join)
        call gsmap_extend(gsmap_d, gsmap_d_join, mpicom_d, mpicom_join, ID_join)

        call gsmap_check(gsmap_s_join, gsmap_d_join)
        call rearr_init(gsmap_s_join, gsmap_d_join, mpicom_join, mapper%rearr)

        call gsMap_clean(gsmap_s_join)
        call gsMap_clean(gsmap_d_join)
   else 
       mapper%map_type = "spmat"
       write(*,*) "Sparse"
   end if

end subroutine mapper_rearrsplit_init


! Build sMat, gsMap_s, gsMap_d must In comp_comm(ID_s)
! 
subroutine mapper_spmat_init(my_proc, mapper,&
                ID_s, &
                nRows, nCols, nElements,&
                gsMap_s, gsMap_d)
    type(proc), intent(inout) :: my_proc
    type(map_mod), intent(inout) :: mapper
    integer, intent(in) :: ID_s
    integer, intent(in) :: nRows ! gsMap_d%gSize
    integer, intent(in) :: nCols ! gsMap_s%gSize
    integer, intent(in) :: nElements ! nums of elem which is not zero
    type(gsMap), intent(inout) :: gsMap_s, gsMap_d
    

    integer comm_rank,ierr,n
    integer, dimension(:), pointer :: rows, cols
    real, dimension(:), pointer :: weights
    



    call mpi_comm_rank(my_proc%comp_comm(ID_s), comm_rank, ierr)

    if (my_proc%iamin_model(ID_s) .and. comm_rank == 0 ) then
        allocate(rows(nElements), cols(nElements), &
                weights(nElements), stat=ierr)
        do n=1, nElements
            rows(n) = n-1
            cols(n) = n-1
            weights(n) = n
        end do
        call sMat_init(mapper%sMat,nRows,nCols,nElements)

        call sMat_importGRowInd(mapper%sMat, rows, size(rows))
        call sMat_importGColInd(mapper%sMat, cols, size(cols))
        call sMat_importMatrixElts(mapper%sMat, weights, size(weights))
        deallocate(rows, cols, weights, stat=ierr)
    endif
    mapper%map_type = "spmat"


    call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)
    call sMatPlus_init(mapper%sMatPlus, &
           mapper%sMat, gsMap_s, gsMap_d, &
           sMat_Xonly, 0, my_proc%comp_comm(ID_s), ID_s)
    call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)


end subroutine mapper_spmat_init

!----------------------------------------
!  read weight from nfl weight to init spmat
!----------------------------------------

!subroutine mapper_spmat_init(mapper, gsmap_src, gsmap_dst, mpicom, &
!                       maprcfile, maprcname, maprctype, samegrid, string)
!    implicit none
!
!    type(map_mod),     intent(inout)  :: mapper
!    type(gsmap),       intent(in)     :: gsmap_src
!    type(gsmap),       intent(in)     :: gsmap_dst
!    integer,           intent(in)     :: mpicom
!    character(len=*),  intent(in)     :: maprcfile
!    character(len=*),  intent(in)     :: maprcname
!    character(len=*),  intent(in)     :: maprctype
!    logical,           intent(in)     :: samegrid
!    character(len=*),  intent(in), optional :: string
!
!    integer   :: ssize, dsize
!   
!    call sMAtPInitnc(mapper%sMatPlus, gsmap_src, gsmap_dst, &
!                   trim(maprcfile), trim(maprcname), trim(maprctype), mpicom)
!
    


!end subroutine mapper_spmat_init

!subroutine mapper_spmat_init(my_proc, mapper,&
!                          ID_s, gsmap_src, gsmap_dst,&
!                          grid_src, grid_dst, map_type)
!    implicit none
!    type(proc),        intent(inout)   :: my_proc
!    type(map_mod),     intent(inout)   :: mapper
!    integer,           intent(inout)   :: ID_s
!    type(gsmap),       intent(in)      :: gsmap_src, gsmap_dst
!    type(gGrid),       intent(in)      :: grid_src, grid_dst
!    character(len=20), intent(in)      :: map_type

!    if(map_type=="spmat")then
!        exit()
!    endif
!    mapper%dom_s=>grid_src
!    mapper%dom_d=>grid_dst
!    mapper%gsmap_s = gsmap_src
!    mapper%gsmap_d = gsmap_dst
!    mapper%map_type = map_type


!end subroutine mapper_spmat_init

subroutine mapper_comp_map(mapper, src, dst, msgtag,  ierr, rList)
    
    implicit none
    type(map_mod),  intent(inout)              :: mapper
    type(AttrVect), intent(inout)           :: src
    type(AttrVect), intent(inout)           :: dst
    integer,        optional,   intent(in)  :: msgtag
    integer,        optional,   intent(inout) :: ierr
    character(len=*),optional,   intent(in)  :: rList

    if(mapper%map_type=="copy")then
        call avect_copy(src, dst)
    else if(mapper%map_type=="rearr")then
        call rearrange(src, dst, mapper%rearr, msgtag)
    else if(mapper%map_type=="spmat")then
        call mapper_comp_avNorm(mapper, src, dst,  rList)
    end if

end subroutine mapper_comp_map

!-------------------------------------------------
! check two gsmap whether they have the same gsize
!-------------------------------------------------
subroutine gsmap_check(gsmap1, gsmap2)

    implicit none
    type(gsMap), intent(in)   :: gsmap1
    type(gsMap), intent(in)   :: gsmap2

    integer :: gsize1, gsize2
     
    gsize1 = gsmap1%gsize
    gsize2 = gsmap2%gsize

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
subroutine mapper_comp_interpolation(mapper,&
                AV_s, AV_d)
    type(map_mod), intent(inout) :: mapper
    type(AttrVect), intent(inout) :: AV_s, AV_d
    integer comm_rank, ierr
    !call mpi_comm_rank(my_proc%comp_comm(ID_s), comm_rank, ierr)
    call sMatAvect_Mult(AV_s, mapper%sMatPlus, AV_d)
    !call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)

end subroutine mapper_comp_interpolation


subroutine mapper_comp_avNorm(mapper,&
                AV_s, AV_d, rList)
    type(map_mod), intent(inout) :: mapper
    type(AttrVect), intent(inout) :: AV_s, AV_d
    character(len=*),optional, intent(in)    :: rList 
    integer comm_rank, ierr
    if (present(rList)) then
        call sMatAvect_Mult(AV_s, mapper%sMatPlus, AV_d, rList=rList)
    else
        call sMatAvect_Mult(AV_s, mapper%sMatPlus, AV_d)
    endif

end subroutine mapper_comp_avNorm


subroutine mapper_comp_avMerge( &
                s1, s2, s3, AV_d, field)
    type(AttrVect), intent(in) :: s1,s2,s3
    type(AttrVect), intent(inout) :: AV_d
    character(len=*), intent(in)    :: field
    integer ix1,ix2,ix3,ix_d, lsize,i
    ix1 = avect_indexRA(s1, field)
    ix2 = avect_indexRA(s2, field)
    ix3 = avect_indexRA(s3, field)
    ix_d = avect_indexRA(AV_d, field)
    lsize = avect_lsize(AV_d) 
    do i=1,lsize
        AV_d%rAttr(ix_d,i) = &
           (s1%rAttr(ix1,i) + s2%rAttr(ix2,i) + s3%rAttr(ix3,i)) / 3.0
    enddo

end subroutine mapper_comp_avMerge

end module comms
