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
        write(*,*) "rearrsplit"
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

subroutine mapper_spmat_init(my_proc, mapper,&
                ID_s, ID_d, ID_join, &
                nRows, nCols, nElements,&
                gsMap_s, gsMap_d)
    type(proc), intent(inout) :: my_proc
    type(map_mod), intent(inout) :: mapper
    integer, intent(in) :: ID_s, ID_d, ID_join 
    integer, intent(in) :: nElements, nRows, nCols
    type(gsMap), intent(inout) :: gsMap_s, gsMap_d
    
    integer    :: mpicom_s, mpicom_d, mpicom_join
    type(gsMap) :: gsmap_s_join
    type(gsMap) :: gsmap_d_join

    integer comm_rank,ierr,n,wr
    integer, dimension(:), pointer :: rows, cols
    real, dimension(:), pointer :: weights
    

    mpicom_s = my_proc%comp_comm(ID_s)
    mpicom_d = my_proc%comp_comm(ID_d)
    mpicom_join = my_proc%comp_comm(ID_join)
    call gsmap_extend(gsmap_s, gsmap_s_join, mpicom_s, mpicom_join, ID_join)
    call gsmap_extend(gsmap_d, gsmap_d_join, mpicom_d, mpicom_join, ID_join)
    call gsmap_check(gsmap_s_join, gsmap_d_join)


    call mpi_comm_rank(my_proc%comp_comm(ID_s), comm_rank, ierr)
    
    
    if (my_proc%iamin_model(ID_s) .and. comm_rank == 0 ) then
        allocate(rows(nElements), cols(nElements), &
                weights(nElements), stat=ierr)
        do n=1, nElements
            rows(n) = n-1
            cols(n) = n-1
            weights(n) = n
        end do
        mapper%map_type = "spmat"
        call sMat_init(mapper%sMat,nRows,nCols,nElements)
        call sMat_importGRowInd(mapper%sMat, rows, size(rows))
        call sMat_importGColInd(mapper%sMat, cols, size(cols))
        call sMat_importMatrixElts(mapper%sMat, weights, size(weights))
        deallocate(rows, cols, weights, stat=ierr)
    endif


    call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)
    call sMatPlus_init(mapper%sMatPlus, &
           mapper%sMat, gsMap_s_join, gsMap_d_join, &
           sMat_Xonly, 0, my_proc%comp_comm(ID_s), ID_s)
        write(*,*) '<<=SpmatPlus Init:', comm_rank, wr, ' OVER ==>>'
    call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)

    call gsMap_clean(gsmap_s_join)
    call gsMap_clean(gsmap_d_join)

end subroutine mapper_spmat_init

subroutine mapper_comp_map(mapper, src, dst, msgtag, ierr)
    
    implicit none
    type(map_mod),  intent(in)              :: mapper
    type(AttrVect), intent(inout)           :: src
    type(AttrVect), intent(inout)           :: dst
    integer,        optional,   intent(in)  :: msgtag
    integer,        optional,   intent(inout) :: ierr

    if(mapper%map_type=="copy")then
        call avect_copy(src, dst)
        write(6,*) "copy"
    else if(mapper%map_type=="rearr")then
        call rearrange(src, dst, mapper%rearr, msgtag)
    else
        write(6,*) "else"
        !call mapper_comp_interpolation()
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
subroutine mapper_comp_interpolation(my_proc, mapper,&
                AV_s, AV_d, ID_s)
    type(proc), intent(inout) :: my_proc
    type(map_mod), intent(inout) :: mapper
    type(AttrVect), intent(inout) :: AV_s, AV_d
    integer, intent(in) :: ID_s
    integer comm_rank, ierr
    call mpi_comm_rank(my_proc%comp_comm(ID_s), comm_rank, ierr)
    call sMatAvect_Mult(AV_s, mapper%sMatPlus, AV_d)
    call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)

end subroutine mapper_comp_interpolation

end module comms
