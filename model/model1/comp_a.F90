module comp_a
use mct_mod
use timeM
use proc_def
    implicit none
    integer  :: comp_id
    !---------------------------------------------------------
    ! notice that the fields of a2x and x2a maybe different
    ! but here we just assume they are same
    !---------------------------------------------------------
    character(len=20) :: fld_ar="x:y"
    character(len=20) :: fld_ai="u:v:w"
    public :: a_init_mct
    public :: a_run_mct
    public :: a_final_mct

contains

!-------------------------------------------------------------------------
!  a_init_mct, init gsmap_aa, avect, avect init with zero, but not init
!  dom at present
!-------------------------------------------------------------------------
subroutine a_init_mct(my_proc, ID, EClock, gsMap_aa, a2x_aa, x2a_aa, ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_aa
    type(AttrVect), intent(inout)    :: a2x_aa
    type(AttrVect), intent(inout)    :: x2a_aa
    integer,  intent(inout)          :: ierr
  
    integer, allocatable  :: start(:)
    integer, allocatable  :: length(:)
    integer               :: root = 0
    integer               :: comm_rank
    integer               :: comm_size
    integer               :: lsize,gsize
    
   
    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    call mpi_comm_size(my_proc%comp_comm(ID), comm_size, ierr)

    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
    write(*,*) '<<========I am Model_A Rank:',comm_rank,' Init ===========>>'
    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
    
    allocate(start(1))
    allocate(length(1))

    gsize = my_proc%a_gsize
    lsize = gsize / comm_size
    start(1) = comm_rank * lsize
    length(1) = lsize

    call gsMap_init(gsMap_aa, start, length, root, my_proc%comp_comm(ID), ID)
    
    call avect_init(a2x_aa, iList=fld_ai, rList=fld_ar, lsize=length(1))
    call avect_init(x2a_aa, iList=fld_ai, rList=fld_ar, lsize=length(1))
   
    call avect_zero(a2x_aa)
    call avect_zero(x2a_aa) 

end subroutine a_init_mct

subroutine a_run_mct(my_proc, ID, EClock, a2x, x2a, ierr, gsMap_aa, gsMap_ax)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: a2x
    type(AttrVect), intent(inout)  :: x2a
    integer, intent(inout)         :: ierr    
    integer comm_rank,i, av_lsize
    
    type(gsMap), intent(in)       :: gsMap_aa
    type(gsMap), intent(in)       :: gsMap_ax
! A2O SparseMatrix elements on root
    type(SparseMatrix) :: sMat
! A2O distributed SparseMatrixPlus variables
    type(SparseMatrixPlus) :: x2asMatPlus
    integer, dimension(:), pointer :: rows, cols
    real, dimension(:), pointer :: weights
    integer num_elements,n, nRows, nCols

    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    
    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
        av_lsize = avect_lsize(a2x) 
        write(*,*) '<<========I am Model_A Rank:',comm_rank,' Avlsize:',av_lsize,& 
        ' Run ===========>>'
    call MPI_Barrier(my_proc%comp_comm(ID), ierr)

    if (comm_rank == 0) then
        num_elements = 15
        allocate(rows(num_elements), cols(num_elements), &
                weights(num_elements), stat=ierr)
        do n=1, num_elements
            rows(n) = n-1
            cols(n) = n-1
            weights(n) = n
        end do
        ! dst gsize
        nRows = 15
        ! src gsize
        nCols = 15
        call sMat_init(sMat,nRows,nCols,num_elements)
        call sMat_importGRowInd(sMat, rows, size(rows))
        call sMat_importGColInd(sMat, cols, size(cols))
        call sMat_importMatrixElts(sMat, weights, size(weights))

        deallocate(rows, cols, weights, stat=ierr)
    endif
    
    call sMatPlus_init(x2asMatPlus, sMat, gsMap_aa, gsMap_aa, &
           sMat_Xonly, 0, my_proc%comp_comm(ID), ID)

    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
        write(*,*) '<<===== I am Model_A Rank:',comm_rank,' sMat Mult ======>>'
    call MPI_Barrier(my_proc%comp_comm(ID), ierr)

    call sMatAvect_Mult(x2a, x2asMatPlus, a2x)


    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
        write(*,*) '<<===A2X_AA Rank:',comm_rank, a2x%rAttr(1,:)
    call MPI_Barrier(my_proc%comp_comm(ID), ierr)


end subroutine a_run_mct

subroutine a_final_mct()

    !write(*,*) "a final"

end subroutine a_final_mct


end module comp_a
