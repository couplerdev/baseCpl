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
subroutine a_init_mct(my_proc, ID, EClock, gsMap_aa, a2x_aa, x2a_aa, domain,ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_aa
    type(AttrVect), intent(inout)    :: a2x_aa
    type(AttrVect), intent(inout)    :: x2a_aa
    type(gGrid), intent(inout)              :: domain
    integer,  intent(inout)          :: ierr
  
    integer, allocatable  :: start(:)
    integer, allocatable  :: length(:)
    integer               :: root = 0
    integer               :: comm_rank
    integer               :: comm_size
    integer               :: lsize,gsize
    
   
    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    call mpi_comm_size(my_proc%comp_comm(ID), comm_size, ierr)

    
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

subroutine a_run_mct(my_proc, ID, EClock, a2x, x2a, ierr)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: a2x
    type(AttrVect), intent(inout)  :: x2a
    integer, intent(inout)         :: ierr    
    integer comm_rank,i, av_lsize
    

    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    
!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
        av_lsize = avect_lsize(a2x) 
!        write(*,*) '<<========I am Model_A Rank:',comm_rank,' Avlsize:',av_lsize,& 
!        ' Run(ADD 1000*rank) ===========>>'
!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
 
    do i=1,av_lsize
        a2x%rAttr(1,i) = x2a%rAttr(1,i) + 1000*(comm_rank+1)
    enddo

!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
!        write(*,*) '<<===A2X_AA_VALUE Rank:',comm_rank, a2x%rAttr(1,:)
 !   call MPI_Barrier(my_proc%comp_comm(ID), ierr)


end subroutine a_run_mct

subroutine a_final_mct()

    !write(*,*) "a final"

end subroutine a_final_mct


end module comp_a
