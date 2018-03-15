module comp_b
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
    public :: b_init_mct
    public :: b_run_mct
    public :: b_final_mct

contains

!-------------------------------------------------------------------------
!  a_init_mct, init gsmap_aa, avect, avect init with zero, but not init
!  dom at present
!-------------------------------------------------------------------------
subroutine b_init_mct(my_proc, ID, EClock, gsMap_bb, b2x_bb, x2b_bb, ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_bb
    type(AttrVect), intent(inout)    :: b2x_bb
    type(AttrVect), intent(inout)    :: x2b_bb
    integer,  intent(inout)          :: ierr
  
    integer, allocatable  :: start(:)
    integer, allocatable  :: length(:)
    integer               :: root = 0
    integer               :: comm_rank
    integer               :: comm_size
    integer               :: lsize
    
    lsize = 100
   
    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    call mpi_comm_size(my_proc%comp_comm(ID), comm_size, ierr)

    allocate(start(1))
    allocate(length(1))

    start(1) = comm_rank*(lsize/comm_size)
    length(1) = lsize - (comm_rank)*(lsize/comm_size)

    call gsMap_init(gsMap_bb, start, length, root, my_proc%comp_comm(ID), ID)
    
    call avect_init(b2x_bb, iList=fld_ai, rList=fld_ar, lsize=length(1))
    call avect_init(x2b_bb, iList=fld_ai, rList=fld_ar, lsize=length(1))
   
    call avect_zero(b2x_bb)
    call avect_zero(x2b_bb) 

end subroutine b_init_mct

subroutine b_run_mct(my_proc, ID, EClock, b2x, x2b, ierr)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: b2x
    type(AttrVect), intent(inout)  :: x2b
    integer, intent(inout)         :: ierr    

    !write(*,*) 'b_run'

end subroutine b_run_mct

subroutine b_final_mct()

end subroutine b_final_mct


end module comp_b
