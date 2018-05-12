module comp_c
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
    public :: c_init_mct
    public :: c_run_mct
    public :: c_final_mct

contains

!-------------------------------------------------------------------------
!  a_init_mct, init gsmap_aa, avect, avect init with zero, but not init
!  dom at present
!-------------------------------------------------------------------------
subroutine c_init_mct(my_proc, ID, EClock, gsMap_cc, c2x_cc, x2c_cc, domain,ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_cc
    type(AttrVect), intent(inout)    :: c2x_cc
    type(AttrVect), intent(inout)    :: x2c_cc
    type(gGrid), intent(inout)              :: domain
    integer,  intent(inout)          :: ierr
  
    integer, allocatable  :: start(:)
    integer, allocatable  :: length(:)
    integer               :: root = 0
    integer               :: comm_rank
    integer               :: comm_size
    integer               :: lsize,gsize
    
    lsize = 100
   
    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    call mpi_comm_size(my_proc%comp_comm(ID), comm_size, ierr)

    allocate(start(1))
    allocate(length(1))

    gsize = my_proc%c_gsize
    lsize = gsize / comm_size
    start(1) = comm_rank * lsize
    length(1) = lsize

    call gsMap_init(gsMap_cc, start, length, root, my_proc%comp_comm(ID), ID)
    
    call avect_init(c2x_cc, iList=fld_ai, rList=fld_ar, lsize=length(1))
    call avect_init(x2c_cc, iList=fld_ai, rList=fld_ar, lsize=length(1))
   
    call avect_zero(c2x_cc)
    call avect_zero(x2c_cc) 

end subroutine c_init_mct

subroutine c_run_mct(my_proc, ID, EClock, c2x, x2c, ierr)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: c2x
    type(AttrVect), intent(inout)  :: x2c
    integer, intent(inout)         :: ierr    

    !write(*,*) 'c_run'

end subroutine c_run_mct

subroutine c_final_mct()

end subroutine c_final_mct


end module comp_c
