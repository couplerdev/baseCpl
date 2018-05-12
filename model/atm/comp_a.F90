module comp_atm
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
    public :: atm_init_mct
    public :: atm_run_mct
    public :: atm_final_mct

contains

!-------------------------------------------------------------------------
!  a_init_mct, init gsmap_aa, avect, avect init with zero, but not init
!  dom at present
!-------------------------------------------------------------------------
subroutine atm_init_mct(my_proc, ID, EClock, gsMap_atmatm, atm2x_atmatm, x2atm_atmatm, ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_atmatm
    type(AttrVect), intent(inout)    :: atm2x_atmatm
    type(AttrVect), intent(inout)    :: x2atm_atmatm
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

    call gsMap_init(gsMap_atmatm, start, length, root, my_proc%comp_comm(ID), ID)
    
    call avect_init(atm2x_atmatm, iList=fld_ai, rList=fld_ar, lsize=length(1))
    call avect_init(x2atm_atmatm, iList=fld_ai, rList=fld_ar, lsize=length(1))
   
    call avect_zero(atm2x_atmatm)
    call avect_zero(x2atm_atmatm) 

end subroutine atm_init_mct

subroutine atm_run_mct(my_proc, ID, EClock, atm2x, x2atm, ierr)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: atm2x
    type(AttrVect), intent(inout)  :: x2atm
    integer, intent(inout)         :: ierr    
    integer comm_rank,i, av_lsize
    

    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    
!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
        av_lsize = avect_lsize(atm2x) 
!        write(*,*) '<<========I am Model_A Rank:',comm_rank,' Avlsize:',av_lsize,& 
!        ' Run(ADD 1000*rank) ===========>>'
!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
 
    do i=1,av_lsize
        atm2x%rAttr(1,i) = x2atm%rAttr(1,i) + 1000*(comm_rank+1)
    enddo

!    call MPI_Barrier(my_proc%comp_comm(ID), ierr)
!        write(*,*) '<<===A2X_AA_VALUE Rank:',comm_rank, a2x%rAttr(1,:)
 !   call MPI_Barrier(my_proc%comp_comm(ID), ierr)


end subroutine atm_run_mct

subroutine atm_final_mct()

    !write(*,*) "a final"

end subroutine atm_final_mct


end module comp_atm
