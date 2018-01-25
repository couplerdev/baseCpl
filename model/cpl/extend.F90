module extend
use mct_mod

    implicit none
    public :: gsmap_extend
    public :: avect_extend

contains

subroutine gsmap_extend(gsmap_s, mpicom_s, gsmap_d, mpicom_d)

    implicit none
    type(gsMap), intent(in)    :: gsmap_s
    integer,     intent(in)    :: mpicom_s
    type(gsMap), intent(inout) :: gsmap_d
    integer,     intent(in)    :: mpicom_d

    integer :: rank, rank_d, rank_dg
    integer :: ngseg
    integer :: gsize
    integer :: iter
    integer :: mpigrp_s, mpigrp_d
    integer :: size_s, size_d
    integer, pointer :: pe_s(:), pe_d(:)
    integer, pointer :: start(:), length(:), peloc(:)
    integer :: ierr

    rank_d = -1
    if(mpicom_s /= MPI_COMM_NULL) then
        call mpi_comm_rank(mpicom_s, rank, ierr)   
        if(rank == 0) then
            call mpi_comm_group(mpicom_s, mpigrp_s, ierr)
            call mpi_comm_group(mpicom_d, mpigrp_d, ierr)
            call mpi_comm_size(mpicom_s, size_s, ierr)
            call mpi_comm_size(mpicom_d, size_d, ierr)

            allocate(pe_s(0:size_s-1), pe_d(0:size_d-1))
            do iter = 0, size_s-1
                pe_s(n) = n
            end do
        
            pe_s = -1
            call mpi_group_translate_ranks(mpigrp_s, size_s, pe_s, mpigrp_d, pe_d, ierr)
        
            ! error checking not implement

            ngseg = gsmap_s%ngseg
            gsize = gsmap_s%gsize
            allocate(start(ngseg), length(ngseg), peloc(ngseg))
            do iter = 1, ngseg
                start(iter)  = gsmap_s%start(iter)
                length(iter) = gsmap_s%length(iter) 
                peloc(iter)  = gsmap_s%peloc(iter) 
            end do
            call gsmap_init(gsmap_d, mpicom_d, ngseg, gsize, start, length, peloc)
            deallocate(pe_s, pe_d, start, length, peloc)
        end if
    end if

    call mpi_allreduce(rank_d, rank_dg, 1, MPI_INTEGER, MPI_MAX, mpicom_d, ierr)
    
    call mct_gsmap_bcast(gsmap_d, rank_dg, mpicom_d)

    call mpi_barrier(mpicom_d, ierr)    
    
end subroutine gsmap_extend

subroutine avect_extend(my_proc, av_s, id_s, id, ierr)
    
    implicit none
    type(proc)    , intent(in)    :: my_proc
    type(AttrVect), intent(inout) :: av_s
    integer       , intent(in)    :: id_s
    integer       , intent(in)    :: id

    integer  :: mpicom
    integer  :: rank, rank2
    integer  :: lsizei, lsizen
    integer  :: srank, srankg
    integer  :: ierr
    integer  :: nints
  
    character(len=100)  :: iList, rList

    mpicom = my_proc%comp_comm(id_s)
    call mpi_comm_rank(mpicom, rank, ierr)
    
    lsizei = -1
    if(rank /= MPI_UNDEFINED)then
        lsizei = avect_lsize(av_s)
    end if
    lsizen = 0
  
    srank = -1
    srankg = -1
    if(lsizei > 0) srank = rank

    call mpi_allreduce(srank, srankg, 1, MPI_INTEGER, MPI_MAX, mpicom, ierr)
    
    iList = " "
    rList = " "
    if(rank == srankg ) then
        if(avect_nIAttr(av_s) /= 0) iList = avect_exportIList2c(av_s)
        if(avect_nRattr(av_s) /= 0) rList = avect_exportRList2c(av_s)
    end if

    call mpi_bcast(iList, len(iList), MPI_CHARACTER, srankg, mpicom, ierr)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, srankg, mpicom, ierr)
   
    
    !----- now allocate the av on any pes where the orig size is zero. those
    !----- should be pes that either have no data and may have been allocated
    !----- before (no harm in doing it again) or have never been allocated

    if (lsizei <= 0) then
        if(len_trim(iList) > 0 .and. len_trim(rList) > 0 ) then
            call avect_init(av_s, iList=iList, rList=rList, lsize=lsizen)
        elseif (len_trim(iList) > 0 .and. len_trim(rList) ==0 )then
            call avect_init(av_s, iList=iList, lsize=lsizen)
        elseif (len_trim(iList) == 0 .and. len_trim(rList > 0)then
            call avect_init(av_s, rList=rList, lsize=lsizen)
        end if
    end if


end subroutine avect_extend



end module extend
