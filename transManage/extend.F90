module extend
!---------------------------------------------------------
! extendMod: when src gsmap, avect etc. have a different 
! comm set, they should extend themselves.
!--------------------------------------------------------- 
use mct_mod
use proc_def

    implicit none
    
    public :: gsmap_init
    public :: gsmap_extend
    public :: gsmap_create
    public :: avect_init
    public :: avect_extend
    public :: avect_create

contains

subroutine gsmap_init(my_proc, gsmap_s, ID_s, gsmap_d, &
                      ID_d, ID_join)

    implicit none
    type(proc), intent(in)     :: my_proc
    type(gsMap), intent(in)    :: gsmap_s
    integer,     intent(in)    :: ID_s
    type(gsMap), intent(inout) :: gsmap_d
    integer,     intent(inout) :: ID_d
    integer,     intent(in)    :: ID_join
    type(gsMap) gsmap_join
    integer :: mpi_comm_s, mpi_comm_d, mpi_comm_join, mct_compid_d, mct_compid_join
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    mpi_comm_join = my_proc%comp_comm(ID_join)
    
    mct_compid_join = my_proc%comp_id(ID_join)
    mct_compid_d = my_proc%comp_id(ID_d)

    call gsmap_o_extend(gsmap_s, gsmap_join,&
                            mpi_comm_s, mpi_comm_join,&
                            mct_compid_join)

    call gsmap_create(gsmap_join, mpi_comm_join, gsmap_d, mpi_comm_d,&
                    mct_compid_d)
    !todo clean
    !call mct_gsMap_clean(gsmap_old_join)

end subroutine gsmap_init

subroutine avect_init(my_proc, AV_s, ID_s, AV_d, ID_d, gsmap, ID_join)
    implicit none
    type(proc),     intent(in)    :: my_proc
    type(AttrVect), intent(inout) :: AV_s
    integer,        intent(in)    :: ID_s
    type(AttrVect), intent(inout) :: AV_d
    integer,        intent(in)    :: ID_d
    integer,        intent(in)    :: ID_join
    integer lsize
    lsize = mct_gsMap_lsize(gsMap_d, my_proc%comp_comm(ID_d))
    call avect_extend_2(my_proc, AV_s, ID_s, ID_d)
    call avect_create(my_proc, AV_s, AV_d,&
                        AV_s, ID_s, &
                        lsize)
end subroutine avect_init

! my implemention, need para mct_compid_o
subroutine gsmap_o_extend(gsmap_i, gsmap_o, &
              mpi_comm_i,mpi_comm_o, &
              mct_compid_o)
    implicit none
    type(gsMap), intent(in)    :: gsmap_i
    type(gsMap), intent(inout) :: gsmap_o
    integer,     intent(in) :: mpi_comm_i, mpi_comm_o, &
                               mct_compid_o
    integer :: procs_i, rank_in_comm_i, ngseg_i, gsize_i
    integer, pointer :: pei(:), peo(:)
    integer, pointer :: start(:), length(:), peloc(:)
    integer  i,status,ier
    call mpi_comm_rank(mpi_comm_o, rank_in_comm_i, ier)
    if(rank_in_comm_i == 0) then 
        ngseg_i = gsmap_i%ngseg
        gsize_i = gsmap_i%gsize
        allocate(start(ngseg_i), length(ngseg_i), peloc(ngseg_i))
        do j = 1, ngseg_i
            length(j)= gsmap_i%length(j)
            start(j) = gsmap_i%start(j)
            peloc(j) = gsmap_i%pe_loc(j)
        enddo

        call MCT_GSMap_init(gsMap_o, mct_compid_o, ngseg_i, &
                            gsize_i, start, length, peloc)
        write(6,*)'gsmap_o_extend', " start:", start, " length:", length, &
        peloc
        deallocate(pe_s, pe_d, start, length, peloc)
    endif
    ! bcast gsMap of mpi_comm_i to all the pe in mpi_comm_o
    call MCT_GSMap_bcast(gsMap_o, 0, mpi_comm_o, status)
    write(6,*)'status: ', status, " rank:", rank_in_comm_i
    call MPI_Barrier(mpi_comm_o, ier)
end subroutine gsmap_o_extend



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
      subroutine avect_o_extend(AV_i, AV_o, &
                      mpi_comm_i,mpi_comm_o, &
                      gsMap_i, iamin_i, lsize, ier)
    
        implicit none
        type(AttrVect), intent(in) :: AV_i
        type(AttrVect), intent(inout) :: AV_o
        integer, intent(in) ::  mpi_comm_i, mpi_comm_o
        type(GlobalSegMap),    intent(in)    :: gsMap_i
        logical, intent(in) :: iamin_i
        integer, intent(in) :: lsize , ier
        integer pid_in_o
        character(len=100) :: iList,rList
        call mpi_comm_rank(mpi_comm_o, pid_in_o, ier)
        iList = " "
        rList = " "
        if(iamin_i) then
            iList = MCT_AtrVt_exportIList(AV_i)
            rList = MCT_AtrVt_exportRList(AV_i)
        endif

        call mpi_bcast(iList, len(iList), MPI_CHARACTER, 0, mpi_comm_o, ier)
        call mpi_bcast(rList, len(rList), MPI_CHARACTER, 0, mpi_comm_o, ier)

        if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
          call MCT_AtrVt_init(AV_o,rList=rList,iList=iList, lsize=lsize)
        else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
          call MCT_AtrVt_init(AV_o,iList=iList,lsize=lsize)
        else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
          call MCT_AtrVt_init(AV_o,rList=rList,lsize=lsize)
        endif
      end subroutine avect_o_extend
    
end subroutine gsmap_extend

!todo my implement need to discussion
subroutine avect_extend_2(my_proc, AV_s&
              ID_s, ID_d)
    implicit none
    type(proc)    , intent(in)    :: my_proc
    type(AttrVect), intent(inout) :: AV_s
    integer, intent(in) ::  ID_s, ID_d
    integer  ::  mpi_comm_s, mpi_comm_d
    character(len=100) :: iList,rList

    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    iList = " "
    rList = " "
    if(my_proc%iamin_model(ID_s)) then
        iList = MCT_AtrVt_exportIList(AV_s)
        rList = MCT_AtrVt_exportRList(AV_s)
    endif
    call mpi_bcast(iList, len(iList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    if(.not. my_proc%iamin_model(ID_s)) then
        if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
          call MCT_AtrVt_init(AV_s,rList=rList,iList=iList, lsize=0)
        else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
          call MCT_AtrVt_init(AV_s,iList=iList,lsize=0)
        else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
          call MCT_AtrVt_init(AV_s,rList=rList,lsize=0)
        endif
    endif
end subroutine avect_extend_2

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

subroutine gsmap_create(gsmapi, mpicomi, gsmapo, mpicomo, compido)

    !-------------------------------------------------------------------------
    ! creates a new gsmap on a subset of pes, requires setting a new decomp
    !-------------------------------------------------------------------------

    implicit none
    type(gsMap), intent(in)      :: gsmapi
    integer    , intent(in)      :: mpicomi
    type(gsMap), intent(inout)   :: gsmapo
    integer    , intent(in)      :: mpicomo
    integer    , intent(in)      :: compido

    integer  :: n,m,k
    integer  :: ktot            ! num of active cells in gsmap
    integer  :: apesi, apeso    ! num of active pes in gsmap
    integer  :: lsizeo          ! local size for lindex
    integer  :: ngsegi, ngsego  ! ngseg of mpicomi, mpicomo
    integer  :: gsizei, gsizeo  ! gsize of mpicomi, mpicomo
    integer  :: msizei, msizeo  ! size of mpicomi, mpicomo
    integer  :: mranki, mranko  ! rank in mpicomi, mpicomo
    integer  :: ierrr
    integer  :: decomp_type
    integer, pointer :: start(:), length(:), peloc(:), perm(:), gindex(:), lindex(:)
    real(8)  :: rpeloc
    real(kind=8),parameter :: c1=1.0
    logical  :: gsmap_bfbflag = .false.  ! normally this should be set to false
    !--- create a new gsmap on new pes based on the old gsmap
    !--- gsmapi must be known on all mpicomo pes, compute the same
    !--- thing on all pes in parallel

    if (mpicomo /= MPI_COMM_NULL) then
    call mpi_comm_rank(mpicomi,mranki,ierr)
    call mpi_comm_size(mpicomi,msizei,ierr)
    call mpi_comm_rank(mpicomo,mranko,ierr)
    call mpi_comm_size(mpicomo,msizeo,ierr)

    ngsegi = gsmapi%ngseg
    gsizei = gsmapi%gsize
    gsizeo = gsizei
    call mct_gsMap_activepes(gsmapi,apesi)

    decomp_type = 0

    if (msizeo == apesi) then      ! preserve segments and decomp
    ! For testing - set decomp_type to 1 - to have gsmapi and gsmapo identical
    if (gsmap_bfbflag) then
    decomp_type = 1     ! better in cpl to have all decomps "same-ish"
    else
    decomp_type = 2
    end if
    elseif (ngsegi >= msizeo) then ! preserve segments, new decomp
    decomp_type = 2
    else                           ! new segments
    decomp_type = 3
    endif

    !tcx       decomp_type = 3 ! over ride setting above for testing
    !       if (mranko == 0) write(logunit,'(2A,4I)') trim(subname),' decomp_type =',decomp_type,ngsegi,msizeo,apesi

    select case (decomp_type)

    case(1)   ! --- preserve segments and decomp ---------------------

    ! -- copy the gsmap and translate the pes
    call mct_gsMap_copy(gsmapi,gsmapo)
    ngsego = ngsegi
    do n = 1,ngsego
    gsmapo%pe_loc(n) = mod(gsmapo%pe_loc(n),msizeo)    ! translate pes 1:1 from old to new
    enddo

    case(2)   ! --- preserve segments, new decomp --------------------

    ! --- preserve segments, sort the start and length, assign a new pe list
    ngsego = ngsegi
    allocate(start(ngsego),length(ngsego),peloc(ngsego),perm(ngsego))
    do n = 1,ngsego
    start(n)  = gsmapi%start(n)
    length(n) = gsmapi%length(n)
    enddo
    ! --- sort gsmap to minimize permute cost in mct
    ! call mct_indexset(perm)
    ! call mct_indexsort(ngsego,perm,start)
    ! call mct_permute(start,perm,ngsego)
    ! call mct_permute(length,perm,ngsego)
    ! --- give each pe "equal" number of segments, use reals to avoid integer overflow
    do n = 1,ngsego
    rpeloc = (((msizeo*c1)*((n-1)*c1))/(ngsego*c1))      ! give each pe "equal" number of segments, use reals to avoid integer overflow
    peloc(n) = int(rpeloc)
    enddo
    call MCT_GSMap_init(gsmapo,ngsego,start,length,peloc,0,mpicomo,compido,gsizeo)
    deallocate(start,length,peloc,perm)

    case(3)   ! --- new segments, new decomp -------------------------

    ! --- new segments, compute gindex, then parse the gridcells out evenly

    k = 0
    do n = 1,ngsegi
    do m = 1,gsmapi%length(n)
    k = k + 1
    if (k > gsizei) then
    write(6,*)' ERROR in gindex ',k,gsizei
    endif
    enddo
    enddo
    ktot = k

    allocate(gindex(ktot),perm(ktot))  

    k = 0
    do n = 1,ngsegi
    do m = 1,gsmapi%length(n)
    k = k + 1
    gindex(k) = gsmapi%start(n) + m - 1
    enddo
    enddo
    ! call mct_indexset(perm)
    ! call mct_indexsort(ktot,perm,gindex)
    ! call mct_permute(gindex,perm,ktot)

    k = 0
    do m = 0,msizeo-1
        lsizeo = ktot/msizeo
        if (m < (ktot - lsizeo*msizeo)) lsizeo = lsizeo + 1
        if (mranko == m) then
            allocate(lindex(lsizeo))
            if (k+lsizeo > ktot) then
                write(6,*),&
            ' ERROR: decomp out of bounds ',mranko,k,lsizeo,ktot
            endif
            lindex(1:lsizeo) = gindex(k+1:k+lsizeo)
        endif
    k = k + lsizeo
    enddo

    if (k /= ktot) then
        write(6,*)' ERROR: decomp incomplete ',k,ktot
    endif

    call MCT_GSMap_init(gsmapo,lindex,mpicomo,compido,size(lindex),gsizeo)
    deallocate(gindex,perm,lindex)

    case default   ! --- unknown ---
        write(6,*)' ERROR decomp_type unknown ',decomp_type
    end select

    if (mranko == 0) then
        call mct_gsmap_activepes(gsmapo,apeso)
    endif


    endif
end subroutine gsmap_create

subroutine avect_create(my_proc, AV_s, ID_s, AV_d, ID_d, lsize)
    implicit none
    type(proc),     intent(in)      :: my_proc
    type(AttrVect), intent(inout)   :: AV_s
    integer,        intent(in)      :: ID_s
    type(AttrVect), intent(inout)   :: AV_d
    integer,        intent(in)      :: ID
    integer,        intent(in)      :: lsize
    integer ::  mpi_comm_s, mpi_comm_d
    logical, intent(in) :: iamin_i
    integer pid_in_d,ier
    character(len=100) :: iList,rList
    
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    iList = " "
    rList = " "
    if(proc%iamin_model(ID_s)) then
        iList = MCT_AtrVt_exportIList(AV_s)
        rList = MCT_AtrVt_exportRList(AV_s)
    endif

    call mpi_bcast(iList, len(iList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, 0, mpi_comm_d, ier)

    if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
        call MCT_AtrVt_init(AV_d,rList=rList,iList=iList, lsize=lsize)
    else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
        call MCT_AtrVt_init(AV_d,iList=iList,lsize=lsize)
    else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
        call MCT_AtrVt_init(AV_d,rList=rList,lsize=lsize)
    endif
end subroutine avect_create

end module extend
