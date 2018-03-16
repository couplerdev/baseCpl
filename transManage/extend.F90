module extend
!---------------------------------------------------------
! extendMod: when src gsmap, avect etc. have a different 
! comm set, they should extend themselves.
!--------------------------------------------------------- 
use mct_mod
use proc_def
    implicit none
    include 'mpif.h'    
    public :: gsmap_init_ext
    public :: gsmap_extend
    public :: gsmap_create
    public :: avect_init_ext
    public :: avect_extend
    public :: avect_create

contains

subroutine gsmap_init_ext(my_proc, gsmap_s, ID_s, gsmap_d, &
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
    
    write(*,*)'my_proc:', my_proc%comp_comm," aa", ID_s,ID_d,ID_join
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    mpi_comm_join = my_proc%comp_comm(ID_join)
    
    mct_compid_join = my_proc%comp_id(ID_join)
    mct_compid_d = my_proc%comp_id(ID_d)
    write(*,*)'gsmap init begin'

    call gsmap_extend(gsmap_s, gsmap_join,&
                            mpi_comm_s, mpi_comm_join,&
                            mct_compid_join)

    call gsmap_create(gsmap_join, mpi_comm_join, gsmap_d, mpi_comm_d,&
                    mct_compid_d)
    !todo clean
    !call mct_gsMap_clean(gsmap_old_join)

end subroutine gsmap_init_ext

subroutine avect_init_ext(my_proc, AV_s, ID_s, AV_d, ID_d, gsmap_d, ID_join)
    implicit none
    type(proc),     intent(in)    :: my_proc
    type(AttrVect), intent(inout) :: AV_s
    integer,        intent(in)    :: ID_s
    type(AttrVect), intent(inout) :: AV_d
    integer,        intent(in)    :: ID_d
    type(gsMap),        intent(in):: gsMap_d
    integer,        intent(in)    :: ID_join
    integer lsize
    lsize = gsMap_lsize(gsMap_d, my_proc%comp_comm(ID_d))
    call avect_extend(my_proc, AV_s, ID_s, ID_d)
    call avect_create(my_proc, AV_s, ID_s,&
                        AV_d, ID_d, &
                        lsize)
end subroutine avect_init_ext

! my implemention, need para mct_compid_o
! need set iam_root
subroutine gsmap_extend(gsmap_i, gsmap_o, &
              mpi_comm_i,mpi_comm_o, &
              mct_compid_o)
    implicit none
    type(gsMap), intent(in)    :: gsmap_i
    type(gsMap), intent(inout) :: gsmap_o
    integer,     intent(in) :: mpi_comm_i, mpi_comm_o, &
                               mct_compid_o
    integer :: procs_i, rank_in_comm_i, ngseg_i, gsize_i
    integer :: rank_in_comm_o
    integer, pointer :: pei(:), peo(:)
    integer, pointer :: start(:), length(:), peloc(:)
    integer  i,j,status,ier
    call mpi_comm_rank(mpi_comm_o, rank_in_comm_o, ier)
    if(rank_in_comm_o == 0) then 
        ngseg_i = gsmap_i%ngseg
        gsize_i = gsmap_i%gsize
        allocate(start(ngseg_i), length(ngseg_i), peloc(ngseg_i))
        do j = 1, ngseg_i
            length(j)= gsmap_i%length(j)
            start(j) = gsmap_i%start(j)
            peloc(j) = gsmap_i%pe_loc(j)
        enddo

        call gsMap_init(gsMap_o, mct_compid_o, ngseg_i, &
                            gsize_i, start, length, peloc)
        deallocate(start, length, peloc)
    endif
    ! bcast gsMap of mpi_comm_i to all the pe in mpi_comm_o
    call gsmap_bcast(gsMap_o, 0, mpi_comm_o, status)
    !write(6,*)'status: ', status, " rank:", rank_in_comm_i
    call MPI_Barrier(mpi_comm_o, ier)
end subroutine gsmap_extend


!todo my implement need to discussion
subroutine avect_extend(my_proc, AV_s, &
              ID_s, ID_d)
    implicit none
    type(proc)    , intent(in)    :: my_proc
    type(AttrVect), intent(inout) :: AV_s
    integer, intent(in) ::  ID_s, ID_d
    integer  ::  mpi_comm_s, mpi_comm_d, ier
    character(len=100) :: iList,rList


    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)

    iList = " "
    rList = " "
    if(my_proc%iamin_model(ID_s)) then
        iList = avect_exportIList2c(AV_s)
        rList = avect_exportRList2c(AV_s)
    endif
    call mpi_bcast(iList, len(iList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    if(.not. my_proc%iamin_model(ID_s)) then
        if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
          call avect_init(AV_s,rList=rList,iList=iList, lsize=0)
        else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
          call avect_init(AV_s,iList=iList,lsize=0)
        else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
          call avect_init(AV_s,rList=rList,lsize=0)
        endif
    endif
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
    integer  :: ierr
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
    call gsMap_activepes(gsmapi,apesi)

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
    call gsMap_copy(gsmapi,gsmapo)
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
    call gsMap_init(gsmapo,ngsego,start,length,peloc,0,mpicomo,compido,gsizeo)
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

    call gsMap_init(gsmapo,lindex,mpicomo,compido,size(lindex),gsizeo)
    deallocate(gindex,perm,lindex)

    case default   ! --- unknown ---
        write(6,*)' ERROR decomp_type unknown ',decomp_type
    end select

    if (mranko == 0) then
        call gsMap_activepes(gsmapo,apeso)
    endif


    endif
end subroutine gsmap_create

subroutine avect_create(my_proc, AV_s, ID_s, AV_d, ID_d, lsize)
    implicit none
    type(proc),     intent(in)      :: my_proc
    type(AttrVect), intent(inout)   :: AV_s
    integer,        intent(in)      :: ID_s
    type(AttrVect), intent(inout)   :: AV_d
    integer,        intent(in)      :: ID_d
    integer,        intent(in)      :: lsize
    integer ::  mpi_comm_s, mpi_comm_d
    integer pid_in_d,ier
    character(len=100) :: iList,rList
    
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    iList = " "
    rList = " "
    if(my_proc%iamin_model(ID_s)) then
        iList = avect_exportIList2c(AV_s)
        rList = avect_exportRList2c(AV_s)
    endif

    call mpi_bcast(iList, len(iList), MPI_CHARACTER, 0, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, 0, mpi_comm_d, ier)

    if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
        call avect_init(AV_d,rList=rList,iList=iList, lsize=lsize)
    else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
        call avect_init(AV_d,iList=iList,lsize=lsize)
    else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
        call avect_init(AV_d,rList=rList,lsize=lsize)
    endif
end subroutine avect_create

end module extend
