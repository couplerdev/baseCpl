module extend
!---------------------------------------------------------
! extendMod: when src gsmap, avect etc. have a different 
! comm set, they should extend themselves.
!--------------------------------------------------------- 
use mct_mod
use proc_def
    implicit none
    include 'mpif.h'    
    include 'netcdf.inc'
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
    
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    mpi_comm_join = my_proc%comp_comm(ID_join)
    
    mct_compid_join = my_proc%comp_id(ID_join)
    mct_compid_d = my_proc%comp_id(ID_d)

    call gsmap_extend(gsmap_s, gsmap_join,&
                            mpi_comm_s, mpi_comm_join,&
                            mct_compid_join)

    call gsmap_create(gsmap_join, mpi_comm_join, gsmap_d, mpi_comm_d,&
                    mct_compid_d)
    !todo clean
    !call mct_gsMap_clean(gsmap_old_join)

end subroutine gsmap_init_ext

subroutine avect_init_ext(my_proc, AV_s, ID_s, AV_d, ID_d, gsMap_d, ID_join)
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
    integer  i,j,status,ier,rank_s,rank_d,srank,rrank
    call mpi_comm_rank(mpi_comm_o, rank_in_comm_o, ier)


    ! becast the rank of comm_d who is the root in comm_s
    rank_s = -1
    srank = -1
    if(mpi_comm_i /= MPI_COMM_NULL) &
        call mpi_comm_rank(mpi_comm_i, rank_s, ier)


    ! only in root of mpi_comm_i init
    if(mpi_comm_i /= MPI_COMM_NULL) then
        if(rank_s == 0) then 
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
    endif
    
    call mpi_comm_rank(mpi_comm_o, rank_d, ier)
    if(rank_s == 0) then
        srank = rank_d
    endif
    call mpi_allreduce(srank, rrank, &
        1, MPI_INT, MPI_MAX,& 
        mpi_comm_o, ier)

    ! bcast gsMap of mpi_comm_i to all the pe in mpi_comm_o
    call gsmap_bcast(gsMap_o, rrank, mpi_comm_o, status)
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
    integer  ::  mpi_comm_s, mpi_comm_d, ier,srank,rrank,rank_s,rank_d
    character(len=100) :: iList,rList


    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)

    ! becast the rank of comm_d who is the root in comm_s
    rank_s = -1
    srank = -1
    if(my_proc%iamin_model(ID_s)) &
        call mpi_comm_rank(mpi_comm_s, rank_s, ier)

    call mpi_comm_rank(mpi_comm_d, rank_d, ier)
    if(rank_s == 0) then
        srank = rank_d
    endif
    call mpi_allreduce(srank, rrank, &
        1, MPI_INT, MPI_MAX,& 
    mpi_comm_d, ier)


    iList = " "
    rList = " "
    if(my_proc%iamin_model(ID_s)) then
        iList = avect_exportIList2c(AV_s)
        rList = avect_exportRList2c(AV_s)
    endif
    call mpi_bcast(iList, len(iList), MPI_CHARACTER, rrank, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, rrank, mpi_comm_d, ier)
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
    integer pid_in_d,ier, rank_s, rank_d, srank, rrank
    character(len=1000) :: iList,rList
    
    mpi_comm_s = my_proc%comp_comm(ID_s)
    mpi_comm_d = my_proc%comp_comm(ID_d)
    
    ! becast the rank of comm_d who is the root in comm_s
    rank_s = -1
    srank = -1
    if(my_proc%iamin_model(ID_s)) &
        call mpi_comm_rank(mpi_comm_s, rank_s, ier)

    call mpi_comm_rank(mpi_comm_d, rank_d, ier)
    if(rank_s == 0) then
        srank = rank_d
    endif
    call mpi_allreduce(srank, rrank, &
        1, MPI_INT, MPI_MAX,& 
    mpi_comm_d, ier)



    iList = " "
    rList = " "
    if(my_proc%iamin_model(ID_s)) then
        iList = avect_exportIList2c(AV_s)
        rList = avect_exportRList2c(AV_s)
    endif



    call mpi_bcast(iList, len(iList), MPI_CHARACTER, rrank, mpi_comm_d, ier)
    call mpi_bcast(rList, len(rList), MPI_CHARACTER, rrank, mpi_comm_d, ier)




    if(len_trim(iList) > 0 .and. len_trim(rList) > 0) then
        call avect_init(AV_d,rList=rList,iList=iList, lsize=lsize)
    else if(len_trim(iList) > 0 .and. len_trim(rList) == 0) then
        call avect_init(AV_d,iList=iList,lsize=lsize)
    else if(len_trim(iList) == 0 .and. len_trim(rList) > 0) then
        call avect_init(AV_d,rList=rList,lsize=lsize)
    endif
end subroutine avect_create

subroutine save_model_av(my_proc, AV, gsMap_AV, ID, time)
    implicit none
    type(proc),     intent(in)      :: my_proc
    type(AttrVect), intent(in)   :: AV
    type(gsMap), intent(in)   :: gsMap_AV
    integer,        intent(in)      :: ID
    integer,        intent(in)      :: time
    integer ::  mpi_comm
    integer i, fd, ierr, nlseg, comm_rank
    INTEGER(KIND=MPI_OFFSET_KIND) :: offset
    integer, dimension(:),pointer :: points
    integer, dimension(MPI_STATUS_SIZE) :: status
    character(len=1000) :: check_point_path

    mpi_comm = my_proc%comp_comm(ID)
    if(mpi_comm /= MPI_COMM_NULL) then
        check_point_path = "file.txt"

        call mpi_comm_rank(mpi_comm, comm_rank, ierr)
        nlseg = gsMap_lsize(gsMap_AV, mpi_comm)        
        call gsMap_order(gsMap_AV,comm_rank,points)

        call MPI_File_Open(mpi_comm, &
            check_point_path, MPI_MODE_WRONLY + MPI_MODE_CREATE, &
            MPI_INFO_NULL, fd, ierr)
        if (ierr /= MPI_SUCCESS) write(*,*) 'Open error on rank ', comm_rank
        call MPI_Barrier(mpi_comm, ierr)

        write(*,*) ' rank',comm_rank, ' write', AV%rAttr(1,:), nlseg

        do i=1,nlseg
            offset = points(i) * 8
            call MPI_File_Write_At(&
                    fd,offset, &
                    AV%rAttr(1,i), 1, MPI_DOUBLE_PRECISION,&
                    status, ierr)
        end do

        if (ierr /= MPI_SUCCESS) &
            write(*,*) 'Write error on rank ', comm_rank, ' ', offset
        call MPI_File_Close(fd, ierr)
    endif
end subroutine save_model_av

subroutine log_msg(my_proc, ID, msg, log_path)
    implicit none
    type(proc),     intent(in)      :: my_proc
    integer,        intent(in)      :: ID
    character(len=*), intent(in) :: msg
    character(len=*), intent(in) :: log_path
    character, pointer :: msgs(:)
    character, pointer :: fmsgs(:)
    integer ::  mpi_comm
    integer i, ierr, comm_rank, comm_size, j
    character(len=22) txt_time
    integer date_time(8)
    character*10 b(3)
    call date_and_time(b(1), b(2), b(3), date_time)

    mpi_comm = my_proc%comp_comm(ID)

    if(mpi_comm /= MPI_COMM_NULL) then
        call mpi_comm_rank(mpi_comm, comm_rank, ierr)
        call mpi_comm_size(mpi_comm, comm_size, ierr)
        allocate(msgs(comm_size * len(msg)))
        allocate(fmsgs(comm_size * len(msg) + 22))
        call mpi_gather(msg, len(msg), MPI_CHARACTER, &
            msgs, len(msg), MPI_CHARACTER, &
            0, mpi_comm, ierr)
        if(comm_rank == 0) then
            open(3, position='Append', file=log_path)
            write(txt_time,"(I4,A1,I2,A1,I2,A1,I2,A1,I2,A1,I2,A2)") &
                date_time(1),'-',&
                date_time(2),'-',&
                date_time(3),' ',&
                date_time(5),'-',&
                date_time(6),'-',date_time(7),': '
            write(3,*) txt_time,msgs
            close(3)
        endif
        call MPI_Barrier(mpi_comm, ierr)
        
        deallocate(msgs)
    endif
end subroutine log_msg


subroutine log_run_msg(my_proc, ID, log_path, time)
    implicit none
    type(proc),     intent(in)      :: my_proc
    integer,        intent(in)      :: ID
    character(len=*), intent(in) :: log_path
    integer,        intent(in)      :: time
    character(len=30) ::  msg
    character(len=4) ::  txt_rank
    integer :: comm_rank, mpi_comm, ierr
    mpi_comm = my_proc%comp_comm(ID)

    if(mpi_comm /= MPI_COMM_NULL) then
        call mpi_comm_rank(mpi_comm, comm_rank, ierr)
        write(txt_rank,"(I4)") comm_rank
        msg = 'COMM_RANK:' // txt_rank // ' STATUS: RUN'
        call MPI_Barrier(mpi_comm, ierr)
        call log_msg(my_proc, ID, msg, log_path)
    endif
end subroutine log_run_msg



subroutine read_netcdf(my_proc, ID, check_point_path, time, AV, gsMap_AV)
    !读取id model的全网格数据，并根据在GSMAP上的网格数据分布 设置到对应AV
    implicit none
    type(proc),     intent(in)      :: my_proc
    integer,        intent(in)      :: ID
    character(len=*), intent(in) :: check_point_path
    integer,        intent(in)      :: time
    type(AttrVect), intent(in)      :: AV
    type(gsMap), intent(in)      :: gsMap_AV
    integer :: comm_rank, mpi_comm, nlseg, gsize, ierr 
    integer :: ncid, rvarid, ivarid,  j,i
    integer, dimension(:),pointer :: points
    type(AttrVect)      :: sAV
    character(len=100) :: iList,rList
    character(len=100),dimension(100) :: iList_s, rList_s
    integer ::ilist_len,rlist_len


    
    iList = avect_exportIList2c(AV)
    rList = avect_exportRList2c(AV)
    call StringSplit(rList,":",rList_s,rlist_len) 
    call StringSplit(iList,":",iList_s,ilist_len) 

    write(*,*) "List: ", trim(rList),' ', trim(iList), gsMap_AV%gsize
    !todo 
    call avect_init(sAV,rList=trim(rList),iList=trim(iList), lsize=2)!gsMap_AV%gsize)

    call check(nf_open(trim(check_point_path),nf_nowrite,ncid))
    write(*,*) 'read open ok:'
    call check(nf_inq_varid(ncid, 'rdata', rvarid)) 
    call check(nf_inq_varid(ncid, 'idata', ivarid))     

    write(*,*) 'get var ok:'
    call check(nf_get_var_real(ncid,rvarid,sAV%rAttr))
    call check(nf_get_var_int(ncid,ivarid,sAV%iAttr))
    write(*,*) 'read data ok'

   
    mpi_comm = my_proc%comp_comm(ID)
    if(mpi_comm /= MPI_COMM_NULL) then
        call mpi_comm_rank(mpi_comm, comm_rank, ierr)
        nlseg = gsMap_lsize(gsMap_AV, mpi_comm)        
        call gsMap_order(gsMap_AV,comm_rank,points)

        do j=1,rlist_len 
            do i=1,nlseg
                AV%rAttr(j,i) = sAV%rAttr(j,points(i)+1)
            end do
        end do

        do j=1,ilist_len 
            do i=1,nlseg
                AV%iAttr(j,i) = sAV%iAttr(j,points(i)+1)
            end do
        end do

        write(*,*) 'scatter data ok'
    endif

    write(*,*) AV%rAttr(1,:)
    write(*,*) AV%rAttr(2,:)
    write(*,*) AV%iAttr(1,:)
    write(*,*) AV%iAttr(2,:)
    write(*,*) AV%iAttr(3,:)

end subroutine read_netcdf

subroutine write_netcdf(my_proc, ID, log_path, time, AV, gsMap_AV)
    !todo
    !将AV数据写入netcdf文件， 需要增加AV gather步骤, 即需要将AV转为全局AV
    implicit none
    type(proc),     intent(in)      :: my_proc
    integer,        intent(in)      :: ID
    character(len=*), intent(in) :: log_path
    integer,        intent(in)      :: time
    type(AttrVect), intent(in)      :: AV
    type(gsMap), intent(in)      :: gsMap_AV
    integer :: comm_rank, mpi_comm, ierr, lsize
    integer :: ncid, ivarid, rvarid
    integer :: list_len,i
    integer,pointer :: rdimsid(:), idimsid(:)
    character(len=100) :: iList,rList
    character(len=100),dimension(100) :: iList_s, rList_s
    integer ::ilist_len,rlist_len
    
    iList = avect_exportIList2c(AV)
    rList = avect_exportRList2c(AV)
    call StringSplit(rList,":",rList_s,rlist_len) 
    call StringSplit(iList,":",iList_s,ilist_len) 

    allocate(rdimsid(rlist_len))
    allocate(idimsid(ilist_len))

    mpi_comm = my_proc%comp_comm(ID)
    if(mpi_comm /= MPI_COMM_NULL) then
        lsize = gsMap_lsize(gsMap_AV, mpi_comm)
        call check(nf_create(log_path, NF_CLOBBER, ncid))
        write(*,*) " create ok"

        do i=1,rlist_len
            call check(nf_def_dim(ncid, trim(rList_s(i)), lsize+1, rdimsid(i)))
        end do

        write(*,*) " def rdims ok"

        do i=1,ilist_len
            call check(nf_def_dim(ncid, trim(iList_s(i)), lsize+1, idimsid(i)))
        end do
        write(*,*) " def idims ok"

        call check(nf_def_var(ncid, "rdata", NF_float, rlist_len, rdimsid, rvarid))

        call check(nf_def_var(ncid, "idata", NF_int, ilist_len, idimsid, ivarid))

        write(*,*) " def var ok"
        ierr=nf_enddef(ncid) 
        write(*,*) " def ok", AV%rAttr(1,:)
        
        call check(nf_put_var_real(ncid, rvarid, AV%rAttr))
        call check(nf_put_var_int(ncid, ivarid, AV%iAttr))
        call check(nf_close(ncid))
    end if

    deallocate(rdimsid)
    deallocate(idimsid)
end subroutine write_netcdf


subroutine check(status)
integer, intent ( in) :: status

if(status /= nf_noerr) then
  print *, trim(nf_strerror(status))
  stop 2
end if
end subroutine check

subroutine StringSplit(InStr,delimiter,StrArray,nsize)  
!----------------------------------------------  
!---将字符串InStr进行分割,结果放入StrArray中  
!---delimiter::分隔符号,例如';,,' 使用;和,分割字符串  
!---nsize:分割数目  
!---吴徐平2011-04-29(wxp07@qq.com)  
!----------------------------------------------  
implicit none  
character(len = *) , Intent( IN ) :: InStr  
character(len = *)  , Intent( IN ) :: delimiter  
character(len = LEN(InStr)),dimension(LEN(InStr)),Intent( OUT ) :: StrArray  
integer, Intent( OUT ) :: nsize ! Effective Size of StrArray  
integer:: i,j ! loop variable  
integer:: istart ! split index for Start Position  
nsize=0  
istart=1  
do i=1,LEN(InStr)  
    do j=1,LEN(delimiter)  
        if (InStr(i:i) == delimiter(j:j)) then  
            if (istart == i) then  
            istart=i+1 ! ---可防止分隔符相连的情况  
            end if  
            if (istart<i) then  
                nsize=nsize+1  
                StrArray(nsize)=InStr(istart:i-1)  
                istart=i+1  
            end if  
        end if  
    end do  
end do  

! ---匹配最后一个子字符串  
if (nsize>0) then  
    if (istart<LEN(InStr)) then  
        nsize=nsize+1  
        StrArray(nsize)=InStr(istart:LEN(InStr))  
    end if  
end if  
! ---如果无可分割的子字符串,则包含整个字符串为数组的第一元素  
if ( (nsize<1) .AND. (LEN(TRIM(InStr)) > 0 )) then  
        nsize=1  
        StrArray(1)=InStr  
end if  
end subroutine StringSplit  


end module extend
