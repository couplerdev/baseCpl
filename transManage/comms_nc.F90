!
! !REVISION HISTORY
!    2018 Auc 18 :alex
!       
!
module comms_nc
use mct_mod
use comms_def
use base_sys, only: base_sys_abort
!use logUnit, only: logUnit
    implicit none
    private :: binary_search

    public :: mct_sMatReadnc
    public :: mct_sMatInitnc
    public :: mct_sMatWritenc

contains

logical function binary_search(idx, starti, counti)

    implicit none
    integer, intent(in) :: idx
    integer, intent(in) :: starti(:)
    integer, intent(in) :: counti(:)
 
    integer :: l, r, i

    logical break = .false.
 
    binary_search = .false.
    lsize = size(starti)
    if (lsize < 1)return     
   
    l = 0
    r = lsize+1
    i = (l+r)/2

    do while( .not. break)
        if(idx<starti(i))then
            r = i
        else if(idx>counti(i)+starti(i))then
            l = i
        else
            break = .true.
            return 
        end if
        i = (l+r)/2
        if (i<0 .or. i> lsize+1 .or. r-l<=2)then
            break = .true.
    end do
    return 
end function binary_search


subroutine mct_sMatReadnc(mapper, filename)

#include <netcdf.inc>
   
    type(map_mod), intent(inout) :: mapper
    character(*),  intent(in)    :: filename
    
    !----local-----
    integer             :: n
    integer             :: na           ! size of src domain
    integer             :: nb           ! size of dst domain
    integer             :: ns           ! none zero in matrix
    integer             :: ni,nj
    integer             :: igrow
    integer             :: igcol
    integer             :: iweight

    real        , allocatable :: rtemp(:)
    integer     , allocatable :: itemp(:) 
     
    integer                   :: rcode
    integer                   :: fid
    integer                   :: vid
    integer                   :: did

    if (mapper%map_type /= "smat") then
         write(*,*)'wrong type error'
         return   ! need modify to error handle 
    end if

    rcode = nf_open(filename, NF_NOWRITE, fid)
    if (rcode /= NF_NOERR)then
        write(*,*)nf_strerror(rcode)
        return   !need modify to error handle
    endif
   
    rcode = nf_inq_dimid(fid, 'n_s', did)
    rcode = nf_inq_dimlen(fid, did, ns)
    rcode = nf_inq_dimid(fid, 'n_a', did)
    rcode = nf_inq_dimlen(fid, did, na)
    rcode = nf_inq_dimid(fid, 'n_b', did)
    rcode = nf_inq_dimlen(fid, did, nb)
    
    call sMat_init(mapper%sMat, nb, na, ns)
    
    igrow = sMat_indexIA(mapper%sMat, 'grow')
    igcol = sMat_indexIA(mapper%sMat, 'gcol')
    iweight = sMat_indexIA(mapper%sMat, 'weight')
    
    allocate(rtemp(ns), stat=rcode)

    rcode = nf_inq_varid(fid, 'S', vid)
    rcode = nf_get_var_double(fid, vid, rtemp)
    if(rcode /= NF_NOERR )then
        write(*,*)'nf get error'
        return  
    end if
   
    mapper%sMat%data%rAttr(iweight, :) = rtemp(:)

    deallocate(rtemp, stat=rcode)
    
    allocate(itemp(ns), stat=rcode)
   
    rcode = nf_inq_varid(fid, 'row',vid)
    rcode = nf_get_var_int(fid, vid, itemp)
    if(rcode /= NF_NOERR)then
        write(*,*)'nf get error'
        return
    endif

    mapper%sMat%data%iAttr(igrow, :) = itemp(:)

    itemp(:) = 0

    rcode = nf_inq_varid(fid, 'col', vid)
    rcode = nf_get_var_int(fid, vid, itemp)
    if(rcode /= NF_NOERR)then
        write(*,*)'nf get error'
    endif

    mapper%sMat%data%iAttr(igcol, :) = itemp(:)
    deallocate(itemp, stat=rcode)
    if(rcode==0)then
        write(*,*)'deallocate fail'
        return 
    endif

    rcode = nf_close(fid)
    if(rcode /= 0)then
        write(*,*)'close fail'
        return
    end if
end subroutine mct_sMatReadnc

subroutine mct_sMatReaddnc(sMat, gsmap_src, gsmap_dst, newdom, areasrc, areadst, &
                           filename, mytask, mpicom, ni_i, nj_i, ni_o, nj_o)

implicit none
#include <netcdf.inc>

    type(SparseMatrix),   intent(out)           :: sMat
    type(gsmap),          intent(in),  target   :: gsmap_src
    type(gsmap),          intent(in),  target   :: gsmap_dst
    character(*),         intent(in)            :: newdom
    type(attrVect),       intent(out), optional :: areasrc
    type(attrVect),       intent(out), optional :: areadst
    character(*),         intent(in)            :: filename
    integer,              intent(in)            :: mytask
    integer,              intent(in)            :: mpicom
    integer,              intent(out), optional :: ni_i
    integer,              intent(out), optional :: nj_i
    integer,              intent(out), optional :: ni_o
    integer,              intent(out), optional :: nj_o

    integer          :: n,m
    integer          :: na
    integer          :: nb
    integer          :: ns
    integer          :: ni, nj
    integer          :: igrow
    integer          :: igcol
    integer          :: iweight
    integer          :: iarea
    integer          :: rsize
    integer          :: cnt
    integer          :: cntold
    integer          :: start(1)
    integer          :: count(1)
    integer          :: bsize
    integer          :: nread
    logical          :: mywt

    real,    allocatable  :: rtemp(:)
    real,    allocatable  :: Sbuf(:)
    integer, allocatable  :: Rbuf(:)
    integer, allocatable  :: Cbuf(:)

    integer               :: lsize
    integer               :: commsize
    integer, allocatable  :: lsstart(:)
    integer, allocatable  :: lscount(:)
    type(gsmap), pointer  :: mygsmap
    integer               :: l1, l2
    logical               :: found

    real,    allocatable  :: Snew(:), Sold(:)
    integer, allocatable  :: Rnew(:), Rold(:)
    integer, allocatable  :: Cnew(:), Cold(:)

    character, allocatable :: str(:)
    character(256)         :: attrstr
    integer                :: rcode
    integer                :: fid
    integer                :: vid
    integer                :: did
    integer, parameter     :: rbuf_size = 100000

    type(attrVect)    :: areasrc0
    type(attrVect)    :: areadst0

    character(*), parameter :: areaAV_field = "aream"

    call mpi_comm_size(mpicom, commsize)
    
    rcode = nf_open(filename, NF_NOWRITE,fid)
    if (rcode /= NF_NOERR)then
        print *, 'fail'    
    end if
    

    !--- get matrix dimensions ----------
    !--- get matrix dimensions ----------
   rcode = nf_inq_dimid (fid, 'n_s', did)  ! size of sparse matrix
   rcode = nf_inq_dimlen(fid, did  , ns)
   rcode = nf_inq_dimid (fid, 'n_a', did)  ! size of  input vector
   rcode = nf_inq_dimlen(fid, did  , na)
   rcode = nf_inq_dimid (fid, 'n_b', did)  ! size of output vector
   rcode = nf_inq_dimlen(fid, did  , nb)
   
   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
      rcode = nf_inq_dimid (fid, 'ni_a', did)  ! number of lons in input grid
      rcode = nf_inq_dimlen(fid, did  , ni_i)
      rcode = nf_inq_dimid (fid, 'nj_a', did)  ! number of lats in input grid
      rcode = nf_inq_dimlen(fid, did  , nj_i)
      rcode = nf_inq_dimid (fid, 'ni_b', did)  ! number of lons in output grid
      rcode = nf_inq_dimlen(fid, did  , ni_o)
      rcode = nf_inq_dimid (fid, 'nj_b', did)  ! number of lats in output grid
      rcode = nf_inq_dimlen(fid, did  , nj_o)
   end if

   !if (s_loglev > 0)then
   !     write(s_logunit,F01) "* matrix dims src x dst      : ",na,' x',nb
   !end if
   !if (s_loglev > 0)then
   !     write(s_logunit,F01) "* number of non-zero elements: ",ns
   !end if
 
   !--- read and load area_a ---
   if (present(areasrc)) then
   if (mytask == 0) then
      call avect_init(areasrc0,' ',areaAV_field,na)
      rcode = nf_inq_varid     (fid,'area_a',vid)
      if (rcode /= NF_NOERR) write(*,*) nf_strerror(rcode)
      rcode = nf_get_var_double(fid, vid, areasrc0%rAttr)
      if (rcode /= NF_NOERR) write(*,*) nf_strerror(rcode)
   endif
   call avect_scatter(areasrc0, areasrc, SgsMap, 0, mpicom, rcode)
   if (rcode /= 0) then!call mct_die("shr_mct_sMatReaddnc","Error on scatter of areasrc0")
       write(*,*) 'failed'
       return
   end if
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(6,*) subName,'Size of src ',mct_aVect_lSize(areasrc0)
!            write(6,*) subName,'min/max src ',minval(areasrc0%rAttr(1,:)),maxval(areasrc0%rAttr(1,:))
!         endif
!      end if
      call avect_clean(areasrc0)
   end if
   end if

   !--- read and load area_b ---
   if (present(areadst)) then
   if (mytask == 0) then
      call avect_init(areadst0,' ',areaAV_field,nb)
      rcode = nf_inq_varid     (fid,'area_b',vid)
      if (rcode /= NF_NOERR) write(*,*) nf_strerror(rcode)
      rcode = nf_get_var_double(fid, vid, areadst0%rAttr)
      if (rcode /= NF_NOERR) write(*,*) nf_strerror(rcode)
   endif
   call avect_scatter(areadst0, areadst, DgsMap, 0, mpicom, rcode)
   if (rcode /= 0) then!call mct_die("shr_mct_sMatReaddnc","Error on scatter of areadst0")
        write(*,*)'failed'
        return 
   end if
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(6,*) subName,'Size of dst ',mct_aVect_lSize(areadst0)
!            write(6,*) subName,'min/max dst ',minval(areadst0%rAttr(1,:)),maxval(areadst0%rAttr(1,:))
!         endif
!      end if
      call avect_clean(areadst0)
   endif
   endif

   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
      call mpi_bcast(ni_i,mpicom,subName//" MPI in ni_i bcast")
      call mpi_bcast(nj_i,mpicom,subName//" MPI in nj_i bcast")
      call mpi_bcast(ni_o,mpicom,subName//" MPI in ni_o bcast")
      call mpi_bcast(nj_o,mpicom,subName//" MPI in nj_o bcast")
   end if

   call mpi_bcast(ns,mpicom,subName//" MPI in ns bcast")
   call mpi_bcast(na,mpicom,subName//" MPI in na bcast")
   call mpi_bcast(nb,mpicom,subName//" MPI in nb bcast")
'''
marks
'''
   !--- setup local seg map, sorted
   if (newdom == 'src') then
      mygsmap => DgsMap
   elseif (newdom == 'dst') then
      mygsmap => SgsMap
   else
      write(*,*)'error'
      call base_sys_abort()
      !write(s_logunit,F00) 'ERROR: invalid newdom value = ',newdom
      !call shr_sys_abort(trim(subName)//" invalid newdom value")
   endif
   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then
         lsize=lsize+1
      endif
   enddo
   allocate(lsstart(lsize),lscount(lsize),stat=rcode)
   !if (rcode /= 0) call mct_perr_die(subName,':: allocate Lsstart',rcode)

   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then  ! on my pe
         lsize=lsize+1
         found = .false.
         l1 = 1
         do while (.not.found .and. l1 < lsize)         ! bubble sort copy
            if (mygsmap%start(n) < lsstart(l1)) then
               do l2 = lsize, l1+1, -1
                  lsstart(l2) = lsstart(l2-1)
                  lscount(l2) = lscount(l2-1)
               enddo
               found = .true.
            else
               l1 = l1 + 1
            endif
         enddo
         lsstart(l1) = mygsmap%start(n)
         lscount(l1) = mygsmap%length(n)
      endif
   enddo
   do n = 1,lsize-1
      if (lsstart(n) > lsstart(n+1)) then
         !write(s_logunit,F00) ' ERROR: lsstart not properly sorted'
         call base_sys_abort()
      endif
   enddo

   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   allocate(Sbuf(rsize),Rbuf(rsize),Cbuf(rsize),stat=rcode)
   !if (rcode /= 0) call mct_perr_die(subName,':: allocate Sbuf',rcode)
   allocate(Snew(bsize),Cnew(bsize),Rnew(bsize),stat=rcode)
   !if (rcode /= 0) call mct_perr_die(subName,':: allocate Snew1',rcode)

   cnt = 0
   do n = 1,nread
      start(1) = (n-1)*rsize + 1
      count(1) = min(rsize,ns-start(1)+1)

      !--- read data on root pe
      if (mytask== 0) then
         rcode = nf_inq_varid      (fid,'S'  ,vid)
         rcode = nf_get_vara_double(fid,vid,start,count,Sbuf)
         !if (rcode /= NF_NOERR .and. s_loglev > 0) write(s_logunit,F00) nf_strerror(rcode)

         rcode = nf_inq_varid      (fid,'row',vid)
         rcode = nf_get_vara_int   (fid,vid,start,count,Rbuf)
         !if (rcode /= NF_NOERR .and. s_loglev > 0) write(s_logunit,F00) nf_strerror(rcode)

         rcode = nf_inq_varid      (fid,'col',vid)
         rcode = nf_get_vara_int   (fid,vid,start,count,Cbuf)
         !if (rcode /= NF_NOERR .and. s_loglev > 0) write(s_logunit,F00) nf_strerror(rcode)
      endif

      !--- send S, row, col to all pes
      call MPI_BCAST(Sbuf, size(Sbuf), MPI_DOUBLE, 0, mpicom, ierr)
      call MPI_BCAST(Rbuf, size(Rbuf), MPI_INT, 0, mpicom, ierr)
      call MPI_BCAST(Cbuf, size(Cbuf), MPI_INT, 0, mpicom, ierr)
      !call shr_mpi_bcast(Sbuf,mpicom,subName//" MPI in Sbuf bcast")
      !call shr_mpi_bcast(Rbuf,mpicom,subName//" MPI in Rbuf bcast")
      !call shr_mpi_bcast(Cbuf,mpicom,subName//" MPI in Cbuf bcast")

      !--- now each pe keeps what it should
      do m = 1,count(1)
         !--- should this weight be on my pe
         if (newdom == 'src') then
            mywt = binary_search(Rbuf(m),lsstart,lscount)
         elseif (newdom == 'dst') then
            mywt = binary_search(Cbuf(m),lsstart,lscount)
         endif

         if (mywt) then
            cntold = cnt
            cnt = cnt + 1

            !--- new arrays need to be bigger
            if (cnt > bsize) then
               !--- allocate old arrays and copy new into old
               allocate(Sold(cntold),Rold(cntold),Cold(cntold),stat=rcode)
               !if (rcode /= 0) call mct_perr_die(subName,':: allocate old',rcode)
               Sold(1:cntold) = Snew(1:cntold)
               Rold(1:cntold) = Rnew(1:cntold)
               Cold(1:cntold) = Cnew(1:cntold)

               !--- reallocate new to bigger size, increase buffer by 50% (arbitrary)
               deallocate(Snew,Rnew,Cnew,stat=rcode)
               !if (rcode /= 0) call mct_perr_die(subName,':: allocate new',rcode)
               bsize = 1.5 * bsize
               !if (s_loglev > 1) write(s_logunit,F01) ' reallocate bsize to ',bsize
               allocate(Snew(bsize),Rnew(bsize),Cnew(bsize),stat=rcode)
               !if (rcode /= 0) call mct_perr_die(subName,':: allocate old',rcode)

               !--- copy data back into new
               Snew(1:cntold) = Sold(1:cntold)
               Rnew(1:cntold) = Rold(1:cntold)
               Cnew(1:cntold) = Cold(1:cntold)
               deallocate(Sold,Rold,Cold,stat=rcode)
               !if (rcode /= 0) call mct_perr_die(subName,':: deallocate old',rcode)
            endif

            Snew(cnt) = Sbuf(m)
            Rnew(cnt) = Rbuf(m)
            Cnew(cnt) = Cbuf(m)
         endif
      enddo  ! count
   enddo   ! nread

   deallocate(Sbuf,Rbuf,Cbuf, stat=rcode)
   !if (rcode /= 0) call mct_perr_die(subName,':: deallocate Sbuf',rcode)

   !----------------------------------------------------------------------------
   ! init the mct sMat data type
   !----------------------------------------------------------------------------
   ! mct_sMat_init must be given the number of rows and columns that
   ! would be in the full matrix.  Nrows= size of output vector=nb.
   ! Ncols = size of input vector = na.
   call sMat_init(sMat, nb, na, cnt)

   igrow = sMat_indexIA(sMat,'grow')
   igcol = sMat_indexIA(sMat,'gcol')
   iwgt  = sMat_indexRA(sMat,'weight')

   if (cnt /= 0) then
      sMat%data%rAttr(iwgt ,1:cnt) = Snew(1:cnt)
      sMat%data%iAttr(igrow,1:cnt) = Rnew(1:cnt)
      sMat%data%iAttr(igcol,1:cnt) = Cnew(1:cnt)
   endif
   deallocate(Snew,Rnew,Cnew, stat=rcode)
   deallocate(lsstart,lscount,stat=rcode)
   !if (rcode /= 0) call mct_perr_die(subName,':: deallocate new',rcode)

   !if (rcode /= 0) call mct_perr_die(subName,':: deallocate new',rcode)

   if (mytask == 0) then
      rcode = nf_close(fid)
      !if (s_loglev > 0) write(s_logunit,F00) "... done reading file"
      !call shr_sys_flush(s_logunit)
   endif


end subroutine mct_sMatReaddnc




subroutine mct_sMatPInitnc_mapfile(sMatP, gsmapX_, gsmapY_, filename, maptype, &
                                  mpicom, ni_i, nj_i, ni_o, nj_o, areasrc,areadst)

    type(SparseMatrixPlus), intent(inout) :: sMatP
    type(gsmap),            intent(in)    :: gsmapX_
    type(gsmap),            intent(in)    :: gsmapY_
    character(*),           intent(in)    :: filename
    character(*),           intent(in)    :: maptype
    integer,                intent(in)    :: mpicom
    integer,                intent(out), optional :: ni_i
    integer,                intent(out), optional :: nj_i
    integer,                intent(out), optional :: ni_o
    integer,                intent(out), optional :: nj_o
    
    type(AttrVect),         intent(out), optional :: areasrc
    type(AttrVect),         intent(out), optional :: areadst

    type(SparseMatrix) :: sMat
    type(AttrVect)     :: areasrc_map
    type(AttrVect)     :: areadst_map
   
    integer         :: lsize
    integer         :: iret
    integer         :: pe_loc
    character(3)    :: Smaptype
    logical         :: usevector = .true.
    character(*), parameter :: areaAV_field = 'aream'    


    call mpi_comm_rank(mpicom, pe_loc)

    lsize = gsmap_lsize(gsmapX_, mpicom)
    call avect_init(areasrc_map, rList=areaAV_field, lsize=lsize)
 
    lsize = gsmap_lsize(gsmapY_, mpicom)
    call avect_init(areadst_map, rList=areaAV_field, lsize=lsize)

    if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
        call mct_sMatReaddnc(sMat, gsmapX_, gsmapY_, Smaptype, areasrc_map, areadst_map, &
                   filename, pe_loc, mpicom, ni_i, nj_i, ni_o, nj_o)
    else 
        call mct_sMatReaddnc(sMat, gsmapX_, gsmapY_, Smaptype, areasrc_map, areadst_map, &
                   filename, pe_loc, mpicom)
    end if

    call sMatPlus_init(sMatP, sMat, gsmapX_, gsmapY_, 0, mpicom, gsmapX_%comp_id)
    lsize = smat_gNumEl(sMatP%Matrix, mpicom)

    if(present(areasrc))then
        call avect_copy(aVin=areasrc_map, aVout=areasrc, vector=usevector)
    end if
    if(present(areadst))then
        call avect_copy(avIn=areadst_map, aVout=areadst, vector=usevector)
    end if

    call avect_clean(areasrc_map)
    call avect_clean(areadst_map)

    call sMat_clean(sMat)

end subroutine mct_sMatPInitnc_mapfile

end module comms_nc
