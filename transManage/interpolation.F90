module interpolation
use mct_mod
use comms_def
use comms
use netcdff
    implicit none
    public :: online_inter_try
    public :: readncFile

contains

subroutine online_inter_tryR(mapper, src, dst, rfld)
    implicit none
    type(map_mod),        intent(in)       :: mapper
    type(AttrVect),       intent(inout)    :: src, dst
    character(len=20),    intent(in)       :: rfld !assume only one fld

    type(gGrid), pointer :: dom_s
    type(gGrid), pointer :: dom_d
    integer :: lsize_s, lsize_d
    integer :: idx_s, idx_d
    real,  pointer :: data_s(:)
    real,  pointer :: data_d(:)

    if (mapper%map_type/='online')then
        write(*,*)'not online map'
        exit()
    end if
    lsize_s = call avect_lsize(src)
    lsize_d = call avect_lsize(dst)
    
    idx_s = avect_indexRA(src, rfld)
    idx_d = avect_indexRA(dst, rfld)
    
    
end subroutine

subroutine readNc_File(mapper,ncfile)
    implicit none
    type(map_mod),     intent(inout)   :: mapper
    character(len=20), intent(in)      :: ncfile

    integer          :: na
    integer          :: nb
    integer          :: ns
    integer          :: ni, nj
    integer          :: igrow
    integer          :: igcol
    integer          :: iwget

    real      , allocatable  :: rtemp(:)
    integer   , allocatable  :: itemp(:)

    integer                  :: rcode

    integer                  :: nccode
    integer                  :: ncfid
    integer                  :: ncvid
    integer                  :: ncdid

    nccode = nf_open(ncfile, NF_NOWRITE, ncfid)
    if(nccode/= NF_NOERR)then
         stop   ! need to modi this sys call
    end if

    nccode = nf_inq_dimid(ncfid, 'n_s', ncdid)
    nccode = nf_inq_dimlen(ncfid, ncdid, ns)
    nccode = nf_inq_dimid(ncfid, 'n_a', ncdid)
    nccode = nf_inq_dimlen(ncfid, ncdid, na)
    nccode = nf_inq_dimdid(ncfid, 'n_b', ncdid)
    ncdode = nf_inq_dimlen(ncfid, ncdid, nb)

    call sMap_init(mapper%sMat, nb, na, ns)

    igrow = sMat_indexIA(sMat, 'grow')
    igcol = sMat_indexIA(sMat, 'gcol')
    iwgt  = sMat_indexRA(sMat, 'weight')

    allocate(rtemp(ns), stat=rcode)
    if(rcode/=0)stop !need modi

    nccode = nf_inq_varid(ncfid, 'S', ncvid)
    nccode = nf_get_var_double(ncfid, ncvid, rtemp)

    if(nccode /= NF_NOERR)stop

    sMat%data%rAttr(iwet, :) = rtemp(:)
    
    deallocate(rtemp, stat=rcode)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  read and load rows
    allocate(itemp(ns), stat=rcode)
    if(rcode /= 0) stop

    nccode = nf_inq_varid(ncfid, 'row', ncvid)
    nccode = nf_get_var_int(ncfid, ncvid, itemp)
    if(nccode /= NF_NOERR)stop

    sMat%data%iAttr(igrow, :) = itemp(:)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  read and load colums
    itemp(:)  = 0
    nccode = nf_inq_varid(ncfid, 'col', ncvid)
    nccode = nf_get_var_int(ncfid, ncvid, itemp)
    if(nccode /= NF_NOERR)stop
    sMat%data%iAtrr(igcol, :) = itemp(:)
    
    deallocate(itemp, stat=rcode)
    if(rcode/=0)stop
    
    nccode = nf_close(nfid)
    
   
end subroutine readNc_File



end module 
