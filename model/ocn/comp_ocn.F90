module comp_ocn
use mct_mod
use timeM
use proc_def
    implicit none
    integer  :: comp_id
    !---------------------------------------------------------
    ! notice that the fields of a2x and x2a maybe different
    ! but here we just assume they are same
    !---------------------------------------------------------
    integer,parameter :: CXX=4096
    character(CXX) :: fld_ar=''
    character(CXX) :: fld_ai=''
    character(*),parameter :: model_name='ocn'

    public :: ocn_init_mct
    public :: ocn_run_mct
    public :: ocn_final_mct

contains

!-------------------------------------------------------------------------
!  a_init_mct, init gsmap_aa, avect, avect init with zero, but not init
!  dom at present
!-------------------------------------------------------------------------
subroutine ocn_init_mct(my_proc, ID, EClock, &
    gsMap_ocnocn, ocn2x_ocnocn, x2ocn_ocnocn, &
    domain, ierr)

    implicit none
    type(proc), intent(inout)        :: my_proc
    integer, intent(in)              :: ID
    type(Clock), intent(in)          :: EClock
    type(gsMap), intent(inout)       :: gsMap_ocnocn
    type(AttrVect), intent(inout)    :: ocn2x_ocnocn
    type(AttrVect), intent(inout)    :: x2ocn_ocnocn
    type(gGrid), intent(inout)              :: domain
    integer,  intent(inout)          :: ierr
    integer ::local_comm,i

    ! control signal
    logical :: first_time = .true. ! if the first time to run or restart

    ! domain parameter
    integer ::ngy,ngx   !global points in x-dir and y-dir
    integer ::nlseg   !num of local segs
    integer ::nproc     !num of pes
    integer ::llseg     !length of each local segs


  
    integer, allocatable  :: start(:)
    integer, allocatable  :: length(:)
    integer               :: root = 0 !root of local communicator
    integer               :: comm_rank
    integer               :: comm_size
    integer               :: lsize,gsize

    !--- formats ---
    integer               :: logunit
    character(*), parameter :: F00   = "('(ocn_init_mct) ',8a)"
    character(*), parameter :: F01   = "('(ocn_init_mct) ',a,4i8)"
    character(*), parameter :: F02   = "('(ocn_init_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(ocn_init_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(ocn_init_mct ',73('='))"
    character(*), parameter :: F91   = "('(ocn_init_mct) ',73('-'))"
    character(*), parameter :: subName = "(ocn_init_mct) "

    
    local_comm = my_proc%comp_comm(ID)
    call mpi_comm_rank(local_comm, comm_rank, ierr)
    call mpi_comm_size(local_comm, comm_size, ierr)
!---
!   init model domain parameter todo init this parameter y xml file
!---
    ngx = 10
    ngy = 10
    nlseg = 1
    nproc = comm_size
! todo bcast this parameter

   
!---
!   log model info todo logo this info to log file
!---
    logunit = 0
    if (comm_rank == root) then
       write(logunit,*  ) ' Read in Xocn input from file= xocn_in'
       write(logunit,F00)
       write(logunit,F00) '         Model  :  ',trim(model_name)
       write(logunit,F01) '           NGX  :  ',ngx
       write(logunit,F01) '           NGY  :  ',ngy
       write(logunit,F01) ' Decomposition  :  ',0
       write(logunit,F03) ' Num pes in X   :  ',nproc
       write(logunit,F03) ' Num of local Segment :  ',nlseg
       write(logunit,F01) '    inst_index  :  ',10
       write(logunit,F00)
    end if

!---
! Init Time Manager todo time set and corresponding logics
!---



!---
! Init MCT grid todo
!---
    allocate(start(nlseg))
    allocate(length(nlseg))

    gsize = my_proc%a_gsize !a_gsize =nx*ny
    lsize = gsize / nproc
    llseg = lsize / nlseg

    do i=1,nlseg
      start(i) = (comm_rank * lsize) + (i-1) * llseg
      length(i) = llseg
    end do
!---
! Init MCT Global seg map
!---
  call gsMap_init(gsMap_ocnocn, start, length, root, local_comm, ID)
!---
! Init MCT Domain todo(now is use constant number to init domain data),
!---
  call ocn_domain_init(local_comm, gsMap_ocnocn, domain)
!----
!Init Attrvect
!----
  ! Init Field list
  call seq_flds_add(fld_ar, 'Sa_z')
  call seq_flds_add(fld_ar, 'Sa_u')
  call seq_flds_add(fld_ar, 'Sa_v')
  call seq_flds_add(fld_ar, 'Sa_tbot')
  call seq_flds_add(fld_ar, 'Sa_ptem')
  call seq_flds_add(fld_ar, 'Sa_shum')
  call seq_flds_add(fld_ar, 'Sa_dens')
  call seq_flds_add(fld_ar, 'Sa_ptem')

  call avect_init(ocn2x_ocnocn, iList=fld_ai, rList=fld_ar, lsize=lsize)
  call avect_init(x2ocn_ocnocn, iList=fld_ai, rList=fld_ar, lsize=lsize)
 
  call avect_zero(ocn2x_ocnocn)
  call avect_zero(x2ocn_ocnocn) 

end subroutine ocn_init_mct

subroutine ocn_run_mct(my_proc, ID, EClock, ocn2x, x2ocn, ierr)

    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: ocn2x
    type(AttrVect), intent(inout)  :: x2ocn
    integer, intent(inout)         :: ierr    
    integer comm_rank,i, av_lsize, n_rflds, n_iflds, n,nf
    

    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
    
    av_lsize = avect_lsize(ocn2x) 
    n_rflds = avect_nRattr(x2ocn)
    n_iflds = avect_nRattr(x2ocn)

    do nf=1,n_rflds
      do n=1,av_lsize
        ocn2x%rAttr(nf,n) = (nf*100)                   
      enddo
    enddo


end subroutine ocn_run_mct

subroutine ocn_final_mct()

    !write(*,*) "a final"

end subroutine ocn_final_mct


subroutine seq_flds_add(outfld, str)
 character(len=*),intent(in)    :: str      ! string 
 character(len=*),intent(inout) :: outfld   ! output field name
 if (trim(outfld) == '') then
    outfld = trim(str)
 else
    outfld = trim(outfld)//':'//trim(str)
 end if

 if (len_trim(outfld) >= 10000) then
    write(6,*)'fields are = ',trim(outfld)
    write(6,*)'fields length = ',len_trim(outfld)  
 end if

end subroutine seq_flds_add

!--- todo

subroutine ocn_domain_init(mpicomm, gsMap_ocnocn, domain)

  integer    , intent(in)   :: mpicomm
  type(gsMap), intent(in)   :: gsMap_ocnocn
  type(gGrid), intent(inout)             :: domain

  !---local variables---
  real(8)    , pointer  :: data(:)     ! temporary
  integer , pointer  :: idata(:)    ! temporary
  integer ::ierr,lsize

  call gGrid_init(GGrid=domain, &
      CoordChars=trim('x:y:z'), &
      otherchars=trim('lat:lon:area:frac:mask:aream'),&
      lsize=gsMap_lsize(gsMap_ocnocn, mpicomm))
  call avect_zero(domain%data)

  lsize=gsMap_lsize(gsMap_ocnocn, mpicomm)

  allocate(data(lsize))
  allocate(idata(lsize))
  data(:) = -9999.0  ! generic special value 	
  call gGrid_importRAttr(domain,"lat" ,data,lsize) 
  call gGrid_importRAttr(domain,"lon" ,data,lsize) 
  call gGrid_importRAttr(domain,"area",data,lsize) 
  call gGrid_importRAttr(domain,"frac",data,lsize) 

  data(:) = 0.0  ! generic special value 	
  call gGrid_importRAttr(domain,"mask" ,data,lsize) 
  call gGrid_importRAttr(domain,"aream",data,lsize) 
  deallocate(data)
  deallocate(idata)


end subroutine

end module comp_ocn
