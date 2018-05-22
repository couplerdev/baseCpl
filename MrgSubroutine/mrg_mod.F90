module mrg_mod

use mct_mod
use proc_def
!use comms
use mpi_comm

    implicit none
     
    !----------------------------------------------------
    !   add user define subroutine
    !----------------------------------------------------
    public :: mrg_x2atm
    !public :: mrg_x2lnd
    !public :: mrg_x2ocn

contains

subroutine mrg_x2atm(my_proc, x2atm_atm, ocn2x_atm, lnd2x_atm, fractions_a)

    type(proc), intent(inout)      :: my_proc 
    type(AttrVect), intent(inout)  :: x2atm_atm
    type(AttrVect), intent(inout)  :: ocn2x_atm
    type(AttrVect), intent(inout)  :: lnd2x_atm
    type(AttrVect), intent(inout)  :: fractions_a
    
    character(len=100) :: field_atm
    character(len=100) :: field_ocn
    character(len=100) :: field_lnd
    character(len=100) :: itemc_atm
    character(len=100) :: itemc_lnd
    character(len=100) :: itemc_ocn   
 
    integer :: n, ka, kl, ko, kof, klf
    integer :: lsize
    real   :: fracl, fraco
    logical :: iamroot
    logical :: first_time = .true.
    logical :: mct_usevector = .false.
    logical, pointer, save :: lmerge(:), omerge(:)
    integer, pointer, save :: lindx(:), oindx(:)
    integer, save          :: naflds, klflds, noflds

    iamroot = my_proc%iam_root !
    
    if (first_time)then
        
         naflds = avect_nRattr(x2atm_atm)
         klflds = avect_nRattr(lnd2x_atm)
         noflds = avect_nRattr(ocn2x_atm)

         allocate(lindx(naflds), lmerge(naflds))
         allocate(oindx(naflds), omerge(naflds)) 
   
         lindx(:) = 0
         oindx(:) = 0
         lmerge(:) = .true.
         omerge(:) = .true.

         !  Field naming rules
         ! Only atm state that are Sx_... will be merged
         ! Only fluxes that are F??x_... will be merged
         ! All fluxes will be multiplied by corresponding componet fraction

         do ka = 1, naflds
            call getfld(ka, x2atm_atm, field_atm, itemc_atm)
            if (field_atm(1:2)=='PF')then
                cycle
            end if
            if (field_atm(1:1) == 'S' .and. field_atm(2:2)/='x') then
                cycle
            end if
            
            do kl = 1,klflds
                call getfld(kl, lnd2x_atm, field_lnd, itemc_lnd)
                if (trim(itemc_atm) == trim(itemc_lnd))then
                    if((trim(field_atm)==trim(field_lnd)))then
                        if(field_lnd(1:1)=='F')lmerge(ka)  = .false.
                    end if
                    lindx(ka) = kl
                    exit
                 end if
            end do

            do ko = 1,noflds
                call getfld(ko, ocn2x_atm, field_ocn, itemc_ocn)
                if(trim(itemc_atm) == trim(itemc_ocn))then
                    if((trim(field_atm)==trim(field_ocn)))then
                        if(field_ocn(1:1) == 'F')omerge(ka) = .false.
                    end if
                    oindx(ka) = ko
                    exit
                end if
            end do
            if(lindx(ka) == 0) itemc_lnd = 'unset'
            if(oindx(ka) == 0) itemc_ocn = 'unset'

            if(iamroot)then
                write(*,*)trim(itemc_atm),trim(itemc_lnd),trim(itemc_ocn)
            end if    
         end do   
         first_time = .false.
     end if
    
     ! Zero attribute vector!!
     call avect_zero(x2atm_atm)

     ! updata surface fractions
     ! klf = avect_indexRA(fractions_a,"lfrac")
     ! kof = avect_indexRA(fractions_a,'ofrac')
     lsize = avect_lsize(x2atm_atm)

     !index_x2a_Sf_lfrac = avect_indexRA(x2atm_atm,'Sf_lfrac')
     !index_x2a_Sf_ofrac = avect_indexRA(x2atm_atm,'Sf_ofrac')
     !do n = 1,lsize
     !    x2atm_atm%rAttr(index_x2a_Sf_lfrac,n) = fractions_atm%Rattr(klf, n)
     !    x2atm_atm%rAttr(index_x2a_Sf_ofrac,n) = fractions_atm%Rattr(kpf, n)
     !end do

     ! Copy attributes that do not need to be merged
     ! These are assumed to have the same name in 
     ! (ocn2x_atm and x2atm_atm) and in (lnd2x_atm and x2atm_atm), etc.
     call avect_copy(aVin=lnd2x_atm, aVout=x2atm_atm, vector=mct_usevector)
     call avect_copy(aVin=ocn2x_atm, aVout=x2atm_atm, vector=mct_usevector)

     ! If flux to atm is coming only from the ocean (based on field being in ocn2x_atm) -
     ! -- then scale by both ocean and ice fraction
     ! If flux to atm is coming only from the land or ice or coupler
     ! -- then scale by fraction above

     do ka =  1, naflds
         do n = 1, lsize
             fracl = fractions_a%Rattr(klf, n)
             fraco = fractions_a%Rattr(kof, n)
             if(lindx(ka)>0 .and. fracl > 0) then
                 if(lmerge(ka))then
                     x2atm_atm%rAttr(ka, n) = x2atm_atm%rAttr(ka,n) + lnd2x_atm%rAttr(lindx(ka),n)*fracl
                 else
                     x2atm_atm%rAttr(ka, n) = lnd2x_atm%rAttr(lindx(ka), n)*fracl
                 end if
             end if
             !if (xindx(ka) >0 .and. fraco>0._r8)then
             !    if(xmerge(ka)) then
             !        x2atm_atm%rAttr(ka, n) = x2atm_atm%rAttr(ka, n) + xao_a%rAttr(xindx(ka), n)*fraco
             !    else
             !        x2atm_atm%rAttr(ka, n) = xao_atm%rAttr(xindx(ka),n)*fraco
             !    end if
             !end if
             if (oindx(ka) > 0)then
                 if(omerge(ka) .and. fraco > 0)then
                     x2atm_atm%rAttr(ka, n) = x2atm_atm%rAttr(ka,n) + ocn2x_atm%rAttr(oindx(ka),n)*fraco
                 end if
                 if(.not. omerge(ka)) then
                     x2atm_atm%rAttr(ka, n) = ocn2x_atm%rAttr(oindx(ka),n)*fraco
                     x2atm_atm%rAttr(ka, n) = x2atm_atm%rAttr(ka, n) + ocn2x_atm%rAttr(oindx(ka),n)*fraco
                end if
             end if
         end do
     end do

end subroutine mrg_x2atm

end module mrg_mod


