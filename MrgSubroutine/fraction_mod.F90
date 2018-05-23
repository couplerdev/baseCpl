module fraction_mod
    use proc_def
    use mct_mod
    use proc_def
    use comms
implicit none

    public   fraction_atm_init
    public   fraction_init
! 注意到其实fraction也是可以满足生成代码形式的，只要构造fraction的方式是
! 利用稀疏矩阵乘法，这也就意味着这个实现还有改进的余地

contains

subroutine fraction_atm_init(my_proc, fraction_atm)

    implicit none
    type(proc), intent(inout)      :: my_proc
    type(AttrVect),intent(inout)   :: fraction_atm
    !type(map_mod)   :: mapper_ocn2atm
    !type(map_mod)   :: mapper_lnd2atm

    type(gGrid), pointer :: dom_atm
    
    character(*), parameter  :: fraclist_atm = 'atmfrac:ocnfrac:lndfrac'
    integer :: lsize
    integer :: ka, kl, ko

    dom_atm=my_proc%atm_grid_domain
    lsize  = avect_lsize(dom_atm%data) !?
    call avect_init(fraction_atm, rList=fraclist_atm, lSize=lsize)
    call avect_zero(fraction_atm)
    ka = avect_indexRA(fraction_atm, "atmfrac")
    fraction_atm%rAttr(ka, :) = 1

    ko = avect_indexRA(fraction_atm, "ocnfrac") 
    fraction_atm%rAttr(ko,:) = 1
  
    kl = avect_indexRA(fraction_atm, "lndfrac")  
    fraction_atm%rAttr(kl, :) = 1
    

end subroutine fraction_atm_init

subroutine fraction_init(my_proc, fraction_atm, fraction_ocn, fraction_lnd)

    implicit none
    type(proc),     intent(in)     :: my_proc
    type(AttrVect), intent(inout)  :: fraction_atm
    type(AttrVect), intent(inout)  :: fraction_ocn
    type(AttrVect), intent(inout)  :: fraction_lnd
   
    type(map_mod), pointer   :: mapper_lnd2atm
    type(map_mod), pointer   :: mapper_ocn2atm
    !type(map_mod), pointer   :: mapper_atm2lnd
    !type(map_mod), pointer   :: mapper_ocn2lnd
    type(map_mod), pointer   :: mapper_atm2ocn

    type(gGrid), pointer :: dom_atm
    type(gGrid), pointer :: dom_ocn
    type(gGrid), pointer :: dom_lnd

    character(*), parameter :: fraclist_atm = 'atmfrac:ocnfrac:lndfrac'
    character(*), parameter :: fraclist_ocn = 'atmfrac:ocnfrac'
    character(*), parameter :: fraclist_lnd = 'afrac:lndfrac'
    
    integer :: j, n
    integer :: ka, ko, kl
    integer :: lsize
    
    mapper_lnd2atm = my_proc%mapper_Smatlnd2atm
    mapper_ocn2atm = my_proc%mapper_Smatocn2atm
    mapper_atm2ocn = my_proc%mapper_Smatatm2ocn
    dom_atm = my_proc%atm_grid_domain
    lsize = avect_lSize(dom_atm%data)
    call avect_init(fraction_atm, rList=fraclist_atm, lsize=lsize)
    call avect_zero(fraction_atm)

    ka = avect_indexRa(fraction_atm, "afrac")
    fraction_atm%rAttr(ka,:) = 1

    dom_ocn = my_proc%ocn_grid_domain
    lsize = avect_lSize(dom_ocn%data)
    call avect_init(fraction_ocn, rList=fraclist_ocn, lsize=lsize)
    call avect_zero(fraction_ocn)
    ko = avect_indexRa(fraction_ocn, "ocnfrac")
    fraction_ocn%rAttr(ko,:) =  1.0
    call mapper_comp_map(mapper_ocn2atm, fraction_ocn, fraction_atm, rList="ocnfrac")
    call mapper_comp_map(mapper_atm2ocn, fraction_atm, fraction_ocn, rList="atmfrac")
    

end subroutine fraction_init

end module fraction_mod
