module timeM
    
    implicit none
    type Clock
        integer :: seconds
        integer :: minites
        integer :: hours
        integer :: days
        !integer :: months
        !integer :: years
        integer :: interval
    end type Clock
    integer :: total_days =  1 

    integer :: time_a_run = 3
    integer :: time_c_run = 7
    integer :: time_b_run = 5
    integer :: time_ocn_run = 7
    integer :: time_atm_run = 3
    integer :: time_lnd_run = 7


    public :: clock_init
    public :: clock_advance
    public :: triger
contains 

subroutine clock_init(EClock, interval)
 
    implicit none
    type(Clock), intent(inout)    :: EClock
    integer, optional, intent(in) :: interval
 
    EClock%seconds = 0 
    EClock%minites = 0
    EClock%hours   = 0
    EClock%days    = 0
    !EClock%months  = 0 
    !EClock%years   = 0 ! from base so far

    if(present(interval))then
        EClock%interval = interval
    else
        EClock%interval = 1
    end if

end subroutine

subroutine clock_advance(EClock)

   implicit none
   type(Clock), intent(inout) :: EClock
   integer  :: plus
   
   EClock%seconds = EClock%seconds + EClock%interval
   if(EClock%seconds < 60)return 
   
   plus = EClock%seconds/60
   EClock%seconds = mod(EClock%seconds, 60) 
   EClock%minites = EClock%minites + plus
   if(EClock%minites < 60)return 
    
   plus = EClock%minites/60
   EClock%minites = mod(EClock%minites, 60)
   EClock%hours   = EClock%hours + plus
   if(EClock%hours < 24)return 

   plus = EClock%hours/24
   EClock%hours = mod(EClock%hours, 24)
   EClock%days  = EClock%days + plus

end subroutine clock_advance

subroutine triger(EClock, flag, flag_name)

    implicit none
    type(Clock), intent(in)       :: EClock
    logical, intent(inout)        :: flag
    character(len=*),  intent(in) :: flag_name
    integer                       :: tmp_m
    integer                       :: tmp_h
    integer                       :: tmp_d 
    integer                       :: tmp_mod

    flag = .false.
    if(flag_name=='stop_clock')then
        if(EClock%days >= total_days)then
            write(*,*) 'work'
            flag=.true.
        end if
    end if

    if(flag_name=='a_run')then
        tmp_m = mod(60, time_a_run)
        tmp_h = mod(60*60, time_a_run)
        tmp_d = mod(tmp_h*24, time_a_run)
        tmp_mod = mod(EClock%seconds, time_a_run) +&
mod(EClock%minites*tmp_m, time_a_run) + &
                  mod(EClock%hours*tmp_h, time_a_run) + &
mod(EClock%days*tmp_d, time_a_run)
        tmp_mod = mod(tmp_mod, time_a_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if

    if(flag_name=='c_run')then
        tmp_m = mod(60, time_c_run)
        tmp_h = mod(60*60, time_c_run)
        tmp_d = mod(tmp_h*24, time_c_run)
        tmp_mod = mod(EClock%seconds, time_c_run) +&
mod(EClock%minites*tmp_m, time_c_run) + &
                  mod(EClock%hours*tmp_h, time_c_run) + &
mod(EClock%days*tmp_d, time_c_run)
        tmp_mod = mod(tmp_mod, time_c_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if

    if(flag_name=='b_run')then
        tmp_m = mod(60, time_b_run)
        tmp_h = mod(60*60, time_b_run)
        tmp_d = mod(tmp_h*24, time_b_run)
        tmp_mod = mod(EClock%seconds, time_b_run) +&
mod(EClock%minites*tmp_m, time_b_run) + &
                  mod(EClock%hours*tmp_h, time_b_run) + &
mod(EClock%days*tmp_d, time_b_run)
        tmp_mod = mod(tmp_mod, time_b_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if

    if(flag_name=='ocn_run')then
        tmp_m = mod(60, time_ocn_run)
        tmp_h = mod(60*60, time_ocn_run)
        tmp_d = mod(tmp_h*24, time_ocn_run)
        tmp_mod = mod(EClock%seconds, time_ocn_run) +&
mod(EClock%minites*tmp_m, time_ocn_run) + &
                  mod(EClock%hours*tmp_h, time_ocn_run) + &
mod(EClock%days*tmp_d, time_ocn_run)
        tmp_mod = mod(tmp_mod, time_ocn_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if

    if(flag_name=='atm_run')then
        tmp_m = mod(60, time_atm_run)
        tmp_h = mod(60*60, time_atm_run)
        tmp_d = mod(tmp_h*24, time_atm_run)
        tmp_mod = mod(EClock%seconds, time_atm_run) +&
mod(EClock%minites*tmp_m, time_atm_run) + &
                  mod(EClock%hours*tmp_h, time_atm_run) + &
mod(EClock%days*tmp_d, time_atm_run)
        tmp_mod = mod(tmp_mod, time_atm_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if

    if(flag_name=='lnd_run')then
        tmp_m = mod(60, time_lnd_run)
        tmp_h = mod(60*60, time_lnd_run)
        tmp_d = mod(tmp_h*24, time_lnd_run)
        tmp_mod = mod(EClock%seconds, time_lnd_run) +&
mod(EClock%minites*tmp_m, time_lnd_run) + &
                  mod(EClock%hours*tmp_h, time_lnd_run) + &
mod(EClock%days*tmp_d, time_lnd_run)
        tmp_mod = mod(tmp_mod, time_lnd_run)
        if(tmp_mod == 0)then
            flag = .true.
        end if
    end if


end subroutine triger

subroutine clock_print(EClock)

    implicit none
    type(Clock), intent(in) :: EClock

    write(*,*) EClock%days, "days:", EClock%hours, "hours:", EClock%minites, "minites:",&
               EClock%seconds, "seconds"

end subroutine clock_print

subroutine clock_info(EClock, info)

    implicit none
    type(Clock),  intent(inout)      :: EClock
    character(len=*), intent(inout)  :: info
    character(len=20)                :: tmpCh  
   
    write(tmpCh,*)EClock%days
    info = info//tmpCh//"days:  "
    
    tmpCh = ""
    write(tmpCh,*)EClock%hours
    info = info//tmpCh//"hours:  "
 
    tmpCh = ""
    write(tmpCh,*)EClock%minites
    info = info//tmpCh//"minites:  "
    
    tmpCh = ""
    write(tmpCh,*)EClock%seconds
    info = info//tmpCh//"seconds:  "

end subroutine clock_info

end module timeM
