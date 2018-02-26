program test
use timeM
use testF
    implicit none
    type(Clock) :: EClock
    logical     :: stop_alarm
    logical     :: a_run
    logical     :: b_run
    logical     :: c_run
    integer     :: i

    call clock_init(EClock)
    call triger(EClock, stop_alarm, 'stop_clock')
   
    i = 0 
    do while(.not. stop_alarm) 
        call triger(EClock, a_run, "a_run")
        call triger(EClock, b_run, "b_run")
        call triger(EClock, c_run, "c_run") 
        call triger(EClock, stop_alarm, "stop_clock")
        i = i + 1
        !if(mod(i,1000) .ne. 0)continue
        !call clock_print(EClock) 
        if(a_run) then
            call a_test() 
            call clock_print(EClock)
        end if
     
        if(b_run) then
            call b_test()
        end if
   
        if(c_run) then
            call c_test()
        end if
        call clock_advance(EClock)
        
    end do

end program test
