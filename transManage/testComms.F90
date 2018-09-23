program testNc

    use comms_nc, only : mct_sMatReadnc
    use mct_mod
    use comms_def

implicit none
character(len=10)    :: filename = ""
type(map_mod)        :: mapper
 
call mct_sMatReadnc(mapper, filename) 




end program
