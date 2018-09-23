program test

    use base_file
    use logUnit, only: logUnit
implicit none
character(len=*) file_ = "mylog"
call log_init(file_, logUnit)
write(logUnit, *) 'logged'

call log_final(file_, logUnit)



end program test
