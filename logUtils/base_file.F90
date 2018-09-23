module base_file

use base_sys
    implicit none

    character(len=20) :: logFile = "basecplLog"

    public :: log_init
    

contains


subroutine log_init(logFile, logUnit)

    implicit none
    character(len=*),    intent(in) :: logFile
    integer,             intent(in) :: logUnit
    logical,                        :: stats

    inquire(logFile, exist=stats)
    if (.not. stats) then
        call base_sys_abort()
    end if
    open(unit=logUnit, file=logFile)
    
end subroutine log_init

subroutine log_final(logFile, logUnit)

    implicit none
    character(len=*),   intent(in)  :: logFile
    integer,            intent(in)  :: logUnit
    close(unit=logUnit, file=logFile)

end subroutine log_final

end module base_file
