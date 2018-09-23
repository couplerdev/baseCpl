program easy_test

implicit none


character(len=20) :: fileLog = "logs"
integer        ::   unitNum = 5
open(file=fileLog, unit=unitNum, status='NEW')
write(unitNum, *) 'hello'
close(unitNum)

end program easy_test
