program main
use m_vector

    implicit none

    integer :: var
    real(8) :: varr
    type(vector) :: vec
    integer :: iter
    integer :: error


    call init(vec, "real")

    do iter=1, 20
        varr = iter*1.0
        call push_back(vec, varr, error)
    end do

    do iter=1, 20
        call v(vec, iter, varr, error)
        write(*,*) varr
    end do
    

end program main
