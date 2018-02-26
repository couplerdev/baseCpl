module testF

    public :: a_test
    public :: b_test
    public :: c_test

contains
subroutine a_test()

    write(*,*) 'a_run'

end subroutine a_test

subroutine b_test()

    write(*,*) 'b_run'

end  subroutine b_test

subroutine c_test()

    write(*,*) 'c_run'

end subroutine c_test

end module testF
