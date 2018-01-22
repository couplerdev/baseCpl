module cpl_vect
    
    implicit none 
    type c_vect
        integer, dimension(:), pointer:: array
        integer :: c_size
    end type c_vect

    public :: init
    public :: vect_size
    public :: del

contains 

subroutine init(vec, num_size)

    implicit none
    type(c_vect), intent(inout) :: vec
    integer, intent(in) :: num_size

    allocate(vec%array(num_size))
    vec%c_size = num_size
    call zero(vec)

end subroutine init

subroutine zero(vec)

    implicit none
    type(c_vect), intent(inout) :: vec
    integer :: iter

    do iter = 1, vec%c_size
        vec%array(iter) = 0
    end do

end subroutine zero

subroutine vect_size(vec, num_size)

    implicit none
    type(c_vect), intent(in) :: vec
    integer, intent(inout) :: num_size
   
    num_size = vec%c_size

end subroutine vect_size

subroutine del(vec)

    implicit none
    type(c_vect), intent(inout) :: vec

    deallocate(vec%array)

end subroutine del

end module cpl_vect
