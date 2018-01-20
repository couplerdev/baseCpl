module m_vector

    !----------------------------------------------------
    ! our init need some callback mechanism , and the 
    ! implementation of type data is ugly
    !----------------------------------------------------

    implicit none
    
    type vector
        character(len=100) :: typename
        integer,dimension(:), pointer :: idata
        real(8), dimension(:), pointer :: rdata 
        integer :: top
        integer :: sizeV
    end type vector
    
    public :: init
    public :: push_back
    public :: v

    interface init; module procedure &


    end interface

    interface push_back; module procedure &
        push_backi_, &
        push_backr_ 
    end interface

    interface v; module procedure &
        vi_, &
        vr_
    end interface

contains

subroutine init(vec, types)
    
    implicit none
    type(vector), intent(inout) :: vec
    character(len=*), intent(in) :: types

    if(types=="integer") then
        call initi_(vec)    
    else
        call initr_(vec)
    endif
end subroutine init


subroutine initi_(vec)

    implicit none
    type(vector), intent(inout) :: vec
    
    vec%top = 0
    vec%sizeV = 100
    vec%typename = "integer"
    allocate(vec%idata(100))

end subroutine

subroutine initr_(vec)

    implicit none
    type(vector), intent(inout) :: vec

    vec%top = 0
    vec%sizeV = 100
    vec%typename = "real"
    allocate(vec%rdata(100))

end subroutine

subroutine push_backi_(vec, val, error)
    
    implicit none
    type(vector), intent(inout) :: vec
    integer, intent(in) :: val
    integer, intent(inout) :: error
    
    error = 0
    if((vec%top+1)==vec%sizeV)then
        error=1
    else
        vec%idata(vec%top+1) = val
        vec%top = vec%top + 1
    end if
    
end subroutine push_backi_

subroutine push_backr_(vec, val, error)

    implicit none
    type(vector), intent(inout) :: vec
    real(8), intent(in) :: val
    integer, intent(inout) :: error
 
    error = 0
    if((vec%top+1)==vec%sizeV)then
        error=1
    else
        vec%rdata(vec%top+1) = val
        vec%top = vec%top + 1
    end if

end subroutine push_backr_

subroutine vi_(vec, i, val, error)
  
    implicit none
    type(vector), intent(inout) :: vec
    integer, intent(in) :: i
    integer, intent(inout) :: val
    integer, intent(inout) :: error

    error = 0
    if(i >= (vec%top+1))then
        error = 1
    else
        val = vec%idata(i)
    end if     

end subroutine vi_

subroutine vr_(vec, i, val, error)
 
    implicit none
    type(vector), intent(inout) :: vec
    integer, intent(in) :: i
    real(8), intent(inout) :: val
    integer, intent(inout) :: error

    error = 0 
    if(i >= (vec%top+1)) then
        error = 1
    else  
        val = vec%rdata(i)
    end if

end subroutine vr_

end module m_vector 
