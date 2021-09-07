module ArrayBase
    use, intrinsic :: iso_fortran_env
    use :: Vector
    implicit none

    !todo, implement generic finalizing interface for array2~3

    type :: Array2
        integer :: m_size(2) ! a vector to represent array dimension
        real(real64), allocatable :: m_data(:, :)
    contains
    end type Array2

    type :: Array3
        integer :: m_size(3) ! a vector to represent array dimension
        real(real64), allocatable :: m_data(:, :, :)
    end type Array3

    ! todo : make constructor of same shape

    interface move
        procedure :: move_array2
        procedure :: move_array3
    end interface

    interface Array3D
        module procedure :: make_array3 ! copy size and allocate same shape
        module procedure :: make_array3_size
        module procedure :: make_array3_move
        module procedure :: make_array3_bound
    end interface


contains


    pure function make_array2(from) result(to)
        class(Array2), intent(in) :: from
        class(Array2), allocatable :: to
        allocate (to)
        to%m_size = from%m_size
        allocate (to%m_data, mold=from%m_data)
    end function make_array2

    function make_array3(from) result(to)
        class(Array3), intent(in) :: from
        type(Array3) :: to
        !allocate (to)
        to%m_size = from%m_size
        allocate (to%m_data, mold=from%m_data)
    end function make_array3

    function make_array3_size(i, j, k) result(arr)
        integer, intent(in) :: i, j, k
        type(Array3) :: arr

        arr%m_size = [i, j, k]
        allocate (arr%m_data(i, j, k))
    end function

    function make_array3_move(from) result(to)
        real(real64), intent(inout), allocatable :: from(:, :, :)
        type(Array3) :: to
        to%m_size = size(from)

        call move_alloc(from, to%m_data)
    end function

    function make_array3_bound(lb, ub) result(arr)
        integer, intent(in) :: lb(3), ub(3)
        integer :: resolution(3)
        type(Array3) :: arr

        resolution = ub(:) - lb(:) + 1

        arr%m_size = resolution

        allocate (arr%m_data(lb(1):ub(1), &
                             lb(2):ub(2), &
                             lb(3):ub(3)))
    end function




    pure subroutine move_array2(to, from)
        class(Array2), allocatable, intent(inout) :: from
        type(Array2), intent(inout) :: to
        to%m_size = from%m_size
        if (allocated(to%m_data)) then
            deallocate (to%m_data)
        end if
        call move_alloc(from%m_data, to%m_data)
        if (allocated(from)) then
            deallocate (from)
        end if
    end subroutine move_array2
    pure subroutine move_array3(to, from)
        class(Array3), allocatable, intent(inout) :: from
        type(Array3), intent(inout) :: to
        to%m_size = from%m_size
        if (allocated(to%m_data)) then
            deallocate (to%m_data)
        end if
        call move_alloc(from%m_data, to%m_data)
        if (allocated(from)) then
            deallocate (from)
        end if
    end subroutine move_array3

end module ArrayBase
