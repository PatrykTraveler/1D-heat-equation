#ifndef _PRECISION
#define _PRECISION 8
#endif

module fdm
    implicit none

contains 
    subroutine generate(A, X, N, beginc, endc, h)
        implicit none
        integer (kind = 4), intent(in) :: N
        real (kind = _PRECISION), intent(in) :: h, beginc, endc
        real (kind = _PRECISION), allocatable, dimension(:, :), intent(inout) :: A
        real (kind = _PRECISION), allocatable, dimension(:), intent(inout) :: X
        integer :: i

        A = 0
        X = 0
        
        do i=2, N
            A(i, i-1) = -1/(h**2)
        end do

        do i=1, N
            A(i, i) = 2/(h**2)
        end do

        do i=1, N-1 
            A(i, i+1) = -1/(h**2)
        end do

        A(1,1) = 1
        A(2,1) = 0
        X(1) = beginc
        X(N) = endc

    end subroutine

    subroutine get_errors(X, N, errors)
        implicit none
        integer (kind = 4), intent(in) :: N
        integer (kind = 4) :: i
        real (kind = 16), intent(inout) :: errors
        real (kind = _PRECISION), allocatable, dimension(:) :: X

        errors = 0
        do i = 1, N
            errors = errors + abs(X(i) - real(i)/real(N))
        end do
    end subroutine
end module fdm