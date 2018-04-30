#ifndef _PRECISION
#define _PRECISION 4
#endif

module fdm
    implicit none

contains 
    subroutine generate(A, X, N, beginc, endc)
        implicit none
        integer (kind = 4), intent(in) :: N
        real (kind = _PRECISION), intent(in) :: beginc, endc
        real (kind = _PRECISION), allocatable, dimension(:, :), intent(inout) :: A
        real (kind = _PRECISION), allocatable, dimension(:), intent(inout) :: X
        integer (kind = 4) :: i
        real (kind = _PRECISION) :: h

        h = 1./N

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

        A(1,1) = endc
        A(2,1) = 0
        A(N, N) = 1
        A(N-1, N) = 0
        
        X(1) = beginc
        X(N) = endc

    end subroutine

    subroutine get_errors(X, N, errors)
        implicit none
        integer (kind = 4), intent(in) :: N
        integer (kind = 4) :: i
        real (kind = 16), intent(inout) :: errors
        real (kind = _PRECISION), allocatable, dimension(:), intent(in) :: X
        real (kind = 16) :: step

        errors = 0
        step = 1/real(N-1, _PRECISION)

        do i = 2, N
            errors = errors + abs(X(i) - (step*(i-1)))
        end do
    end subroutine
end module fdm