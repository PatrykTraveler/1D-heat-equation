module fdm
    implicit none

contains 
    subroutine generate(A, X, N, beginc, endc, h)
        implicit none
        integer (kind = 4), intent(in) :: N, beginc, endc
        real (kind = 8), intent(in) :: h
        real (kind = 8), allocatable, dimension(:, :), intent(inout) :: A
        real (kind = 8), allocatable, dimension(:), intent(inout) :: X
        integer :: i

        A = 0
        X = 0
        
        do i=2, N
            A(i, i-1) = 1/(h**2)
        end do

        do i=1, N
            A(i, i) = -2/(h**2)
        end do

        do i=1, N-1 
            A(i, i+1) = 1/(h**2)
        end do

        X(1) = beginc
        X(N) = endc

    end subroutine
end module fdm


