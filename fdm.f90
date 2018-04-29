module fdm
    implicit none

contains 
    subroutine generate(A, X, N, beginc, endc, h)
        implicit none
        integer (kind = 4), intent(in) :: N
        real (kind = 8), intent(in) :: beginc, endc, h
        real (kind = 8), allocatable, dimension(:, :), intent(out) :: A
        real (kind = 8), allocatable, dimension(:), intent(out) :: X
        integer :: i

        allocate(A(N, N))
        allocate(X(N))
        A = 0
        X = 0
        
        do i=1, N
            A(i, i-1) = 1/(h**2)
        end do

        do i=1, N
            A(i, i) = -2/(h**2)
        end do

        do i=1, N 
            A(i, i+1) = 1/(h**2)
        end do

        X(0) = beginc
        X(N) = endc

    end subroutine
end module fdm


