module fdm
    implicit none

contains 
    subroutine generate(N, A, X, begin, end)
        implicit none
        real (kind = 8), allocatable, dimension(:, :), intent(out) :: A
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

        X(N) = 1

    end subroutine
end module fdm


