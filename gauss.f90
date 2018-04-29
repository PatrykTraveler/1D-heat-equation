module gauss
    implicit none

contains
    subroutine mmatrix_product(A, X, N)
        implicit none
        integer(kind = 4), intent(in)::N
        real(kind = 8), intent(inout)::A(N, N)
        real(kind = 8), intent(inout)::X(N)
        real(kind = 8) :: c
        integer(kind = 4) :: i, j

        do i=1, N
            do j=1, N
                if(i .NE. j) then
                    c = A(i, j)/A(i, i)
                    A(:,j) = A(:,j) - c*A(:, i)
                    X(j) = X(j) - c*X(j)
                    X(i) = X(i)/A(i,i)
                    A(:,i) = A(:,i)/A(i,i)
                end if
            end do
        end do
    end subroutine
end module

                    