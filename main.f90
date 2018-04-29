program main
    !use gauss
    !use fdm
    implicit none

    integer (kind = 4) :: num_args, arg_i, N
    character(len=12), dimension(:), allocatable :: args
    !real (kind = 8), allocatable, dimension(:, :), intent(out) :: A
    !real (kind = 8), allocatable, dimension(:), intent(out) :: X
    real (kind = 8) :: h

    !parse command line arguments
    num_args = command_argument_count()
    allocate(args(num_args))

    do arg_i = 1, num_args
        call get_command_argument(arg_i, args(arg_i))
    end do

    read(args(1), '(i5)') N

    !call generate(10, A, X, 0, 1)
    !call do_gauss(A, X, 10)
    write(*,*) N
end program