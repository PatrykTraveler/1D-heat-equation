program main
    use gauss
    use fdm
    implicit none

    integer (kind = 4) :: num_args, arg_i, N
    character(len=12), dimension(:), allocatable :: args
    real (kind = 8), allocatable, dimension(:, :):: A
    real (kind = 8), allocatable, dimension(:):: X
    real (kind = 8) :: h

    !parse command line arguments
    num_args = command_argument_count()
    allocate(args(num_args))

    do arg_i = 1, num_args
        call get_command_argument(arg_i, args(arg_i))
    end do

    if(num_args > 0) then
        read(args(1), '(i5)') N
    end if

    allocate(A(N, N))
    allocate(X(N))

    h = 1./N

    call generate(A, X, N, 0, 1, h)
    call do_gauss(A, X, N)
    
    write(*, '(20G12.4)') X

end program