#ifndef _PRECISION
#define _PRECISION 8
#endif
#define STEP 100

program main
    use gauss
    use fdm
    implicit none

    integer (kind = 4) :: num_args, arg_i, N
    character(len=12), dimension(:), allocatable :: args
    real (kind = 16) :: errors
    real (kind = _PRECISION), allocatable, dimension(:, :):: A
    real (kind = _PRECISION), allocatable, dimension(:):: X
    real (kind = _PRECISION) :: h, beginc, endc

    !parse command line arguments
    num_args = command_argument_count()
    allocate(args(num_args))

    do arg_i = 1, num_args
        call get_command_argument(arg_i, args(arg_i))
    end do

    N = STEP
    if(num_args > 0) then
        read(args(1), '(i5)') N
    end if

    allocate(A(N, N))
    allocate(X(N))

    h = 1./N
    beginc = 0
    endc = 1

    call generate(A, X, N, beginc, endc, h)
    call do_gauss(A, X, N)
    call get_errors(X, N, errors)
    
    open(unit = 7, file = "results")
    write(7, '(F15.10)') X
    write(*,*) "Computational error: ", errors," Results have been written to 'results' file."
    close(7)

end program