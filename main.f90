#ifndef _PRECISION
#define _PRECISION 4
#endif

program main
    use gauss
    use fdm
    implicit none

    logical :: exist
    integer (kind = 4) :: num_args, arg_i, N, i
    character(len=12), dimension(:), allocatable :: args
    real (kind = 16) :: errors
    real (kind = _PRECISION), allocatable, dimension(:, :):: A
    real (kind = _PRECISION), allocatable, dimension(:):: X
    real (kind = _PRECISION) :: beginc, endc

    !parse command line arguments
    num_args = command_argument_count()
    allocate(args(num_args))

    do arg_i = 1, num_args
        call get_command_argument(arg_i, args(arg_i))
    end do

    if(num_args > 0) then
        read(args(1), '(i5)') N
    end if

    beginc = 0
    endc = 1

    inquire(file="errors.txt", exist=exist)
    if(exist) then
        open(7, file="errors.txt", status="old", position="append", action="write")
    else
        open(7, file="errors.txt", status="new", action="write")
    end if
    
    !redirecting computational errors to file
    do i = 50,800,50
        allocate(A(i, i))
        allocate(X(i))
        call generate(A, X, i, beginc, endc)
        call do_gauss(A, X, i)
        call get_errors(X, i, errors)
        write(7,*) errors
        deallocate(A)
        deallocate(X)
    end do
    close(7)

end program