program sieve
    implicit none

    integer, parameter :: n = 30, dm = n -1
    integer :: i, j

    integer, dimension(dm) :: arr

    ! fill array
    do i = 1, dm
        arr(i) = i+1
    end do

    ! crossed out elements are equal to 0
    outer: do i = 1, dm
        if (arr(i) == 0) cycle outer
        inner_check: do j = i+i, dm, i 
            if (arr(j) /= 0 .and. mod(arr(j),arr(i)) == 0) arr(j) = 0
        end do inner_check
    end do outer
    
    print_loop: do i = 1, dm
        if (arr(i) /= 0) print *, arr(i)
    end do print_loop


end program sieve