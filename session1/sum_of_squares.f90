program sum_of_squares
    implicit none
    integer, parameter :: long = selected_int_kind(14)
    integer(kind=long), parameter :: max = 100000000000_long
    integer, parameter :: dm = 100000
    integer(kind=long), dimension(dm) :: arr
    integer(kind=long) :: sum
    integer :: i

    arr = 0
    sum = 0
    fill_array: do i=1,dm
        sum = sum + i * i
        if (sum > max) then
            exit fill_array
        end if
        arr(i) = i*i
    end do fill_array

    print_array: do i=1, dm
        if (arr(i) == 0) then
            exit
        end if
        write (*,*) arr(i)
    end do print_array
    
end program sum_of_squares