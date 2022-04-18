program sort_string
    implicit none

    character (len=80) :: s
    integer, dimension(128) :: counter
    integer :: i, j, ascii_val
    character (len=80) :: sorted_s

    intrinsic :: trim
    
    ! intialize
    counter = 0
    sorted_s = ''
    read *, s
    s = trim(s)
    letter_counter: do i=1, 80
        ascii_val = iachar(s(i:i))
        if (ascii_val == iachar(' ')) exit letter_counter
        counter(ascii_val) = counter(ascii_val) + 1
    end do letter_counter

    print_counter: do i=1, 128
        if (counter(i) > 0) then 
            write (*,*) achar(i), counter(i)
            do j = 1, counter(i)
                sorted_s = trim(sorted_s) // achar(i)
            end do
        end if
    end do print_counter

    print *, sorted_s

end program sort_string