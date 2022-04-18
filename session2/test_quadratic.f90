program test_quadratic
    use mod_solver
    implicit none
    call check_function(2.0, -2.0, -1.5)
    call check_function(2.0, 7.4, 0.2)
    call check_function(0.0, 7.4, 0.2)
    call check_function(2.0, 7.4, 0.00002)

  contains
    subroutine check_function(i,j,k)
      real :: i, j, k, x1, x2
      integer :: n
      intrinsic :: abs

      call solve_quad(i, j, k, x1, x2, n)
      if (n == 0) then
         write(*, *) 'No real-valued solutions for ', i, j, k
      else if (n == 1) then
         write(*, *) 'For ', i, j, k, ' there is one solution x = ', x1
         write(*,*) 'Check: ', (i * x1**2 + j * x1 + k) / abs(x1)
       else
            write(*, *) 'For ', i, j, k, ' there are two solutions x1 = ', x1, ' x2 = ', x2
            write(*,*) 'Check: ', (i * x1**2 + j * x1 + k) / abs(x1)
            write(*,*) 'Check: ', (i * x2**2 + j * x2 + k) / abs(x2)
      end if
    end subroutine check_function
    
end program test_quadratic