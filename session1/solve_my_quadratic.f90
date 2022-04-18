program solve_my_quadratic 
  implicit none
  real :: a, b, c
  real :: x1, x2
  intrinsic :: sqrt
  integer :: input_stat

  print *,'Enter a value for a, b and c: '
  read (*,*,iostat=input_stat) a, b, c

  if (input_stat == 0) then   
    if (b**2 - 4. * a * c >= 0) then
      x1 = ( -b + sqrt(b**2 - 4. * a * c) ) / ( 2. * a )
      x2 = ( -b - sqrt(b**2 - 4. * a * c) ) / ( 2. * a )
    
      write(*, fmt='(A,E15.10,1X,E18.10)') 'Solutions are: ', x1, x2
    else 
      write(*, fmt=*) 'No real solution for the given values.'
    end if
  else
    write(*, fmt=*) 'Invalid value(s) for a, b or c.'
  end if
end program

