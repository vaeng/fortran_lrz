program triangle
  implicit none
  integer, parameter :: ndim = 4
  integer :: i
  real :: a(ndim), b(ndim), c(ndim), area(ndim)
  real :: s
  intrinsic :: sqrt
! 
! input values
  a(1) = 2.0; b(1) = 3.0; c(1) = 4.0
  a(2) = 1.0; b(2) = 1.0; c(2) = 5.0
  a(3) = 1.0; b(3) = 1.000005; c(3) = 0.000006
  a(4) = -2.0; b(4) = 3.0; c(4) = 4.0
!
! Please add code for calculation below

do i=1, ndim
  s = a(i) + b(i) + c(i) * 0.5
  s = s * (s-a(i)) * (s-b(i)) * (s-c(i))
  if (s <= 0) cycle
  print *, 'a: ', a(i), 'b: ', b(i), 'c: ', c(i), 'A: ', a(i) 

end do
 
end program triangle
