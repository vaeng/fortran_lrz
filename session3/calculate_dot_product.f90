program calculate_dot_product
  use blas77
  implicit none
  integer, parameter :: nd = 100
  real, dimension(nd) :: x, y
  real :: dot_result
  integer :: i

  intrinsic :: mod

  data x / nd * 1.0 /
  data (y(i), i=1,nd,2) / 50*1.0 /
  data (y(i), i=2,nd,4) / 25*2.0 /
  data (y(i), i=4,nd,4) / 25*-2.0 /

!   FIXME: complete the assignment statement
  dot_result = sDOT(nd, x, 1, y, 1) 

  write(*, fmt='(A, E14.7)') 'Dot product has value ', dot_result
end program
