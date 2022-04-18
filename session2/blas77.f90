module blas77
  implicit none
  interface
    real function SDOT(N, X, INCX, Y, INCY) 
      integer, intent(in) :: N, INCX, INCY
      real, intent(in), dimension(*) :: X, Y
    end function
  end interface
end module
