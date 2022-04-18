module mod_body
  implicit none
  private
  public :: body

  type :: body
     private
     real :: mass
     real :: pos(3)
     real :: vel(3)
  end type body
contains
  subroutine print_body(this)
    type(body), intent(in) :: this
    write(*,fmt='(''body with mass '',e9.2)') this%mass
    write(*,fmt='(''          position: '',3(e9.2))') this%pos
    write(*,fmt='(''          velocity: '',3(e9.2))') this%vel
  end subroutine print_body

  type(body) pure function set_body(mass, pos, vel) result(b)
    real, intent(in) :: mass
    real, intent(in), dimension(3) :: pos, vel
    b%mass = mass
    b%vel = vel
    b%pos = pos 
  end function

end module mod_body

