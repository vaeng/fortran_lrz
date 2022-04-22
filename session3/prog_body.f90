program prog_body
  use mod_body
  implicit none
  type(body) :: ball

  ball = set_body( mass=1.8, pos=[ 0.0, 0.0, 0.5 ], & 
         vel=[ 0.01, 4.0, 0.0 ] )
  call print_body(ball)

end program prog_body
