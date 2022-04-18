module mod_solver
    implicit none
    
    contains

    subroutine solve_quad(a, b, c, x1, x2, n)
        implicit none
        real,intent(in) :: a, b, c
        real,intent(out) :: x1, x2
        integer, intent(out) :: n
        real :: tmp

        intrinsic :: sqrt

        tmp = b**2 - 4. * a * c 
        x1 = 0
        x2 = 0
        
        if (tmp > 0) then
            x1 = ( -b + tmp ) / ( 2. * a )
            x2 = ( -b - tmp ) / ( 2. * a )
        else if (tmp == 0) then
            x1 = -b  / ( 2. * a )
            n = 1
        else 
            n = 0
        end if

    end subroutine solve_quad
    
end module mod_solver