module mod_gcd
    implicit none
    
contains
    pure integer function gcd(a, b)
        integer, value :: a,b
        integer :: r

        intrinsic :: abs, mod

        a = abs(a); b = abs(b)
        if ( a < b) then
            r = a
            a = b
            b = r
        end if

        if (b == 0) then
            b = a
        else 
            r = b
            euclid: do
                r = mod(a,b)
                if (r == 0) exit euclid
                a = b 
                b = r
            end do euclid
        end if
        gcd = abs(b)
    end function

    pure recursive integer function gcd_r(a, b) result(gcd)
        integer, value :: a,b
        integer :: r

        intrinsic :: abs, mod

        a = abs(a); b = abs(b)
        if ( a < b) then
            r = a
            a = b
            b = r
        end if

        if (b == 0) b = a
        if (b == 0) then
            gcd = b
        else if (mod(a,b) == 0) then
            gcd = b
        else
            gcd = gcd_r(mod(a,b), b)
        end if
    end function
end module mod_gcd