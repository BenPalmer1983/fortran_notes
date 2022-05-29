SUBROUTINE terminalwrite()
!########################################################################
IMPLICIT NONE
!########################################################################
WRITE(*, *) "Free text"
WRITE(*, "(A20)") "20 character string."
WRITE(*, "(A24)") "20 character string."
WRITE(*, "(I4)") 1000
WRITE(*, "(I10)") 1000                            ! Integer
WRITE(*, "(F20.8)") 123456789.123456789e1         ! Fixed Point
WRITE(*, "(E20.12)") 123456789.123456789e1        ! Floating Point - Exponential form
WRITE(*, "(ES20.12)") 123456789.123456789e1       ! Floating Point - Scientific form
WRITE(*, "(EN20.12)") 123456789.123456789e1       ! Floating Point - Engineering form
WRITE(*, "(D20.12)") 123456789.123456789d1        ! Double
WRITE(*, "(E20.12E3)") 123456789.123456789e1      ! Floating Point - Exponential form
WRITE(*, "(ES20.12E3)") 123456789.123456789e1     ! Floating Point - Scientific form
WRITE(*, "(EN20.12E3)") 123456789.123456789e1     ! Floating Point - Engineering form
END SUBROUTINE terminalwrite