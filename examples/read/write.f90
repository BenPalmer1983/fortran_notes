PROGRAM write1

CALL main()


CONTAINS


SUBROUTINE main ()

INTEGER(8) ::            status, ios
INTEGER(8) ::            n, m
INTEGER(8) ::            lines
REAL(8) ::              arr(1:10,1:3)


!#################################
! Make random data
!#################################

CALL RANDOM_NUMBER(arr)
arr = 1.0D10 * (0.5D0 - arr)



!#################################
! Write to file
!#################################

OPEN(unit=99, file='out.txt') 
DO n = 1, 10
  WRITE(99, "(ES16.7, ES16.7, ES16.7)") arr(n, 1), arr(n, 2), arr(n, 3)
END DO
CLOSE(99)



END SUBROUTINE main


END PROGRAM write1
