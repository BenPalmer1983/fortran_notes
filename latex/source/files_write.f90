! Write file
SUBROUTINE write ()
!################################################
INTEGER(kind=INT16) ::            status, ios
INTEGER(kind=INT16) ::            n, m
INTEGER(kind=INT16) ::            lines
REAL(kind=REAL64) ::              arr(1:10,1:3)
!################################################
! Populate array with random floats
CALL RANDOM_NUMBER(arr)
arr = 1.0D10 * (0.5D0 - arr)
! Write to file
OPEN(unit=99, file='out.txt') 
DO n = 1, 10
  WRITE(99, "(ES16.7, ES16.7, ES16.7)") arr(n, 1), arr(n, 2), arr(n, 3)
END DO
CLOSE(99)
END SUBROUTINE write