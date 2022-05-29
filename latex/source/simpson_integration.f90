!  Simpson integration
!  Must have an even number of intervals 
!  (odd number of data points)
!
FUNCTION integrate(x, y)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::    x(:)
REAL(kind=REAL64), INTENT(IN) ::    y(:)
REAL(kind=REAL64) ::                integrate
!################################################
REAL(kind=REAL64) ::                h, odd, even
INTEGER(kind=INT32) ::              xsize
INTEGER(kind=INT32) ::              n
!################################################
IF(SIZE(x) .NE. SIZE(y))THEN
  STOP "Error integrate() unequal array sizes"
END IF
odd = 0.0d0
even = 0.0d0
xsize = SIZE(x, 1)
h = (x(xsize) - x(1)) / (xsize - 1)
! Odd and even terms
n = 1
DO WHILE(n < xsize)
  n = n + 1
  even = even + y(n)
  n = n + 1
  odd = odd + y(n)
END DO
! Finish
integrate = (h / 3.0d0) * (y(1) + y(xsize) + 4.0d0 * even + 2.0d0 * odd)
END FUNCTION integrate