!  Simpson integration
!  Interpolates to odd number of points
!  Requires interpfill subroutine
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
REAL(kind=REAL64), ALLOCATABLE ::   xt(:)
REAL(kind=REAL64), ALLOCATABLE ::   yt(:)
!################################################
odd = 0.0d0
even = 0.0d0
IF(SIZE(x) .NE. SIZE(y))THEN
  STOP "Error integrate() unequal array sizes"
END IF
IF(MOD(SIZE(x, 1), 2) .EQ. 0)THEN
  ALLOCATE(xt(1:SIZE(x, 1)+1))
  ALLOCATE(yt(1:SIZE(y, 1)+1))
  CALL interpfill(x, y, 3, xt(:), yt(:))
  xsize = SIZE(xt, 1)
  h = (xt(xsize) - xt(1)) / (xsize - 1)
  ! Odd and even terms
  n = 1
  DO WHILE(n < xsize)
    n = n + 1
    even = even + yt(n)
    n = n + 1
    odd = odd + yt(n)
  END DO
ELSE
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
END IF
! Finish
integrate = (h / 3.0d0) * (y(1) + y(xsize) + 4.0d0 * even + 2.0d0 * odd)
END FUNCTION integrate