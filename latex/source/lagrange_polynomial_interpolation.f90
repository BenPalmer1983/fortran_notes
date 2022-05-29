!  Lagrange interpolation
!
FUNCTION interpn(xi, x, y)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::    xi
REAL(kind=REAL64), INTENT(IN) ::    x(:)
REAL(kind=REAL64), INTENT(IN) ::    y(:)
REAL(kind=REAL64) ::                interpn
!################################################
REAL(kind=REAL64) ::                li
INTEGER(kind=INT32) ::              xsize
INTEGER(kind=INT32) ::              i, j
!################################################
IF(SIZE(x) .NE. SIZE(y))THEN
  STOP "Error interpn() unequal array sizes"
END IF
! Set values
interpn = 0.0D0
xsize = SIZE(x)
! Loop
DO i = 1, xsize
  li = 1.0D0
  DO j = 1, xsize
    IF(i .NE. j) THEN
      li = li * (xi - x(j)) / (x(i) - x(j))
    END IF
  END DO
  interpn = interpn + li * y(i)
END DO
END FUNCTION interpn
