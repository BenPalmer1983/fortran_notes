!
!  Lagrange interpolation
!
FUNCTION interpngrad(xi, x, y)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::    xi
REAL(kind=REAL64), INTENT(IN) ::    x(:)
REAL(kind=REAL64), INTENT(IN) ::    y(:)
REAL(kind=REAL64) ::                interpngrad
!################################################
REAL(kind=REAL64) ::                fx, gx, psum
INTEGER(kind=INT32) ::              xsize
INTEGER(kind=INT32) ::              i, j, k
!################################################
IF(SIZE(x) .NE. SIZE(y))THEN
  STOP "Error interpngrad() unequal array sizes"
END IF
! Set values
xsize = SIZE(x,1)
interpngrad = 0.0d0
DO i=1,SIZE(x,1)
  fx = 1.0d0
  gx = 0.0d0
  DO j=1,SIZE(x,1)
    IF(i .NE. j) THEN
      fx = fx / (x(i) - x(j))
      psum = 1.0d0
      DO k=1,SIZE(x,1)
        IF((i .NE. k) .AND. (j .NE. k))THEN
          psum = psum * (xi - x(k))
        END IF
      END DO
      gx = gx + psum
    END IF
  END DO
  interpngrad = interpngrad + fx * gx * y(i)
END DO
END FUNCTION interpngrad