! Linspace

SUBROUTINE linspace(xmin, xmax, x)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::       xmin
REAL(kind=REAL64), INTENT(IN) ::       xmax
REAL(kind=REAL64), INTENT(INOUT) ::    x(:)
!################################################
INTEGER(kind=INT32) ::                 n
REAL(kind=REAL64) ::                   m
!################################################
m = (xmax - xmin) / (SIZE(x,1) - 1)
DO n = 1, SIZE(x,1)
  x(n) = xmin + (n - 1) * m
END DO
!################################################
END SUBROUTINE linspace