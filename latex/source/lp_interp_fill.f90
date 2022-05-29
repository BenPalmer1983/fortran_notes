SUBROUTINE interpfill(x_in, y_in, k, x_out, y_out)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::       x_in(:)
REAL(kind=REAL64), INTENT(IN) ::       y_in(:)
INTEGER(kind=INT32), INTENT(IN) ::     k
REAL(kind=REAL64), INTENT(INOUT) ::    x_out(:)
REAL(kind=REAL64), INTENT(INOUT) ::    y_out(:)
!################################################
INTEGER(kind=INT32) ::                 s_in, s_out
INTEGER(kind=INT32) ::                 n, nn, a, b, i, j
REAL(kind=REAL64) ::                   m
REAL(kind=REAL64) ::                   li
!################################################
IF(SIZE(x_in) .NE. SIZE(y_in) .OR. SIZE(x_out) .NE. SIZE(y_out))THEN
  STOP "Error interpolate() unequal array sizes"
END IF
! Array sizes
s_in = SIZE(x_in)
s_out = SIZE(x_out)
! Fill X
m = (x_in(s_in) - x_in(1)) / (s_out - 1)
DO n = 1, s_out
  x_out(n) = x_in(1) + (n - 1) * m
END DO
! Fill Y
y_out(:) = 0.0d0
nn = 1
DO n = 1, s_out
  DO WHILE(nn .LT. s_in .AND. .NOT. (x_out(n) .GE. x_in(nn) .AND. x_out(n) .LE. x_in(nn+1)))
    nn = nn + 1
  END DO
  ! Choose subset of points for interpolation
  a = MIN(nn, s_in - k + 1)
  b = a + k - 1
  DO i = a, b
    li = 1.0D0
    DO j = a, b
      IF(i .NE. j) THEN
        li = li * (x_out(n) - x_in(j)) / (x_in(i) - x_in(j))
      END IF
    END DO
    y_out(n) = y_out(n) + li * y_in(i)
  END DO
END DO
END SUBROUTINE interpfill