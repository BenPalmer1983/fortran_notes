PROGRAM interpolate
!===============================================================================
USE iso_fortran_env

IMPLICIT NONE

CALL main()


!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE main()
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64) ::    arr(1:11, 1:2)
REAL(kind=REAL64) ::    arr_interp(1:41, 1:2)
INTEGER(kind=INT8) ::   n
!################################################


print *, "Test Interpolate"

CALL linspace(0.0d0, 3.14159265358979323846d0, arr(:,1))
arr(:,2) = SIN(arr(:,1))

print *, arr(:,1)
print *, arr(:,2)

!print *, interpn(0.5d0, arr(:,1), arr(:,2))

CALL interpfill(arr(:,1), arr(:,2), 3, arr_interp(:,1), arr_interp(:,2))

print *, ""
DO n = 1, SIZE(arr, 1)
  print *, arr(n, 1), arr(n, 2)
END DO

print *, ""
DO n = 1, SIZE(arr_interp, 1)
  print *, arr_interp(n, 1), arr_interp(n, 2)
END DO

!################################################
END SUBROUTINE main


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




SUBROUTINE linspace(xmin, xmax, x)
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



!===============================================================================
END PROGRAM interpolate
