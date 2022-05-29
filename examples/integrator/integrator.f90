PROGRAM integrator
!===============================================================================
USE iso_fortran_env

IMPLICIT NONE

CALL main()


!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE main()
IMPLICIT NONE
!################################################
REAL(kind=REAL64) ::    arr(1:100, 1:2)
!################################################


print *, "Test Integrator"

CALL linspace(0.0d0, 3.14159265358979323846d0, arr(:,1))
arr(:,2) = SIN(arr(:,1))

print *, integrate(arr(:,1), arr(:,2))
print *, integrate2(arr(:,1), arr(:,2))


!################################################
END SUBROUTINE main


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





SUBROUTINE linspace(xmin, xmax, x)
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::       xmin
REAL(kind=REAL64), INTENT(IN) ::       xmax
REAL(kind=REAL64), INTENT(INOUT) ::    x(:)
!################################################
INTEGER(kind=INT32) ::                n
REAL(kind=REAL64) ::                   m
!################################################
m = (xmax - xmin) / (SIZE(x,1) - 1)
DO n = 1, SIZE(x,1)
  x(n) = xmin + (n - 1) * m
END DO
!################################################
END SUBROUTINE linspace















!  Simpson integration
!  Interpolates to odd number of points
!
FUNCTION integrate2(x, y)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(IN) ::    x(:)
REAL(kind=REAL64), INTENT(IN) ::    y(:)
REAL(kind=REAL64) ::                integrate2
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
integrate2 = (h / 3.0d0) * (y(1) + y(xsize) + 4.0d0 * even + 2.0d0 * odd)

END FUNCTION integrate2




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

!===============================================================================
END PROGRAM integrator
