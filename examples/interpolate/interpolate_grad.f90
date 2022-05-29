! gfortran -o interpolate_grad.x interpolate_grad.f90 && ./interpolate_grad.x 

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

print *, interpngrad(0.785398163d0, arr(1:4,1), arr(1:4,2))


!################################################
END SUBROUTINE main

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
