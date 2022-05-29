! gfortran differentiation.f90 -o differentiation.x && ./differentiation.x

PROGRAM testprog

USE iso_fortran_env

CALL main()

CONTAINS

SUBROUTINE main()
!########################################################################
IMPLICIT NONE
!########################################################################

print *, 2.0d0, fgrad(2.0d0), grad(f, 2.0d0), fgrad2(2.0d0), grad2(f, 2.0d0)
print *, -1.0d0, fgrad(-1.0d0), grad(f, -1.0d0), fgrad2(-1.0d0), grad2(f, -1.0d0)
print *, 4.0d0, fgrad(4.0d0), grad(f, 4.0d0), fgrad2(4.0d0), grad2(f, 4.0d0)


END SUBROUTINE main



FUNCTION f(x)
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64), INTENT(IN) ::            x
REAL(kind=REAL64) ::                        f
!########################################################################
f = 3.0d0 * x**3 - 1.2d0 * x**2 + 0.02d0 * x + 1.8d1
END FUNCTION f

FUNCTION fgrad(x)
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64), INTENT(IN) ::            x
REAL(kind=REAL64) ::                        fgrad
!########################################################################
fgrad = 9.0d0 * x**2 - 2.4d0 * x + 0.02d0
END FUNCTION fgrad

FUNCTION fgrad2(x)
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64), INTENT(IN) ::            x
REAL(kind=REAL64) ::                        fgrad2
!########################################################################
fgrad2 = 18.0d0 * x - 2.4d0
END FUNCTION fgrad2


FUNCTION grad(f_in, x)
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64), EXTERNAL ::              f_in
REAL(kind=REAL64), INTENT(IN) ::            x
REAL(kind=REAL64) ::                        grad
!########################################################################
REAL(kind=REAL64) ::                        h
!########################################################################
h = 1.0D-5
grad = (f_in(x+h) - f_in(x-h)) / (2 * h)
END FUNCTION grad

FUNCTION grad2(f_in, x)
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64), EXTERNAL ::              f_in
REAL(kind=REAL64), INTENT(IN) ::            x
REAL(kind=REAL64) ::                        grad2
!########################################################################
REAL(kind=REAL64) ::                        h
!########################################################################
h = 1.0D-5
grad2 = (f_in(x+h) - 2.0d0 * f_in(x) + f_in(x-h)) / h**2
END FUNCTION grad2



END PROGRAM testprog
