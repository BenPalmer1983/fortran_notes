PROGRAM my_prog
!===============================================================================
!USE module1
!USE module2, ONLY: sub_a, sub_b, func_e
USE iso_fortran_env

IMPLICIT NONE

CALL main()

!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE main()
IMPLICIT NONE
REAL(kind=REAL64) ::     a(1:10, 1:4)
REAL(kind=REAL64) ::     b(1:10, 1:4)
REAL(kind=REAL64) ::     s

a(:,:) = 3.0d0
b(:,:) = 5.0d0
s = sum_arrays(a, b)

END SUBROUTINE main


SUBROUTINE other_sub()
IMPLICIT NONE
END SUBROUTINE other_sub


FUNCTION sum_arrays(a, b)
!#######################################
REAL(kind=REAL64) ::     a(1:10, 1:4)
REAL(kind=REAL64) ::     b(1:10, 1:4)
REAL(kind=REAL64) ::     sum_arrays
!#######################################
INTEGER(kind=INT32) ::   i, j
!#######################################

sum_arrays = 0.0D0
DO j = 1, 4         ! Loop over columns (outer)
  DO i = 1, 10      ! Loop over rows (inner)
    sum_arrays = sum_arrays + a(i, j) + b(i, j)
  END DO
END DO
print *, sum_arrays

! Better solution 
sum_arrays = 0.0D0
sum_arrays = SUM(a(:,:)) + SUM(b(:,:))
print *, sum_arrays

END FUNCTION sum_arrays


!===============================================================================
END PROGRAM my_prog
