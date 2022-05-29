FUNCTION sum_arrays(a, b)
!#######################################
REAL(kind=REAL64) ::     a(1:10, 1:4)
REAL(kind=REAL64) ::     b(1:10, 1:4)
REAL(kind=REAL64) ::     sum_arrays
!#######################################
INTEGER(kind=INT32) ::   i, j
!#######################################

sum_arrays = 0.0D0
DO j = 1, 4            ! Loop over columns (outer)
  DO i = 1, 10         ! Loop over rows (inner)
    sum_arrays = sum_arrays + a(i, j) + b(i, j)
  END DO
END DO

! Better solution 
sum_arrays = 0.0D0
sum_arrays = SUM(a(:,:)) + SUM(b(:,:))

END FUNCTION sum_arrays



