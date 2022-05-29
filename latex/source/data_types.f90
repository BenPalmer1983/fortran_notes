PROGRAM data_types
!===============================================================================

USE iso_fortran_env

IMPLICIT NONE

! Call main subroutine
!################################################
CALL main()

!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE main()
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL32) ::       float32       ! 32 bit signed   1/8/23   s/e/m
REAL(kind=REAL64) ::       float64
INTEGER(kind=INT8) ::      int8          ! 8 bit signed
INTEGER(kind=INT16) ::     int16         
INTEGER(kind=INT32) ::     int32   
INTEGER(kind=INT64) ::     int64
!################################################

float32 = 3.4028235d38
float64 = -4.29d-18
int8 = 127
int16 = 32767
int32 = 2147483647
int64 = 2**63-1

print *, "ISO FORTRAN DATA TYPES"
print *, float32
print *, float64
print *, int8
print *, int16
print *, int32
print *, int64

!################################################
END SUBROUTINE main

!===============================================================================
END PROGRAM data_types