! Read file - into array, 2 columns
SUBROUTINE readfile(filepath, arr)
!########################################################################
IMPLICIT NONE
!########################################################################
CHARACTER(LEN=*), INTENT(IN) ::                      filepath
REAL(kind=REAL64), ALLOCATABLE, INTENT(INOUT) ::     arr(:,:)
!########################################################################
INTEGER(kind=INT32), PARAMETER ::                    bsize = 1000000
CHARACTER(LEN=255) ::                                line
CHARACTER(LEN=255) ::                                buffer(1:bsize)
INTEGER(kind=INT32) ::                               status, ios, fh
INTEGER(kind=INT32) ::                               n
INTEGER(kind=INT32) ::                               lines
LOGICAL ::                                           exists
!########################################################################
! Check file exists
INQUIRE (file=filepath, exist=exists)
IF(.NOT. exists)THEN
  STOP "File " // TRIM(filepath) // " does not exist"
END IF
! Count Lines & Read To Buffer
lines = 0
OPEN(newunit=fh, file=filepath, action='read', iostat=status) 
DO n = 1, bsize
  READ(fh,"(A255)",IOSTAT=ios) line
  IF(TRIM(line) .NE. "")THEN
    lines = lines + 1   
    buffer(lines) = line
  END IF
  If(ios /= 0)Then
    EXIT
  End If 
END DO
CLOSE(fh)
! Allocate array
IF(ALLOCATED(arr)) DEALLOCATE(arr)
ALLOCATE(arr(1:lines, 1:2))
! Read into array
DO n = 1, lines
  READ(buffer(n),"(10F8.2,10F8.2)",IOSTAT=ios) arr(n, 1), arr(n, 2)
END DO
END SUBROUTINE readfile