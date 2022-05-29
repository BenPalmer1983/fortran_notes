PROGRAM read1

CALL main()


CONTAINS


SUBROUTINE main ()

CHARACTER(LEN=255) ::    line
CHARACTER(LEN=255) ::    buffer(1:10000)
INTEGER(8) ::            status, ios
INTEGER(8) ::            n, m
INTEGER(8) ::            lines
REAL(16), ALLOCATABLE :: arr(:,:)
REAL(16) :: a, b


!#################################
! Count Lines & Read To Buffer
!#################################

lines = 0
open(unit=99, file='numbers.txt', action='read', iostat=status) 
DO n = 1, 1000000
  Read(99,"(A255)",IOSTAT=ios) line
  IF(TRIM(line) .NE. "")THEN
    lines = lines + 1   
    buffer(lines) = line
  END IF
  If(ios /= 0)Then
    EXIT
  End If 
End Do 
close(99)


!#################################
! Allocate array
!#################################

IF(ALLOCATED(arr)) DEALLOCATE(arr)
ALLOCATE(arr(1:lines, 1:2))


!#################################
! Read into array
!#################################

DO n = 1, lines
  Read(buffer(n),"(10F8.2,10F8.2)",IOSTAT=ios) arr(n, 1), arr(n, 2)
End Do



END SUBROUTINE main


END PROGRAM read1
