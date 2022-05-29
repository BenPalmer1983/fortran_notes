PROGRAM my_prog
!===============================================================================
USE module1
USE module2, ONLY: sub_a, sub_b, func_e

IMPLICIT NONE

CALL main()

!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE main()
IMPLICIT NONE
END SUBROUTINE main


SUBROUTINE other_sub()
IMPLICIT NONE
END SUBROUTINE other_sub


!===============================================================================
END PROGRAM my_prog
