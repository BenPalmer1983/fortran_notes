SUBROUTINE zero(x)
!################################################
IMPLICIT NONE
!################################################
REAL(kind=REAL64), INTENT(INOUT) ::    x(:, :)
!################################################
x(:, :) = 0.0d0
!################################################
END SUBROUTINE zero