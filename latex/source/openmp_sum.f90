!
! gfortran -fopenmp -o sum.x sum.f90 && ./sum.x
!

! Possibily not efficient, over head may outweigh benefit

SUBROUTINE parallelsum()
!########################################################################
IMPLICIT NONE
!########################################################################
REAL(kind=REAL64) ::               myarray(1:100000)
REAL(kind=REAL64) ::               s_thread, s_total
INTEGER(kind=INT64) ::             n, tid, tcount
INTEGER(kind=INT64) ::             OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!########################################################################
s_total = 0.0d0
myarray(1:100000) = 1.0D0
!$OMP PARALLEL &
!$OMP PRIVATE(n, s_thread, tid) &
!$OMP SHARED(myarray, s_total)
s_thread = 0.0d0
!$OMP DO
DO n = 1, 100000
  s_thread = s_thread + myarray(n)
END DO
!$OMP END DO
!$OMP CRITICAL
tcount = OMP_GET_NUM_THREADS()
tid = OMP_GET_THREAD_NUM()
s_total = s_total + s_thread
WRITE(*,*) tid , "/" , tcount, "   ", s_thread
!$OMP END CRITICAL
!$OMP END PARALLEL
WRITE(*,*) s_total
END SUBROUTINE parallelsum