subroutine run_fortran_omp_parallel_ipfstr(KTIDS, FUNC, CDSTR)
use parkind1, only : JPIM
implicit none
INTEGER(KIND=JPIM), INTENT(IN) :: KTIDS
EXTERNAL :: FUNC
CHARACTER(LEN=*), INTENT(IN) :: CDSTR
!$OMP PARALLEL NUM_THREADS(KTIDS)
CALL FUNC(CDSTR)
!$OMP END PARALLEL
end subroutine run_fortran_omp_parallel_ipfstr

