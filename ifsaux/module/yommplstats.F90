MODULE YOMMPLSTATS

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
! Module for communications statistics.
! Module is internal to the MPLSTATS package -
! routines MPL_SENDSTATS, MPL_RECVSTATS

! LMPLSTATS - TRUE for gathering communications statistics


LOGICAL :: LMPLSTATS = .FALSE.
REAL(KIND=JPRB), ALLOCATABLE    :: MPLSENDBYTES(:), MPLRECVBYTES(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: MPLSENDNUM(:), MPLRECVNUM(:)

END MODULE YOMMPLSTATS




