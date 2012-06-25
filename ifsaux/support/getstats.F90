SUBROUTINE GETSTATS(KNUM,PCOST)

!**** *GETSTATS*  - get timing statistics

!      PURPOSE.
!      --------
!        To get some timing statistics

!      EXPLICIT ARGUMENTS
!      ------------------
!      KNUM  - timing event number (input)
!      PCOST - current time accumulated for event KNUM (output)

!      AUTHOR.
!      -------
!        George Mozdzynski

!      MODIFICATIONS.
!      --------------
!        ORIGINAL : 2008-Sept-29
!
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB

USE YOMGSTATS

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN) :: KNUM
REAL(KIND=JPRB),INTENT(OUT) :: PCOST

PCOST=TIMESUM(KNUM)

RETURN

END SUBROUTINE GETSTATS
