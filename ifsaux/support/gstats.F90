SUBROUTINE GSTATS(KNUM,KSWITCH)

!**** *GSTATS*  - Gather timing statistics

!     PURPOSE.
!     --------
!       To gather timings for subsequent output by routine STATS_OUTPUT 


!**   INTERFACE.
!     ----------
!       *CALL* *GSTATS(KNUM,KSWITCH)

!        EXPLICIT ARGUMENTS
!        --------------------
!        KNUM - timing event number (for list of already defined events
!               see routine STATS_OUTPUT)
!        KSWITCH  - KSWITCH=0 - switch on timer
!                   KSWITCH=1 - switch off timer
!                   KSWITCH=2 - suspend timer
!                   KSWITCH=3 - resume  timer

!        IMPLICIT ARGUMENTS
!        --------------------
!        Module YOMSTATS

!     METHOD.
!     -------


!     EXTERNALS.   USER_CLOCK - timing routine
!     ----------   MPL_BARRIER - syncronization of processors

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!        Mats Hamrud ECMWF

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 98-11-15
!        D.Salmond: 02-02-25  Return if not master thread when called from a 
!                             parallel region. 
!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE YOMGSTATS
USE MPL_MODULE
USE YOMOML

IMPLICIT NONE

INTEGER_M,INTENT(IN) :: KNUM
INTEGER_M,INTENT(IN) :: KSWITCH

INTEGER_M :: IMOD,ICALL
REAL_B :: ZTIMED,ZCLOCK,ZTIME,ZTCPU,ZVCPU
LOGICAL :: LLFIRST=.TRUE.

INTERFACE
#include "user_clock.h"
END INTERFACE

!     ------------------------------------------------------------------

IF(OML_MY_THREAD().GT.1)RETURN
IF((KNUM > 500  .AND.KNUM < 1001).AND.(.NOT.LSTATS_COMMS))RETURN
IF((KNUM > 1000 .AND.KNUM < 2001).AND.(.NOT.LSTATS_OMP))RETURN

IF(LSTATS) THEN
  IF(KNUM/=0) THEN
    IF(LSYNCSTATS .AND.(KSWITCH==0.OR. KSWITCH==2)) THEN
      IF(.NOT.OML_IN_PARALLEL())CALL MPL_BARRIER(CDSTRING='GSTATS:')
    ENDIF
  ENDIF
  CALL USER_CLOCK(PELAPSED_TIME=ZCLOCK)
  IF (LSTATSCPU.OR.KNUM==0) THEN
    CALL USER_CLOCK(PTOTAL_CP=ZTCPU,PVECTOR_CP=ZVCPU)
  ELSE
    ZTCPU = _ZERO_
    ZVCPU = _ZERO_
  ENDIF

  IF (LLFIRST) THEN
    NCALLS(:) = 0
    TIMESUM(:) = _ZERO_
    TIMESQSUM(:) = _ZERO_
    TIMEMAX(:) = _ZERO_
    TIMESUMB(:) = _ZERO_
    TTCPUSUM(:) = _ZERO_
    TVCPUSUM(:) = _ZERO_
    TIMELCALL(:) = ZCLOCK
    TIME_LAST_CALL = ZCLOCK
    LLFIRST = .FALSE.
  ENDIF

  IF(KNUM < 0.OR. KNUM > JPMAXSTAT) CALL ABOR1('GSTATS')
  IF(KSWITCH == 0.OR. KSWITCH == 1) THEN
    NCALLS(KNUM) = NCALLS(KNUM)+1
  ENDIF
  IMOD = MOD(NCALLS(KNUM),2)
  IF(.NOT.((KSWITCH == 0.AND. IMOD == 1) .OR.&
   &(KSWITCH == 2.AND. IMOD == 1) .OR.&
   &(KSWITCH == 3.AND. IMOD == 1) .OR.&
   &(KSWITCH == 1.AND. IMOD == 0))) THEN
    WRITE(JPERR,*) 'KNUM,KSWITCH,IMOD,NCALLS(KNUM)',&
     &KNUM,KSWITCH,IMOD,NCALLS(KNUM)
    CALL ABOR1('GSTATS')
  ENDIF

  IF( KSWITCH == 0 ) THEN
! Start timing event
    ZTIMED = ZCLOCK-TIME_LAST_CALL
    TIMESUMB(KNUM) = TIMESUMB(KNUM)+ZTIMED
    THISTIME(KNUM) = _ZERO_
    TIMELCALL(KNUM) = ZCLOCK
    TTCPULCALL(KNUM) = ZTCPU
    TVCPULCALL(KNUM) = ZVCPU
    THISTCPU(KNUM) = _ZERO_
    THISVCPU(KNUM) = _ZERO_
  ELSEIF( KSWITCH == 1 ) THEN
! Finish timing event
    ZTIME = THISTIME(KNUM)+(ZCLOCK-TIMELCALL(KNUM))
    TIMESUM(KNUM) = TIMESUM(KNUM)+ZTIME
    TIMESQSUM(KNUM) = TIMESQSUM(KNUM)+ZTIME**2
    TIMEMAX(KNUM) = MAX(TIMEMAX(KNUM),ZTIME)
    TTCPUSUM(KNUM) = TTCPUSUM(KNUM)+THISTCPU(KNUM)+ZTCPU-TTCPULCALL(KNUM)
    TVCPUSUM(KNUM) = TVCPUSUM(KNUM)+THISVCPU(KNUM)+ZVCPU-TVCPULCALL(KNUM)
  ELSEIF( KSWITCH == 2 ) THEN
! Suspend timing event
    ZTIMED = ZCLOCK-TIMELCALL(KNUM)
    THISTIME(KNUM) = THISTIME(KNUM)+ZTIMED
    THISTCPU(KNUM) = THISTCPU(KNUM)+ZTCPU-TTCPULCALL(KNUM)
    THISVCPU(KNUM) = THISVCPU(KNUM)+ZVCPU-TVCPULCALL(KNUM)
  ELSEIF( KSWITCH == 3 ) THEN
! Resume timing event
    TIMELCALL(KNUM) = ZCLOCK
    TTCPULCALL(KNUM) = ZTCPU
    TVCPULCALL(KNUM) = ZVCPU
  ENDIF
  TIME_LAST_CALL = ZCLOCK
!   Trace stats
  NCALLS_TOTAL = NCALLS_TOTAL+1
  IF (LTRACE_STATS .AND. NCALLS_TOTAL <= NTRACE_STATS) THEN
    ICALL = NCALLS_TOTAL
    TIME_TRACE(ICALL) = ZCLOCK
    NCALL_TRACE(ICALL) = (JPMAXSTAT+1)*KSWITCH+KNUM
  ENDIF

ENDIF

END SUBROUTINE GSTATS
