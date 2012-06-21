SUBROUTINE GSTATS_SETUP( KPROC,KMYPROC,KPRCIDS,&
 & LDSTATS,LDSTATSCPU,LDSYNCSTATS,LDDETAILED_STATS,&
 & LDTRACE_STATS,KTRACE_STATS)

!**** *GSTATS_SETUP*  - Setup stats package

!     PURPOSE.  
!     --------
!      Setup gstats package


!**   INTERFACE.
!     ----------
!       *CALL* *GSTATS_SETUP

!        EXPLICIT ARGUMENTS   None
!        --------------------

!        IMPLICIT ARGUMENTS
!        --------------------
!        Module YOMSTATS

!     METHOD.
!     -------


!     EXTERNALS.   USER_CLOCK - timing routine
!     ----------

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!        Mats Hamrud ECMWF

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 98-11-15
!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE YOMGSTATS

IMPLICIT NONE

LOGICAL :: LDSTATS
LOGICAL :: LDSTATSCPU
LOGICAL :: LDSYNCSTATS
LOGICAL :: LDDETAILED_STATS
LOGICAL :: LDTRACE_STATS
INTEGER_M :: KTRACE_STATS
INTEGER_M :: KPROC,KMYPROC
INTEGER_M :: KPRCIDS(KPROC)
!     ------------------------------------------------------------------

LSTATS = LDSTATS
LSTATSCPU = LDSTATSCPU
LSYNCSTATS = LDSYNCSTATS
LDETAILED_STATS=LDDETAILED_STATS
LTRACE_STATS = LDTRACE_STATS
IF(LTRACE_STATS) NTRACE_STATS = KTRACE_STATS

MYPROC_STATS = KMYPROC
NPROC_STATS  = KPROC
ALLOCATE(NPRCIDS_STATS(NPROC_STATS))
NPRCIDS_STATS(:) = KPRCIDS(1:NPROC_STATS)
IF(NPROC_STATS == 1) LSYNCSTATS = .FALSE.

IF (LTRACE_STATS .AND. NTRACE_STATS>0 ) THEN
  ALLOCATE(NCALL_TRACE(NTRACE_STATS))
  ALLOCATE(TIME_TRACE (NTRACE_STATS))
  NCALL_TRACE(:) = 0
  TIME_TRACE (:) = _ZERO_
ENDIF

RETURN
END SUBROUTINE GSTATS_SETUP


