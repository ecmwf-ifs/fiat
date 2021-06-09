! (C) Copyright 2005- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

SUBROUTINE DR_HOOK_UTIL(LDHOOK,CDNAME,KCASE,PKEY,CDFILENAME,KSIZEINFO)
USE PARKIND_FAUX  ,ONLY : JPIM,JPRD
USE OML_MOD       ,ONLY : OML_MY_THREAD
USE YOMHOOK       ,ONLY : LHOOK

IMPLICIT NONE

! Arguments
LOGICAL,INTENT(INOUT)         :: LDHOOK
CHARACTER(LEN=*),INTENT(IN)   :: CDNAME,CDFILENAME
INTEGER(KIND=JPIM),INTENT(IN) :: KCASE,KSIZEINFO
REAL(KIND=JPRD),INTENT(INOUT) :: PKEY

! Persistent variables, setup at first call
LOGICAL,SAVE :: LL_INIT       = .FALSE.
LOGICAL,SAVE :: LL_DRHACK     = .FALSE. ! Will be set to .TRUE. if envvar DR_HACK=1
LOGICAL,SAVE :: LL_STACKCHECK = .FALSE. ! Will be set to .TRUE. if envvar DR_HOOK_STACKCHECK=1
LOGICAL,SAVE :: LL_HEAPCHECK  = .FALSE. ! Will be set to .TRUE. if envvar DR_HOOK_HEAPCHECK=1

! Local variables
INTEGER(KIND=JPIM) :: IMYTID
INTEGER(KIND=8)    :: MAXMEM=0 ! For comparing memory between HEAPCHECK_START and HEAPCHECK_END

#include "dr_hook_init.intfb.h"
#include "user_clock.intfb.h"

IF (.NOT.LDHOOK) RETURN

IMYTID = OML_MY_THREAD()

IF (.NOT.LL_INIT) THEN
  LL_INIT = .TRUE.
  CALL DR_HOOK_INIT()
  IF(.NOT.LHOOK) RETURN ! LHOOK is set to .TRUE. within DR_HOOK_INIT() only when envvar DR_HOOK=1

  CALL DR_HACK_INIT()
  CALL STACKCHECK_INIT()
  CALL HEAPCHECK_INIT()
ENDIF ! .NOT.LL_INIT

IF (LL_STACKCHECK) CALL STACKCHECK()

IF (KCASE == 0) THEN
  CALL C_DRHOOK_START(CDNAME, IMYTID, PKEY, CDFILENAME, KSIZEINFO)
  IF(LL_HEAPCHECK) CALL HEAPCHECK_START()
ELSE IF (KCASE == 1) THEN
  IF(LL_HEAPCHECK) CALL HEAPCHECK_END()
  CALL C_DRHOOK_END  (CDNAME, IMYTID, PKEY, CDFILENAME, KSIZEINFO)
ENDIF

IF (LL_DRHACK) THEN 
  CALL DR_HACK(CDNAME,KCASE)
ENDIF

CALL GSTATS_FINDSUMB() ! currently only dead code within

!-------------------- END SUBROUTINE DR_HOOK_UTIL -----------------

CONTAINS

FUNCTION MYPROC()
  USE MPL_DATA_MODULE ,ONLY : MPL_NUMPROC
  USE MPL_MYRANK_MOD  ,ONLY : MPL_MYRANK
  INTEGER(KIND=JPIM) :: MYPROC
  IF( MPL_NUMPROC > 0 ) THEN
    MYPROC = MPL_MYRANK()
  ELSE
    MYPROC = 1
  ENDIF
END FUNCTION MYPROC

SUBROUTINE DR_HACK_INIT()
  USE MPL_DATA_MODULE ,ONLY : MPL_NUMPROC
  USE MPL_MYRANK_MOD  ,ONLY : MPL_MYRANK
  USE YOMLUN_FAUX     ,ONLY : NULDRHACK
  IMPLICIT NONE
  CHARACTER(LEN=512) :: CLENV
  CALL GET_ENVIRONMENT_VARIABLE('DR_HACK',CLENV)
  IF( CLENV == 'yes'  .OR. CLENV == 'YES'  .OR. &
    & CLENV == 'true' .OR. CLENV == 'TRUE' .OR. &
    & CLENV == 'on'   .OR. CLENV == 'ON'   .OR. &
    & CLENV == '1' ) THEN
    IF( MYPROC() == 1 ) THEN
      LL_DRHACK=.TRUE.
      OPEN (UNIT = NULDRHACK, file = "drhack.txt",position="append",action="write")
    ENDIF
  ENDIF
END SUBROUTINE DR_HACK_INIT

SUBROUTINE DR_HACK(ROUTINE,START)
!
! Florian Suzat (METEO-FRANCE) Sept 2017 : add drHack functionality
!
! drHack documentation:
! ----------------------------------
! ARPIFS has become a huge and complicated program. Debugging it can be very
! painful especially for newbies. Documenting it is also is a huge and tedious
! job.
! The idea behind “drHack” is basically to hack drHook: using the calls 
! "IF (LHOOK) CALL DR_HOOK('XXX',I,ZHOOK_HANDLE)" 
! (where XXX is the name of a routine, and I is 0 at the beginning of the
! routine and 1 at
! the end) in order to build a big XML file describing the ARPIFS calling tree.
! At initialization, if both environmental variables DR_HOOK and DR_HACK are set
! equal to 1, 
! then the hack is activated, otherwise everything works as usual.

! IMPORTANT: for the moment, it does not work with openmp
! (need to run with openmp=1) 

! When active, we first open a file drhack.txt.
! Every time the program enters a routine, we append <ROUTINE_NAME> to the
! file, and every time the routine is left, we append </ROUTINE_NAME> (mind the
! “/” extra character).
! Then, at the end of the run, the (big!) file drhack.txt contains the calling
! tree of the MPI processor number 0 as an XML file:
! <MASTER>
! <STACK_MIX_INIT_STACK>
! <STACK_MIX_GETSTACKUSAGEX>
! </STACK_MIX_GETSTACKUSAGEX>
! </STACK_MIX_INIT_STACK>
! <CNT0>
! <GEOMETRY_MOD_GEOMETRY_SET>
! </GEOMETRY_MOD_GEOMETRY_SET>
! ....
! 
! The resulting files are not usable as is (because they are too big). But with
! a few
! lines of python, it is easy to produce a condensed version of the drhack.txt
! file
! (if you want an example script, you may ask florian.suzat@meteo.fr).
! Then, with html and javascript, these condensed files are read and a
! dynamic collapsible search tree is built.
! Illustrations of such pages can be seen at http://intra.cnrm.meteo.fr/drhack/ 
! (only from the MeteoFrance network... If you want an export, mail
! florian.suzat@meteo.fr)

! Hope this help...

! -----------------------------------------------------------------
! Different implementation of this have been tested, but this one, even if it is
! not elegant at all, is almost fast.... 

USE PARKIND_FAUX  ,ONLY : JPIM
USE YOMLUN_FAUX   ,ONLY : NULDRHACK
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: ROUTINE
INTEGER(KIND=JPIM),INTENT(IN) :: START
INTEGER(KIND=JPIM) :: i
CHARACTER(LEN(ROUTINE)) :: ROUTINE_CLEAN

! replace some special character
DO i = 1,LEN(ROUTINE)
  SELECT CASE (ROUTINE(i:i))
  CASE ("<")
    ROUTINE_CLEAN (i:i)="_"
  CASE (">")
    ROUTINE_CLEAN (i:i)="_"
  CASE (":")
    ROUTINE_CLEAN (i:i)="_"
  CASE (" ")
    ROUTINE_CLEAN (i:i)="_"
  CASE DEFAULT
    ROUTINE_CLEAN (i:i)=ROUTINE(i:i)
  END SELECT
END DO

IF (START==0) THEN
  WRITE(NULDRHACK,*) '<',ROUTINE_CLEAN,'>'
ELSE
  WRITE(NULDRHACK,*) '</',ROUTINE_CLEAN,'>'
  !CLOSE FILE IF LAST ROUTINE
  IF (ROUTINE_CLEAN .eq. 'MODEL_MOD_MODEL_DELETE') THEN
    CLOSE (NULDRHACK)
  ENDIF
ENDIF
END SUBROUTINE DR_HACK

SUBROUTINE HEAPCHECK_INIT()
IMPLICIT NONE
CHARACTER(LEN=4)  :: CHEAP
!JFH---Initialisation to monitor heap usage-----------------------
CALL GET_ENVIRONMENT_VARIABLE('DR_HOOK_HEAPCHECK',CHEAP)
IF( CHEAP == 'yes'  .OR. CHEAP == 'YES'  .OR. &
  & CHEAP == 'true' .OR. CHEAP == 'TRUE' .OR. &
  & CHEAP == 'on'   .OR. CHEAP == 'ON'   .OR. &
  & CHEAP == '1' ) THEN
  LL_HEAPCHECK = .TRUE.
  IF(IMYTID == 1) THEN
    CALL SETHEAPCHECK()
  ENDIF
ENDIF
!JFH------------ End ---------------------------------------------
END SUBROUTINE HEAPCHECK_INIT

SUBROUTINE HEAPCHECK_START()
!JFH---Code to monitor heap usage -------------------------
USE YOMLUN_FAUX ,ONLY : NULERR
IMPLICIT NONE 
INTEGER*8 :: GETMAXLOC
INTEGER*8 :: GETMAXMEM
IF(IMYTID == 1) THEN
  IF( MYPROC() == 1) THEN
    GETMAXMEM=GETMAXLOC()
    IF(GETMAXMEM .GT. MAXMEM) THEN
      MAXMEM = GETMAXMEM
      WRITE(NULERR,*) "HEAPCHECK Max heap at beg of routine =",MAXMEM," ",CDNAME
    ENDIF
  ENDIF
ENDIF
!JFH------------ End ---------------------------------------------
END SUBROUTINE HEAPCHECK_START

SUBROUTINE HEAPCHECK_END()
!JFH---Code to monitor heap usage -------------------------
USE YOMLUN_FAUX ,ONLY : NULERR
IMPLICIT NONE
INTEGER(KIND=8) :: GETMAXLOC
INTEGER(KIND=8) :: GETMAXMEM
IF(IMYTID == 1) THEN
  IF( MYPROC() == 1) THEN
    GETMAXMEM=GETMAXLOC()
    IF(GETMAXMEM .GT. MAXMEM) THEN
      MAXMEM = GETMAXMEM
      WRITE(NULERR,*) "HEAPCHECK Max heap at end of routine =",MAXMEM," ",CDNAME
    ENDIF
  ENDIF
ENDIF
!JFH------------ End ---------------------------------------------
END SUBROUTINE HEAPCHECK_END

SUBROUTINE STACKCHECK_INIT()
  USE YOMHOOKSTACK  ,ONLY : LL_THREAD_FIRST,ISAVE,IMAXSTACK,CSTACK   ! For monitoring thread stack usage
  USE OML_MOD       ,ONLY : OML_MAX_THREADS
  IMPLICIT NONE
  INTEGER(KIND=JPIM) :: INUMTIDS

  INUMTIDS = OML_MAX_THREADS()

  !JFH---Initialisation to monitor stack usage by threads-------------
  CALL GET_ENVIRONMENT_VARIABLE('DR_HOOK_STACKCHECK',CSTACK)
  IF ( CSTACK == 'yes'  .OR. CSTACK == 'YES'  .OR. &
     & CSTACK == 'true' .OR. CSTACK == 'TRUE' .OR. &
     & CSTACK == 'on'   .OR. CSTACK == 'ON'   .OR. &
     & CSTACK == '1' ) THEN
    LL_STACKCHECK = .TRUE.
    IF(IMYTID == 1 ) THEN
      ALLOCATE(LL_THREAD_FIRST(INUMTIDS))
      ALLOCATE(ISAVE(INUMTIDS))
      ALLOCATE(IMAXSTACK(INUMTIDS))
      LL_THREAD_FIRST=.TRUE.
      ISAVE=0
      IMAXSTACK=0
    ENDIF
  ENDIF
!JFH------------ End ---------------------------------------------
END SUBROUTINE STACKCHECK_INIT


SUBROUTINE STACKCHECK()
!JFH---Code to monitor stack usage by threads---------------------
#ifndef NAG
USE YOMHOOKSTACK ,ONLY : LL_THREAD_FIRST,ISAVE,IMAXSTACK
USE YOMLUN_FAUX  ,ONLY : NULERR
IMPLICIT NONE
INTEGER(KIND=8) :: ILOC  ! For monitoring thread stack usage
IF(IMYTID > 1) THEN
  IF(LL_THREAD_FIRST(IMYTID))THEN 
    LL_THREAD_FIRST(IMYTID)=.FALSE.
    ISAVE(IMYTID)=LOC(ILOC)
  ENDIF
  
  ILOC=LOC(ILOC)

  IF(ISAVE(IMYTID)-ILOC > IMAXSTACK(IMYTID)) THEN
    IMAXSTACK(IMYTID)=ISAVE(IMYTID)-ILOC
    WRITE(NULERR,'(A,I3,A,I12,2X,A)')"STACKCHECK Max stack usage by thread",IMYTID," =",IMAXSTACK(IMYTID),CDNAME
  ENDIF
ENDIF
#endif
!JFH------------ End ---------------------------------------------
END SUBROUTINE STACKCHECK


SUBROUTINE GSTATS_FINDSUMB()
!GM---Code to find gstats SUMB time-------------------------------
!!!! Willem Deconinck - June 2021:
!!!!     Note following code was dead as LLFINDSUMB = .FALSE. hardcoded.
!!!!     From gstats.F90 documentation: LLFINDSUMB - when set is used detect gstat counter problems.
!!!!     If agreed, this could be removed altogether, and remove dependency of dr_hook on gstats
!!!!     Now removed from compilation with #if 0
#if 0
USE YOMGSTATS, ONLY : LAST_KNUM,LAST_KSWITCH,LDETAILED_STATS,MYPROC_STATS, &
                      NHOOK_MESSAGES,TIME_LAST_CALL
IMPLICIT NONE
LOGICAL, PARAMETER :: LLFINDSUMB=.FALSE.
REAL(KIND=JPRD) :: ZCLOCK
REAL(KIND=JPRD) :: ZDIFF
CHARACTER(LEN=7) CLSTR

IF( LDETAILED_STATS .AND. LLFINDSUMB )THEN
  IF( IMYTID==1 .AND. LAST_KNUM>=500 .AND. MYPROC_STATS <= 2 )THEN
    IF( LAST_KSWITCH==1 .OR. LAST_KSWITCH==2 )THEN
      CALL USER_CLOCK(PELAPSED_TIME=ZCLOCK)
      ZDIFF=ZCLOCK-TIME_LAST_CALL
      IF( ZDIFF > 0.1_JPRD )THEN
        IF( KCASE == 0 )THEN
          CLSTR='ENTERED'
        ELSE
          CLSTR='EXITED'
        ENDIF
        IF( NHOOK_MESSAGES < 100000 )THEN
          WRITE(0,'("DR_HOOK_UTIL: ",A,2X,A," TIMESUMB=",F10.6)')CDNAME,CLSTR,ZDIFF
          NHOOK_MESSAGES=NHOOK_MESSAGES+1
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
#endif
!GM------------ End --------------------------------------------- 
END SUBROUTINE GSTATS_FINDSUMB

END SUBROUTINE DR_HOOK_UTIL

