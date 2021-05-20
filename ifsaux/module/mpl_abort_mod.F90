! (C) Copyright 2005- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_ABORT_MOD

USE MPL_DATA_MODULE
USE MPL_MPIF
USE OML_MOD
USE YOMABRT, ONLY : MAB_CNT
USE SDL_MOD, ONLY : SDL_TRACEBACK, SDL_DISABORT
#ifdef NAG
USE F90_UNIX_IO, ONLY: FLUSH
#endif
USE PARKIND_FAUX  ,ONLY : JPIM
USE YOMHOOK  , ONLY : LHOOK

PRIVATE
PUBLIC MPL_ABORT

CONTAINS 

SUBROUTINE MPL_ABORT(CDMESSAGE)

IMPLICIT NONE

CHARACTER*(*),INTENT(IN),OPTIONAL :: CDMESSAGE
INTEGER(KIND=JPIM) :: IRETURN_CODE,IERROR,ITID,INUMTH

CHARACTER(LEN=80) :: CLTRBK

ITID=OML_MY_THREAD()
INUMTH=OML_MAX_THREADS()

IF (MPL_UNIT > 0) CALL FLUSH(MPL_UNIT)
!------Traceback from only one thread
!$OMP CRITICAL (CRIT_MPL_ABORT)
!$OMP FLUSH(MAB_CNT)
IF (MAB_CNT == 0) THEN
  IF(PRESENT(CDMESSAGE)) THEN
    WRITE(MPL_ERRUNIT,'(A,I0,A,I0,A,A)') 'MPL_ABORT [PROC=',MPL_RANK,',THRD=',ITID, '] : ', CDMESSAGE
  ELSE
    WRITE(MPL_ERRUNIT,'(A,I0,A,I0,A)') 'MPL_ABORT [PROC=',MPL_RANK,',THRD=',ITID, ']'
  ENDIF
  MAB_CNT=1
!$OMP FLUSH(MAB_CNT)

#if defined(__INTEL_COMPILER)
  CALL GET_ENVIRONMENT_VARIABLE("EC_LINUX_TRBK",CLTRBK)
#else
  CLTRBK='1'
#endif
  IF (LHOOK .AND. CLTRBK=='1') THEN
     CALL TABORT() ! should not hang and calls DrHook's error traceback processing (more robust nowadays)
  ELSE
     CALL SDL_TRACEBACK(ITID) ! this will no longer hang with Intel compiler because intel tracebackqq is called, not linux traceback
  ENDIF
ENDIF
!$OMP END CRITICAL (CRIT_MPL_ABORT)
CALL SDL_DISABORT()

END SUBROUTINE MPL_ABORT

END MODULE MPL_ABORT_MOD
