MODULE SDL_MODULE

!    Interface between user applications and system-dependent intrinsic
!    routines, provided by the computer vendors.

!    All routines which wish to call these routines must contain:
!    USE SDL_MODULE

! Author :
! ------
!   11-Apr-2005 R. El Khatib  *METEO-FRANCE*

USE PARKIND1  ,ONLY : JPIM  ,JPRB
USE YOMHOOK   ,ONLY : LHOOK ,DR_HOOK

IMPLICIT NONE

SAVE

PRIVATE

PUBLIC :: SRL_ABORT, DIS_ABORT

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE SRL_ABORT

! Purpose :
! -------
!   To abort in serial environment

#ifdef VPP

CALL ERRTRA
CALL ABORT

#elif RS6K

CALL XL__TRBK
CALL FLUSH(0)
CALL ABORT

#elif SX4

CALL EXIT

#else

STOP 'SRL_ABORT'

#endif

END SUBROUTINE SRL_ABORT
!-----------------------------------------------------------------------------
SUBROUTINE DIS_ABORT(KCOMM)

! Purpose :
! -------
!   To abort in distributed environment

!   KCOMM : communicator

INTEGER(KIND=JPIM), INTENT(IN) :: KCOMM

INTEGER(KIND=JPIM) :: IRETURN_CODE,IERROR

#ifdef VPP

CALL ERRTRA
CALL SLEEP(30)
CALL VPP_ABORT()

#elif RS6K

CALL XL__TRBK
CALL FLUSH(0)
IRETURN_CODE=1
CALL MPI_ABORT(KCOMM,IRETURN_CODE,IERROR)

#else

STOP 'DIS_ABORT'

#endif

END SUBROUTINE DIS_ABORT
!-----------------------------------------------------------------------------

END MODULE SDL_MODULE
