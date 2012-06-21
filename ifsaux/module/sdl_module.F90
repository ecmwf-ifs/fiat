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

#ifdef NAG
USE F90_UNIX_PROC, ONLY : ABORT
#endif

IMPLICIT NONE

SAVE

PRIVATE

PUBLIC :: SDL_SRLABORT, SDL_DISABORT, SDL_TRACEBACK

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE SDL_TRACEBACK(KTID)

! Purpose :
! -------
!   Traceback

!   KTID : thread 

INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KTID
CHARACTER(LEN=14) :: CL='SDL_TRACEBACK:'

#ifdef VPP
  CALL ERRTRA
  IF (PRESENT(KTID)) CALL SLEEP(28)
#elif RS6K
  WRITE(0,*)'SDL_TRACEBACK: Calling XL_TRBK, THRD = ',KTID
  CALL XL__TRBK()
  WRITE(0,*)'SDL_TRACEBACK: Done XL_TRBK, THRD = ',KTID
#elif NECSX
  CALL MESPUT(CL,14,1)
#else
  WRITE(0,*)'SDL_TRACEBACK: No traceback implemented.'
#endif

END SUBROUTINE SDL_TRACEBACK
!-----------------------------------------------------------------------------
SUBROUTINE SDL_SRLABORT

! Purpose :
! -------
!   To abort in serial environment

#if defined(VPP) || defined(RS6K) || defined(NAG)
CALL ABORT
#elif SX4
CALL EXIT
#else
STOP 'SDL_SRLABORT'
#endif

END SUBROUTINE SDL_SRLABORT
!-----------------------------------------------------------------------------
SUBROUTINE SDL_DISABORT(KCOMM)

! Purpose :
! -------
!   To abort in distributed environment

!   KCOMM : communicator

INTEGER(KIND=JPIM), INTENT(IN) :: KCOMM

INTEGER(KIND=JPIM) :: IRETURN_CODE,IERROR

#ifdef VPP

CALL VPP_ABORT()

#elif RS6K

IRETURN_CODE=1
CALL MPI_ABORT(KCOMM,IRETURN_CODE,IERROR)

#else

IRETURN_CODE=1
CALL MPI_ABORT(KCOMM,IRETURN_CODE,IERROR)
CALL ABORT
STOP 'SDL_DISABORT'

#endif

END SUBROUTINE SDL_DISABORT
!-----------------------------------------------------------------------------

END MODULE SDL_MODULE
