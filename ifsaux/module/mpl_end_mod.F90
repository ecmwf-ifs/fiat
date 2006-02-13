MODULE MPL_END_MOD

!**** MPL_END - Terminates the message passing environment

!     Purpose.
!     --------
!     Cleans up all of the MPI state. 
!     Subsequently, no MPI routine can be called

!**   Interface.
!     ----------
!        CALL MPL_END

!        Input required arguments :
!        -------------------------
!           none

!        Input optional arguments :
!        -------------------------
!           none

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_END aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PUBLIC MPL_END
PRIVATE

INTEGER :: IERROR

CONTAINS 

SUBROUTINE MPL_END(KERROR)

INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR
INTEGER(KIND=JPIM)                      :: IERROR
LOGICAL                      :: LLABORT=.TRUE.

IF(MPL_NUMPROC < 1) THEN
  IF(MPL_NUMPROC == -1) THEN
    CALL MPL_MESSAGE(CDMESSAGE=' MPL_END CALLED BEFORE MPL_INIT ')
  ELSEIF(MPL_NUMPROC == -2) THEN
    CALL MPL_MESSAGE(CDMESSAGE=' MPL_END CALLED MULTIPLE TIMES ')
  ENDIF
  IF(PRESENT(KERROR)) THEN
    IERROR=0
    KERROR=IERROR
  ENDIF
  RETURN
ENDIF


IF (ALLOCATED(MPL_ATTACHED_BUFFER)) THEN
  CALL MPI_BUFFER_DETACH(MPL_ATTACHED_BUFFER,MPL_MBX_SIZE,IERROR)
  IF(PRESENT(KERROR)) THEN
    KERROR=IERROR
  ELSE
    IF( IERROR /= 0 )THEN
      CALL MPL_MESSAGE(IERROR,'MPL_END ',LDABORT=LLABORT)
    ENDIF
  ENDIF
  DEALLOCATE(MPL_ATTACHED_BUFFER)
ENDIF
IF (LINITMPI_VIA_MPL) THEN
  CALL MPI_FINALIZE(IERROR)
ELSE
  IERROR = 0
ENDIF
MPL_NUMPROC = -2

IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ENDIF


RETURN
END SUBROUTINE MPL_END

END MODULE MPL_END_MOD
