MODULE MPL_BARRIER_MOD

!**** MPL_BARRIER - Barrier synchronisation

!     Purpose.
!     --------
!     Blocks the caller until all group members have called it.

!**   Interface.
!     ----------
!        CALL MPL_BARRIER

!        Input required arguments :
!        -------------------------
!           none

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_BARRIER aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE


PRIVATE

INTEGER :: ICOMM,IERROR
LOGICAL :: LLABORT=.TRUE.

PUBLIC MPL_BARRIER

CONTAINS

SUBROUTINE MPL_BARRIER(KCOMM,CDSTRING,KERROR)

INTEGER_M,INTENT(IN),OPTIONAL   :: KCOMM
INTEGER_M,INTENT(OUT),OPTIONAL  :: KERROR
CHARACTER*(*),INTENT(IN),OPTIONAL :: CDSTRING

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE(CDSTRING=CDSTRING,&
  & CDMESSAGE='MPL_BARRIER: MPL NOT INITIALISED ',LDABORT=LLABORT)
 
IF(PRESENT(KCOMM)) THEN
  ICOMM=KCOMM
ELSE
  ICOMM=MPL_COMM
ENDIF

#ifdef VPP
CALL VPP_Barrier
#else
CALL MPI_BARRIER(ICOMM,IERROR)
#endif


IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ELSE
  IF(IERROR /= 0 ) CALL MPL_MESSAGE(IERROR,'MPL_BARRIER',CDSTRING,LDABORT=LLABORT)
ENDIF
  
RETURN
END SUBROUTINE MPL_BARRIER

END MODULE MPL_BARRIER_MOD
