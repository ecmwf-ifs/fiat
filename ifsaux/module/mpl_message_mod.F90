MODULE MPL_MESSAGE_MOD

!**** MPL_MESSAGE - Prints message

!     Purpose.
!     --------
!     Creates an ASCII message for printing and optionally aborts

!**   Interface.
!     ----------
!        CALL MPL_MESSAGE

!        Input required arguments :
!        -------------------------
!           CDMESSAGE-  character string for message

!        Input optional arguments :
!        -------------------------
!           KERROR   -  Error number
!           CDSTRING -  Optional additional message 
!                       prepended to CDMESSAGE
!           LDABORT  -  forces ABORT if true

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           none
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

USE MPL_DATA_MODULE
USE MPL_ABORT_MOD

#include "tsmbkind.h"

PRIVATE

#include "mpif.h"

PUBLIC MPL_MESSAGE

CONTAINS 

SUBROUTINE MPL_MESSAGE(KERROR,CDMESSAGE,CDSTRING,LDABORT)

IMPLICIT NONE

INTEGER_M,INTENT(IN),OPTIONAL     :: KERROR
CHARACTER*(*),INTENT(IN)          :: CDMESSAGE
CHARACTER*(*),INTENT(IN),OPTIONAL :: CDSTRING
LOGICAL      ,INTENT(IN),OPTIONAL :: LDABORT

CHARACTER*(MPI_MAX_ERROR_STRING)  :: CLMPI_ERROR
CHARACTER*10                      :: CLERROR
INTEGER_M                         :: IRESULTLEN,IERROR

IF(PRESENT(KERROR)) THEN
  WRITE(CLERROR,'(I10)') KERROR
ELSE
  CLERROR=' '
ENDIF
IF(PRESENT(CDSTRING)) THEN
  WRITE(MPL_UNIT,'(4A,I8)') CDSTRING,CDMESSAGE,CLERROR, &
                               & ' FROM PROCESSOR ',MPL_RANK
ELSE
  WRITE(MPL_UNIT,'(3A,I8)') CDMESSAGE,CLERROR, &
                               & ' FROM PROCESSOR ',MPL_RANK
ENDIF

IF(PRESENT(KERROR)) THEN
  CALL MPI_ERROR_STRING(KERROR,CLMPI_ERROR,IRESULTLEN,IERROR)
  WRITE(MPL_UNIT,'(2A,I8)') CLMPI_ERROR(1:IRESULTLEN),' in process ',MPL_RANK
ENDIF

IF(PRESENT(LDABORT)) THEN
  IF(LDABORT) THEN
    WRITE(0,'(2A,I8)') CLMPI_ERROR(1:IRESULTLEN),' in process ',MPL_RANK
    CALL MPL_ABORT('ABORT')
  ENDIF
ENDIF
RETURN
END SUBROUTINE MPL_MESSAGE

END MODULE MPL_MESSAGE_MOD
