MODULE MPL_BUFFER_METHOD_MOD

!**** MPL_BUFFER_METHOD Establish message passing default method

!     Purpose.
!     --------
!     Setup the message passing buffering 
!     by allocating an attached buffer if required.

!**   Interface.
!     ----------
!        CALL MPL_BUFFER_METHOD

!        Input required arguments :
!        -------------------------
!           KMP_TYPE -  buffering type
!                       possible values are :
!                       JP_BLOCKING_STANDARD, JP_BLOCKING_BUFFERED
!                       defined as parameters in MPL_DATA_MODULE

!        Input optional arguments :
!        -------------------------
!           KMBX_SIZE - Size (in bytes) of attached buffer 
!                       if KMP_TYPE=JP_BLOCKING_BUFFERED
!           KPROCIDS  - array of processor ids

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_BUFFER_METHOD aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PUBLIC MPL_BUFFER_METHOD

CONTAINS 

SUBROUTINE MPL_BUFFER_METHOD(KMP_TYPE,KMBX_SIZE,KERROR,KPROCIDS)

#include "tsmbkind.h"

INTEGER_M,INTENT(IN) ::  KMP_TYPE
INTEGER_M,OPTIONAL,INTENT(IN)  :: KMBX_SIZE
INTEGER_M,OPTIONAL,INTENT(IN)  :: KPROCIDS(:)
INTEGER_M,OPTIONAL,INTENT(OUT) :: KERROR
INTEGER_M :: IMBX_DEFAULT_SIZE = 1000000
INTEGER_M :: IBUFFMPI,IERROR
LOGICAL :: LLABORT=.TRUE.

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE( &
  & CDMESSAGE='MPL_BUFFER_METHOD: MPL NOT INITIALISED ',LDABORT=LLABORT) 

if(KMP_TYPE == JP_BLOCKING_STANDARD) then
  IBUFFMPI=0
ELSE IF(KMP_TYPE == JP_BLOCKING_BUFFERED) THEN
  IF(MPL_NUMPROC > 1) then
    IBUFFMPI=kmbx_size
    if(IBUFFMPI == 0) IBUFFMPI=imbx_default_size
!    convert to bytes
    IBUFFMPI=IBUFFMPI/JP_ATTACHED_BUFFER_BYTES
    ALLOCATE(MPL_ATTACHED_BUFFER(IBUFFMPI))
    CALL MPI_BUFFER_ATTACH(MPL_ATTACHED_BUFFER,IBUFFMPI,IERROR)
    IF(PRESENT(KERROR)) THEN
      KERROR=IERROR
    ELSE
      IF( IERROR /= 0 )THEN
        CALL MPL_MESSAGE(IERROR,'MPL_BUFFER_METHOD ','MPI_BUFFER_ATTACH ERROR',LDABORT=LLABORT)
      ENDIF
    ENDIF
  ENDIF
ELSE
!    invalid type
  IF(PRESENT(KERROR)) THEN
    KERROR=1
  ELSE
    WRITE(MPL_UNIT,'(A,2I8)') 'MPL_BUFFER_METHOD ERROR ',KMP_TYPE
  ENDIF
ENDIF

MPL_MBX_SIZE=IBUFFMPI
MPL_METHOD=KMP_TYPE

IF (MPL_OUTPUT >= 1) THEN
  WRITE(MPL_UNIT,'(A,I2,I9)') 'MPL_BUFFER_METHOD ',MPL_METHOD,MPL_MBX_SIZE
ENDIF

IF(PRESENT(KPROCIDS)) THEN
  IF(SIZE(KPROCIDS) < MPL_NUMPROC) THEN
    CALL MPL_MESSAGE(CDMESSAGE='MPL_BUFFER_METHOD: KPROCIDS NOT CORRECT',LDABORT=LLABORT)
  ELSE
    MPL_IDS=KPROCIDS
  ENDIF
ENDIF

RETURN
END SUBROUTINE MPL_BUFFER_METHOD

END MODULE MPL_BUFFER_METHOD_MOD
