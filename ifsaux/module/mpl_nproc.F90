MODULE MPL_NPROC_MOD
!**** MPL_NPROC - return Number of processes 

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 

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
PUBLIC MPL_NPROC

CONTAINS 
FUNCTION MPL_NPROC(KCOMM)
INTEGER_M,INTENT(IN),OPTIONAL :: KCOMM
INTEGER_M :: MPL_NPROC

INTEGER_M :: IERROR,IPROC
LOGICAL   :: LLABORT=.TRUE.

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE( &
  & CDMESSAGE='MPL_MYRANK: MPL NOT INITIALISED ',LDABORT=LLABORT) 
IF(PRESENT(KCOMM)) THEN
  CALL MPI_COMM_SIZE(KCOMM,IPROC,IERROR)
  MPL_NPROC = IPROC
ELSE
  MPL_NPROC = MPL_NUMPROC 
ENDIF

  
END FUNCTION MPL_NPROC
END MODULE MPL_NPROC_MOD

