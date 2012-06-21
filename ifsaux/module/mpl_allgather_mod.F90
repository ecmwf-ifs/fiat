MODULE MPL_ALLGATHER_MOD

!**** MPL_ALLGATHER Send data to all processes

!     Purpose.
!     --------
!     Send a message to all processes from a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_ALLGATHER

!        Input required arguments :
!        -------------------------
!           PSENDBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           PRECVBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KRECVCOUNTS-number of elements received from each process

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           KRECVDISPL -displacements in PRECVBUF at which to place 
!                       the incoming data
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_ALLGATHER aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-11-23

!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PRIVATE

#include "mpif.h"      

INTEGER_M :: IR,ISENDCOUNT,IRECVCOUNT,ICOMM,IERROR
LOGICAL :: LLABORT=.TRUE.

INTERFACE MPL_ALLGATHER
MODULE PROCEDURE MPL_ALLGATHER_REAL8
END INTERFACE

PUBLIC MPL_ALLGATHER

CONTAINS

SUBROUTINE MPL_ALLGATHER_REAL8(PSENDBUF,PRECVBUF,KRECVCOUNTS,KRECVDISPL, &
                            & KCOMM,KERROR,CDSTRING)


REAL_B            :: PSENDBUF(:)
REAL_B            :: PRECVBUF(:)
INTEGER_M,INTENT(IN) :: KRECVCOUNTS(:)
INTEGER_M,INTENT(IN),OPTIONAL :: KRECVDISPL(:),KCOMM
INTEGER_M,INTENT(OUT),OPTIONAL :: KERROR
CHARACTER*(*),INTENT(IN),OPTIONAL :: CDSTRING

INTEGER_M :: ICOMM,IERROR
INTEGER_M :: IRECVDISPL(SIZE(KRECVCOUNTS))

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE( &
  & CDMESSAGE='MPL_ALLGATHER: MPL NOT INITIALISED ',LDABORT=LLABORT) 
IF(SIZE(KRECVCOUNTS)  < MPL_NUMPROC) THEN
  WRITE(MPL_ERRUNIT,*)'MPL_ALLGATHER: ERROR KRECVCOUNTS dimension=',SIZE(KRECVCOUNTS)
  CALL MPL_MESSAGE(CDMESSAGE='MPL_ALLGATHER: ERROR KRECVCOUNTS dimension is wrong',LDABORT=LLABORT)
ENDIF

IF(PRESENT(KCOMM)) THEN
  ICOMM=KCOMM
ELSE
  ICOMM=MPL_COMM
ENDIF

ISENDCOUNT = SIZE(PSENDBUF)
IRECVCOUNT = SIZE(PRECVBUF)

IF(PRESENT(KRECVDISPL)) THEN
  IRECVDISPL(:) = KRECVDISPL(:)
ELSE
  IRECVDISPL(:) = 0
  DO IR=2, MPL_NUMPROC
    IRECVDISPL(IR) = IRECVDISPL(IR-1) + KRECVCOUNTS(IR-1)
  ENDDO
ENDIF
DO IR=1, MPL_NUMPROC
  IF(IRECVDISPL(IR)+KRECVCOUNTS(IR) > IRECVCOUNT) THEN
    WRITE(MPL_ERRUNIT,'(A,4I10)')'MPL_ALLGATHER:RECV BUFFER TOO SMALL  ', &
     & IR,IRECVDISPL(IR),KRECVCOUNTS(IR),IRECVCOUNT
    CALL MPL_MESSAGE(IERROR,'MPL_ALLGATHER',CDSTRING,LDABORT=LLABORT)
  ENDIF
ENDDO

CALL MPI_ALLGATHERV(PSENDBUF,ISENDCOUNT,MPI_REAL8,PRECVBUF,KRECVCOUNTS, &
                &  IRECVDISPL,MPI_REAL8,ICOMM,IERROR)

IF(MPL_OUTPUT > 1 )THEN
  WRITE(MPL_UNIT,'(A,5I8)') ' MPL_ALLGATHER ',ISENDCOUNT,IRECVCOUNT,ICOMM
ENDIF
IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ELSE
  IF(IERROR /= 0 ) CALL MPL_MESSAGE(IERROR,'MPL_ALLGATHER',CDSTRING,LDABORT=LLABORT)
ENDIF

END SUBROUTINE MPL_ALLGATHER_REAL8

END MODULE MPL_ALLGATHER_MOD
