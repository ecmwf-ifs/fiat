MODULE MPL_INIT_MOD

!**** MPL_INIT - Initialises the Message passing environment

!     Purpose.
!     --------
!     Must be called before any other MPL routine.

!**   Interface.
!     ----------
!        CALL MPL_INIT

!        Input required arguments :
!        -------------------------
!           none

!        Input optional arguments :
!        -------------------------
!           KOUTPUT  -  Level of printing for MPL routines
!                       =0: none
!                       =1: intermediate (default)
!                       =2: full trace
!           KUNIT    -  Fortran Unit to receive printed trace

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_INIT aborts when an error is detected.
!           KPROCS   -  Number of processes which have been initialised
!                       in the MPI_COMM_WORLD communicator
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
INTEGER,EXTERNAL :: MPL_MYRANK


PUBLIC MPL_INIT
PRIVATE

INTEGER_M :: IERROR,IP,ICOMM,IRANK

CONTAINS 

SUBROUTINE MPL_INIT(KOUTPUT,KUNIT,KERROR,KPROCS)

#include "mpif.h"

INTEGER_M,INTENT(IN),OPTIONAL :: KOUTPUT,KUNIT
INTEGER_M,INTENT(OUT),OPTIONAL :: KERROR,KPROCS
LOGICAL                      :: LLABORT=.TRUE.

IF(MPL_NUMPROC /= -1) THEN
  IERROR = MPL_NUMPROC
  CALL MPL_MESSAGE(IERROR,CDMESSAGE=' MPL_INIT CALLED MULTIPLE TIMES ')
  IF(PRESENT(KERROR)) THEN
    KERROR=0
  ENDIF
  IF(PRESENT(KPROCS)) THEN
    KPROCS=MPL_NUMPROC
  ENDIF
  RETURN
ENDIF

IF(PRESENT(KOUTPUT)) THEN
  MPL_OUTPUT=KOUTPUT
ELSE
  MPL_OUTPUT=1
ENDIF
IF(PRESENT(KUNIT)) THEN
  MPL_UNIT=KUNIT
ELSE
  MPL_UNIT=6
ENDIF

CALL MPI_INIT(IERROR)

IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ELSE
  IF(IERROR /= 0) THEN
    CALL MPL_MESSAGE(IERROR,CDMESSAGE=' MPL_INIT ERROR ',LDABORT=LLABORT)
  ENDIF
ENDIF

CALL MPI_COMM_SIZE(MPI_COMM_WORLD,MPL_NUMPROC,IERROR)

IF(PRESENT(KPROCS)) THEN
  KPROCS=MPL_NUMPROC
ENDIF

ALLOCATE (MPL_IDS(MPL_NUMPROC))
DO IP=1,MPL_NUMPROC
  MPL_IDS(IP)=IP
ENDDO

MPL_COMM=MPI_COMM_WORLD
MPL_MBX_SIZE=1000000
#ifdef VPP
MPL_METHOD=JP_BLOCKING_STANDARD
#else
MPL_METHOD=JP_BLOCKING_BUFFERED
#endif
ICOMM=MPL_COMM
CALL MPI_COMM_RANK(ICOMM, IRANK, IERROR)
MPL_RANK=IRANK+1

RETURN
END SUBROUTINE MPL_INIT

END MODULE MPL_INIT_MOD
