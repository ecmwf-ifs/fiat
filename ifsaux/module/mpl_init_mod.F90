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

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD
USE MPL_BUFFER_METHOD_MOD
USE MPL_TOUR_TABLE_MOD

IMPLICIT NONE
INTEGER,EXTERNAL :: MPL_MYRANK


PUBLIC MPL_INIT
PRIVATE

INTEGER_M :: IERROR,IP,ICOMM,IRANK

CONTAINS 

SUBROUTINE MPL_INIT(KOUTPUT,KUNIT,KERROR,KPROCS)


INTEGER_M,INTENT(IN),OPTIONAL :: KOUTPUT,KUNIT
INTEGER_M,INTENT(OUT),OPTIONAL :: KERROR,KPROCS
LOGICAL                      :: LLABORT=.TRUE.
CHARACTER(LEN=12) :: CL_MBX_SIZE

IF(MPL_NUMPROC /= -1) THEN
  CALL MPL_MESSAGE(CDMESSAGE=' MPL_INIT CALLED MULTIPLE TIMES ')
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
#ifdef VPP
MPL_METHOD=JP_BLOCKING_STANDARD
MPL_MBX_SIZE=4000000
CL_MBX_SIZE=' '
CALL GETENV('VPP_MBX_SIZE',CL_MBX_SIZE)
IF(CL_MBX_SIZE == ' ') THEN
  CALL GETENV('MPL_MBX_SIZE',CL_MBX_SIZE)
ENDIF
IF(CL_MBX_SIZE /= ' ') THEN
  READ(CL_MBX_SIZE,*) MPL_MBX_SIZE
ENDIF
WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_STANDARD'
WRITE(MPL_UNIT,'(A,I12)')'MPL_INIT : MAILBOX SIZE=',MPL_MBX_SIZE
#else
MPL_METHOD=JP_BLOCKING_BUFFERED
MPL_MBX_SIZE=1000000
CL_MBX_SIZE=' '
CALL GETENV('MPL_MBX_SIZE',CL_MBX_SIZE)
IF (CL_MBX_SIZE /= ' ') THEN
  READ(CL_MBX_SIZE,*) MPL_MBX_SIZE
ENDIF
WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_BUFFERED'
WRITE(MPL_UNIT,'(A,I12)')'MPL_INIT : MAILBOX SIZE=',MPL_MBX_SIZE

CALL MPL_BUFFER_METHOD(kmp_type=MPL_METHOD,kmbx_size=MPL_MBX_SIZE)
#endif
ICOMM=MPL_COMM
CALL MPI_COMM_RANK(ICOMM, IRANK, IERROR)
MPL_RANK=IRANK+1

ALLOCATE(MPL_OPPONENT(MPL_NUMPROC+1))
CALL MPL_TOUR_TABLE(MPL_OPPONENT)


RETURN
END SUBROUTINE MPL_INIT

END MODULE MPL_INIT_MOD
