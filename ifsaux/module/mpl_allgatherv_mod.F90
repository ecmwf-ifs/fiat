MODULE MPL_ALLGATHERV_MOD

!   Purpose.
!   --------
!     Gather array on all processors.
!
!     This new version is necessary because MPL_ALLGATHER (which
!     in fact already corresponds to MPI_ALLGATHERV) is hard wired
!     to use ALL processors despite the communicator in the interface.
!
!   Author.
!   -------
!     Yannick Tremolet
!
!   Modifications.
!   --------------
!     Original   02-03-19
!
! ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_MPIF
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PRIVATE
PUBLIC MPL_ALLGATHERV

LOGICAL :: LLABORT=.true.

CONTAINS

! ------------------------------------------------------------------

SUBROUTINE MPL_ALLGATHERV(PSENDBUF,KSENDLEN,PRECVBUF,KRECVCOUNTS,KCOMM)

REAL_B, INTENT(IN)  :: PSENDBUF(:)
REAL_B, INTENT(OUT) :: PRECVBUF(:)
INTEGER_M,INTENT(IN) :: KSENDLEN
INTEGER_M,INTENT(IN) :: KRECVCOUNTS(:)
INTEGER_M,INTENT(IN) :: KCOMM

INTEGER_M :: ierr, isize, ii
INTEGER_M, ALLOCATABLE :: IRECVDISPL(:)

CALL MPI_COMM_SIZE(KCOMM, isize, ierr)
IF (SIZE(KRECVCOUNTS)/=isize) &
    & CALL ABOR1('MPL_ALLGATHERV: KRECVCOUNTS wrong size')
ALLOCATE(IRECVDISPL(isize))
IRECVDISPL(1)=0
DO ii=2,isize
  IRECVDISPL(ii)=IRECVDISPL(ii-1)+KRECVCOUNTS(ii-1)
ENDDO

ierr=0
CALL MPI_ALLGATHERV(PSENDBUF,KSENDLEN,MPI_REAL8,PRECVBUF,KRECVCOUNTS, &
                 &  IRECVDISPL,MPI_REAL8,KCOMM,ierr)

IF (ierr/=0) THEN
  CALL MPL_MESSAGE(ierr,'MPL_ALLGATHERV',LDABORT=LLABORT)
ENDIF
DEALLOCATE(IRECVDISPL)

END SUBROUTINE MPL_ALLGATHERV

! ------------------------------------------------------------------

END MODULE MPL_ALLGATHERV_MOD
