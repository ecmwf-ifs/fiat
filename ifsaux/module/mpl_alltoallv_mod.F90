MODULE MPL_ALLTOALLV_MOD

!     Purpose.
!     --------
!       Use MPI_ALLTOALLV in IFS.

!     Author.
!     -------
!       Y. Tremolet

!     Modifications.
!     --------------
!       Original: 02-03-21
! ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE
PRIVATE
PUBLIC MPL_ALLTOALLV

LOGICAL :: LLABORT=.true.

CONTAINS
! ------------------------------------------------------------------

SUBROUTINE MPL_ALLTOALLV(PSEND,KSEND,KDISPS,PRECV,KRECV,KDISPR,KCOMM)

IMPLICIT NONE
INTEGER_M, INTENT(IN) :: ksend(:), kdisps(:), krecv(:), kdispr(:), kcomm
REAL_B, INTENT(IN)  :: PSEND(:)
REAL_B, INTENT(OUT) :: PRECV(:)

INTEGER_M :: ierr

ierr=0
CALL mpi_alltoallv(psend,ksend,kdisps,MPI_REAL8, &
                 & precv,krecv,kdispr,MPI_REAL8,kcomm,ierr)
IF (ierr/=0) CALL mpl_message(ierr,'Error in MPL_ALLTOALLV',LDABORT=LLABORT)

END SUBROUTINE MPL_ALLTOALLV

! ------------------------------------------------------------------

END MODULE MPL_ALLTOALLV_MOD
