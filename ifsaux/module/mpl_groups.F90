MODULE MPL_GROUPS

!     Purpose.
!     --------
!       Use MPI groups for easier to read code (and more efficient
!       communications, at least on IBM).

!     Author.
!     -------
!       Y. Tremolet

!     Modifications.
!     --------------
!       Original: 02-03-13
! ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE
PRIVATE
PUBLIC MPL_COMM_GRID, MPL_ALL_LEVS_COMM, MPL_ALL_MS_COMM, &
     & MPL_GROUPS_CREATE, MPL_CART_RANK, MPL_CART_COORDS

INTEGER_M :: MPL_COMM_GRID, MPL_ALL_LEVS_COMM, MPL_ALL_MS_COMM, &
           & MPL_GP_GRID

CONTAINS
! ------------------------------------------------------------------

SUBROUTINE MPL_GROUPS_CREATE(kprocw, kprocv)

IMPLICIT NONE
INTEGER_M, INTENT(IN) :: kprocw, kprocv

INTEGER_M :: idims(2), ierr
LOGICAL :: ltorus(2), ldims(2), lreorder

idims(1)=kprocw
idims(2)=kprocv
ltorus(1)=.false.
ltorus(2)=.false.
lreorder=.false.

CALL MPI_CART_CREATE(MPL_COMM, 2, idims, ltorus, lreorder, &
                   & MPL_COMM_GRID, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_GROUPS_CREATE: MPI_CART_CREATE')

CALL mpi_comm_group(MPL_COMM_GRID, MPL_GP_GRID, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_GROUPS_CREATE: mpi_comm_group')

! Group all levels for same Ms
! ----------------------------
ldims(1)=.false.
ldims(2)=.true.
CALL mpi_cart_sub(MPL_COMM_GRID, ldims, MPL_ALL_LEVS_COMM, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_GROUPS_CREATE: mpi_cart_sub 1')

! Group all Ms for same levels
! ----------------------------
ldims(1)=.true.
ldims(2)=.false.
CALL mpi_cart_sub(MPL_COMM_GRID, ldims, MPL_ALL_MS_COMM, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_GROUPS_CREATE: mpi_cart_sub 2')

END SUBROUTINE MPL_GROUPS_CREATE

! ------------------------------------------------------------------

FUNCTION MPL_CART_RANK(kprocw, kprocv)

IMPLICIT NONE
INTEGER_M, INTENT(IN)  :: kprocw, kprocv
INTEGER_M :: MPL_CART_RANK

INTEGER_M :: idims(2), iproc, ierr

idims(1)=kprocw-1
idims(2)=kprocv-1

CALL mpi_cart_rank(MPL_COMM_GRID, idims, iproc, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_CART_RANK: mpi_cart_rank')

MPL_CART_RANK=iproc+1

END FUNCTION MPL_CART_RANK

! ------------------------------------------------------------------

SUBROUTINE MPL_CART_COORDS(kproc, kprocw, kprocv)

IMPLICIT NONE
INTEGER_M, INTENT(IN)   :: kproc
INTEGER_M, INTENT(OUT)  :: kprocw, kprocv

INTEGER_M :: idims(2), iproc, ierr

iproc=kproc-1

CALL mpi_cart_coords(MPL_COMM_GRID, iproc, 2, idims, ierr)
IF (ierr/=0) CALL mpl_message(ierr,'MPL_CART_COORDS: mpi_cart_coords')

kprocw=idims(1)+1
kprocv=idims(2)+1

END SUBROUTINE MPL_CART_COORDS

! ------------------------------------------------------------------

END MODULE MPL_GROUPS
