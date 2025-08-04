! (C) Copyright 2023- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_COMM_COMPARE_MOD

!**** *MPL_COMM_COMPARE_MOD*  - Compare two communicators

!     Author.
!     -------
!      Willem Deconinck *ECMWF*
!      Original : 31-08-2023

USE EC_PARKIND, ONLY : JPIM

USE MPL_MPIF, ONLY : MPI_COMM, MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR

IMPLICIT NONE

PRIVATE
PUBLIC :: MPL_COMM_COMPARE

CONTAINS

SUBROUTINE MPL_COMM_COMPARE (KCOMM1, KCOMM2, KRES, KERR, CDSTRING)

INTEGER (KIND=JPIM), INTENT (IN)  :: KCOMM1
INTEGER (KIND=JPIM), INTENT (IN)  :: KCOMM2
INTEGER (KIND=JPIM), INTENT (OUT) :: KRES
INTEGER (KIND=JPIM), INTENT (OUT) :: KERR
CHARACTER (LEN=*),   INTENT (IN), OPTIONAL :: CDSTRING
TYPE(MPI_COMM) :: ICOMM1, ICOMM2

ICOMM1%MPI_VAL = KCOMM1
ICOMM2%MPI_VAL = KCOMM2

CALL MPI_COMM_COMPARE (ICOMM1, ICOMM2, KRES, KERR)
if( KRES == MPI_IDENT ) THEN
    KRES = 0 ! contexts and groups are the same
ELSEIF (KRES == MPI_CONGRUENT) THEN
    KRES = 1 ! different contexts but identical groups
ELSEIF (KRES == MPI_SIMILAR) THEN
    KRES = 2 ! different contexts but similar groups
ELSE ! (KRES == MPI_UNEQUAL) THEN
    KRES = 3 ! otherwise
ENDIF

END SUBROUTINE MPL_COMM_COMPARE

END MODULE MPL_COMM_COMPARE_MOD
