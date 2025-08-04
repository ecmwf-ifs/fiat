! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_MYGATHERV_MOD

USE EC_PARKIND  ,ONLY : JPRD, JPIM

USE MPL_MPIF,        ONLY : MPI_COMM, MPI_REAL8
USE MPL_MESSAGE_MOD, ONLY : MPL_MESSAGE

IMPLICIT NONE
PRIVATE
PUBLIC MPL_MYGATHERV

LOGICAL :: LLABORT=.TRUE.

CONTAINS

! ------------------------------------------------------------------
SUBROUTINE MPL_MYGATHERV(PSEND,KSEND,PRECV,KRECV,KDISPL,KROOT,KCOMM)


#ifdef USE_8_BYTE_WORDS
  USE MPI4TO8, ONLY : &
    MPI_GATHERV => MPI_GATHERV8
#endif


REAL(KIND=JPRD), INTENT(IN)  :: PSEND(:)
REAL(KIND=JPRD), INTENT(OUT) :: PRECV(:)
INTEGER(KIND=JPIM), INTENT(IN) :: KSEND, KRECV(:), KDISPL(:)
INTEGER(KIND=JPIM), INTENT(IN) :: KROOT, KCOMM
INTEGER(KIND=JPIM) :: IERR
TYPE(MPI_COMM)     :: KCOMM_LOCAL

KCOMM_LOCAL%MPI_VAL=KCOMM
CALL MPI_GATHERV(PSEND,KSEND,MPI_REAL8, &
               & PRECV,KRECV,KDISPL,MPI_REAL8,KROOT-1,KCOMM_LOCAL,IERR)

IF (IERR/=0) CALL MPL_MESSAGE(CDMESSAGE='MPL_MYGATHERV',KERROR=IERR,LDABORT=LLABORT)

END SUBROUTINE MPL_MYGATHERV
! ------------------------------------------------------------------

END MODULE MPL_MYGATHERV_MOD
