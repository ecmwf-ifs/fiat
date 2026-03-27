! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_ALLTOALLV_MOD

!**** MPL_ALLTOALLV - Everyone sends different data to everyone

!     Purpose.
!     --------
!     Interface to MPI_ALLTOALLV

!     The data may be REAL*8,or INTEGER

!**   Interface.
!     ----------
!        CALL MPL_ALLTOALLV

!        Input required arguments :
!        -------------------------
!           PSENDBUF -  buffer containing message
!                       (can be type  REAL*8 or INTEGER)
!           PRECVBUF -  buffer containing message
!                       (can be type REAL*8 or INTEGER)
!           KRECVCOUNTS-number of elements received from each process
!           KSENDCOUNTS-number of elements to be sent to each process

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD
!                       or from that established as the default
!                       by an MPL communicator routine
!           KRECVDISPL -displacements in PRECVBUF at which to place
!                       the incoming data
!           KSENDDISPL -displacements in PSENDBUF from which to send
!                       the data
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KERROR   -  return error code.     If not supplied,
!                       MPL_ALLTOALLV aborts when an error is detected.

!     Author.
!     -------
!       Y. Tremolet

!     Modifications.
!     --------------
!       Original: 02-03-21
!       Modified : 25-09-02 M.Hamrud - generalize
!      F. Vana  05-Mar-2015  Support for single precision
! ------------------------------------------------------------------

   IMPLICIT NONE

   INTERFACE MPL_ALLTOALLV

! ------------------------------------------------------------------

   MODULE SUBROUTINE MPL_ALLTOALLV_REAL8(PSENDBUF,KSENDCOUNTS,PRECVBUF,KRECVCOUNTS,&
   & KSENDDISPL,KRECVDISPL,KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

      USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

      IMPLICIT NONE
      REAL(KIND=JPRD),    INTENT(IN)  :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
      INTEGER(KIND=JPIM), INTENT(IN)  :: KSENDCOUNTS(:), KRECVCOUNTS(:)
      REAL(KIND=JPRD),    INTENT(OUT) :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
      INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KCOMM ,KMP_TYPE
      INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL, TARGET :: KSENDDISPL(:), KRECVDISPL(:)
      CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: CDSTRING
      INTEGER(KIND=JPIM), INTENT(OUT),OPTIONAL :: KREQUEST,KERROR

   END SUBROUTINE MPL_ALLTOALLV_REAL8

   MODULE SUBROUTINE MPL_ALLTOALLV_REAL4(PSENDBUF,KSENDCOUNTS,PRECVBUF,KRECVCOUNTS,&
   & KSENDDISPL,KRECVDISPL,KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

      USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

      IMPLICIT NONE
      INTEGER(KIND=JPIM), INTENT(IN)  :: KSENDCOUNTS(:), KRECVCOUNTS(:)
      REAL(KIND=JPRM),    INTENT(IN)  :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
      REAL(KIND=JPRM),    INTENT(OUT) :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
      INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL  :: KCOMM  ,KMP_TYPE
      INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL, TARGET  :: KSENDDISPL(:), KRECVDISPL(:)
      CHARACTER(LEN=*),   INTENT(IN),OPTIONAL  :: CDSTRING
      INTEGER(KIND=JPIM), INTENT(OUT),OPTIONAL :: KREQUEST,KERROR

   END SUBROUTINE MPL_ALLTOALLV_REAL4


   MODULE SUBROUTINE MPL_ALLTOALLV_INTEGER(KSENDBUF,KSENDCOUNTS,KRECVBUF,KRECVCOUNTS,&
   & KSENDDISPL,KRECVDISPL,KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

      USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

      IMPLICIT NONE
      INTEGER(KIND=JPIM), INTENT(IN)  :: KSENDBUF(..)
IGNORE_DEVICE KSENDBUF
      INTEGER(KIND=JPIM), INTENT(IN)  :: KSENDCOUNTS(:), KRECVCOUNTS(:)
      INTEGER(KIND=JPIM), INTENT(OUT) :: KRECVBUF(..)
IGNORE_DEVICE KRECVBUF
      INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KSENDDISPL(:), KRECVDISPL(:), KCOMM,KMP_TYPE
      CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: CDSTRING
      INTEGER(KIND=JPIM), INTENT(OUT),OPTIONAL :: KREQUEST,KERROR


   END SUBROUTINE MPL_ALLTOALLV_INTEGER

! ------------------------------------------------------------------
   END INTERFACE

END MODULE MPL_ALLTOALLV_MOD
