! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_SCATTERV_MOD

!**** MPL_SCATTERV Scatter data from specific processor

!     Purpose.
!     --------
!     Scatter data from specific processor
!     The data may be REAL*8,or INTEGER, one dimensional array
!
!**   Interface.
!     ----------
!        CALL MPL_SCATTERV

!        Input required arguments :
!        -------------------------
!           PRECVBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           PSENDBUF -  buffer containing message
!                       (required from kroot)
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KSENDCOUNTS-number of elements to be sent to each process
!                       (required from kroot processor)

!        Input optional arguments :
!        -------------------------
!           KROOT    -  rank of sending processor (default 1)
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD
!                       or from that established as the default
!                       by an MPL communicator routine
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           KSENDDISPL -displacements in PRECVBUF at which to place
!                       the incoming data
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KERROR   -  return error code.     If not supplied,
!                       MPL_SCATTERV aborts when an error is detected.
!     Author.
!     -------
!        Y. Tremolet, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 02-03-13
!        M.Hamrud     : 2014-10-22 : Add nonblocking option
!      F. Vana  05-Mar-2015  Support for single precision

! --- *NOT* THREAD SAFE YET ---

!     ----------------------------------------------------------------

IMPLICIT NONE

! ------------------------------------------------------------------

INTERFACE MPL_SCATTERV

MODULE SUBROUTINE MPL_SCATTERV_REAL8(PRECVBUF,KROOT,PSENDBUF,KSENDCOUNTS,KSENDDISPL,&
  & KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM, JPRM
#ifdef USE_8_BYTE_WORDS
USE MPI4TO8, ONLY : &
  MPI_SCATTERV => MPI_SCATTERV8
#endif

REAL(KIND=JPRD), INTENT(OUT) :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM), INTENT(IN) :: KROOT
REAL(KIND=JPRD), INTENT(IN),OPTIONAL  :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KSENDCOUNTS(:)
INTEGER(KIND=JPIM), INTENT(IN),TARGET,OPTIONAL :: KSENDDISPL(:)
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SCATTERV_REAL8
! ------------------------------------------------------------------
MODULE SUBROUTINE MPL_SCATTERV_REAL4(PRECVBUF,KROOT,PSENDBUF,KSENDCOUNTS,KSENDDISPL,&
  &  KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM, JPRM
#ifdef USE_8_BYTE_WORDS
USE MPI4TO8, ONLY : &
  MPI_SCATTERV => MPI_SCATTERV8
#endif

REAL(KIND=JPRM), INTENT(OUT) :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM), INTENT(IN) :: KROOT
REAL(KIND=JPRM), INTENT(IN),OPTIONAL  :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KSENDCOUNTS(:)
INTEGER(KIND=JPIM), INTENT(IN),TARGET,OPTIONAL :: KSENDDISPL(:)
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SCATTERV_REAL4


MODULE SUBROUTINE MPL_SCATTERV_INTEGER(KRECVBUF,KROOT,KSENDBUF,KSENDCOUNTS,&
  & KSENDDISPL,KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM, JPRM
#ifdef USE_8_BYTE_WORDS
USE MPI4TO8, ONLY : &
  MPI_SCATTERV => MPI_SCATTERV8
#endif

INTEGER(KIND=JPIM), INTENT(OUT) :: KRECVBUF(..)
IGNORE_DEVICE KRECVBUF
INTEGER(KIND=JPIM), INTENT(IN) :: KROOT
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KSENDBUF(..)
IGNORE_DEVICE KSENDBUF
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KSENDCOUNTS(:)
INTEGER(KIND=JPIM), INTENT(IN),TARGET,OPTIONAL :: KSENDDISPL(:)
INTEGER(KIND=JPIM), INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SCATTERV_INTEGER

END INTERFACE
! ------------------------------------------------------------------

END MODULE MPL_SCATTERV_MOD
