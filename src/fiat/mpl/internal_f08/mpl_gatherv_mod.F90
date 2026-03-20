! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_GATHERV_MOD

!**** MPL_GATHERV Gather data to specific processor

!     Purpose.
!     --------
!     Gather data to specific processor
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_GATHERV

!        Input required arguments :
!        -------------------------
!           PSENDBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           PRECVBUF -  buffer containing message (required from kroot)
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KRECVCOUNTS-number of elements received from each process
!                       (required from kroot processor)

!        Input optional arguments :
!        -------------------------
!           KROOT    -  rank of receiveing processor (default 1)
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
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
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KERROR   -  return error code.     If not supplied,
!                       MPL_GATHERV aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original:  2000-11-23
!        M.Hamrud:  2014-10-22 : Add nonblocking option
!        F.Vana:    2015-03-05 : Support for single precision
!        P.Gillies: 2018-06-25 : Add SENDCOUNT argument, needed for zero-length sends
! --- *NOT* THREAD SAFE YET ---

!     ------------------------------------------------------------------

IMPLICIT NONE

INTERFACE MPL_GATHERV

MODULE SUBROUTINE MPL_GATHERV_REAL4(PSENDBUF,KROOT,PRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
  & KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

REAL(KIND=JPRM),INTENT(IN) :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT
REAL(KIND=JPRM),INTENT(OUT),OPTIONAL  :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVCOUNTS(:),KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_GATHERV_REAL4

MODULE SUBROUTINE MPL_GATHERV_REAL8(PSENDBUF,KROOT,PRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
  & KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

REAL(KIND=JPRD)                           :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
INTEGER(KIND=JPIM),INTENT(IN),   OPTIONAL :: KRECVCOUNTS(:),KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),   OPTIONAL :: KROOT
REAL(KIND=JPRD),   INTENT(INOUT),OPTIONAL :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM),INTENT(IN),   OPTIONAL :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),  OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),  INTENT(IN),   OPTIONAL :: CDSTRING

END SUBROUTINE MPL_GATHERV_REAL8

MODULE SUBROUTINE MPL_GATHERV_INT(KSENDBUF,KROOT,KRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
  &  KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

INTEGER(KIND=JPIM),TARGET,INTENT(IN)           :: KSENDBUF(..)
IGNORE_DEVICE KSENDBUF
INTEGER(KIND=JPIM),       INTENT(IN), OPTIONAL :: KROOT
INTEGER(KIND=JPIM),TARGET,INTENT(OUT),OPTIONAL :: KRECVBUF(..)
IGNORE_DEVICE KRECVBUF
INTEGER(KIND=JPIM),       INTENT(IN), OPTIONAL :: KRECVCOUNTS(:)
INTEGER(KIND=JPIM),       INTENT(IN), OPTIONAL :: KSENDCOUNT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),TARGET,INTENT(IN), OPTIONAL :: KRECVDISPL(:)
INTEGER(KIND=JPIM),       INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),         INTENT(IN), OPTIONAL :: CDSTRING

END SUBROUTINE MPL_GATHERV_INT

MODULE SUBROUTINE MPL_GATHERV_CHAR_SCALAR(CSENDBUF,KROOT,CRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
  & KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM

CHARACTER(LEN=*)                        :: CSENDBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL  :: KRECVCOUNTS(:),KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL  :: KROOT
CHARACTER(LEN=*),OPTIONAL               :: CRECVBUF(:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL  :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL    :: CDSTRING

END SUBROUTINE MPL_GATHERV_CHAR_SCALAR

END INTERFACE

END MODULE MPL_GATHERV_MOD

