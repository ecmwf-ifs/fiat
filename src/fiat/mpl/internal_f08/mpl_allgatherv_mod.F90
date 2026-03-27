! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_ALLGATHERV_MOD

!**** MPL_ALLGATHERV Send data to all processes

!     Purpose.
!     --------
!     Send a message to all processes from a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_ALLGATHERV

!        Input required arguments :
!        -------------------------
!           PSENDBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           PRECVBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KRECVCOUNTS-number of elements received from each process

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD
!                       or from that established as the default
!                       by an MPL communicator routine
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
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
!                       MPL_ALLGATHERV aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original:   2000-11-23
!        J.Hague:    2004-12-15 : Threadsafe
!        M.Hamrud:   2014-10-22 : Add nonblocking option
!        F.Vana:     2015-03-05 : Support for single precision
!        P.Gillies:  2018-05-30 : Add KSENDCOUNT argument, needed for zero length sends

!     ------------------------------------------------------------------

IMPLICIT NONE

INTERFACE MPL_ALLGATHERV

MODULE SUBROUTINE MPL_ALLGATHERV_REAL4(PSENDBUF,PRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
& KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM ,JPRM

IMPLICIT NONE
REAL(KIND=JPRM)            :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
REAL(KIND=JPRM)            :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KRECVCOUNTS(:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
!TARGET :: KRECVDISPL

END SUBROUTINE MPL_ALLGATHERV_REAL4

MODULE SUBROUTINE MPL_ALLGATHERV_REAL8(PSENDBUF,PRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
& KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM ,JPRM

IMPLICIT NONE
REAL(KIND=JPRD)            :: PSENDBUF(..)
IGNORE_DEVICE PSENDBUF
REAL(KIND=JPRD)            :: PRECVBUF(..)
IGNORE_DEVICE PRECVBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KRECVCOUNTS(:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
!TARGET :: KRECVDISPL

END SUBROUTINE MPL_ALLGATHERV_REAL8

MODULE SUBROUTINE MPL_ALLGATHERV_INT(KSENDBUF,KRECVBUF,KRECVCOUNTS,KSENDCOUNT,KRECVDISPL, &
& KMP_TYPE,KCOMM,KERROR,KREQUEST,CDSTRING)

USE EC_PARKIND, ONLY : JPRD, JPIM ,JPRM

IMPLICIT NONE
INTEGER(KIND=JPIM)         :: KSENDBUF(..)
IGNORE_DEVICE KSENDBUF
INTEGER(KIND=JPIM)         :: KRECVBUF(..)
IGNORE_DEVICE KRECVBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVCOUNTS(:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KSENDCOUNT
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KRECVDISPL(:),KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
!TARGET :: KRECVDISPL

END SUBROUTINE MPL_ALLGATHERV_INT

END INTERFACE

END MODULE MPL_ALLGATHERV_MOD
