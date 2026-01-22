! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_RECV_MOD

!**** MPL_RECV Receive a message

!     Purpose.
!     --------
!     Receive a message from a named source into a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or REAL or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_RECV

!        Input required arguments :
!        -------------------------
!           PBUF     -  buffer to receive the message
!                       (can be type REAL*4, REAL*8 or INTEGER)

!        Input optional arguments :
!        -------------------------
!           KTAG     -  message tag
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           KSOURCE  -  rank of process sending the message
!                       default is MPI_ANY_SOURCE
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KFROM    -  rank of process sending the message
!           KRECVTAG -  tag of received message
!           KOUNT    -  number of items in received message
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_RECV aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01
!      F. Vana  05-Mar-2015  Support for single precision

!     ------------------------------------------------------------------


IMPLICIT NONE

INTERFACE MPL_RECV

MODULE SUBROUTINE MPL_RECV_REAL4(PBUF,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,&
 &KOUNT,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

REAL(KIND=JPRM)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_RECV_REAL4  

!     ------------------------------------------------------------------

MODULE SUBROUTINE MPL_RECV_REAL8(PBUF,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,&
 &KOUNT,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

! real_b,intent(in) :: PBUF(:)
REAL(KIND=JPRD)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_RECV_REAL8

!     ------------------------------------------------------------------

MODULE SUBROUTINE MPL_RECV_INT(KBUF,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,KOUNT,&
 &KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

INTEGER(KIND=JPIM)           :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_RECV_INT

MODULE SUBROUTINE MPL_RECV_INT8(KBUF,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,KOUNT,&
 &KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

INTEGER(KIND=JPIB)           :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_RECV_INT8

!     ------------------------------------------------------------------

MODULE SUBROUTINE MPL_RECV_CHAR_SCALAR(CDCHAR,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,&
 &KOUNT,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

CHARACTER(LEN=*) :: CDCHAR
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING


END SUBROUTINE MPL_RECV_CHAR_SCALAR

MODULE SUBROUTINE MPL_RECV_CHAR(CDCHAR,KSOURCE,KTAG,KCOMM,KFROM,KRECVTAG,&
 &KOUNT,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND,      ONLY : JPRD, JPIB, JPIM, JPRM

CHARACTER(LEN=*) :: CDCHAR(:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KTAG,KCOMM,KMP_TYPE,KSOURCE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_RECV_CHAR

END INTERFACE
!     ------------------------------------------------------------------

END MODULE MPL_RECV_MOD
