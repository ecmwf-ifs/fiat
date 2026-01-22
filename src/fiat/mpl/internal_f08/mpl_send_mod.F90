! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_SEND_MOD

!**** MPL_SEND Send a message

!     Purpose.
!     --------
!     Send a message to a named source from a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_SEND

!        Input required arguments :
!        -------------------------
!           PBUF     -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KTAG     -  message tag
!           KDEST    -  rank of process to receive the message

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
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
!                       MPL_SEND aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01
!        P. Marguinaud : 01-Jan-2011 : Do not raise an error when
!                        the numproc is beyond model limits and KCOMM is passed
!                        as argument
!      F. Vana  05-Mar-2015  Support for single precision
!     ------------------------------------------------------------------

IMPLICIT NONE

INTERFACE MPL_SEND

MODULE SUBROUTINE MPL_SEND_REAL4(PBUF,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
! real_m,intent(in) :: PBUF(:)
REAL(KIND=JPRM)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_REAL4  

MODULE SUBROUTINE MPL_SEND_REAL8(PBUF,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
! real_b,intent(in) :: PBUF(:)
REAL(KIND=JPRD)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_REAL8

MODULE SUBROUTINE MPL_SEND_INT(KBUF,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
INTEGER(KIND=JPIM)           :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_INT

MODULE SUBROUTINE MPL_SEND_INT8(KBUF,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
INTEGER(KIND=JPIB)            :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_INT8

MODULE SUBROUTINE MPL_SEND_CHAR_SCALAR(CDCHAR,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
CHARACTER(LEN=*) :: CDCHAR
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_CHAR_SCALAR

MODULE SUBROUTINE MPL_SEND_CHAR(CDCHAR,KDEST,KTAG,KCOMM,KMP_TYPE,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
CHARACTER(LEN=*) :: CDCHAR(:)
INTEGER(KIND=JPIM),INTENT(IN) :: KDEST,KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KREQUEST,KERROR
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_SEND_CHAR

END INTERFACE

END MODULE MPL_SEND_MOD
