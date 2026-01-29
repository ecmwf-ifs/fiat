! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_BROADCAST_MOD

!**** MPL_BROADCAST Message broadcast

!     Purpose.
!     --------
!     Broadcasts a message from the process with rank root
!     to all processes in the group.

!**   Interface.
!     ----------
!        CALL MPL_BROADCAST

!        Input required arguments :
!        -------------------------
!           PBUF     -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KTAG     -  message tag

!        Input optional arguments :
!        -------------------------
!           KROOT    -  number of root process (default=1)
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided
!                   

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_BROADCAST aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud, S.Saarinen    ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01
!        P.Marguinaud : 2012-04-13 : Cleaning & refactor PREAMB1
!        P.Marguinaud : 2012-09-11 : Add MPL_BROADCAST_LOGICAL1
!        M.Hamrud     : 2014-10-22 : Add nonblocking option
!      F. Vana  05-Mar-2015  Support for single precision

!     ------------------------------------------------------------------

IMPLICIT NONE

LOGICAL :: LLABORT=.TRUE.

INTERFACE MPL_BROADCAST

MODULE SUBROUTINE MPL_BROADCAST_REAL4(PBUF,KTAG,KROOT,KMP_TYPE,&
                               KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
REAL(KIND=JPRM)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING

END SUBROUTINE MPL_BROADCAST_REAL4

MODULE SUBROUTINE MPL_BROADCAST_REAL8(PBUF,KTAG,KROOT,KMP_TYPE,&
                               KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
REAL(KIND=JPRD)            :: PBUF(..)
IGNORE_DEVICE PBUF
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_REAL8

MODULE SUBROUTINE MPL_BROADCAST_LONG(KBUF,KTAG,KROOT,KMP_TYPE,&
                             KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
INTEGER(KIND=JPIB)                      :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN)           :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL  :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_LONG



MODULE SUBROUTINE MPL_BROADCAST_INT(KBUF,KTAG,KROOT,KMP_TYPE,&
                             KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
INTEGER(KIND=JPIM)           :: KBUF(..)
IGNORE_DEVICE KBUF
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_INT

MODULE SUBROUTINE MPL_BROADCAST_CHAR_SCALAR(CDBUF,KTAG,KROOT,KMP_TYPE,&
                                    KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
CHARACTER(LEN=*)                       :: CDBUF
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_CHAR_SCALAR

MODULE SUBROUTINE MPL_BROADCAST_CHAR1(CDBUF,KTAG,KROOT,KMP_TYPE,&
                               KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
CHARACTER(LEN=*)                       :: CDBUF (:)
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_CHAR1

MODULE SUBROUTINE MPL_BROADCAST_LOGICAL_SCALAR(LDBUF,KTAG,KROOT,KMP_TYPE,&
                                    KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
LOGICAL                                :: LDBUF
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_LOGICAL_SCALAR

MODULE SUBROUTINE MPL_BROADCAST_LOGICAL1(LDBUF,KTAG,KROOT,KMP_TYPE,&
                                  KCOMM,KERROR,KREQUEST,CDSTRING)
USE EC_PARKIND, ONLY : JPRD, JPIM, JPIB, JPRM
LOGICAL                                :: LDBUF (:)
INTEGER(KIND=JPIM),INTENT(IN)          :: KTAG
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KROOT,KCOMM,KMP_TYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KREQUEST
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CDSTRING
END SUBROUTINE MPL_BROADCAST_LOGICAL1

END INTERFACE

END MODULE MPL_BROADCAST_MOD
