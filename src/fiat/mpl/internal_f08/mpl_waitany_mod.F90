! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_WAITANY_MOD

!**** MPL_WAITANY Waits for completion of any request

!     Purpose.
!     --------
!     Returns control when any operation identified by the request
!     is completed.
!     Normally used in conjunction with non-blocking buffering type

!**   Interface.
!     ----------
!        CALL MPL_WAITANY

!        Input required arguments :
!        -------------------------
!           KREQUEST -  array or scalar containing
!                       Communication request(s)
!                       as provided by MPL_RECV or MPL_SEND

!        Input optional arguments :
!        -------------------------
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           KINDEX - index of received request

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_WAITANY aborts when an error is detected.
!     Author.
!     -------
!        R. El Khatib *Meteo-France*

!     Modifications.
!     --------------
!        Original: 02-Sep-2014

!     ------------------------------------------------------------------

USE EC_PARKIND, ONLY : JPIM ,JPRM, JPIB

USE MPL_MPIF,        ONLY : MPI_REQUEST, MPI_STATUS, MPI_UNDEFINED
USE MPL_DATA_MODULE, ONLY : MPL_NUMPROC
USE MPL_MESSAGE_MOD, ONLY : MPL_MESSAGE

IMPLICIT NONE

PRIVATE
PUBLIC MPL_WAITANY

CONTAINS 

SUBROUTINE MPL_WAITANY(KREQUEST,KINDEX,CDSTRING,KERROR)


#ifdef USE_8_BYTE_WORDS
  USE MPI4TO8, ONLY : &
    MPI_WAITANY => MPI_WAITANY8
#endif

INTEGER(KIND=JPIM),INTENT(INOUT)        :: KREQUEST(:)
INTEGER(KIND=JPIM),INTENT(OUT)          :: KINDEX
CHARACTER(LEN=*)  ,INTENT(IN), OPTIONAL :: CDSTRING
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR

TYPE(MPI_REQUEST)  :: IREQUEST_LOCAL(SIZE(KREQUEST))
INTEGER(KIND=JPIM) :: IWAITERR,IREQLEN,J
TYPE(MPI_STATUS)   :: IWAIT_STATUS
LOGICAL :: LLABORT

LLABORT=.TRUE.
IWAITERR=0
IREQUEST_LOCAL(:)%MPI_VAL=KREQUEST

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE( &
  & CDMESSAGE='MPL_WAITANY: MPL NOT INITIALISED ',LDABORT=LLABORT) 

KINDEX = MPI_UNDEFINED
IREQLEN=SIZE(KREQUEST)

#ifndef MPI1
CALL MPI_WAITANY(IREQLEN,IREQUEST_LOCAL,KINDEX,IWAIT_STATUS,IWAITERR)
#else
!CALL ABOR1('MPI_WAITANY not built with MPI2')
IWAITERR = MPI_ERR_UNKNOWN ! Initialized in case all requests already NULL (= logic err in code)
DO J=1,IREQLEN
  IF (IREQUEST_LOCAL(J) /= MPI_REQUEST_NULL) THEN
    CALL MPI_WAIT(IREQUEST_LOCAL(J),IWAIT_STATUS,IWAITERR)
    KINDEX = J
    EXIT
  ENDIF
ENDDO
#endif
KREQUEST(:)=IREQUEST_LOCAL(:)%MPI_VAL

IF(PRESENT(KERROR))THEN
  KERROR=IWAITERR
ELSE IF(IWAITERR /= 0) THEN
  CALL MPL_MESSAGE(CDMESSAGE='MPL_WAITANY_WAITING',CDSTRING=CDSTRING,KERROR=IWAITERR,LDABORT=LLABORT)
ENDIF

RETURN
END SUBROUTINE MPL_WAITANY


END MODULE MPL_WAITANY_MOD
