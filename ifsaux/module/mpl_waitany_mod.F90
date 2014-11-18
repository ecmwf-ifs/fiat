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

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPRM, JPIB

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PRIVATE
PUBLIC MPL_WAITANY

CONTAINS 

SUBROUTINE MPL_WAITANY(KREQUEST,KINDEX,CDSTRING,KERROR)


#ifdef USE_8_BYTE_WORDS
  Use mpi4to8, Only : &
    MPI_WAITANY => MPI_WAITANY8
#endif

INTEGER(KIND=JPIM),INTENT(IN)           :: KREQUEST(:)
INTEGER(KIND=JPIM),INTENT(OUT)          :: KINDEX
CHARACTER*(*)     ,INTENT(IN), OPTIONAL :: CDSTRING
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR

INTEGER(KIND=JPIM) :: IWAITERR,IREQLEN
INTEGER(KIND=JPIM) :: IWAIT_STATUS(MPI_STATUS_SIZE)
LOGICAL :: LLABORT
LLABORT=.TRUE.
IWAITERR=0

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE( &
  & CDMESSAGE='MPL_WAITANY: MPL NOT INITIALISED ',LDABORT=LLABORT) 

IREQLEN=SIZE(KREQUEST)
CALL MPI_WAITANY(IREQLEN,KREQUEST,KINDEX,IWAIT_STATUS,IWAITERR)

IF(PRESENT(KERROR))THEN
  KERROR=IWAITERR
ELSE IF(IWAITERR /= 0) THEN
  CALL MPL_MESSAGE(IWAITERR,'MPL_WAITANY_WAITING',CDSTRING,LDABORT=LLABORT)
ENDIF

RETURN
END SUBROUTINE MPL_WAITANY


END MODULE MPL_WAITANY_MOD
