MODULE NAMELIST_MOD

! This module contains namelist utilities, especially subroutines/function to
! position on the requested namelist "block" to read variables.
!
! To the legacy POSNAM and POSNAME subroutines from IAL (IFS-ARPEGE-LAM),
! a POSNAMEF function is added to enable a shorter syntax:
! IF (POSNAMEF(KULNAM, CDNAML, ...) == 0) READ(KULNAM, NAML)

IMPLICIT NONE

PUBLIC :: POSNAME, POSNAM, POSNAMEF

CONTAINS
!     ------------------------------------------------------------------
SUBROUTINE POSNAME(KULNAM,CDNAML,KSTAT,LDNOREWIND)

!**** *POSNAME* - position namelist file for reading; return error code
!                 if namelist is not found

!     Purpose.
!     --------
!     To position namelist file at correct place for reading
!     namelist CDNAML. Replaces use of Cray specific ability
!     to skip to the correct namelist.

!**   Interface.
!     ----------
!        *CALL* *POSNAME*(..)

!        Explicit arguments :     KULNAM - file unit number (input)
!        --------------------     CDNAML - namelist name    (input)
!                                 KSTAT  - non-zero if namelist not found
!                                          1 = namelist not found

!     Author.
!     -------
!      P.Marguinaud 22-Nov-2010

!     Modifications.
!     --------------
!      R.Hogan      20-Jan-2022  Added no-rewind optional argument

!     --------------------------------------------------------------

USE EC_PARKIND ,ONLY : JPIM
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KULNAM
CHARACTER(LEN=*)  ,INTENT(IN)    :: CDNAML
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTAT
LOGICAL,OPTIONAL,  INTENT(IN)    :: LDNOREWIND

#include "abor1.intfb.h"


CHARACTER (LEN = 40) ::  CLINE
CHARACTER (LEN =  1) ::  CLTEST

INTEGER(KIND=JPIM) :: ILEN, IND1, ISTATUS, ISCAN
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE

!      -----------------------------------------------------------

!*       1.    POSITION FILE
!              -------------

IF (LHOOK) CALL DR_HOOK('POSNAME',0,ZHOOK_HANDLE)

KSTAT = 0

CLINE='                                        '
! Rewind by default, but not if LDNOREWIND is present and TRUE. This
! is useful for reading an array of structures of arbitrary length
! from a namelist, by repeated use of the same group name.
IF (.NOT. PRESENT(LDNOREWIND)) THEN
  REWIND(KULNAM)
ELSEIF (.NOT. LDNOREWIND) THEN
  REWIND(KULNAM)
ENDIF

ILEN=LEN(CDNAML)
ISTATUS=0
ISCAN=0
DO WHILE (ISTATUS==0 .AND. ISCAN==0)
  READ(KULNAM,'(A)',IOSTAT=ISTATUS) CLINE
  SELECT CASE (ISTATUS)
  CASE (:-1)
    KSTAT=1
    ISCAN=-1
  CASE (0)
    IF (INDEX(CLINE(1:10),'&') == 0) THEN
      ISCAN=0
    ELSE
      IND1=INDEX(CLINE,'&'//CDNAML)
      IF (IND1 == 0) THEN
        ISCAN=0
      ELSE
        CLTEST=CLINE(IND1+ILEN+1:IND1+ILEN+1)
        IF (   (LGE(CLTEST,'0').AND.LLE(CLTEST,'9')) &
         & .OR.(LGE(CLTEST,'A').AND.LLE(CLTEST,'Z')) ) THEN
          ISCAN=0
        ELSE
          ISCAN=1
        ENDIF
      ENDIF
    ENDIF
  CASE (1:)
    CALL ABOR1 ('POSNAME: AN ERROR OCCURRED WHILE READING THE NAMELIST')
  END SELECT
ENDDO
BACKSPACE(KULNAM)

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('POSNAME',1,ZHOOK_HANDLE)
END SUBROUTINE POSNAME


FUNCTION POSNAMEF(KULNAM, CDNAML, LDNOREWIND, LDFATAL, LDVERBOSE, KULOUT) RESULT(ISTAT)
!**** *POSNAMEF* - function to position namelist file for reading and return error code
!                  if namelist is not found

!     Purpose.
!     --------
!     To position namelist file at correct place for reading namelist CDNAML.

!**   Interface.
!     ----------
!        IF (POSNAMEF(KULNAM, CDNAML, ...) == 0) READ(KULNAM, NAML)

!        Explicit arguments :     KULNAM - file unit number (input)
!        --------------------     CDNAML - namelist name    (input)
!                                 LDNOREWIND - no rewind; This is useful for
!                                   reading an array of structures of arbitrary
!                                   length from a namelist, by repeated use of
!                                   the same group name.
!                                 LDFATAL - to call ABOR1 in case CDNAML not present in file
!                                   The default value is .TRUE. or can be defined via env var
!                                   POSNAMEF_DEFAULT_FATAL=0
!                                 LDVERBOSE - verbosity
!                                 KULOUT - output unit for verbosity

USE EC_PARKIND, ONLY : JPIM
USE EC_LUN, ONLY : NULOUT
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN) :: KULNAM
CHARACTER(LEN=*),   INTENT(IN) :: CDNAML
LOGICAL,OPTIONAL,   INTENT(IN) :: LDNOREWIND
LOGICAL,OPTIONAL,   INTENT(IN) :: LDFATAL
LOGICAL,OPTIONAL,   INTENT(IN) :: LDVERBOSE
INTEGER(KIND=JPIM),OPTIONAL, INTENT(IN) :: KULOUT

INTEGER(KIND=JPIM) :: ISTAT
CHARACTER(LEN=256) :: CLFILE
LOGICAL :: LLNOREWIND
LOGICAL :: LLFATAL
CHARACTER(LEN=256) :: CLFATAL
LOGICAL :: LLVERBOSE
INTEGER :: ILULOUT

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "abor1.intfb.h"

IF (LHOOK) CALL DR_HOOK('POSNAMEF',0,ZHOOK_HANDLE)
! defaults
LLNOREWIND = .FALSE.
CALL GET_ENVIRONMENT_VARIABLE('POSNAMEF_DEFAULT_FATAL',CLFATAL)
IF (TRIM(CLFATAL) == '' .OR. TRIM(CLFATAL) == '1') THEN
  LLFATAL = .TRUE.
ELSE
  LLFATAL = .FALSE.
ENDIF
LLVERBOSE = .TRUE.
ILULOUT = NULOUT
! optional arguments
IF (PRESENT(LDNOREWIND)) LLNOREWIND = LDNOREWIND
IF (PRESENT(LDFATAL)) LLFATAL = LDFATAL
IF (PRESENT(LDVERBOSE)) LLVERBOSE = LDVERBOSE
IF (PRESENT(KULOUT)) ILULOUT = KULOUT

CLFILE=""

INQUIRE(KULNAM,NAME=CLFILE)
IF (CLFILE == "") THEN
  ! No file is explicitely connected to this logical unit number yet
  ! (because the file is not yet opened)
  ! then give it its standard name "fort.KULNAM" :
  IF (KULNAM <= 9) THEN
    WRITE(CLFILE,'(''fort.'',I1)') KULNAM
  ELSE
    WRITE(CLFILE,'(''fort.'',I2)') KULNAM
  ENDIF
ENDIF
IF (LLVERBOSE) WRITE(ILULOUT,"('Reading namelist ',A,' from ',A)") CDNAML,TRIM(CLFILE)

CALL POSNAME (KULNAM, CDNAML, ISTAT, LDNOREWIND=LLNOREWIND)

SELECT CASE (ISTAT)
  CASE (0)
  CASE (1)
    IF (LLFATAL) THEN
      CALL ABOR1 ('POSNAM:CANNOT LOCATE '//CDNAML//' ')
    ENDIF
  CASE DEFAULT
    CALL ABOR1 ('POSNAM:READ ERROR IN NAMELIST FILE')
END SELECT

IF (LHOOK) CALL DR_HOOK('POSNAMEF',1,ZHOOK_HANDLE)
END FUNCTION POSNAMEF


SUBROUTINE POSNAM(KULNAM, CDNAML)

!**** *POSNAM* - position namelist file for reading

!     Purpose.
!     --------
!     To position namelist file at correct place for reading
!     namelist CDNAML. Replaces use of Cray specific ability
!     to skip to the correct namelist.

!**   Interface.
!     ----------
!        *CALL* *POSNAM*(..)

!        Explicit arguments :     KULNAM - file unit number (input)
!        --------------------     CDNAML - namelist name    (input)

!        Implicit arguments :     None
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.   None
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Mats Hamrud *ECMWF*

!     Modifications.
!     --------------
!        Original : 93-06-22
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        M.Hamrud      01-Dec-2003 CY28R1 Cleaning
!      R. El Khatib 04-08-10 Apply norms + proper abort if namelist is missing
!      P. Marguinaud   Proxy to POSNAME
!      H Petithomme Sept 2023: some cleaning
!      R. El Khatib 11-Feb-2025 Fix uninitialized filename and arbitrary choice "fort.4"
!      A.Mary       22-05-2025: move contents to POSNAMEF
!     --------------------------------------------------------------

USE EC_PARKIND, ONLY : JPIM
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN) :: KULNAM
CHARACTER(LEN=*)  ,INTENT(IN) :: CDNAML

INTEGER(KIND=JPIM) :: ISTAT
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('POSNAM',0,ZHOOK_HANDLE)
ISTAT = POSNAMEF(KULNAM, CDNAML, &
               & LDNOREWIND=.FALSE., &
               & LDFATAL=.TRUE., &
               & LDVERBOSE=.TRUE.)
IF (LHOOK) CALL DR_HOOK('POSNAM',1,ZHOOK_HANDLE)
END SUBROUTINE POSNAM

END MODULE NAMELIST_MOD
