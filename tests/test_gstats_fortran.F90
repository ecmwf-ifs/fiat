! (C) COPYRIGHT 2005- ECMWF.
!
! LICENSE: APACHE LICENSE VERSION 2.0
! SEE http://www.apache.org/licenses/LICENSE-2.0
! IN APPLYING THIS LICENCE, ECMWF DOES NOT WAIVE ANY PRIVILEGES
! AND DOES NOT SUBMIT TO ANY JURISDICTION.

PROGRAM TEST_GSTATS
    USE OML_MOD
    USE MPL_MODULE
    #ifdef WITH_FCKIT
    USE FCKIT_MODULE
    #endif
    IMPLICIT NONE
    
    INTEGER :: NPROC, MYPROC, KLEN
    REAL :: AVEARRAY(2)
    KLEN = 2

    CALL TEST_INIT()
    
    CALL GSTATS(1,0)
    CALL WORK_SECTION_1()
    CALL GSTATS(1,1)
    
    CALL GSTATS(2,0)
    CALL WORK_SECTION_2()
    CALL GSTATS(2,1)
    
    CALL GSTATS_PRINT(0,AVEARRAY, KLEN)

    CALL TEST_END()
    
    ! CHECK THAT THE CSV FILE EXISTS
    CALL CHECK_OUTPUT()
    
    CONTAINS
    
    ! ---------------------------------------------------------------------
    SUBROUTINE WORK_SECTION_1
      IMPLICIT NONE
      REAL :: X
      INTEGER :: I
      X = 0.0
      DO I = 1, 5000000
         X = X + SIN(REAL(I))
      END DO
    END SUBROUTINE WORK_SECTION_1
    ! ---------------------------------------------------------------------
    
    SUBROUTINE WORK_SECTION_2
      IMPLICIT NONE
      REAL :: X
      INTEGER :: I
      X = 1.0
      DO I = 1, 4000000
         X = X * 1.0000001
      END DO
    END SUBROUTINE WORK_SECTION_2
    ! ---------------------------------------------------------------------
    
    SUBROUTINE TEST_INIT
      USE EC_ENV_MOD, ONLY : EC_SETENV
    
      INTEGER :: KPROC, KMYPROC, KPRCIDS
      LOGICAL :: LDSTATS, LDSTATSCPU, LDSYNCSTATS
      LOGICAL :: LDDETAILED_STATS, LDBARRIER_STATS, LDBARRIER_STATS2
      LOGICAL :: LDSTATS_OMP, LDSTATS_COMMS, LDSTATS_MEM
      LOGICAL :: LDSTATS_ALLOC, LDTRACE_STATS, LDXML_STATS
      INTEGER :: KSTATS_MEM, KTRACE_STATS, KPRNT_STATS
    
      ! INITIALIZE ENVIRONMENT
      IF ( MPL() ) THEN
         CALL MPL_INIT(LDINFO=.TRUE.)
         NPROC = MPL_NUMPROC
         MYPROC = MPL_RANK
    #ifdef WITH_FCKIT
      ELSEIF ( FCKIT() ) THEN
         CALL FCKIT_MAIN%INIT()
         NPROC = FCKIT_MPI%SIZE()
         MYPROC = FCKIT_MPI%RANK() + 1
    #else
      ELSE
         NPROC = 1
         MYPROC = 1
    #endif
      END IF
    
      ! ENABLE DETAILED GSTATS → CSV OUTPUT
      LDSTATS          = .TRUE.
      LDSTATSCPU       = .FALSE.
      LDSYNCSTATS      = .FALSE.
      LDDETAILED_STATS = .TRUE.
      LDBARRIER_STATS  = .FALSE.
      LDBARRIER_STATS2 = .FALSE.
      LDSTATS_OMP      = .FALSE.
      LDSTATS_COMMS    = .FALSE.
      LDSTATS_MEM      = .FALSE.
      KSTATS_MEM       = 0
      LDSTATS_ALLOC    = .FALSE.
      LDTRACE_STATS    = .FALSE.
      KTRACE_STATS     = 0
      KPRNT_STATS      = NPROC
      LDXML_STATS      = .FALSE.
    
      KPROC   = NPROC
      KMYPROC = MYPROC
      KPRCIDS = 0
    
      CALL GSTATS_SETUP( KPROC, KMYPROC, KPRCIDS, &
           LDSTATS, LDSTATSCPU, LDSYNCSTATS, LDDETAILED_STATS, &
           LDBARRIER_STATS, LDBARRIER_STATS2, &
           LDSTATS_OMP, LDSTATS_COMMS, LDSTATS_MEM, KSTATS_MEM, LDSTATS_ALLOC, &
           LDTRACE_STATS, KTRACE_STATS, KPRNT_STATS, LDXML_STATS )
      CALL GSTATS_PSUT
      CALL GSTATS_LABEL(1, "CAT 1","WORK 1")
      CALL GSTATS_LABEL(2, "CAT 1","WORK 2")

    END SUBROUTINE TEST_INIT
    ! ---------------------------------------------------------------------
    
    SUBROUTINE TEST_END
      IF ( MPL() ) THEN
         CALL MPL_END(LDMEMINFO=.FALSE.)
      END IF
    END SUBROUTINE TEST_END
    ! ---------------------------------------------------------------------
    
    FUNCTION MPL() RESULT(LMPL)
      LOGICAL :: LMPL
      CHARACTER(LEN=512) :: ENV
      CALL GET_ENVIRONMENT_VARIABLE("MPL",ENV)
      IF (ENV == '0') THEN
         LMPL = .FALSE.
      ELSE
         LMPL = .TRUE.
      END IF
    END FUNCTION MPL
    ! ---------------------------------------------------------------------
    
    FUNCTION FCKIT() RESULT(LFCKIT)
      LOGICAL :: LFCKIT
      CHARACTER(LEN=512) :: ENV
      CALL GET_ENVIRONMENT_VARIABLE("FCKIT",ENV)
      IF (ENV == '0') THEN
         LFCKIT = .FALSE.
      ELSE
         LFCKIT = .TRUE.
      END IF
    END FUNCTION FCKIT
    ! ---------------------------------------------------------------------
    
    SUBROUTINE CHECK_OUTPUT
      IMPLICIT NONE
      LOGICAL :: EXISTS
      CHARACTER(LEN=32) :: FNAME
      INTEGER :: U
      
      WRITE(FNAME, '(A,I0,A)') 'gstats.', MYPROC, '.csv'    
      INQUIRE(FILE=FNAME, EXIST=EXISTS)
    
      IF (.NOT. EXISTS) THEN
         PRINT *, "ERROR: GSTATS DID NOT CREATE FILE: ", TRIM(FNAME)
         STOP 1
      ELSE
         OPEN(NEWUNIT=U, FILE=FNAME, STATUS='OLD')
         CLOSE(U, STATUS='DELETE')
         PRINT *, "OK: FILE GENERATED: ", TRIM(FNAME)
      END IF
    END SUBROUTINE CHECK_OUTPUT
    ! ---------------------------------------------------------------------
    
    END PROGRAM TEST_GSTATS
    