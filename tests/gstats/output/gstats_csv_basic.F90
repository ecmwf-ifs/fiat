! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

PROGRAM TEST_GSTATS
    USE YOMGSTATS, ONLY : JPMAXSTAT
    USE EC_PARKIND, ONLY : JPRD
    USE EC_LUN, ONLY : NULOUT
    IMPLICIT NONE
    REAL(JPRD) :: AVEARRAY(0:JPMAXSTAT)

    CALL TEST_INIT()
    CALL GSTATS(0,0)
    CALL GSTATS(1,0)
    CALL WORK_SECTION_1()
    CALL GSTATS(1,1)
    CALL GSTATS(2,0)
    CALL WORK_SECTION_2()
    CALL GSTATS(2,1)
    CALL GSTATS(0,1)
    CALL GSTATS_PRINT(NULOUT,AVEARRAY, JPMAXSTAT)

    ! CHECK THAT THE CSV FILE EXISTS
    CALL CHECK_OUTPUT()
    CALL TEST_END()

CONTAINS
    
    ! ---------------------------------------------------------------------
    SUBROUTINE WORK_SECTION_1
      IMPLICIT NONE
      REAL(JPRD) :: X
      INTEGER :: I
      X = 0.0_JPRD
      DO I = 1, 5000000
         X = X + SIN(REAL(I, JPRD))
      END DO
    END SUBROUTINE WORK_SECTION_1
    ! ---------------------------------------------------------------------
    
    SUBROUTINE WORK_SECTION_2
      IMPLICIT NONE
      REAL(JPRD) :: X
      INTEGER :: I
      X = 1.0_JPRD
      DO I = 1, 4000000
         X = X * 1.0000001_JPRD
      END DO
    END SUBROUTINE WORK_SECTION_2
    ! ---------------------------------------------------------------------
    
    SUBROUTINE TEST_INIT
      USE MPL_MODULE, ONLY : MPL_RANK, MPL_NUMPROC, MPL_INIT
      USE EC_LUN, ONLY : NULOUT

      INTEGER :: KPROC, KMYPROC, JPROC
      INTEGER, ALLOCATABLE :: KPRCIDS(:)
      LOGICAL :: LDSTATS, LDSTATSCPU, LDSYNCSTATS
      LOGICAL :: LDDETAILED_STATS, LDBARRIER_STATS, LDBARRIER_STATS2
      LOGICAL :: LDSTATS_OMP, LDSTATS_COMMS, LDSTATS_MEM
      LOGICAL :: LDSTATS_ALLOC, LDTRACE_STATS, LDXML_STATS, LDCSV_STATS
      INTEGER :: KSTATS_MEM, KTRACE_STATS, KPRNT_STATS
      LOGICAL :: LUSE_MPI

#include "gstats_setup.intfb.h"

      ! INITIALIZE ENVIRONMENT
      LUSE_MPI = DETECT_MPIRUN()
      KPROC   = 1
      KMYPROC = 1
      IF (LUSE_MPI) THEN
        CALL MPL_INIT(LDINFO=.TRUE.)
        KMYPROC = MPL_RANK
        KPROC   = MPL_NUMPROC
      ENDIF
      IF (KMYPROC == 1) THEN
        WRITE(0,*) "LUSE_MPI: ", LUSE_MPI
        WRITE(0,*) "NPROC:    ", KPROC
      ELSE
        ! All other ranks set EC_LUN's NULOUT to /dev/null
        OPEN(UNIT=NULOUT, FILE='/dev/null')
      ENDIF
      ALLOCATE(KPRCIDS(KPROC))

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
      KPRNT_STATS      = KPROC
      LDXML_STATS      = .FALSE.
      LDCSV_STATS      = .TRUE.
      DO JPROC = 1, KPROC
        KPRCIDS(JPROC) = JPROC
      ENDDO

      CALL GSTATS_SETUP( KPROC, KMYPROC, KPRCIDS, &
           LDSTATS, LDSTATSCPU, LDSYNCSTATS, LDDETAILED_STATS, &
           LDBARRIER_STATS, LDBARRIER_STATS2, &
           LDSTATS_OMP, LDSTATS_COMMS, LDSTATS_MEM, KSTATS_MEM, LDSTATS_ALLOC, &
           LDTRACE_STATS, KTRACE_STATS, KPRNT_STATS, LDXML_STATS, LDCSV_STATS )
      CALL GSTATS_LABEL(0, "   ", "TOTAL EXECUTION")
      CALL GSTATS_LABEL(1, "CAT 1", "WORK 1") ! Note that second argument will be truncated to 3 chars
      CALL GSTATS_LABEL(2, "CAT 1", "WORK 2") ! Note that second argument will be truncated to 3 chars
      CALL GSTATS_PSUT

    END SUBROUTINE TEST_INIT
    ! ---------------------------------------------------------------------
    
    SUBROUTINE TEST_END
      USE MPL_MODULE, ONLY : MPL_END, MPL_NUMPROC
      IF (MPL_NUMPROC > 0) THEN
        CALL MPL_END(LDMEMINFO=.FALSE.)
      ENDIF
    END SUBROUTINE TEST_END
    ! ---------------------------------------------------------------------
    
    SUBROUTINE CHECK_OUTPUT
      USE MPL_MODULE, ONLY : MPL_RANK, MPL_NUMPROC
      IMPLICIT NONE
      LOGICAL :: EXISTS
      CHARACTER(LEN=32) :: FNAME
      INTEGER :: U
      INTEGER :: MYPROC

      MYPROC = 1
      IF (MPL_NUMPROC > 0) THEN
        MYPROC = MPL_RANK
      ENDIF
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
    
    function detect_mpirun() result(lmpi_required)
      logical :: lmpi_required
      integer :: ilen
      integer, parameter :: nvars = 4
      character(len=32), dimension(nvars) :: cmpirun_detect
      character(len=4) :: clenv_value
      integer :: ivar
      lmpi_required = .false.

      call get_environment_variable(name='FIAT_USE_MPI', value=clenv_value, length=ilen)
      write(0,*) "FIAT_USE_MPI: ", clenv_value
      if (ilen > 0) then
        if (clenv_value == '1' .or. clenv_value == 'TRUE' .or. clenv_value == 'ON') then
          write(0,*) "FIAT_USE_MPI environment variable set to a true value, MPI will be used"
          lmpi_required = .true.
        else
          write(0,*) "FIAT_USE_MPI environment variable set to a false value, MPI will NOT be used"
          lmpi_required = .false.
        endif
        return
      endif

      ! Environment variables that are set when mpirun, srun, aprun, ... are used
      cmpirun_detect(1) = 'OMPI_COMM_WORLD_SIZE'  ! openmpi
      cmpirun_detect(2) = 'ALPS_APP_PE'           ! cray pe
      cmpirun_detect(3) = 'PMI_SIZE'              ! intel
      cmpirun_detect(4) = 'SLURM_NTASKS'          ! slurm

      do ivar = 1, nvars
        call get_environment_variable(name=trim(cmpirun_detect(ivar)), length=ilen)
        if (ilen > 0) then
          lmpi_required = .true.
          exit ! break
        endif
      enddo
    end function

    END PROGRAM TEST_GSTATS
    