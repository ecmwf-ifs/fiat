! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

SUBROUTINE GSTATS(KNUM, KSWITCH)

!**** *GSTATS*  - Gather timing statistics

!     PURPOSE.
!     --------
!       To gather timings for subsequent output by routine STATS_OUTPUT 


!**   INTERFACE.
!     ----------
!       *CALL* *GSTATS(KNUM,KSWITCH)

!        EXPLICIT ARGUMENTS
!        --------------------
!        KNUM - timing event number (for list of already defined events
!               see routine STATS_OUTPUT)
!        KSWITCH  - KSWITCH=0 - switch on timer
!                   KSWITCH=1 - switch off timer
!                   KSWITCH=2 - suspend timer
!                   KSWITCH=3 - resume  timer

!        IMPLICIT ARGUMENTS
!        --------------------
!        Module YOMSTATS

!     METHOD.
!     -------


!     EXTERNALS.   USER_CLOCK - timing routine
!     ----------

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!        Mats Hamrud ECMWF

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 98-11-15
!        D.Salmond: 02-02-25  Return if not master thread when called from a 
!                             parallel region. 
!        J.Hague:   03-06-11  Memory tracing (for NSTATS_MEM MPI tasks)
!        G.Mozdzynski: 18 Apr 2008 Many corrections to gstats,
!                             see LLFINDSUMB - when set is used detect gstat counter problems.
!        G.Mozdzynski: 20 Jan 2010 Further corrections to gstats to get timed sections and
!                             SUMB to 100 percent of the total time.
!      F. Vana  05-Mar-2015  Support for single precision
!     ------------------------------------------------------------------

USE EC_PARKIND, ONLY: JPRD, JPIM ,JPIB
USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
USE YOMGSTATS, ONLY: JPMAXSTAT, LSTATS, LGSTATS_LABEL, CCDESC, CCTYPE, LSTATSCPU, TIMESUM, NCALLS, &
  &                  NSWITCHVAL, TIMESQSUM, TIMEMAX, TIMESUMB, TTCPUSUM, TVCPUSUM, TIMELCALL, &
  &                  NTMEM, TIME_LAST_CALL, JPERR, MYPROC_STATS, THISTIME, TTCPULCALL, TVCPULCALL, &
  &                  THISTCPU, THISVCPU, NSTATS_MEM, LSTATS_ALLOC, LSTATS_MPL, UNKNOWN_NUMSEND, &
  &                  UNKNOWN_NUMRECV, UNKNOWN_SENDBYTES, UNKNOWN_RECVBYTES, NUMSEND, NUMRECV, &
  &                  SENDBYTES, RECVBYTES, JPMAXDELAYS, NDELAY_INDEX, NDELAY_COUNTER, &
  &                  TDELAY_VALUE, CDELAY_TIME, LTRACE_STATS, NTRACE_STATS, NCALLS_TOTAL, &
  &                  TIME_TRACE, NCALL_TRACE, LAST_KSWITCH, LAST_KNUM
USE MPL_STATS_MOD, ONLY: MPL_STATSON, MPL_STATSREAD
USE OML_MOD, ONLY: OML_MY_THREAD, OML_GET_MAX_THREADS, OML_MAX_THREADS

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN) :: KNUM
INTEGER(KIND=JPIM), INTENT(IN) :: KSWITCH

INTEGER(KIND=JPIM) :: IMOD, ICALL
INTEGER(KIND=JPIM) :: IIMEM, IIPAG, IIMEMC
INTEGER(KIND=JPIB) :: IMEM, IMEMH, IMEMS, IMEMC, IPAG, INUM
INTEGER(KIND=JPIB) :: GETMAXRSS, GETHWM, GETSTK, GETCURHEAP, GETPAG
EXTERNAL GETMAXRSS, GETHWM, GETSTK, GETCURHEAP, GETPAG
REAL(KIND=JPRD) :: ZTIMED, ZCLOCK, ZCLOCK1, ZTIME, ZTCPU, ZVCPU
LOGICAL :: LLFIRST = .TRUE.
LOGICAL :: LLMFIRST = .TRUE.
CHARACTER(LEN=32), SAVE :: CCDESC_DRHOOK(JPMAXSTAT)
CHARACTER(LEN=32), SAVE :: CCDESC_BARR(JPMAXSTAT)
SAVE IIMEM, IIPAG, IIMEMC

INTEGER(KIND=JPIM), SAVE :: NUM_THREADS
INTEGER(KIND=JPIM) :: INUMTH ! Current value <= NUM_THREADS
REAL(KIND=JPHOOK), ALLOCATABLE,SAVE :: ZHOOK_HANDLE(:)
REAL(KIND=JPHOOK), SAVE :: ZHOOK_HANDLE_COMMS, ZHOOK_HANDLE_COMMS1
REAL(KIND=JPHOOK), SAVE :: ZHOOK_HANDLE_TRANS
REAL(KIND=JPHOOK), SAVE :: ZHOOK_HANDLE_BARR
CHARACTER*4 CC

CHARACTER(LEN=10) ::  CLDATEOD, CLZONEOD
INTEGER(KIND=JPIM) :: IVALUES(8)

! Change LLFINDSUMB to TRUE to add diagnostics to help find SUMB times
! Note that a similar setting exists in dr_hook_util for the same objective
LOGICAL :: LLFINDSUMB = .FALSE.
INTEGER(KIND=JPIM), SAVE :: ISUMBSTACK(10)
INTEGER(KIND=JPIM) :: J
REAL(KIND=JPRD)    :: SBYTES, RBYTES
INTEGER(KIND=JPIM) :: NSEND, NRECV
CHARACTER(LEN=4) :: CL_MAXSTAT
CHARACTER(LEN=66) :: CL_ERROR_MESSAGE

#include "user_clock.intfb.h"

! Process GSTATS calls if LSTATS is .TRUE. and we are the master
IF (LSTATS .AND. OML_MY_THREAD() == 1) THEN
  IF (.NOT. ALLOCATED(ZHOOK_HANDLE)) THEN
    NUM_THREADS = OML_GET_MAX_THREADS()
    ALLOCATE(ZHOOK_HANDLE(NUM_THREADS))
  ENDIF

  IF (LGSTATS_LABEL) THEN
    DO INUM = 1, JPMAXSTAT
      WRITE(CC,'(I4)') INUM
      CCDESC_BARR(INUM) = '>BAR-' // CCDESC(INUM)(1:21) // '(' // CC // ')'
    ENDDO
    DO INUM = 1, JPMAXSTAT
      WRITE(CC,'(I4)') INUM
      IF (CCTYPE(INUM) .EQ. "TRS" .OR. CCTYPE(INUM) .EQ. 'MP-' .OR. CCTYPE(INUM) .EQ. 'MPL' &
        & .OR. CCTYPE(INUM) .EQ. 'BAR' .OR. CCTYPE(INUM) .EQ. 'OMP') THEN
        CCDESC_DRHOOK(INUM) = '>' // CCTYPE(INUM) // '-' // CCDESC(INUM)(1:21) // '(' // CC // ')'
      ENDIF
    ENDDO
    LGSTATS_LABEL = .FALSE.
  ENDIF
!     ------------------------------------------------------------------

  CALL USER_CLOCK(PELAPSED_TIME=ZCLOCK)
  IF (LSTATSCPU .OR. KNUM == 0) THEN
    CALL USER_CLOCK(PTOTAL_CP=ZTCPU, PVECTOR_CP=ZVCPU)
  ELSE
    ZTCPU = 0.0_JPRD
    ZVCPU = 0.0_JPRD
  ENDIF

  IF (LLFIRST) THEN
    TIMESUM(:) = 0.0_JPRD
    NCALLS(:) = 0
  ENDIF

  IF (LHOOK .AND. (KSWITCH == 0 .OR. KSWITCH == 1)) THEN
    IF (CCTYPE(KNUM) .EQ. "TRS") THEN
      CALL DR_HOOK(CCDESC_DRHOOK(KNUM), KSWITCH, ZHOOK_HANDLE_TRANS)
    ELSEIF (CCTYPE(KNUM) .EQ. 'MP-') THEN
      CALL DR_HOOK(CCDESC_DRHOOK(KNUM), KSWITCH, ZHOOK_HANDLE_COMMS)
    ELSEIF (CCTYPE(KNUM) .EQ. 'MPL' .AND. KNUM .NE. 682) THEN
      CALL DR_HOOK(CCDESC_DRHOOK(KNUM), KSWITCH, ZHOOK_HANDLE_COMMS1)
    ELSEIF (CCTYPE(KNUM) .EQ. 'OMP') THEN
      ! The prevailing number of threads -- could now be less than the absolute max (i.e. export
      ! OMP_NUM_THREADS=<value>)
      INUMTH = OML_MAX_THREADS()
      CALL DR_HOOK(CCDESC_DRHOOK(KNUM), KSWITCH, ZHOOK_HANDLE(1:INUMTH))
    ELSEIF (CCTYPE(KNUM) .EQ. 'BAR')THEN
      CALL DR_HOOK(CCDESC_DRHOOK(KNUM), KSWITCH, ZHOOK_HANDLE_BARR)
    ENDIF
    ! Measure GSTATS HOOK overhead
    CALL USER_CLOCK(PELAPSED_TIME=ZCLOCK1)
    TIMESUM(401) = TIMESUM(401) + ZCLOCK1 - ZCLOCK
    NCALLS(401) = NCALLS(401) + 1
    ZCLOCK = ZCLOCK1
  ENDIF

  IF (LLFIRST) THEN

    NSWITCHVAL(:) = -1
    TIMESQSUM(:) = 0.0_JPRD
    TIMEMAX(:) = 0.0_JPRD
    TIMESUMB(:) = 0.0_JPRD
    IF (LLFINDSUMB) THEN
      ISUMBSTACK(:) = 0
    ENDIF
    TTCPUSUM(:) = 0.0_JPRD
    TVCPUSUM(:) = 0.0_JPRD
    TIMELCALL(:) = ZCLOCK
    CCDESC = ""
    CCTYPE = ""
    NTMEM = 0
    NTMEM(:,5) = 99999999
    IIMEM = 0
    IIPAG = 0
    IIMEMC = 0
    TIME_LAST_CALL = ZCLOCK
    LLFIRST = .FALSE.
  ENDIF

  ! Check KNUM is valid (> 0 and < JPMAXSTAT)
  IF (KNUM < 0) CALL ABOR1('GSTATS: KNUM cannot be negative')
  IF (KNUM > JPMAXSTAT) THEN
    WRITE(CL_MAXSTAT,'(I4)') JPMAXSTAT
    CALL ABOR1('GSTATS: KNUM cannot be greater than ' // CL_MAXSTAT)
  ENDIF

  IF (KSWITCH == 0 .OR. KSWITCH == 1) THEN
    NCALLS(KNUM) = NCALLS(KNUM) + 1
  ENDIF
  IMOD = MOD(NCALLS(KNUM), 2)

  ! Check we haven't opened or closed a region twice in a row
  IF (.NOT.((KSWITCH == 0 .AND. IMOD == 1) .OR. (KSWITCH == 2 .AND. IMOD == 1) .OR. &
    &       (KSWITCH == 3 .AND. IMOD == 1) .OR. (KSWITCH == 1 .AND. IMOD == 0))) THEN
    WRITE(CL_ERROR_MESSAGE,'(A42,I4)') "Invalid GSTATS call - check region KNUM = ", KNUM
    CALL ABOR1('GSTATS: ' // CL_ERROR_MESSAGE)
  ENDIF

  NSWITCHVAL(KNUM) = KSWITCH

  IF (KSWITCH == 0) THEN
    ! Start timing event
    IF (KNUM >= 500) THEN
      ZTIMED = ZCLOCK - TIME_LAST_CALL
      TIMESUMB(KNUM) = TIMESUMB(KNUM) + ZTIMED
    ELSE
      ZTIMED = 0.0_JPRD
    ENDIF

    IF (LLFINDSUMB .AND. MYPROC_STATS <= 2)THEN
      ! Diagnostic code to find source of sumb (this should only be activated temporarily)
      DO J = 9, 1, -1
        ISUMBSTACK(J+1) = ISUMBSTACK(J)
      ENDDO
      ISUMBSTACK(1) = KNUM
      IF (ZTIMED > 0.1_JPRD .AND. (TIMESUMB(KNUM) > 1.0_JPRD)) THEN
        WRITE(0,'("GSTATS(SUMB): KNUM=",I4," ZTIMED=",F10.6," TIMESUMB=",F10.6)') &
        & KNUM, ZTIMED, TIMESUMB(KNUM)
        DO J = 1, 10
          IF (ISUMBSTACK(J) > 0) THEN
            WRITE(0,'("GSTATS(SUMB): ",I4,2X,I8,2X,A40)') ISUMBSTACK(J), NCALLS(ISUMBSTACK(J)), &
              & CCDESC(ISUMBSTACK(J))
          ENDIF
        ENDDO
      ENDIF
      ! Check if grouped counters are overlapping
      DO J = 0, JPMAXSTAT
        IF (J /= KNUM)THEN
          IF (CCTYPE(J   ) /= '   ' .AND. CCTYPE(J   ) /= 'TRS' .AND. CCTYPE(J   ) /= 'MP-' .AND. &
            & CCTYPE(KNUM) /= '   ' .AND. CCTYPE(KNUM) /= 'TRS' .AND. CCTYPE(KNUM) /= 'MP-') THEN
            IF (NSWITCHVAL(J) == 0 .OR. NSWITCHVAL(J) == 3) THEN
              WRITE(0,'("GSTATS(SUMB): OVERLAPPING COUNTERS ",I4,2X,I4)') KNUM, J
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    THISTIME(KNUM) = 0.0_JPRD
    TIMELCALL(KNUM) = ZCLOCK
    TTCPULCALL(KNUM) = ZTCPU
    TVCPULCALL(KNUM) = ZVCPU
    THISTCPU(KNUM) = 0.0_JPRD
    THISVCPU(KNUM) = 0.0_JPRD
    IF (MYPROC_STATS .LE. NSTATS_MEM .AND. MYPROC_STATS .NE. 0) THEN
      IMEM = GETMAXRSS() / 1024
      IPAG = GETPAG()
      IMEMH = GETHWM() / 1024
      IMEMS = GETSTK() / 1024
      IMEMC = 0
      IF (LSTATS_ALLOC) IMEMC = GETCURHEAP() / 1024
      IF (IMEM > IIMEM .OR. IPAG > IIPAG .OR. (LSTATS_ALLOC .AND. (IMEMC .NE. IIMEMC))) THEN
        IF (LLMFIRST) THEN
          WRITE(0,*) ".---------------------------------------------------------"
          WRITE(0,*) "| Memory trace details"
          WRITE(0,*) "| --------------------"
          WRITE(0,*) "| Memory examined at each GSTATS call if NSTATS_MEM>0."
          WRITE(0,*) "| Header for each trace line is:"    
          WRITE(0,*) "|"
          WRITE(0,*) "|   RSS_INC: Increase in RSS_MAX (KB)"
          WRITE(0,*) "|   RSS_MAX: Maximum real working set so far (KB)"
          WRITE(0,*) "|   HEAP_MX: High Water Mark for heap so far (KB)"
          WRITE(0,*) "|   STK:     Current Stack usage (KB)"
          WRITE(0,*) "|   PGS:     Page faults w I/O since last trace line"
          WRITE(0,*) "|   CALL:    Number of gstats call"
          WRITE(0,*) "|   HEAP:    Current malloc'd total (KB)"
          WRITE(0,*) "|" 
          WRITE(0,*) "| Trace line written for NSTATS_MEM MPI tasks if RSS_MAX"
          WRITE(0,*) "| RSS_MAX increases, PGS>0, or HEAP changed"
          WRITE(0,*) "| (if LTATS_ALLOC=.TRUE.)"
          WRITE(0,*) "`---------------------------------------------------------"
          WRITE(0,*) ""
          WRITE(0,'(A10,A5,21X,A7,2A8,A7,A5,A5,A8)') &
           & "MEMORY    "," KNUM","RSS_INC"," RSS_MAX"," HEAP_MX","    STK", &
           & "  PGS"," CALL","    HEAP"
          LLMFIRST = .FALSE.
        ENDIF
        WRITE(0,'(A10,I5,1X,A20,1X,I6,2(1X,I7),1X,I6,1X,I4,1X,I4,1X,I7)') &
             & "MEMORY bfr", KNUM, CCDESC(KNUM), IMEM - IIMEM, IMEM, IMEMH, IMEMS, IPAG - IIPAG, &
             & (NCALLS(KNUM) + 1) / 2, IMEMC
      ENDIF
      NTMEM(KNUM,2) = IMEM
      IIMEM = IMEM
      IIPAG = IPAG
      IIMEMC = IMEMC
    ENDIF
    IF (LSTATS_MPL .AND. CCTYPE(KNUM) .EQ. 'MPL') THEN
      CALL MPL_STATSON(NSEND, SBYTES, NRECV, RBYTES)
      UNKNOWN_NUMSEND(KNUM) = UNKNOWN_NUMSEND(KNUM) + NSEND
      UNKNOWN_NUMRECV(KNUM) = UNKNOWN_NUMRECV(KNUM) + NRECV
      UNKNOWN_SENDBYTES(KNUM) = UNKNOWN_SENDBYTES(KNUM) + SBYTES
      UNKNOWN_RECVBYTES(KNUM) = UNKNOWN_RECVBYTES(KNUM) + RBYTES
    ENDIF
  ELSEIF (KSWITCH == 1) THEN
    ! Finish timing event
    ZTIME = THISTIME(KNUM) + (ZCLOCK - TIMELCALL(KNUM))
    IF (LSTATS_MPL .AND. CCTYPE(KNUM) .EQ. 'MPL') THEN
      CALL MPL_STATSREAD(NSEND, SBYTES, NRECV, RBYTES)
      NUMSEND(KNUM) = NUMSEND(KNUM) + NSEND
      NUMRECV(KNUM) = NUMRECV(KNUM) + NRECV
      SENDBYTES(KNUM) = SENDBYTES(KNUM) + SBYTES
      RECVBYTES(KNUM) = RECVBYTES(KNUM) + RBYTES
    ENDIF
    TIMESUM(KNUM) = TIMESUM(KNUM) + ZTIME
    TIMESQSUM(KNUM) = TIMESQSUM(KNUM) + ZTIME ** 2
    TIMEMAX(KNUM) = MAX(TIMEMAX(KNUM), ZTIME)
    TTCPUSUM(KNUM) = TTCPUSUM(KNUM) + THISTCPU(KNUM) + ZTCPU - TTCPULCALL(KNUM)
    TVCPUSUM(KNUM) = TVCPUSUM(KNUM) + THISVCPU(KNUM) + ZVCPU - TVCPULCALL(KNUM)
    IF (MYPROC_STATS .LE. NSTATS_MEM .AND. MYPROC_STATS .NE. 0) THEN
      IMEM = GETMAXRSS() / 1024
      IPAG = GETPAG()
      IMEMH = GETHWM() / 1024
      IMEMS = GETSTK() / 1024
      IMEMC = 0
      IF (LSTATS_ALLOC) IMEMC = GETCURHEAP() / 1024
      IF (IMEM > IIMEM .OR. IPAG > IIPAG .OR. (LSTATS_ALLOC .AND. (IMEMC .NE. IIMEMC))) THEN
        WRITE(0,'(A10,I5,1X,A20,1X,I6,2(1X,I7),1X,I6,1X,I4,1X,I4,1X,I7)') &
             & "MEMORY aft ", KNUM, CCDESC(KNUM), IMEM - IIMEM, IMEM, IMEMH, IMEMS, IPAG - IIPAG, &
             & NCALLS(KNUM) / 2, IMEMC
      ENDIF
      IIMEM = IMEM
      IIPAG = IPAG
      IIMEMC = IMEMC
      IMEM = IMEM - NTMEM(KNUM, 2)
      NTMEM(KNUM,4) = NTMEM(KNUM, 4) + IMEM
      IF (IMEM > NTMEM(KNUM,1)) THEN
        NTMEM(KNUM,1) = IMEM
        NTMEM(KNUM,3) = NCALLS(KNUM)
      ENDIF
      IF (IMEM < NTMEM(KNUM,5)) NTMEM(KNUM,5) = IMEM
    ENDIF
    ! Save counters that result in large delays
    IF (KNUM >= 500 .AND. NCALLS(KNUM) / 2 > 10)THEN
      IF (ZTIME > TIMESUM(KNUM) / FLOAT(NCALLS(KNUM)/2) + 0.2_JPRD) THEN
        ! Ignore counters 1007 and 1013 due to NFRLW frequency LW radiation calls 
        ! in ec_phys_tl and ec_phys_ad call trees
        ! also ignore 635 and 636 due to increasing sujbwavallo matrix sizes
        IF (KNUM /= 1007 .AND. KNUM /= 1013 .AND. KNUM /= 635 .AND. KNUM /= 636 ) THEN
          IF (NDELAY_INDEX < JPMAXDELAYS) THEN
            NDELAY_INDEX = NDELAY_INDEX + 1
            NDELAY_COUNTER(NDELAY_INDEX) = KNUM
            TDELAY_VALUE(NDELAY_INDEX) = ZTIME - TIMESUM(KNUM) / FLOAT(NCALLS(KNUM) / 2)
            CALL DATE_AND_TIME(CLDATEOD, CDELAY_TIME(NDELAY_INDEX), CLZONEOD, IVALUES)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSEIF (KSWITCH == 2) THEN
    ! Suspend timing event
    ZTIMED = ZCLOCK - TIMELCALL(KNUM)
    THISTIME(KNUM) = THISTIME(KNUM) + ZTIMED
    THISTCPU(KNUM) = THISTCPU(KNUM) + ZTCPU - TTCPULCALL(KNUM)
    THISVCPU(KNUM) = THISVCPU(KNUM) + ZVCPU - TVCPULCALL(KNUM)
    IF (LSTATS_MPL .AND. CCTYPE(KNUM) .EQ. 'MPL') THEN
      CALL MPL_STATSREAD(NSEND, SBYTES, NRECV, RBYTES)
      NUMSEND(KNUM) = NUMSEND(KNUM) + NSEND
      NUMRECV(KNUM) = NUMRECV(KNUM) + NRECV
      SENDBYTES(KNUM) = SENDBYTES(KNUM) + SBYTES
      RECVBYTES(KNUM) = RECVBYTES(KNUM) + RBYTES
    ENDIF
  ELSEIF (KSWITCH == 3) THEN
    ! Resume timing event
    TIMELCALL(KNUM) = ZCLOCK
    TTCPULCALL(KNUM) = ZTCPU
    TVCPULCALL(KNUM) = ZVCPU
    IF (LSTATS_MPL .AND. CCTYPE(KNUM) .EQ. 'MPL') THEN
      CALL MPL_STATSON(NSEND, SBYTES, NRECV, RBYTES)
      UNKNOWN_NUMSEND(KNUM) = UNKNOWN_NUMSEND(KNUM) + NSEND
      UNKNOWN_NUMRECV(KNUM) = UNKNOWN_NUMRECV(KNUM) + NRECV
      UNKNOWN_SENDBYTES(KNUM) = UNKNOWN_SENDBYTES(KNUM) + SBYTES
      UNKNOWN_RECVBYTES(KNUM) = UNKNOWN_RECVBYTES(KNUM) + RBYTES
    ENDIF
    IF (KNUM >= 500) THEN
      ZTIMED = ZCLOCK - TIME_LAST_CALL
      TIMESUMB(KNUM) = TIMESUMB(KNUM) + ZTIMED
    ENDIF
  ENDIF
  IF (KNUM >= 500) THEN
    TIME_LAST_CALL = ZCLOCK
  ENDIF

  ! Trace stats
  NCALLS_TOTAL = NCALLS_TOTAL + 1
  IF (LTRACE_STATS .AND. NCALLS_TOTAL <= NTRACE_STATS) THEN
    ICALL = NCALLS_TOTAL
    TIME_TRACE(ICALL) = ZCLOCK
    NCALL_TRACE(ICALL) = (JPMAXSTAT+1) * KSWITCH + KNUM
  ENDIF

  ! Measure gstats overhead
  CALL USER_CLOCK(PELAPSED_TIME=ZCLOCK1)
  TIMESUM(400) = TIMESUM(400) + ZCLOCK1 - ZCLOCK
  NCALLS(400) = NCALLS(400) + 1
  LAST_KSWITCH = KSWITCH
  LAST_KNUM = KNUM
  
ENDIF

END SUBROUTINE GSTATS
