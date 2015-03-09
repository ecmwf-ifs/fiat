!RJ: make interfaceable; generalization

      SUBROUTINE CPTIME(PVCP,PTCP)
      USE PARKIND1, ONLY : JPIM, JPRB
      IMPLICIT NONE
      REAL(KIND=JPRB) :: PVCP
      REAL(KIND=JPRB) :: PTCP
!
#if defined (NEWTIMER)
! this routine should work better with OpenMP
! But doesn't work on Cray - and in any case does not return 
! CPU time for all threads combined 
      INTEGER(KIND=JPIM),SAVE :: ifirst=1
      INTEGER(KIND=JPIM),SAVE :: kfirst,ktps
      INTEGER(KIND=JPIM) :: ktick
!     Usage of Fortran95 intrinsic function system_clock for ELAPSED time,
!     thus taking into account the parallelism if inside an open-mp region. REK
      if(ifirst.eq.1) then
         ifirst=0
         call system_clock(kfirst,ktps)
         pvcp=0.0_JPRB
         ptcp=pvcp
      else
         call system_clock(ktick)
         PVCP=REAL(ktick-kfirst,KIND=JPRB)/REAL(ktps,KIND=JPRB)
         PTCP=PVCP
      endif
#else
      INTEGER(KIND=JPIM),SAVE :: IFIRST=0
      REAL(KIND=JPRB),SAVE :: ZFIRST
      REAL(KIND=JPRB) :: ZSEC
!     Usage of Fortran95 intrinsic function for CPU timing.
      if(ifirst.eq.0) then
         ifirst=1
         call cpu_time(zfirst)
         pvcp=0.0_JPRB
         ptcp=pvcp
      else
         call cpu_time(ZSEC)
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
#endif
!
      RETURN
      END SUBROUTINE CPTIME

