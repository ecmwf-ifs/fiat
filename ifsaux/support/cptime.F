!RJ: make interfaceable; generalization

      SUBROUTINE CPTIME(PVCP,PTCP)
      USE PARKIND1, ONLY : JPIM, JPRB
      IMPLICIT NONE
      REAL(KIND=JPRB) :: PVCP
      REAL(KIND=JPRB) :: PTCP
!
#if defined (VPP)
!     Fujitsu function
      call clockv(PVCP,PTCP,0,2)
#elif defined (RS6K)
      INTEGER(KIND=JPIM),SAVE :: IFIRST=0
      REAL(KIND=JPRB),SAVE :: ZFIRST
      REAL(KIND=JPRB) :: ZSEC
!RJ       data ifirst/0/
!RJ       save ifirst,zfirst
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=second()
         pvcp=0.0
         ptcp=pvcp
      else
         ZSEC=SECOND()
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
#elif defined (OLDTIMER)
      INTEGER(KIND=JPIM),SAVE :: IFIRST=0
      REAL(KIND=JPRB),SAVE :: ZFIRST
      REAL(KIND=JPRB) :: ZSEC
!RJ       data ifirst/0/
!RJ       save ifirst,zfirst
!     Usage of Fortran90 intrinsic function for CPU timing.
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
#else
! this routine should work better with OpenMP
      INTEGER(KIND=JPIM),SAVE :: ifirst=1
      INTEGER(KIND=JPIM),SAVE :: kfirst,ktps
      INTEGER(KIND=JPIM) :: ktick
!RJ       DATA ifirst/1/
!RJ       save ifirst,kfirst,ktps
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
#endif
!
      RETURN
      END SUBROUTINE CPTIME

