#if defined (VPP)
      SUBROUTINE CPTIME(PVCP,PTCP)
!     Fujitsu function
      call clockv(PVCP,PTCP,0,2)
      return
      ENDSUBROUTINE CPTIME
#elif defined (RS6K)
      SUBROUTINE CPTIME(PVCP,PTCP)
      data ifirst/0/
      save ifirst,zfirst
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
      return
      ENDSUBROUTINE CPTIME
#else
      SUBROUTINE CPTIME(PVCP,PTCP)
      USE PARKIND1, ONLY : JPIM, JPRB
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: IFIRST
      REAL(KIND=JPRB) :: PVCP, PTCP
      REAL(KIND=JPRB) :: ZFIRST, ZSEC
      data ifirst/0/
      save ifirst,zfirst
!     Usage of Fortran90 intrinsic function for CPU timing.
      if(ifirst.eq.0) then
         ifirst=1
         call cpu_time(zfirst)
         pvcp=0.0
         ptcp=pvcp
      else
         call cpu_time(ZSEC)
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
      return
      ENDSUBROUTINE CPTIME
#endif
