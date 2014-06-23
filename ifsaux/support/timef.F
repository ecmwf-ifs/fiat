#if defined ( VPP )
      function timef()
      data ifirst/0/
      save ifirst,zfirst
!
!   fujitsu special
!
      call gettod(ztimef)
      ztimef=ztimef/1000.0
!
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=(ztimef-zfirst)
      endif
!
      return
      endfunction timef
#elif defined ( RS6K )
      function timef()
      integer*8 irtc
      external irtc
      data ifirst/0/
      save ifirst,zfirst
!
      ztimef=irtc()*1.0e-6
!
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=(ztimef-zfirst)
      endif
!
      return
      endfunction timef
#elif defined ( _CRAYFTN )
      function timef()
      integer*8 i_time1,i_rate
      real*8 ztimef,zfirst
      data ifirst/0/
      save ifirst,zfirst

c     This is a real time clock in milliseconds.
c     Usage of Fortran90 intrinsic function.
      CALL SYSTEM_CLOCK (COUNT=i_time1,COUNT_RATE=i_rate)
      ztimef=float(i_time1)/float(i_rate)

      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=1000.0*(ztimef-zfirst)
      endif

      return
      endfunction timef

#else
      function timef()
      USE PARKIND1, ONLY : JPIM, JPRB
      IMPLICIT NONE
      REAL(KIND=JPRB)         :: TIMEF
      INTEGER(KIND=JPIM),SAVE :: IFIRST
      INTEGER(KIND=JPIM)      :: I_RATE, I_TIME1
      REAL(KIND=JPRB),SAVE    :: ZFIRST
      REAL(KIND=JPRB)         :: ZTIMEF
      data ifirst/0/
!RJ       save ifirst,zfirst
!
!     This is a real time clock in milliseconds.
!     Usage of Fortran90 intrinsic function.
      CALL SYSTEM_CLOCK (COUNT=i_time1,COUNT_RATE=i_rate)
      ztimef=REAL(i_time1,kind=JPRB)/REAL(i_rate,kind=JPRB)

      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=1000.0*(ztimef-zfirst)
      endif

      return
      endfunction timef
#endif
