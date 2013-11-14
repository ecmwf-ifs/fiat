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
      end
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
      end
#else
      function timef()
      IMPLICIT NONE
      REAL(KIND=8)      :: TIMEF
      INTEGER,SAVE      :: IFIRST
      INTEGER           :: I_RATE, I_TIME1
      REAL(KIND=8),SAVE :: ZFIRST
      REAL(KIND=8)      :: ZTIMEF
      data ifirst/0/
!RJ       save ifirst,zfirst
!
!     This is a real time clock in milliseconds.
!     Usage of Fortran90 intrinsic function.
      CALL SYSTEM_CLOCK (COUNT=i_time1,COUNT_RATE=i_rate)
      ztimef=REAL(i_time1,kind=8)/REAL(i_rate,kind=8)

      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=1000.0*(ztimef-zfirst)
      endif

      return
      end

#endif
