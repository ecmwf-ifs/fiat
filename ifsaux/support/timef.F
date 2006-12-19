#if defined ( VPP )
      function timef()
      data ifirst/0/
      save ifirst,zfirst
c
c   fujitsu special
c
      call gettod(ztimef)
      ztimef=ztimef/1000.0
c
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=(ztimef-zfirst)
      endif
c
      return
      end
#elif defined ( RS6K )
      function timef()
      integer*8 irtc
      external irtc
      data ifirst/0/
      save ifirst,zfirst
c
      ztimef=irtc()*1.0e-6
c
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         timef=0.0
      else
         timef=(ztimef-zfirst)
      endif
c
      return
      end
#else
      function timef()
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
      end

#endif
