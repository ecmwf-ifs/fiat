      function timef()
      data ifirst/0/
      save ifirst,zfirst
c
#ifdef SGI
      call system_clock(ic,icr,icm)
      if(icm.eq.0) then
        print *,' no system clock'
        ztimef=-99.
      else
        ztimef=1000.*ic/icr
      endif
#endif
#ifdef VPP
c
c   fujitsu special
c
      call gettod(ztimef)
      ztimef=ztimef/1000.0
#endif
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
