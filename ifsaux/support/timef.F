      function timef()
      data ifirst/0/
      save ifirst,zfirst
c
      call system_clock(ic,icr,icm)
      if(icm.eq.0) then
        print *,' no system clock'
        ztimef=-99.
      else
        ztimef=1000.*ic/icr
      endif
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         ztimef=0.0
      else
         ztimef=ztimef-zfirst
      endif
c
      timef=ztimef
c
      return
      end    
