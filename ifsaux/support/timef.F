      function timef()
      data ifirst/0/
      save ifirst,zfirst
c
cportable      call system_clock(ic,icr,icm)
cportable      if(icm.eq.0) then
cportable        print *,' no system clock'
cportable        ztimef=-99.
cportable      else
cportable        ztimef=1000.*ic/icr
cportable      endif
c
c   fujitsu special
c
      call gettod(ztimef)
c
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=ztimef
         ztimef=0.0
      else
         ztimef=ztimef-zfirst
      endif
c
      timef=ztimef/1000.
c
      return
      end    
