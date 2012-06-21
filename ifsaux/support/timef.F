#ifdef VPP
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
#endif
#ifdef SGI
      function dummytimef()
      dummytimef=0
      return
      end
#endif
#ifdef RS6K
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
#endif
