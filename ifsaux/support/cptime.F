      SUBROUTINE CPTIME(PVCP,PTCP)
C
C        Fujitsu function
C
#ifdef VPP
      call clockv(PVCP,PTCP,0,2)
#endif
#ifdef SGI
      data ifirst/0/
      save ifirst,zfirst
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=secondr()
         pvcp=0.0
         ptcp=pvcp
      else
         ZSEC=SECONDR()
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
#endif
      return
      END
