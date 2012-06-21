      SUBROUTINE CPTIME(PVCP,PTCP)
C
C        Fujitsu function
C
#ifdef VPP
      call clockv(PVCP,PTCP,0,2)
#else
      data ifirst/0/
      save ifirst,zfirst
      if(ifirst.eq.0) then
         ifirst=1
#ifdef RS6K
         zfirst=second()
#else
         zfirst=secondr()
#endif
         pvcp=0.0
         ptcp=pvcp
      else
#ifdef RS6K
         ZSEC=SECOND()
#else
         ZSEC=SECONDR()
#endif
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
#endif
      return
      END
