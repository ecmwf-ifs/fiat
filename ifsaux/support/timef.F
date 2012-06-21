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
