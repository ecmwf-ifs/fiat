      integer function getopt(clopt,clarg)
C
C     CRAY function
C
C     The getopt utility breaks up options in command lines for easy parsing
C     by shell procedures and checks for legal options.
C     CLOPT is a string of recognized option letters.
C     CLARG is the option-argument.
C
C     Author : Vijay Saravane *METEO-FRANCE*
C
      character(len=*) clopt,clarg
      character(len=len(clarg)) copt
      character*1 copt1
      data iarg /1/
      save iarg
c
c-----------------------------------------------
c
#ifdef T3D
      nbargs=IPXFARGC()
#else
      nbargs=iargc()
#endif
c si on a lu tous les arguments on sort ...
      if (iarg.gt.nbargs) then
         getopt = 0
         return
      endif
c sinon on prend l'option en cours ...
#ifdef T3D
      call PXFGETARG(iarg,copt,ilen,ierr)
#else
      call getarg(iarg,copt)
#endif
      copt1=copt(2:2)
c si c'est pas une bonne option on sort ...
      if (index(clopt,copt1).eq.0) then
         getopt = 0
         return
      endif
c sinon on prend l'argument de l'option
c cet argument est utilise dans getvarg
      if (len_trim(copt).ge.3) then  ! l argument est colle a l'option
         clarg=copt(3:len_trim(copt))
         iarg=iarg+1
      else                           ! l argument est separe par un blanc
#ifdef T3D
         call PXFGETARG(iarg+1,clarg,ilen,ierr)
#else
         call getarg(iarg+1,clarg)
#endif
         iarg=iarg+2
      endif
c
c et on retourne la valeur du caractere de l'option
      getopt = iachar(copt1)
c
      return
      end

