      INTEGER FUNCTION getopt(y_optstr, y_optarg)
      CHARACTER*(*)           y_optstr, y_optarg

      CHARACTER*512           y_arg
      CHARACTER*1             y_opt
      LOGICAL                 ll_endopts

      COMMON/gopt_com1/ initial  ,n_args  ,n_arg   ,ndx_arg ,ll_endopts
      COMMON/gopt_com2/ y_arg

C  Initialise on 1st. call
C  -----------------------

      IF (initial .NE. 123456) THEN
         initial    = 123456
         n_args     = iargc()
         n_arg      = 0
         ndx_arg    = 0
         ll_endopts = .FALSE.
      ENDIF

      y_optarg = ' '

C  Get length of "y_optstr"
C  ------------------------

      l_optstr = 0
      DO 10 i = len(y_optstr), 1, -1
      IF (y_optstr(i : i) .NE. ' ') THEN
         l_optstr = i
         GOTO 20
      ENDIF
   10 CONTINUE
   20 CONTINUE

C  If already at end of options, return
C  ------------------------------------

      IF ((n_arg .GT. n_args) .OR. ll_endopts) THEN
         ll_endopts = .TRUE.
         getopt = -1
         RETURN
      ENDIF

C  If we need to get the next argument, do so. Check for end of options
C  --------------------------------------------------------------------

      IF (ndx_arg .EQ. 0) THEN
         n_arg = n_arg + 1

         IF (n_arg .GT. n_args) THEN
            ll_endopts = .TRUE.
            getopt     = -1
            RETURN
         ENDIF

         y_arg   = ' '
         CALL getarg(n_arg, y_arg)
         ndx_arg = 1

         IF (y_arg(1 : 1) .NE. '-') THEN
            ll_endopts = .TRUE.
            getopt     = -1
            RETURN
         ELSE IF (y_arg .EQ. '--') THEN
            ll_endopts = .TRUE.
            n_arg      = n_arg + 1
            y_arg      = ' '
            IF (n_arg .LE. n_args) THEN
               CALL getarg(n_arg, y_arg)
            ENDIF
            getopt     = -1
            RETURN
         ENDIF
         ndx_arg = 2
      ENDIF

C  We have an option, now see if it is valid
C  -----------------------------------------

      y_opt   = y_arg(ndx_arg : ndx_arg)
      ndx_opt = index(y_optstr, y_opt)

      IF (ndx_opt .EQ. 0) THEN
         ndx_arg = ndx_arg + 1
         IF (y_arg(ndx_arg : ) .EQ. ' ') THEN
            ndx_arg = 0
         ENDIF
         getopt = ichar(y_opt)
         RETURN
      ENDIF

C  We have a valid option, see if it should have an argument
C  ---------------------------------------------------------

      ndx_arg = ndx_arg + 1
      IF (y_arg(ndx_arg :) .EQ. ' ') THEN
         ndx_arg = 0
      ENDIF

      IF (ndx_opt .EQ. l_optstr) THEN
         getopt = ichar(y_opt)
         RETURN
      ELSE IF (y_optstr(ndx_opt + 1 : ndx_opt + 1) .NE. ':') THEN
         getopt = ichar(y_opt)
         RETURN
      ENDIF


C  A valid option with an argument
C  -------------------------------

      IF (ndx_arg .EQ. 0) THEN
         IF (n_arg .EQ. n_args) THEN
            ll_endopts = .TRUE.
            getopt     = 0
            RETURN
         ENDIF

         n_arg = n_arg + 1
         y_arg = ' '
         call getarg(n_arg, y_arg)
         ndx_arg = 1
         ll_endopts = n_arg .eq. n_args
      ENDIF

      y_optarg = y_arg(ndx_arg : )
      ndx_arg  = 0
      getopt   = ichar(y_opt)

      RETURN
      END
