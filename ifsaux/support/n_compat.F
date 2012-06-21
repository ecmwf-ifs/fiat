#define         RNAME    NR_COMPAT_EC
#define         DNAME    ND_COMPAT_EC
#define        YRNAME   'NR_COMPAT_EC'
#define        YDNAME   'ND_COMPAT_EC'

C
C*****************************************************************************
C**
C  NAME
C
C      INTEGER FUNCTION RNAME(value)
C      REAL                   value(2)
C      INTEGER FUNCTION DNAME(value)
C      DOUBLE PRECISION   value(2)
C
C  DESCRIPTION
C
C      The actual name of the routine is given in the "#defines" above.
C      The user calls these functions to ensure that his compilation is
C      compatible with that of the library in which these functions reside.
C
C      Problems arise on systems that allow different precisions for REAL and
C      DOUBLE-PRECISION variables, according to what options are specified to
C      the Fortran compiler. These routines go some way to prevent this. See
C      the example below for more information.
C
C  PARAMETER
C
C      value - A 2-element array of type:
C                                        REAL             (for RNAME)
C                                        DOUBLE PRECISION (for DNAME)
C
C              Both elements of this array must be set to the value 1.0.
C
C  RETURN VALUE
C
C      An INTEGER value greater than 0, is a good return. See the example.
C
C      <0 - The precision of REAL (or DOUBLE PRECISION) variables in the
C           calling routine is not compatible with the precision of these
C           variables when this routine was compiled (within the library).
C           The ABSOLUTE of the value returned is the number of bytes of
C           storage such a variable occupies in this routine. The values are:
C                           -4, -8, -16
C
C       0 - A mistake has been made when specifying the "value" array. This
C           usually means that a simple variable was passed, instead of a
C           2-element array, or that one or both of the array elements does
C           not contain the value 1.0.
C
C      >0 - The precision of REAL (or DOUBLE PRECISION) variables in the
C           calling routine is compatible with the precision of these
C           variables when this routine was compiled (within the library).
C           The actual value returned is the number of bytes of storage that
C           such a variable occupies. The values are:
C                           4, 8 or 16
C  EXAMPLE
C
C            REAL              x(2)
C            DOUBLE PRECISION  d(2)
C
C            DO 10 i = 1, 2
C            x(i) = 1.0
C        10  d(i) = 1.0
C
C      C  Check that ECLIB was compiled with the same precision for REALs and
C      C  DOUBLE PRECISION as we are being compiled.
C
C            nr = nr_compat_ec(x)
C            nd = nd_compat_ec(d)
C
C            IF (nr .LE. 0  .OR.  nd .LE. 0) THEN
C               CALL abort('Compilation incompatible with ECLIB')
C            ENDIF
C**
C*****************************************************************************
C

      INTEGER FUNCTION RNAME(value)
      REAL                   value(2)
      REAL                   val(2)
      INTEGER                n_precision,   nval,   nvalue

      val(1) = 1.0
      val(2) = 1.0

      nval   = n_precision(val)
      nvalue = n_precision(value)

      IF (nval   .EQ. 0) THEN
         PRINT *, 'ROUTINE: ', YRNAME,
     1            ': INTERNAL ERROR'
         RNAME = 0
         RETURN
      ENDIF

      IF (nvalue .EQ. 0) THEN
         PRINT *, 'ROUTINE: ', YRNAME,
     1            ': CALLED WITH WRONG PARAMETER'
         RNAME = 0
         RETURN
      ENDIF

      IF (nvalue .EQ. nval) THEN
         RNAME = nvalue
         RETURN
      ENDIF

      PRINT *, 'ROUTINE: ', YRNAME,
     1         ': INCOMPATIBLE SINGLE PRECISION (',
     2         nvalue, 'Bytes, should be ', nval, ')'

      RNAME = -nval
      RETURN
      END

      INTEGER FUNCTION DNAME(value)
#ifndef _CRAYIEEE
      DOUBLE PRECISION   value(2)
      DOUBLE PRECISION   val(2)
#else
      REAL               value(2)
      REAL               val(2)
#endif
      INTEGER            n_precision,   nval,   nvalue

      val(1) = 1.0
      val(2) = 1.0

      nval   = n_precision(val)
      nvalue = n_precision(value)

      IF (nval   .EQ. 0) THEN
         PRINT *, 'ROUTINE: ', YDNAME,
     1            ': INTERNAL ERROR'
         DNAME = 0
         RETURN
      ENDIF

      IF (nvalue .EQ. 0) THEN
         PRINT *, 'ROUTINE: ', YDNAME,
     1            ': CALLED WITH WRONG PARAMETER'
         DNAME = 0
         RETURN
      ENDIF

      IF (nvalue .EQ. nval) THEN
         DNAME = nvalue
         RETURN
      ENDIF

      PRINT *, 'ROUTINE: ', YDNAME,
     1         ': INCOMPATIBLE DOUBLE PRECISION (',
     2         nvalue, 'Bytes, should be ', nval, ')'

      DNAME = -nval
      RETURN
      END
