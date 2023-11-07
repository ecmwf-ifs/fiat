MODULE stream_mod
  !=======================================================================
  ! Program: STREAM
  ! Programmer: John D. McCalpin
  ! RCS Revision: $Id: stream.f,v 5.6 2005/10/04 00:20:48 mccalpin Exp mccalpin $
  !-----------------------------------------------------------------------
  ! Copyright 1991-2003: John D. McCalpin
  !-----------------------------------------------------------------------
  ! License:
  !  1. You are free to use this program and/or to redistribute
  !     this program.
  !  2. You are free to modify this program for your own use,
  !     including commercial use, subject to the publication
  !     restrictions in item 3.
  !  3. You are free to publish results obtained from running this
  !     program, or from works that you derive from this program,
  !     with the following limitations:
  !     3a. In order to be referred to as "STREAM benchmark results",
  !         published results must be in conformance to the STREAM
  !         Run Rules, (briefly reviewed below) published at
  !         http://www.cs.virginia.edu/stream/ref.html
  !         and incorporated herein by reference.
  !         As the copyright holder, John McCalpin retains the
  !         right to determine conformity with the Run Rules.
  !     3b. Results based on modified source code or on runs not in
  !         accordance with the STREAM Run Rules must be clearly
  !         labelled whenever they are published.  Examples of
  !         proper labelling include:
  !         "tuned STREAM benchmark results" 
  !         "based on a variant of the STREAM benchmark code"
  !         Other comparable, clear and reasonable labelling is
  !         acceptable.
  !     3c. Submission of results to the STREAM benchmark web site
  !         is encouraged, but not required.
  !  4. Use of this program or creation of derived works based on this
  !     program constitutes acceptance of these licensing restrictions.
  !  5. Absolutely no warranty is expressed or implied.
  !-----------------------------------------------------------------------
  ! This program measures sustained memory transfer rates in MB/s for
  ! simple computational kernels coded in FORTRAN.
  !
  ! The intent is to demonstrate the extent to which ordinary user
  ! code can exploit the main memory bandwidth of the system under
  ! test.
  use yomhook, only : LHOOK,DR_HOOK,JPHOOK

CONTAINS
  SUBROUTINE stream_combinations()
    IMPLICIT NONE
    INTEGER*8 n
    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
    n=100000000
    IF (LHOOK) CALL DR_HOOK('STREAM',0,ZHOOK_HANDLE)
    CALL stream(n)
    IF (LHOOK) CALL DR_HOOK('STREAM',1,ZHOOK_HANDLE)

  END SUBROUTINE stream_combinations

  SUBROUTINE stream(n)
    INTEGER*8 n,offset,ndim
    INTEGER*8 ntimes
    PARAMETER (offset=0,ntimes=10)
    !     ..
    !     .. Local Scalars ..
    DOUBLE PRECISION scalar,t
    INTEGER j,k,nbpw,quantum
    !     ..
    !     .. Local Arrays ..
    DOUBLE PRECISION maxtime(4),mintime(4),avgtime(4), &
         times(4,ntimes)
    INTEGER bytes(4)
    CHARACTER label(4)*11
    !     ..
    !     .. External Functions ..
    DOUBLE PRECISION mysecond
    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
    REAL(KIND=JPHOOK) :: ZHOOK_1,ZHOOK_2,ZHOOK_3,ZHOOK_4
!    INTEGER realsize
    EXTERNAL mysecond !,checktick,realsize
    !$    INTEGER omp_get_num_threads
    !$    EXTERNAL omp_get_num_threads
    !     ..
    !     .. Intrinsic Functions ..
    !
    INTRINSIC dble,max,min,nint,sqrt
    !     ..
    !     .. Arrays in Common ..
    DOUBLE PRECISION, allocatable :: a(:),b(:),c(:)
    !dir$ attributes align:64 :: A, B, C
!    CHARACTER(len=40) :: suffix
    !     ..
    !     .. Common blocks ..
    !     COMMON a,b,c
    !     ..
    !     .. Data statements ..
    DATA avgtime/4*0.0D0/,mintime/4*1.0D+36/,maxtime/4*0.0D0/
    DATA label/'Copy:      ','Scale:     ','Add:       ','Triad:     '/
    DATA bytes/2,2,3,3/
    !     ..
!    WRITE(suffix,'(A,I30)')"_",n
    !       --- SETUP --- determine precision and check timing ---
    ndim=n+offset
    allocate(a(ndim),b(ndim),c(ndim))
    nbpw = realsize()

    PRINT *,'----------------------------------------------'
    PRINT *,'STREAM Version $Revision: 5.6 $'
    PRINT *,'----------------------------------------------'
    WRITE (*,FMT=9010) 'Array size = ',n
    WRITE (*,FMT=9010) 'Offset     = ',offset
    WRITE (*,FMT=9020) 'The total memory requirement is ', &
         3*nbpw*n/ (1024*1024),' MB'
    WRITE (*,FMT=9030) 'You are running each test ',ntimes,' times'
    WRITE (*,FMT=9030) '--'
    WRITE (*,FMT=9030) 'The *best* time for each test is used'
    WRITE (*,FMT=9030) '*EXCLUDING* the first and last iterations'

    !$OMP PARALLEL
    !$OMP MASTER
    PRINT *,'----------------------------------------------'
    !$    PRINT *,'Number of Threads = ',OMP_GET_NUM_THREADS()
    !$OMP END MASTER
    !$OMP END PARALLEL

    PRINT *,'----------------------------------------------'
    !$OMP PARALLEL
    PRINT *,'Printing one line per active thread....'
    !$OMP END PARALLEL

    !$OMP PARALLEL DO
    DO 10 j = 1,n
       a(j) = 2.0d0
       b(j) = 0.5D0
       c(j) = 0.0D0
10  END DO
    t = mysecond()
    !$OMP PARALLEL DO
    DO 20 j = 1,n
       a(j) = 0.5d0*a(j)
20  END DO
    t = mysecond() - t
    PRINT *,'----------------------------------------------------'
    quantum = checktick()
    WRITE (*,FMT=9000) &
         'Your clock granularity/precision appears to be ',quantum, &
         ' microseconds'
    PRINT *,'----------------------------------------------------'

    !       --- MAIN LOOP --- repeat test cases NTIMES times ---
    scalar = 0.5d0*a(1)
    DO 70 k = 1,ntimes

       IF (LHOOK) CALL DR_HOOK('STREAM_COPY',0,ZHOOK_1)
       t = mysecond()
       a(1) = a(1) + t
       !$OMP PARALLEL DO
       DO 30 j = 1,n
          c(j) = a(j)
30     END DO
       t = mysecond() - t
       IF (LHOOK) CALL DR_HOOK('STREAM_COPY',1,ZHOOK_1)

       c(n) = c(n) + t
       times(1,k) = t

       IF (LHOOK) CALL DR_HOOK('STREAM_SCALE',0,ZHOOK_2)
       t = mysecond()
       c(1) = c(1) + t
       !$OMP PARALLEL DO
       DO 40 j = 1,n
          b(j) = scalar*c(j)
40     END DO
       t = mysecond() - t
       IF (LHOOK) CALL DR_HOOK('STREAM_SCALE',1,ZHOOK_2)

       b(n) = b(n) + t
       times(2,k) = t

       IF (LHOOK) CALL DR_HOOK('STREAM_ADD',0,ZHOOK_3)
       t = mysecond()
       a(1) = a(1) + t
       !$OMP PARALLEL DO
       DO 50 j = 1,n
          c(j) = a(j) + b(j)
50     END DO
       t = mysecond() - t
       IF (LHOOK) CALL DR_HOOK('STREAM_ADD',1,ZHOOK_3)
       c(n) = c(n) + t
       times(3,k) = t

       IF (LHOOK) CALL DR_HOOK('STREAM_TRIAD',0,ZHOOK_4)
       t = mysecond()
       b(1) = b(1) + t
       !$OMP PARALLEL DO
       DO 60 j = 1,n
          a(j) = b(j) + scalar*c(j)
60     END DO
       t = mysecond() - t
       IF (LHOOK) CALL DR_HOOK('STREAM_TRIAD',1,ZHOOK_4)

       a(n) = a(n) + t
       times(4,k) = t
70  END DO

    !       --- SUMMARY ---
    DO 90 k = 2,ntimes
       DO 80 j = 1,4
          avgtime(j) = avgtime(j) + times(j,k)
          mintime(j) = min(mintime(j),times(j,k))
          maxtime(j) = max(maxtime(j),times(j,k))
80     END DO
90  END DO
    WRITE (*,FMT=9040)
    DO 100 j = 1,4
       avgtime(j) = avgtime(j)/dble(ntimes-1)
       WRITE (*,FMT=9050) label(j),n*bytes(j)*nbpw/mintime(j)/1.0D6, &
            avgtime(j),mintime(j),maxtime(j)
100 END DO
    PRINT *,'----------------------------------------------------'
    CALL checksums (a,b,c,n,ntimes)
    PRINT *,'----------------------------------------------------'

9000 FORMAT (1x,a,i6,a)
9010 FORMAT (1x,a,i10)
9020 FORMAT (1x,a,i7,a)
9030 FORMAT (1x,a,i3,a,a)
9040 FORMAT ('Function',5x,'Rate (MB/s)  Avg time   Min time  Max time' &
         )
9050 FORMAT (a,4 (f12.4,2x))
  END SUBROUTINE stream

 !-------------------------------------
 ! INTEGER FUNCTION dblesize()
 !
 ! A semi-portable way to determine the precision of DOUBLE PRECISION
 ! in Fortran.
 ! Here used to guess how many bytes of storage a DOUBLE PRECISION
 ! number occupies.
 !
 INTEGER FUNCTION realsize()
   !     IMPLICIT NONE

   !     .. Local Scalars ..
   DOUBLE PRECISION result,test
   INTEGER j,ndigits
   !     ..
   !     .. Local Arrays ..
   DOUBLE PRECISION ref(30)
   !     ..
   !     .. External Subroutines ..
!   EXTERNAL confuse
   !     ..
   !     .. Intrinsic Functions ..
   INTRINSIC abs,acos,log10,sqrt
   !     ..

   !       Test #1 - compare single(1.0d0+delta) to 1.0d0

10 DO 20 j = 1,30
      ref(j) = 1.0d0 + 10.0d0** (-j)
20 END DO

   DO 30 j = 1,30
      test = ref(j)
      ndigits = j
      CALL confuse(test,result)
      IF (test.EQ.1.0D0) THEN
         GO TO 40
      END IF
30 END DO
   GO TO 50

40 WRITE (*,FMT='(a)') &
        '----------------------------------------------'
   WRITE (*,FMT='(1x,a,i2,a)') 'Double precision appears to have ', &
        ndigits,' digits of accuracy'
   IF (ndigits.LE.8) THEN
      realsize = 4
   ELSE
      realsize = 8
   END IF
   WRITE (*,FMT='(1x,a,i1,a)') 'Assuming ',realsize, &
        ' bytes per DOUBLE PRECISION word'
   WRITE (*,FMT='(a)') &
        '----------------------------------------------'
   RETURN

50 PRINT *,'Hmmmm.  I am unable to determine the size.'
   PRINT *,'Please enter the number of Bytes per DOUBLE PRECISION', &
        ' number : '
   READ (*,FMT=*) realsize
   IF (realsize.NE.4 .AND. realsize.NE.8) THEN
      PRINT *,'Your answer ',realsize,' does not make sense.'
      PRINT *,'Try again.'
      PRINT *,'Please enter the number of Bytes per ', &
           'DOUBLE PRECISION number : '
      READ (*,FMT=*) realsize
   END IF
   PRINT *,'You have manually entered a size of ',realsize, &
        ' bytes per DOUBLE PRECISION number'
   WRITE (*,FMT='(a)') &
        '----------------------------------------------'
 END FUNCTION realsize

SUBROUTINE confuse(q,r)
  !     IMPLICIT NONE
  !     .. Scalar Arguments ..
  DOUBLE PRECISION q,r
  !     ..
  !     .. Intrinsic Functions ..
  INTRINSIC cos
  !     ..
  r = cos(q)
  RETURN
END SUBROUTINE confuse

! A semi-portable way to determine the clock granularity
! Adapted from a code by John Henning of Digital Equipment Corporation
!
INTEGER FUNCTION checktick()
  !     IMPLICIT NONE

  !     .. Parameters ..
  INTEGER n
  PARAMETER (n=20)
  !     ..
  !     .. Local Scalars ..
  DOUBLE PRECISION t1,t2
  INTEGER i,j,jmin
  !     ..
  !     .. Local Arrays ..
  DOUBLE PRECISION timesfound(n)
  !     ..
  !     .. External Functions ..
  DOUBLE PRECISION mysecond
  EXTERNAL mysecond
  !     ..
  !     .. Intrinsic Functions ..
  INTRINSIC max,min,nint
  !     ..
  i = 0

10 t2 = mysecond()
  IF (t2.EQ.t1) GO TO 10

  t1 = t2
  i = i + 1
  timesfound(i) = t1
  IF (i.LT.n) GO TO 10

  jmin = 1000000
  DO 20 i = 2,n
     j = nint((timesfound(i)-timesfound(i-1))*1d6)
     jmin = min(jmin,max(j,0))
20 END DO

  IF (jmin.GT.0) THEN
     checktick = jmin
  ELSE
     PRINT *,'Your clock granularity appears to be less ', &
          'than one microsecond'
     checktick = 1
  END IF
  RETURN

  !      PRINT 14, timesfound(1)*1d6
  !      DO 20 i=2,n
  !         PRINT 14, timesfound(i)*1d6,
  !     &       nint((timesfound(i)-timesfound(i-1))*1d6)
  !   14    FORMAT (1X, F18.4, 1X, i8)
  !   20 END DO

END FUNCTION checktick




SUBROUTINE checksums(a,b,c,n,ntimes)
  !     IMPLICIT NONE
  !     ..
  !     .. Arguments ..
 DOUBLE PRECISION a(*),b(*),c(*)
 INTEGER*8 n,ntimes
 !     ..
 !     .. Local Scalars ..
 DOUBLE PRECISION aa,bb,cc,scalar,suma,sumb,sumc,epsilon
 INTEGER k
 !     ..

 !     Repeat the main loop, but with scalars only.
 !     This is done to check the sum & make sure all
 !     iterations have been executed correctly.

 aa = 2.0D0
 bb = 0.5D0
 cc = 0.0D0
 aa = 0.5D0*aa
 scalar = 0.5d0*aa
 DO k = 1,ntimes
    cc = aa
    bb = scalar*cc
    cc = aa + bb
    aa = bb + scalar*cc
 END DO
 aa = aa*DBLE(n-2)
 bb = bb*DBLE(n-2)
 cc = cc*DBLE(n-2)

 !     Now sum up the arrays, excluding the first and last
 !     elements, which are modified using the timing results
 !     to confuse aggressive optimizers.

 suma = 0.0d0
 sumb = 0.0d0
 sumc = 0.0d0
 !$OMP PARALLEL DO REDUCTION(+:suma,sumb,sumc)
 DO 110 j = 2,n-1
    suma = suma + a(j)
    sumb = sumb + b(j)
    sumc = sumc + c(j)
110 END DO

 epsilon = 1.D-6

 IF (ABS(suma-aa)/suma .GT. epsilon) THEN
    PRINT *,'Failed Validation on array a()'
    PRINT *,'Target   Sum of a is = ',aa
    PRINT *,'Computed Sum of a is = ',suma
 ELSEIF (ABS(sumb-bb)/sumb .GT. epsilon) THEN
    PRINT *,'Failed Validation on array b()'
    PRINT *,'Target   Sum of b is = ',bb
    PRINT *,'Computed Sum of b is = ',sumb
 ELSEIF (ABS(sumc-cc)/sumc .GT. epsilon) THEN
    PRINT *,'Failed Validation on array c()'
    PRINT *,'Target   Sum of c is = ',cc
    PRINT *,'Computed Sum of c is = ',sumc
 ELSE
    PRINT *,'Solution Validates!'
 ENDIF

END SUBROUTINE checksums

function itoa(i) result(res)
  character(:),allocatable :: res
  integer,intent(in) :: i
  character(range(i)+2) :: tmp
  write(tmp,'(i0)') i
  res = trim(tmp)
end function itoa

END MODULE stream_mod
