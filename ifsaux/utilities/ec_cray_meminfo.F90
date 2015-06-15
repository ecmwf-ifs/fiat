SUBROUTINE EC_CRAY_MEMINFO(IU,IDSTRING,KCOMM)

IMPLICIT NONE

#include "mpif.h"

INTEGER(KIND=4), INTENT(IN) :: KCOMM
INTEGER(KIND=4), INTENT(IN) :: IU
CHARACTER*(*), INTENT(IN) :: IDSTRING
INTEGER(KIND = 4) :: ID,KULOUT
INTEGER(KIND=4) :: I,J,MYPROC,NPROC,LEN,ERROR,ITAG,NODENUM
INTEGER(KIND=8) :: TASKSMALL,NODEHUGE,MEMFREE,CACHED
INTEGER(KIND=8) :: SMALLPAGE0,SMALLPAGE1,HUGEPAGE0,HUGEPAGE1
INTEGER(KIND=8) :: SENDBUF(9),RECVBUF(9)
INTEGER(KIND=8) :: GETHWM
INTEGER(KIND=8) :: HEAP_SIZE
INTEGER(KIND=4), SAVE :: PAGESIZE = 0
INTEGER(KIND=4) :: NODE0(18),NODE1(18)
INTEGER(KIND=8) :: BUCKET0(18),BUCKET1(18)
REAL(KIND=4) :: PERCENT_USED
LOGICAL, SAVE :: FIRST = .TRUE.
CHARACTER(LEN=512) :: TMPDIR
CHARACTER(LEN=512) :: PROGRAM
CHARACTER(LEN=8)  :: NODENAME,LASTNODE
CHARACTER(LEN=4)  :: VAL
CHARACTER(LEN=1)  :: M
CHARACTER(LEN=160) ::LINE
CHARACTER(LEN=56) :: FILENAME
CHARACTER(LEN=128) :: JOBNAME
CHARACTER(LEN=128) :: JOBID
CHARACTER (LEN = 10) ::  CLDATEOD,CLTIMEOD,CLZONEOD
INTEGER(KIND=4) :: IVALUES(8)
INTEGER(KIND=4) :: IRECV_STATUS(MPI_STATUS_SIZE)
INTEGER OMP_GET_MAX_THREADS
EXTERNAL OMP_GET_MAX_THREADS


CALL FLUSH(0)

CALL MPI_BARRIER(KCOMM,ERROR)

CALL MPI_COMM_RANK(KCOMM, MYPROC, ERROR)


IF(ERROR /= 0 ) THEN
  WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_COMM_RANK"
  CALL MPI_ABORT(KCOMM,-1,ERROR)
ENDIF

CALL MPI_COMM_SIZE(KCOMM,NPROC,ERROR)

IF(ERROR /= 0 ) THEN
  WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_COMM_SIZE"
  CALL MPI_ABORT(KCOMM,-1,ERROR)
ENDIF

IF(MYPROC.EQ.0) THEN 
  CALL GETARG(0,PROGRAM)
!
! Use already open file for output or $TMPDIR/meminfo
!
  IF(IU.EQ.-1) THEN
    CALL GETENV('TMPDIR',TMPDIR)
    KULOUT=501
    OPEN(UNIT=KULOUT,FILE=TRIM(TMPDIR)//"/"//"meminfo",STATUS='unknown', &
       ACTION='write',POSITION='append')
  ELSE
    KULOUT=IU
  ENDIF

  CALL DATE_AND_TIME(CLDATEOD,CLTIMEOD,CLZONEOD,IVALUES)
  CALL GETENV('EC_JOB_NAME',JOBNAME)
  IF(JOBNAME.EQ.'') THEN
    CALL GETENV('PBS_JOBNAME',JOBNAME)
  ENDIF
  CALL GETENV('PBS_JOBID',JOBID)

  WRITE(KULOUT,'(a,/,a)') "## EC_MEMINFO ","## EC_MEMINFO"
  WRITE(KULOUT,'(4a)') "## EC_MEMINFO Detailed memory information ", &
                       "for program ",TRIM(PROGRAM)
  WRITE(KULOUT,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
                       "## EC_MEMINFO Running with ",NPROC, &
                       " tasks and ", OMP_GET_MAX_THREADS(), " threads at time ", &
                       CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
                       " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
  WRITE(KULOUT,'(4a)') "## EC_MEMINFO The Job Name is ",TRIM(JOBNAME), &
                       " and the Job ID is ",TRIM(JOBID)
  WRITE(KULOUT,'(a)')  "## EC_MEMINFO "
  WRITE(KULOUT,'(3a)') "## EC_MEMINFO ", &
                       "              | TC    | MEMORY USED(MB) |", &
                       "          MEMORY FREE(MB)        INCLUDING CACHED | %USED"
  WRITE(KULOUT,'(4a)') "## EC_MEMINFO ", &
                       "              | Malloc| Inc Heap        |", &
                       " Numa node 0    | Numa node 1    |                |"
  WRITE(KULOUT,'(4a)') "## EC_MEMINFO ", &
                       "Node Name     | Heap  | RSS(sum)        |", &
                       " Small  Huge or | Small  Huge or | Total          |" 
  WRITE(KULOUT,'(4a)') "## EC_MEMINFO ", &
                       "              | (sum) | Small    Huge   |", &
                       "  Only   Small  |  Only   Small  | Memfree+Cached |" 
  IF(IU.EQ.-1) THEN
    WRITE(0,'(a,/,a)') "## EC_MEMINFO ","## EC_MEMINFO"
    WRITE(0,'(4a)') "## EC_MEMINFO Detailed memory information ", &
                    "for program ",TRIM(PROGRAM)
    WRITE(0,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
                    "## EC_MEMINFO Running with ",NPROC, &
                    " tasks and ", OMP_GET_MAX_THREADS(), " threads at time ", &
                    CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
                    " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
    WRITE(0,'(4a)') "## EC_MEMINFO The Job Name is ",TRIM(JOBNAME), &
                    " and the Job ID is ",TRIM(JOBID)
    WRITE(0,'(a)')  "## EC_MEMINFO "
    WRITE(0,'(3a)') "## EC_MEMINFO ", &
                    "              | TC    | MEMORY USED(MB) |", &
                    "          MEMORY FREE(MB)        INCLUDING CACHED | %USED"
    WRITE(0,'(4a)') "## EC_MEMINFO ", &
                    "              | Malloc| Inc Heap        |", &
                    " Numa node 0    | Numa node 1    |                |"
    WRITE(0,'(4a)') "## EC_MEMINFO ", &
                    "Node Name     | Heap  | RSS(sum)        |", &
                    " Small  Huge or | Small  Huge or | Total          |" 
    WRITE(0,'(4a)') "## EC_MEMINFO ", &
                    "              | (sum) | Small    Huge   |", &
                    "  Only   Small  |  Only   Small  | Memfree+Cached |" 
  ENDIF
ENDIF

IF(ERROR /= 0 ) THEN
  WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_BARRIER"
  CALL MPI_ABORT(KCOMM,-1,ERROR)
ENDIF

CALL CRAYHOSTNAME(NODENAME)

IF(FIRST) THEN
  PAGESIZE=2048
  FIRST=.FALSE.
  CALL GETENV("HUGETLB_DEFAULT_PAGE_SIZE",VAL)
  I=INDEX(VAL,"M")
  IF(I.GT.0) THEN
    READ(VAL(1:I-1),*) PAGESIZE
    PAGESIZE=PAGESIZE*1024
  ENDIF
ENDIF

NODEHUGE=0

IF(PAGESIZE.GT.100000) THEN
  WRITE(FILENAME,'(a,i6,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             PAGESIZE,"kB/nr_hugepages"
ELSEIF(PAGESIZE.GT.10000) THEN
  WRITE(FILENAME,'(a,i5,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             PAGESIZE,"kB/nr_hugepages"
ELSE
  WRITE(FILENAME,'(a,i4,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             PAGESIZE,"kB/nr_hugepages"
ENDIF

IF(PAGESIZE.GT.0) THEN
  OPEN(502,FILE=FILENAME,STATUS="old")
  READ(502,*) NODEHUGE
  CLOSE(502)
ENDIF

NODEHUGE=NODEHUGE*PAGESIZE
      
OPEN(FILE="/proc/meminfo",UNIT=502)
DO I=1,4
  READ(502,'(a)') LINE
  IF(LINE(1:7).EQ."MemFree") THEN
    READ(LINE(9:80),*) MEMFREE 
  ELSEIF(LINE(1:6).EQ."Cached") THEN
    READ(LINE(8:80),*) CACHED
  ENDIF
ENDDO
CLOSE(502)

NODEHUGE=NODEHUGE/1024
MEMFREE=MEMFREE/1024
CACHED=CACHED/1024

OPEN(502,FILE='/proc/self/status')
DO I=1,20
  READ(502,'(a)') LINE
  IF (LINE(1:6).EQ.'VmRSS:' ) READ(LINE(8:80),*) TASKSMALL
ENDDO
CLOSE(502)
TASKSMALL=TASKSMALL/1024

OPEN(FILE="/proc/buddyinfo",UNIT=502)

READ(502,'(a)') LINE
READ(502,'(a)') LINE
READ(502,'(a)') LINE
READ(LINE(22:160),*) NODE0
NODE1(:)=0
READ(502,'(a)',END=99) LINE
READ(LINE(22:160),*) NODE1

99 CLOSE(502)

BUCKET0(1)=NODE0(1)*4096
BUCKET1(1)=NODE1(1)*4096
DO I=2,18
  BUCKET0(I)=NODE0(I)*4096
  BUCKET1(I)=NODE1(I)*4096
  DO J=2,I
    BUCKET0(I)=BUCKET0(I)*2
    BUCKET1(I)=BUCKET1(I)*2
  ENDDO
ENDDO

SMALLPAGE0=0
SMALLPAGE1=0
DO I=1,9
   SMALLPAGE0=SMALLPAGE0+BUCKET0(I)
   SMALLPAGE1=SMALLPAGE1+BUCKET1(I)
ENDDO
HUGEPAGE0=0
HUGEPAGE1=0
DO I=10,18
   HUGEPAGE0=HUGEPAGE0+BUCKET0(I)
   HUGEPAGE1=HUGEPAGE1+BUCKET1(I)
ENDDO

SMALLPAGE0=SMALLPAGE0/(1024*1024)
SMALLPAGE1=SMALLPAGE1/(1024*1024)
HUGEPAGE0=HUGEPAGE0/(1024*1024)
HUGEPAGE1=HUGEPAGE1/(1024*1024)

HEAP_SIZE=GETHWM()/(1024*1024)

ITAG = 98765
IF(MYPROC.EQ.0) THEN
    NODENUM=1
    LASTNODE=NODENAME
    DO I=1,NPROC-1
        CALL MPI_RECV(NODENAME(1:8),8,MPI_BYTE,I,ITAG,KCOMM,IRECV_STATUS,ERROR)
        IF(ERROR /= 0 ) THEN
          WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_RECV"
          CALL MPI_ABORT(KCOMM,-1,ERROR)
        ENDIF
        CALL MPI_RECV(RECVBUF(1:9),9,MPI_INTEGER8,I,ITAG+1,KCOMM,IRECV_STATUS,ERROR)
        IF(ERROR /= 0 ) THEN
          WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_RECV"
          CALL MPI_ABORT(KCOMM,-1,ERROR)
        ENDIF
        IF(LASTNODE==NODENAME)THEN
          HEAP_SIZE=HEAP_SIZE+RECVBUF(8)
          TASKSMALL=TASKSMALL+RECVBUF(9)
        ELSE
          IF(HEAP_SIZE.GT.NODEHUGE) THEN
! running with small pages
            PERCENT_USED=100.0*(TASKSMALL+NODEHUGE)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
          ELSE
! running with huge pages
            PERCENT_USED=100.0*HEAP_SIZE/NODEHUGE
          ENDIF
          WRITE(KULOUT,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,f4.1,1x,a)') "## EC_MEMINFO ", &
 &                      NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                      SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                      PERCENT_USED,IDSTRING
          IF(IU.EQ.-1) THEN
            WRITE(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,f4.1,1x,a)') "## EC_MEMINFO ", &
 &                   NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                   SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                   PERCENT_USED,IDSTRING
          ENDIF

          NODEHUGE=RECVBUF(1)
          MEMFREE=RECVBUF(2)
          CACHED=RECVBUF(3)
          SMALLPAGE0=RECVBUF(4)
          SMALLPAGE1=RECVBUF(5)
          HUGEPAGE0=RECVBUF(6)
          HUGEPAGE1=RECVBUF(7)
          HEAP_SIZE=RECVBUF(8)
          TASKSMALL=RECVBUF(9)
          NODENUM=NODENUM+1
          LASTNODE=NODENAME
        ENDIF
    ENDDO
    IF(HEAP_SIZE.GT.NODEHUGE) THEN
! running with small pages
      PERCENT_USED=100.0*(TASKSMALL+NODEHUGE)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
    ELSE
! running with huge pages
      PERCENT_USED=100.0*HEAP_SIZE/NODEHUGE
    ENDIF
    WRITE(KULOUT,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,f4.1,1x,a)') "## EC_MEMINFO ", &
 &                NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                PERCENT_USED,IDSTRING

    IF(IU.EQ.-1) THEN
      WRITE(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,f4.1,1x,a)') "## EC_MEMINFO ", &
 &                  NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                  SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                  PERCENT_USED,IDSTRING
      CLOSE(KULOUT)
    ENDIF
ELSE
    CALL MPI_SEND(NODENAME(1:8),8,MPI_BYTE,0,ITAG,KCOMM,ERROR)
    IF(ERROR /= 0 ) THEN
       WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_SEND"
       CALL MPI_ABORT(KCOMM,-1,ERROR)
    ENDIF
    SENDBUF(1)=NODEHUGE
    SENDBUF(2)=MEMFREE
    SENDBUF(3)=CACHED
    SENDBUF(4)=SMALLPAGE0
    SENDBUF(5)=SMALLPAGE1
    SENDBUF(6)=HUGEPAGE0
    SENDBUF(7)=HUGEPAGE1
    SENDBUF(8)=HEAP_SIZE
    SENDBUF(9)=TASKSMALL
    CALL MPI_SEND(SENDBUF(1:9),9,MPI_INTEGER8,0,ITAG+1,KCOMM,ERROR)
    IF(ERROR /= 0 ) THEN
       WRITE(0,*) "## EC_CRAY_MEMINFO error code ",ERROR," from MPI_SEND"
       CALL MPI_ABORT(KCOMM,-1,ERROR)
    ENDIF
ENDIF

CALL MPI_BARRIER(KCOMM,ERROR)

END
