subroutine ec_cray_meminfo(iu,idstring,kcomm)

IMPLICIT NONE

#include "mpif.h"

integer(kind=4), intent(in) :: kcomm
integer(kind=4), intent(in) :: iu
character*(*), intent(in) :: idstring
integer(kind = 4) :: id,kulout
integer(kind=4) :: i,j,myproc,nproc,len,error,itag,nodenum
integer(kind=8) :: tasksmall,nodehuge,memfree,cached
integer(kind=8) :: smallpage0,smallpage1,hugepage0,hugepage1
integer(kind=8) :: sendbuf(9),recvbuf(9)
integer(kind=8) :: gethwm
integer(kind=8) :: heap_size
integer(kind=4), save :: pagesize = 0
integer(kind=4) :: node0(18),node1(18)
integer(kind=8) :: bucket0(18),bucket1(18)
logical, save :: first = .true.
character(len=512) :: tmpdir
character(len=512) :: program
character(len=8)  :: nodename,lastnode
character(len=4)  :: val
character(len=1)  :: M
character(len=160) ::line
character(len=56) :: filename
character(len=128) :: jobname
character(len=128) :: jobid
CHARACTER (LEN = 10) ::  cldateod,cltimeod,clzoneod
INTEGER(KIND=4) :: ivalues(8)
integer(kind=4) :: irecv_status(MPI_STATUS_SIZE)
integer omp_get_max_threads
external omp_get_max_threads


call flush(0)

CALL MPI_BARRIER(kcomm,error)

CALL MPI_COMM_RANK(KCOMM, myproc, ERROR)


if(error /= 0 ) then
  write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_COMM_RANK"
  call MPI_ABORT(kcomm,-1,error)
endif

CALL MPI_COMM_SIZE(KCOMM,NPROC,ERROR)

if(error /= 0 ) then
  write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_COMM_SIZE"
  call MPI_ABORT(kcomm,-1,error)
endif

if(myproc.eq.0) then 
  call GETARG(0,program)
!
! Use already open file for output or $TMPDIR/meminfo
!
  if(iu.eq.-1) then
    call GETENV('TMPDIR',tmpdir)
    kulout=501
    open(unit=kulout,file=TRIM(tmpdir)//"/"//"meminfo",status='unknown', &
       action='write',position='append')
  else
    kulout=iu
  endif

  call date_and_time(cldateod,cltimeod,clzoneod,ivalues)
  call GETENV('EC_JOB_NAME',jobname)
  if(jobname.eq.'') then
    call GETENV('PBS_JOBNAME',jobname)
  endif
  call GETENV('PBS_JOBID',jobid)

  write(kulout,'(a,/,a)') "## EC_MEMINFO ","## EC_MEMINFO"
  write(kulout,'(4a)') "## EC_MEMINFO Detailed memory information ", &
                       "for program ",TRIM(program)
  write(kulout,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
                       "## EC_MEMINFO Running with ",nproc, &
                       " tasks and ", omp_get_max_threads(), " threads at time ", &
                       cltimeod(1:2),cltimeod(3:4),cltimeod(5:10), &
                       " on ",cldateod(7:8),cldateod(5:6),cldateod(3:4)
  write(kulout,'(4a)') "## EC_MEMINFO The Job Name is ",TRIM(jobname), &
                       " and the Job ID is ",TRIM(jobid)
  write(kulout,'(a)')  "## EC_MEMINFO "
  write(kulout,'(3a)') "## EC_MEMINFO ", &
                       "              | TC    | MEMORY USED(MB) |", &
                       "          MEMORY FREE(MB)        INCLUDING CACHED |"
  write(kulout,'(4a)') "## EC_MEMINFO ", &
                       "              | Malloc| Inc Heap        |", &
                       " Numa node 0    | Numa node 1    |                |"
  write(kulout,'(4a)') "## EC_MEMINFO ", &
                       "Node Name     | Heap  | RSS(sum)        |", &
                       " Small  Huge or | Small  Huge or | Total          |" 
  write(kulout,'(4a)') "## EC_MEMINFO ", &
                       "              | (sum) | Small    Huge   |", &
                       "  Only   Small  |  Only   Small  | Memfree+Cached |" 
  if(iu.eq.-1) then
    write(0,'(a,/,a)') "## EC_MEMINFO ","## EC_MEMINFO"
    write(0,'(4a)') "## EC_MEMINFO Detailed memory information ", &
                    "for program ",TRIM(program)
    write(0,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
                    "## EC_MEMINFO Running with ",nproc, &
                    " tasks and ", omp_get_max_threads(), " threads at time ", &
                    cltimeod(1:2),cltimeod(3:4),cltimeod(5:10), &
                    " on ",cldateod(7:8),cldateod(5:6),cldateod(3:4)
    write(0,'(4a)') "## EC_MEMINFO The Job Name is ",TRIM(jobname), &
                    " and the Job ID is ",TRIM(jobid)
    write(0,'(a)')  "## EC_MEMINFO "
    write(0,'(3a)') "## EC_MEMINFO ", &
                    "              | TC    | MEMORY USED(MB) |", &
                    "          MEMORY FREE(MB)        INCLUDING CACHED |"
    write(0,'(4a)') "## EC_MEMINFO ", &
                    "              | Malloc| Inc Heap        |", &
                    " Numa node 0    | Numa node 1    |                |"
    write(0,'(4a)') "## EC_MEMINFO ", &
                    "Node Name     | Heap  | RSS(sum)        |", &
                    " Small  Huge or | Small  Huge or | Total          |" 
    write(0,'(4a)') "## EC_MEMINFO ", &
                    "              | (sum) | Small    Huge   |", &
                    "  Only   Small  |  Only   Small  | Memfree+Cached |" 
  endif
endif

if(error /= 0 ) then
  write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_BARRIER"
  call MPI_ABORT(kcomm,-1,error)
endif

call crayhostname(nodename)

if(first) then
  pagesize=2048
  first=.false.
  call getenv("HUGETLB_DEFAULT_PAGE_SIZE",val)
  i=index(val,"M")
  if(i.gt.0) then
    read(val(1:i-1),*) pagesize
    pagesize=pagesize*1024
  endif
endif

nodehuge=0

if(pagesize.gt.100000) then
  write(filename,'(a,i6,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             pagesize,"kB/nr_hugepages"
elseif(pagesize.gt.10000) then
  write(filename,'(a,i5,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             pagesize,"kB/nr_hugepages"
else
  write(filename,'(a,i4,a)') "/sys/kernel/mm/hugepages/hugepages-", &
                             pagesize,"kB/nr_hugepages"
endif

if(pagesize.gt.0) then
  open(502,file=filename,status="old")
  read(502,*) nodehuge
  close(502)
endif

nodehuge=nodehuge*pagesize
      
open(file="/proc/meminfo",unit=502)
do i=1,4
  read(502,'(a)') line
  if(line(1:7).eq."MemFree") then
    read(line(9:80),*) memfree 
  elseif(line(1:6).eq."Cached") then
    read(line(8:80),*) cached
  endif
enddo
close(502)

nodehuge=nodehuge/1024
memfree=memfree/1024
cached=cached/1024

open(502,file='/proc/self/status')
do i=1,20
  read(502,'(a)') line
  if (line(1:6).eq.'VmRSS:' ) read(line(8:80),*) tasksmall
enddo
close(502)
tasksmall=tasksmall/1024

open(file="/proc/buddyinfo",unit=502)

read(502,'(a)') line
read(502,'(a)') line
read(502,'(a)') line
read(line(22:160),*) node0
read(502,'(a)') line
read(line(22:160),*) node1

close(502)

bucket0(1)=node0(1)*4096
bucket1(1)=node1(1)*4096
do i=2,18
  bucket0(i)=node0(i)*4096
  bucket1(i)=node1(i)*4096
  do j=2,i
    bucket0(i)=bucket0(i)*2
    bucket1(i)=bucket1(i)*2
  enddo
enddo

smallpage0=0
smallpage1=0
do i=1,9
   smallpage0=smallpage0+bucket0(i)
   smallpage1=smallpage1+bucket1(i)
enddo
hugepage0=0
hugepage1=0
do i=10,18
   hugepage0=hugepage0+bucket0(i)
   hugepage1=hugepage1+bucket1(i)
enddo

smallpage0=smallpage0/(1024*1024)
smallpage1=smallpage1/(1024*1024)
hugepage0=hugepage0/(1024*1024)
hugepage1=hugepage1/(1024*1024)

heap_size=gethwm()/(1024*1024)

ITAG = 98765
if(myproc.eq.0) then
    nodenum=1
    lastnode=nodename
    do i=1,nproc-1
        call MPI_RECV(nodename(1:8),8,MPI_BYTE,i,itag,kcomm,irecv_status,error)
        if(error /= 0 ) then
          write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_RECV"
          call MPI_ABORT(kcomm,-1,error)
        endif
        call MPI_RECV(recvbuf(1:9),9,MPI_INTEGER8,i,itag+1,kcomm,irecv_status,error)
        if(error /= 0 ) then
          write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_RECV"
          call MPI_ABORT(kcomm,-1,error)
        endif
        if(lastnode==nodename)then
          heap_size=heap_size+recvbuf(8)
          tasksmall=tasksmall+recvbuf(9)
        else
          write(kulout,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,a)') "## EC_MEMINFO ", &
 &                      nodenum,lastnode,heap_size,tasksmall,nodehuge,   &
 &                      smallpage0,hugepage0,smallpage1,hugepage1,memfree,cached, &
 &                      idstring
          if(iu.eq.-1) then
            write(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,a)') "## EC_MEMINFO ", &
 &                   nodenum,lastnode,heap_size,tasksmall,nodehuge,   &
 &                   smallpage0,hugepage0,smallpage1,hugepage1,memfree,cached, &
 &                   idstring
          endif

          nodehuge=recvbuf(1)
          memfree=recvbuf(2)
          cached=recvbuf(3)
          smallpage0=recvbuf(4)
          smallpage1=recvbuf(5)
          hugepage0=recvbuf(6)
          hugepage1=recvbuf(7)
          heap_size=recvbuf(8)
          tasksmall=recvbuf(9)
          nodenum=nodenum+1
          lastnode=nodename
        endif
    enddo
    write(kulout,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,a)') "## EC_MEMINFO ", &
 &                nodenum,lastnode,heap_size,tasksmall,nodehuge,   &
 &                smallpage0,hugepage0,smallpage1,hugepage1,memfree,cached, &
 &                idstring

    if(iu.eq.-1) then
      write(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,a)') "## EC_MEMINFO ", &
 &                  nodenum,lastnode,heap_size,tasksmall,nodehuge,   &
 &                  smallpage0,hugepage0,smallpage1,hugepage1,memfree,cached, &
 &                idstring
      close(kulout)
    endif
else
    call MPI_SEND(nodename(1:8),8,MPI_BYTE,0,itag,kcomm,error)
    if(error /= 0 ) then
       write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_SEND"
       call MPI_ABORT(kcomm,-1,error)
    endif
    sendbuf(1)=nodehuge
    sendbuf(2)=memfree
    sendbuf(3)=cached
    sendbuf(4)=smallpage0
    sendbuf(5)=smallpage1
    sendbuf(6)=hugepage0
    sendbuf(7)=hugepage1
    sendbuf(8)=heap_size
    sendbuf(9)=tasksmall
    call MPI_SEND(sendbuf(1:9),9,MPI_INTEGER8,0,itag+1,kcomm,error)
    if(error /= 0 ) then
       write(0,*) "## EC_CRAY_MEMINFO error code ",error," from MPI_SEND"
       call MPI_ABORT(kcomm,-1,error)
    endif
endif

CALL MPI_BARRIER(kcomm,error)

end
