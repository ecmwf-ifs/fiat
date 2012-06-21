SUBROUTINE getmemstat(kout, cdlabel)

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB

use mpl_module

implicit none

INTEGER(KIND=JPIM), intent(in) :: kout
character(len=*), intent(in) :: cdlabel
INTEGER(KIND=JPIM) :: i, imyproc, inproc, ioffset
INTEGER(KIND=JPIM), PARAMETER :: JP_MEMKEYS = 5  ! pls. consult ifsaux/utilities/getmemvals.F90
INTEGER(KIND=JPIM) imemkeys(JP_MEMKEYS)
INTEGER(KIND=JPIB) imemvals(JP_MEMKEYS)
REAL(KIND=JPRB), allocatable :: zsend(:), zrecv(:)
INTEGER(KIND=JPIM), allocatable :: icounts(:)

if (kout >= 0) then
  imyproc = mpl_myrank()
  inproc  = mpl_nproc()

  allocate(zsend(JP_MEMKEYS))
  allocate(zrecv(JP_MEMKEYS * inproc))
  allocate(icounts(inproc))

!                 1=MAXHEAP, 2=MAXRSS, 3=CURRENTHEAP, 5=MAXSTACK, 6=PAGING
  IMEMKEYS(:) = (/1,         2,        3,             5,          6/) 
  CALL GETMEMVALS(JP_MEMKEYS, IMEMKEYS, IMEMVALS)

  zsend(:) = 0
  do i=1,JP_MEMKEYS
    zsend(i) = imemvals(i)
  enddo
  zrecv(:) = -1

  icounts(:) = JP_MEMKEYS
  call mpl_gatherv(zsend(:), kroot=1, krecvcounts=icounts(:), &
                  &precvbuf=zrecv, cdstring='GETMEMSTAT:')

  if (imyproc == 1) then
     WRITE(KOUT,9000) trim(cdlabel)
9000 FORMAT(/,"Memory Utilization Information (in bytes) : ",a,/,79("="),//,&
         &  "Node   Max heapsize   Max resident   Current heap      Max stack   I/O-paging #",/,&
         &  "====   ============   ============   ============   ============   ============",//)
     IOFFSET = 0
     DO I=1,INPROC
       IMEMVALS(:) = ZRECV(IOFFSET+1:IOFFSET+JP_MEMKEYS)
       WRITE(KOUT,'(I4,5(3X,I12))') I,IMEMVALS(:)
       IOFFSET = IOFFSET + JP_MEMKEYS
     ENDDO
     write(kout,'(/,a,/)') 'End of Memory Utilization Information'
  ENDIF

  deallocate(zsend)
  deallocate(zrecv)
  deallocate(icounts)

  CALL getheapstat(kout, cdlabel)
endif
END SUBROUTINE getmemstat
