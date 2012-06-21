SUBROUTINE getheapstat(kout, cdlabel)

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB

use mpl_module

implicit none

INTEGER(KIND=JPIM), intent(in) :: kout
character(len=*), intent(in) :: cdlabel
INTEGER(KIND=JPIM) :: i, imyproc, inproc, iret, ioffset, ii
INTEGER(KIND=JPIM), parameter :: JP_NPROFILE = 9 ! pls. consult ifsaux/utilities/getcurheap.c
INTEGER(KIND=JPIM), parameter :: isize = JP_NPROFILE+1
INTEGER(KIND=JPIB) ilimit(isize)
INTEGER(KIND=JPIB) ihitcnt(isize)
REAL(KIND=JPRB), allocatable :: zsend(:), zrecv(:)
INTEGER(KIND=JPIM), allocatable :: icounts(:)
character(len=1) CLenv
character(len=50) CLtext(0:2)

call getenv("EC_PROFILE_HEAP", CLenv) ! turn OFF by export EC_PROFILE_HEAP=0

if (kout >= 0 .and. CLenv /= '0') then
  imyproc = mpl_myrank()
  inproc  = mpl_nproc()

  do i=1,isize
    ilimit(i) = i ! power of 10's ; pls. consult ifsaux/utilities/getcurheap.c
  enddo

  allocate(zsend(isize))
  allocate(zrecv(isize * inproc))
  allocate(icounts(inproc))

!!  CLtext(0) = "free()/DEALLOCATE"
  CLtext(1) = "malloc()/ALLOCATE"
  CLtext(2) = "malloc()/ALLOCATE minus free()/DEALLOCATE"

!!  do ii=0,2
  do ii=1,2
    ihitcnt(:) = 0
    CALL profile_heap_get(ihitcnt, isize, ii, iret)

    zsend(:) = 0
    do i=1,iret
      zsend(i) = ihitcnt(i)
    enddo
    zrecv(:) = -1

    icounts(:) = isize
    call mpl_gatherv(zsend(:), kroot=1, krecvcounts=icounts(:), &
                    &precvbuf=zrecv, cdstring='GETHEAPSTAT:')

    if (imyproc == 1) then
      write(kout,9000) trim(CLtext(ii)),trim(cdlabel), "Node", &
                     & (ilimit(i),i=1,min(JP_NPROFILE,9)), "Larger"
9000  format(/,"Heap Utilization Profile (",a," -hits per byte range): ",a,&
            &/,"========================",&
            &//,(a4,2x,9(:,2x,5x,"< 10^",i1),:,2x,a11))
      write(kout,9001)
9001  format(4("="),2x,10(2x,11("="))/)
      ioffset = 0
      do i=1,inproc
        ihitcnt(:) = zrecv(ioffset+1:ioffset+isize)
        write(kout,'(i4,2x,(10(:,2x,i11)))') i,ihitcnt(:)
        ioffset = ioffset + isize
      enddo
    endif
  enddo

  if (imyproc == 1) then
    write(kout,'(/,a,/)') 'End of Heap Utilization Profile'
  endif

  deallocate(zsend)
  deallocate(zrecv)
  deallocate(icounts)
endif
END SUBROUTINE getheapstat
