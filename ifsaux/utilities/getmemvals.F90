SUBROUTINE getmemvals(n, key, kval)
USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB
implicit none
INTEGER(KIND=JPIM), intent(in) :: n, key(n)
INTEGER(KIND=JPIB), intent(out):: kval(n)
!--------------------------------- key ----------------------------------------------
INTEGER(KIND=JPIB), external :: gethwm    !  1  High Water Mark for HEAP-alloc
INTEGER(KIND=JPIB), external :: getrss    !  2  Maximum resident memory so far
INTEGER(KIND=JPIB), external :: getcurheap!  3  Instantaneous allocation from ALLOCATE/malloc
INTEGER(KIND=JPIB), external :: getstk    !  4  Instantaneous stack usage
INTEGER(KIND=JPIB), external :: getmaxstk !  5  Maximum stack usage so far
INTEGER(KIND=JPIB), external :: getpag    !  6  I/O caused by paging
! -- add more as required (all 64-bit integers upon return, though) --

INTEGER(KIND=JPIM) j

do j=1,n
  if (key(j) == 1) then
    kval(j) = gethwm()
  else if (key(j) == 2) then
    kval(j) = getrss()
  else if (key(j) == 3) then
    kval(j) = getcurheap()
  else if (key(j) == 4) then
    kval(j) = getstk()
  else if (key(j) == 5) then
    kval(j) = getmaxstk()
  else if (key(j) == 6) then
    kval(j) = getpag()
!  else if (key(j) == ) then
!    kval(j) = get()
  endif
enddo

END SUBROUTINE getmemvals
