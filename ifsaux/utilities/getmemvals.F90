SUBROUTINE getmemvals(n, key, kval)
#include "tsmbkind.h"
implicit none
INTEGER_M, intent(in) :: n, key(n)
INTEGER_B, intent(out):: kval(n)
!--------------------------------- key ----------------------------------------------
INTEGER_B, external :: gethwm    !  1  High Water Mark for HEAP-alloc
INTEGER_B, external :: getrss    !  2  Maximum resident memory so far
INTEGER_B, external :: getcurheap!  3  Instantaneous allocation from ALLOCATE/malloc
INTEGER_B, external :: getstk    !  4  Instantaneous stack usage
INTEGER_B, external :: getmaxstk !  5  Maximum stack usage so far
INTEGER_B, external :: getpag    !  6  I/O caused by paging
! -- add more as required (all 64-bit integers upon return, though) --

INTEGER_M j

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
