MODULE ECsort
#include "tsmbkind.h"

USE strhandler, only : TOUPPER

!..   Author: Sami Saarinen, ECMWF, 10/02/98
!     Fixes : Sami Saarinen, ECMWF, 08/11/99 : Sub-arrays go now correctly (look for addrdiff)
!                                              Genuine real(4) sort "re-habilitated"
!                                              sizeof_int, _real4 & _real8 HARDCODED !
!             Sami Saarinen, ECMWF, 11/10/00 : REAL*4 version included (REAL_M)


IMPLICIT NONE

PRIVATE

INTEGER_M, parameter :: sizeof_int   = 4
INTEGER_M, parameter :: sizeof_real4 = 4
INTEGER_M, parameter :: sizeof_real8 = 8

INTEGER_M, parameter :: min_method = 1
INTEGER_M, parameter :: max_method = 2
!     integer, parameter :: max_method = 3

INTEGER_M, parameter :: radixsort_method = 1
INTEGER_M, parameter :: heapsort_method  = 2
!--   To be implemented: QuickSort
!     integer, parameter :: quicksort_method = 3

INTEGER_M, parameter :: default_method = radixsort_method
INTEGER_M            :: current_method = default_method

INTERFACE keysort
MODULE PROCEDURE &
     &int_keysort_1D, int_keysort_2D, &
     &real8_keysort_1D, real8_keysort_2D, &
     &real4_keysort_1D, real4_keysort_2D
END INTERFACE

INTERFACE sorting_method
MODULE PROCEDURE int_sorting_method, str_sorting_method
END INTERFACE

PUBLIC :: keysort
PUBLIC :: init_index, get_rank
PUBLIC :: sorting_method

CONTAINS

!----------------------------
!--   Public subroutines   --
!----------------------------

SUBROUTINE int_sorting_method(inew, iold)
INTEGER_M, intent(in)  :: inew
INTEGER_M, intent(out) :: iold
INTEGER_M :: itmp
itmp = inew
if (itmp == -1) itmp = default_method
iold = current_method
current_method = min(max(min_method,itmp),max_method)
END SUBROUTINE int_sorting_method


SUBROUTINE str_sorting_method(cdnew, iold)
character(len=*), intent(in) :: cdnew
INTEGER_M, intent(out) :: iold
character(len=len(cdnew)) clnew
clnew = cdnew
CALL toupper(clnew)
select case (clnew)
case ('RADIX')
CALL sorting_method(radixsort_method, iold)
case ('HEAP')
CALL sorting_method(heapsort_method, iold)
!     case ('QUICK')
!        CALL sorting_method(quicksort_method, iold)
case default
CALL sorting_method(default_method, iold)
end select
END SUBROUTINE str_sorting_method


SUBROUTINE init_index(index)
INTEGER_M, intent(out):: index(:)
INTEGER_M :: i, n
n = size(index)
do i=1,n
  index(i) = i
enddo
END SUBROUTINE init_index


SUBROUTINE get_rank(index, rank)
INTEGER_M, intent(in) :: index(:)
INTEGER_M, intent(out):: rank(:)
INTEGER_M :: i, n
n = min(size(index),size(rank))
do i=1,n
  rank(index(i)) = i
enddo
END SUBROUTINE get_rank


SUBROUTINE int_keysort_1D(rc, a, n,method, descending,index, init)
INTEGER_M, intent(out)           :: rc
INTEGER_M, intent(inout)         :: a(:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: method
logical, intent(in), OPTIONAL  :: descending
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
INTEGER_M :: aa(size(a),1)
INTEGER_M :: ikey
rc = 0
if (n <= 0) return
if (size(a) <= 0) return
aa(:,1) = a(:)
ikey = 1
if (present(descending)) then
  if (descending) ikey = -1
endif
CALL keysort(rc, aa, n, key=ikey, method=method, index=index, init=init)
a(:) = aa(:,1)
END SUBROUTINE int_keysort_1D


SUBROUTINE real4_keysort_1D(rc, a, n,method, descending,index, init)
INTEGER_M, intent(out)           :: rc
REAL_M, intent(inout)         :: a(:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: method
logical, intent(in), OPTIONAL  :: descending
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
REAL_M :: aa(size(a),1)
INTEGER_M :: ikey
rc = 0
if (n <= 0) return
if (size(a) <= 0) return
aa(:,1) = a(:)
ikey = 1
if (present(descending)) then
  if (descending) ikey = -1
endif
CALL keysort(rc, aa, n, key=ikey, method=method, index=index, init=init)
a(:) = aa(:,1)
END SUBROUTINE real4_keysort_1D


SUBROUTINE real8_keysort_1D(rc, a, n,method, descending,index, init)
INTEGER_M, intent(out)           :: rc
REAL_B, intent(inout)         :: a(:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: method
logical, intent(in), OPTIONAL  :: descending
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
REAL_B :: aa(size(a),1)
INTEGER_M :: ikey
rc = 0
if (n <= 0) return
if (size(a) <= 0) return
aa(:,1) = a(:)
ikey = 1
if (present(descending)) then
  if (descending) ikey = -1
endif
!CALL keysort(rc, aa, n, key=ikey, method=method, index=index, init=init)
CALL real8_keysort_2D(rc, aa, n, key=ikey, method=method, index=index, init=init)
a(:) = aa(:,1)
END SUBROUTINE real8_keysort_1D


SUBROUTINE int_keysort_2D(&
     &rc, a, n,&
     &key, multikey, method,&
     &index, init, transposed)

INTEGER_M, intent(out)           :: rc
INTEGER_M, intent(inout)         :: a(:,:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: key, method
INTEGER_M, intent(in), OPTIONAL  :: multikey(:)
logical, intent(in), OPTIONAL  :: transposed
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
INTEGER_M, POINTER :: iindex(:)
INTEGER_M :: ikey, istride, imethod
INTEGER_M :: lda, iptr, i, j, sda, idiff
INTEGER_M, allocatable :: data(:)
INTEGER_M, allocatable :: ikeys(:)
logical iinit, descending, LLtrans

rc = 0
lda = size(a, dim=1)
sda = size(a, dim=2)
if (n <= 0 .or. lda <= 0 .or. sda <= 0) return

imethod = current_method
if (present(method)) then
  imethod = min(max(min_method,method),max_method)
endif

ikey = 1
if (present(key)) ikey = key

if (present(multikey)) then
  allocate(ikeys(size(multikey)))
  ikeys(:) = multikey(:)
else
  allocate(ikeys(1))
  ikeys(1) = ikey
endif

!--   Only the RADIX-sort gives the result we want with multiple keys
if (size(ikeys) > 1) imethod = radixsort_method

iinit = .FALSE.
if (present(init)) iinit = init

if (present(index)) then
  iindex => index(1:n)
else
  allocate(iindex(n))
  iinit = .TRUE.
endif

if (iinit) CALL init_index(iindex)

istride = 1
LLtrans = .FALSE.
if (present(transposed)) LLtrans = transposed
if (LLtrans) then
  istride = lda
else if (sda >= 2 .and. lda >= 1) then
!-- Check for presence of sub-array and adjust lda automatically
  call addrdiff(a(1,1),a(1,2),idiff)
  lda = idiff/sizeof_int  ! The true leading dimension; overrides sub-array's one
endif

do j=size(ikeys),1,-1
!--   Sort by the least significant key first
  ikey = abs(ikeys(j))

  if (istride == 1) then
    iptr = lda * (ikey - 1) + 1
  else
    iptr = ikey
  endif

  descending = (ikeys(j) < 0)
  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif

  select case (imethod)
  case (radixsort_method)
    CALL rsort32_func(11, n, istride, iptr, a(1,1), iindex(1), 1, rc)
  case (heapsort_method)
    if (istride == 1) then
      CALL int_heapsort(n, a(1:n, ikey), iindex, rc)
    else
      CALL int_heapsort(n, a(ikey, 1:n), iindex, rc)
    endif
  end select

  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif
enddo

deallocate(ikeys)

if (.not.present(index)) then
  allocate(data(n))

  if (istride == 1) then
    do j=1,sda
      do i=1,n
        data(i) = a(iindex(i),j)
      enddo
      do i=1,n
        a(i,j) = data(i)
      enddo
    enddo
  else
    do i=1,lda
      do j=1,n
        data(j) = a(i,iindex(j))
      enddo
      do j=1,n
        a(i,j) = data(j)
      enddo
    enddo
  endif

  deallocate(data)
  deallocate(iindex)
endif

END SUBROUTINE int_keysort_2D


SUBROUTINE real4_keysort_2D(&
     &rc, a, n,&
     &key, multikey, method,&
     &index, init, transposed)

INTEGER_M, intent(out)           :: rc
REAL_M, intent(inout)         :: a(:,:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: key, method
INTEGER_M, intent(in), OPTIONAL  :: multikey(:)
logical, intent(in), OPTIONAL  :: transposed
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
INTEGER_M, POINTER :: iindex(:)
INTEGER_M :: ikey, istride, imethod
INTEGER_M :: lda, iptr, i, j, sda, idiff
REAL_M, allocatable :: data(:)
INTEGER_M, allocatable :: ikeys(:)
logical iinit, descending, LLtrans

rc = 0
lda = size(a, dim=1)
sda = size(a, dim=2)
if (n <= 0 .or. lda <= 0 .or. sda <= 0) return

imethod = current_method
if (present(method)) then
  imethod = min(max(min_method,method),max_method)
endif

ikey = 1
if (present(key)) ikey = key

if (present(multikey)) then
  allocate(ikeys(size(multikey)))
  ikeys(:) = multikey(:)
else
  allocate(ikeys(1))
  ikeys(1) = ikey
endif

!--   Only the RADIX-sort gives the result we want with multiple keys
if (size(ikeys) > 1) imethod = radixsort_method

iinit = .FALSE.
if (present(init)) iinit = init

if (present(index)) then
  iindex => index(1:n)
else
  allocate(iindex(n))
  iinit = .TRUE.
endif

if (iinit) CALL init_index(iindex)

istride = 1
LLtrans = .FALSE.
if (present(transposed)) LLtrans = transposed
if (LLtrans) then
  istride = lda
else if (sda >= 2 .and. lda >= 1) then
!-- Check for presence of sub-array and adjust lda automatically
  call addrdiff(a(1,1),a(1,2),idiff)
  lda = idiff/sizeof_real4  ! The true leading dimension; overrides sub-array's one
endif

do j=size(ikeys),1,-1
!--   Sort by least significant key first
  ikey = abs(ikeys(j))

  if (istride == 1) then
    iptr = lda * (ikey - 1) + 1
  else
    iptr = ikey
  endif

  descending = (ikeys(j) < 0)
  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif

  select case (imethod)
  case (radixsort_method)
    CALL rsort32_func(13, n, istride, iptr, a(1,1), iindex(1), 1, rc)
  case (heapsort_method)
    if (istride == 1) then
      CALL real4_heapsort(n, a(1:n, ikey), iindex, rc)
    else
      CALL real4_heapsort(n, a(ikey, 1:n), iindex, rc)
    endif
  end select

  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif
enddo

deallocate(ikeys)

if (.not.present(index)) then
  allocate(data(n))

  if (istride == 1) then
    do j=1,sda
      do i=1,n
        data(i) = a(iindex(i),j)
      enddo
      do i=1,n
        a(i,j) = data(i)
      enddo
    enddo
  else
    do i=1,lda
      do j=1,n
        data(j) = a(i,iindex(j))
      enddo
      do j=1,n
        a(i,j) = data(j)
      enddo
    enddo
  endif

  deallocate(data)
  deallocate(iindex)
endif

END SUBROUTINE real4_keysort_2D


SUBROUTINE real8_keysort_2D(&
     &rc, a, n,&
     &key, multikey, method,&
     &index, init, transposed)

INTEGER_M, intent(out)           :: rc
REAL_B, intent(inout)         :: a(:,:)
INTEGER_M, intent(in)            :: n
INTEGER_M, intent(in), OPTIONAL  :: key, method
INTEGER_M, intent(in), OPTIONAL  :: multikey(:)
logical, intent(in), OPTIONAL  :: transposed
INTEGER_M, intent(inout), TARGET, OPTIONAL :: index(:)
logical, intent(in), OPTIONAL  :: init
! === END OF INTERFACE BLOCK ===
INTEGER_M, POINTER :: iindex(:)
INTEGER_M :: ikey, istride, imethod
INTEGER_M :: lda, iptr, i, j, sda, idiff
REAL_B, allocatable :: data(:)
INTEGER_M, allocatable :: ikeys(:)
logical iinit, descending, LLtrans

rc = 0
lda = size(a, dim=1)
sda = size(a, dim=2)
if (n <= 0 .or. lda <= 0 .or. sda <= 0) return

imethod = current_method
if (present(method)) then
  imethod = min(max(min_method,method),max_method)
endif

ikey = 1
if (present(key)) ikey = key

if (present(multikey)) then
  allocate(ikeys(size(multikey)))
  ikeys(:) = multikey(:)
else
  allocate(ikeys(1))
  ikeys(1) = ikey
endif

!--   Only the RADIX-sort gives the result we want with multiple keys
if (size(ikeys) > 1) imethod = radixsort_method

iinit = .FALSE.
if (present(init)) iinit = init

if (present(index)) then
  iindex => index(1:n)
else
  allocate(iindex(n))
  iinit = .TRUE.
endif

if (iinit) CALL init_index(iindex)

istride = 1
LLtrans = .FALSE.
if (present(transposed)) LLtrans = transposed
if (LLtrans) then
  istride = lda
else if (sda >= 2 .and. lda >= 1) then
!-- Check for presence of sub-array and adjust lda automatically
  call addrdiff(a(1,1),a(1,2),idiff)
  lda = idiff/sizeof_real8  ! The true leading dimension; overrides sub-array's one
endif

do j=size(ikeys),1,-1
!--   Sort by least significant key first
  ikey = abs(ikeys(j))

  if (istride == 1) then
    iptr = lda * (ikey - 1) + 1
  else
    iptr = ikey
  endif

  descending = (ikeys(j) < 0)
  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif

  select case (imethod)
  case (radixsort_method)
    CALL rsort32_func(12, n, istride, iptr, a(1,1), iindex(1), 1, rc)
  case (heapsort_method)
    if (istride == 1) then
      CALL real8_heapsort(n, a(1:n, ikey), iindex, rc)
    else
      CALL real8_heapsort(n, a(ikey, 1:n), iindex, rc)
    endif
  end select

  if (descending) then
    if (istride == 1) then
      a(1:n,ikey) = -a(1:n,ikey)
    else
      a(ikey, 1:n) = -a(ikey, 1:n)
    endif
  endif
enddo

deallocate(ikeys)

if (.not.present(index)) then
  allocate(data(n))

  if (istride == 1) then
    do j=1,sda
      do i=1,n
        data(i) = a(iindex(i),j)
      enddo
      do i=1,n
        a(i,j) = data(i)
      enddo
    enddo
  else
    do i=1,lda
      do j=1,n
        data(j) = a(i,iindex(j))
      enddo
      do j=1,n
        a(i,j) = data(j)
      enddo
    enddo
  endif

  deallocate(data)
  deallocate(iindex)
endif

END SUBROUTINE real8_keysort_2D

!-----------------------------
!--   Private subroutines   --
!-----------------------------

SUBROUTINE int_heapsort(n, a, index, rc)

INTEGER_M, intent(in)  :: n
INTEGER_M, intent(in)  :: a(:)
INTEGER_M, intent(inout) :: index(:), rc
INTEGER_M :: i,j,right,left, idx
INTEGER_M :: tmp
rc = n
if (n <= 1) return
left  = n/2+1
right = n
LOOP: do
  if (left > 1) then
    left = left - 1
    idx  = index(left)
  else
    idx = index(right)
    index(right) = index(1)
    right = right - 1
    if (right == 1) then
      index(1) = idx
      exit LOOP
    endif
  endif
  tmp = a(idx)
  i = left
  j = 2*left
  do while (j <= right)
    if (j < right) then
      if (a(index(j)) < a(index(j+1))) j = j + 1
    endif
    if (tmp < a(index(j))) then
      index(i) = index(j)
      i = j
      j = 2*j
    else
      j = right + 1
    endif
  enddo
  index(i) = idx
enddo LOOP
END SUBROUTINE int_heapsort


SUBROUTINE real4_heapsort(n, a, index, rc)

INTEGER_M, intent(in)  :: n
REAL_M, intent(in)  :: a(:)
INTEGER_M, intent(inout) :: index(:), rc
INTEGER_M :: i,j,right,left, idx
REAL_M :: tmp
rc = n
if (n <= 1) return
left  = n/2+1
right = n
LOOP: do
  if (left > 1) then
    left = left - 1
    idx  = index(left)
  else
    idx = index(right)
    index(right) = index(1)
    right = right - 1
    if (right == 1) then
      index(1) = idx
      exit LOOP
    endif
  endif
  tmp = a(idx)
  i = left
  j = 2*left
  do while (j <= right)
    if (j < right) then
      if (a(index(j)) < a(index(j+1))) j = j + 1
    endif
    if (tmp < a(index(j))) then
      index(i) = index(j)
      i = j
      j = 2*j
    else
      j = right + 1
    endif
  enddo
  index(i) = idx
enddo LOOP
END SUBROUTINE real4_heapsort


SUBROUTINE real8_heapsort(n, a, index, rc)

INTEGER_M, intent(in)  :: n
REAL_B, intent(in)  :: a(:)
INTEGER_M, intent(inout) :: index(:), rc
INTEGER_M :: i,j,right,left, idx
REAL_B :: tmp
rc = n
if (n <= 1) return
left  = n/2+1
right = n
LOOP: do
  if (left > 1) then
    left = left - 1
    idx  = index(left)
  else
    idx = index(right)
    index(right) = index(1)
    right = right - 1
    if (right == 1) then
      index(1) = idx
      exit LOOP
    endif
  endif
  tmp = a(idx)
  i = left
  j = 2*left
  do while (j <= right)
    if (j < right) then
      if (a(index(j)) < a(index(j+1))) j = j + 1
    endif
    if (tmp < a(index(j))) then
      index(i) = index(j)
      i = j
      j = 2*j
    else
      j = right + 1
    endif
  enddo
  index(i) = idx
enddo LOOP
END SUBROUTINE real8_heapsort

END MODULE ECsort
