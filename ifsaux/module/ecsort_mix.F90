#ifdef RS6K
@PROCESS NOCHECK
#endif
MODULE ecsort_mix
USE PARKIND1   , ONLY : JPIM     ,JPIB     ,JPRB     ,JPRM
USE YOMHOOK    , ONLY : LHOOK, DR_HOOK
USE OML_MOD     , ONLY : OML_MAX_THREADS, OML_MY_THREAD, OML_IN_PARALLEL
USE STRHANDLER_MOD , ONLY : TOUPPER
USE MPL_MODULE , ONLY : MPL_MYRANK, MPL_NPROC

!$
!..   Author: Sami Saarinen, ECMWF, 10/02/98
!     Fixes : Sami Saarinen, ECMWF, 08/11/99 : Sub-arrays go now correctly (look for addrdiff)
!                                              Genuine real(4) sort "re-habilitated"
!                                              sizeof_int, _real4 & _real8 HARDCODED !
!             Sami Saarinen, ECMWF, 11/10/00 : REAL*4 version included (REAL_M)
!             Sami Saarinen, ECMWF, 28/11/03 : Calls to DR_HOOK added manually (on top of CY28)
!             Sami Saarinen, ECMWF, 18/02/05 : 64-bit integer sorting introduced (for CY30)
!             Sami Saarinen, ECMWF, 22/02/05 : Using genuine 64-bit rsort64() => one-pass through data
!             Sami Saarinen, ECMWF, 06/07/05 : "current_method" made OpenMP-thread aware (for max. # of threads = NTHRDS)
!             Sami Saarinen, ECMWF, 07/07/05 : Quick-sort method finally arrived (and applicable to multikeys, too)
!                                              Quick-sort the default for scalar machines ("non-VPP"), VPPs is radix-sort
!             Sami Saarinen, ECMWF, 03/07/07 : Quick-sort method uses stable approach (was not guaranteed so before)
!             Sami Saarinen, ECMWF, 15/10/07 : Subroutines put into a common file ../include/ecsort_shared.h and
!                                              preprocessed from there
!             Sami Saarinen, ECMWF, 15/10/07 : NTHRDS increased from 32 to 64
!             Sami Saarinen, ECMWF, 16/10/07 : The default sorting method can be overriden via export EC_SORTING_METHOD=[<number>|<string>]
!             Sami Saarinen, ECMWF, 30/10/07 : Support for CountingSort added as part of QuickSort speedup
!             Sami Saarinen, ECMWF, 31/10/07 : CALL SORTING_METHOD() now prints the prevailing method from EC_SORTING_METHOD
!             Sami Saarinen, ECMWF, 01/11/07 : CountingSort implemented as independent method
!             Sami Saarinen, ECMWF, 06/11/07 : index_adj added as an optional argument to init_index ; new routine adjust_index()
!             Sami Saarinen, ECMWF, 07/11/07 : threshold length "nomp" (see below). Override with EC_SORTING_NOMP.
!             Sami Saarinen, ECMWF, 12/11/07 : Gnome-sort -- the easiest sort on Earth (and very slow for large arrays)
!             Sami Saarinen, ECMWF, 15/11/07 : OpenMP-sorting still under development. Do NOT override the EC_SORTING_NOMP yet.
!             Sami Saarinen, ECMWF, 05/12/07 : When export EC_SORTING_INFO=0, then no info messages are printed from CALL sorting_method()
!             Sami Saarinen, ECMWF, 20/12/07 : export EC_SORTING_INFO=0, rather than =1 is now the default --> less hassling output


IMPLICIT NONE
SAVE
PRIVATE

INTEGER(KIND=JPIM), parameter :: NTHRDS = 64 ! ***Note: A hardcoded max number of threads !!!

INTEGER(KIND=JPIM), parameter :: sizeof_int4  = 4
INTEGER(KIND=JPIM), parameter :: sizeof_int8  = 8
INTEGER(KIND=JPIM), parameter :: sizeof_real4 = 4
INTEGER(KIND=JPIM), parameter :: sizeof_real8 = 8

INTEGER(KIND=JPIM), parameter :: min_method = 1
INTEGER(KIND=JPIM), parameter :: max_method = 5

INTEGER(KIND=JPIM), parameter :: radixsort_method = 1
INTEGER(KIND=JPIM), parameter :: heapsort_method  = 2
INTEGER(KIND=JPIM), parameter :: quicksort_method = 3
INTEGER(KIND=JPIM), parameter :: countingsort_method = 4
INTEGER(KIND=JPIM), parameter :: gnomesort_method = 5

CHARACTER(LEN=12), parameter :: method_name(min_method:max_method) = &
     & (/&
     &   'RADIXSORT   ' &
     &  ,'HEAPSORT    ' &
     &  ,'QUICKSORT   ' &
     &  ,'COUNTINGSORT' &
     &  ,'GNOMESORT   ' &
     &  /)

!-- Select such method for default_method, which also works for multikey sorts
!   Vector machines should choose radixsort_method, others quicksort_method (oh, sorry, countingsort_method!)
!
!   Note: Occasionally radixsort_method may be faster on non-vector machines, too
#if defined(VPP) || defined(NECSX)
INTEGER(KIND=JPIM) :: default_method = radixsort_method
INTEGER(KIND=JPIM) :: current_method(NTHRDS) = radixsort_method
#else
INTEGER(KIND=JPIM) :: default_method = countingsort_method
INTEGER(KIND=JPIM) :: current_method(NTHRDS) = countingsort_method
#endif

!-- A threshold length after which OpenMP in sorting, merging, copying may kick in.
!   Override with EC_SORTING_NOMP. Detected while initializing/calling SORTING_METHOD
!   Non-positive values (<= 0) indicate that OpenMP will NOT be attempted at all
INTEGER(KIND=JPIM) :: nomp = -1

!-- EC_SORTING_INFO = MPL-proc id [1..$NPES] to print the info message when CALL sorting_method()
!                   : 0 (no print; the default)
!                   : 1 (print on MPL-task one)
!                   : > 1 and <= $NPES (some other MPL-task than 1 prints)
INTEGER(KIND=JPIM) :: nsinfo = 0

INTERFACE keysort
MODULE PROCEDURE &
     &int4_keysort_1D, int4_keysort_2D, &
     &int8_keysort_1D, int8_keysort_2D, &
     &real8_keysort_1D, real8_keysort_2D, &
     &real4_keysort_1D, real4_keysort_2D
END INTERFACE

INTERFACE sorting_method
MODULE PROCEDURE int_sorting_method, str_sorting_method
END INTERFACE

PUBLIC :: keysort
PUBLIC :: init_index, get_rank, adjust_index
PUBLIC :: sorting_method

CONTAINS

!----------------------------
!--   Public subroutines   --
!----------------------------

SUBROUTINE int_sorting_method(inew, iold)
INTEGER(KIND=JPIM), OPTIONAL, intent(in)  :: inew
INTEGER(KIND=JPIM), OPTIONAL, intent(out) :: iold
INTEGER(KIND=JPIM) :: itmp, imyproc, inpes
INTEGER(KIND=JPIM) :: ITID
character(len=20) clenv
logical, save :: LLfirst = .TRUE.
logical LLomp, LLhook_ok
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-- This maybe called from the very first call of DR_HOOK_UTIL ...
LLhook_ok = LHOOK .and. (present(inew) .or. present(iold))
IF (LLhook_ok) CALL DR_HOOK('ECSORT_MIX:INT_SORTING_METHOD',0,ZHOOK_HANDLE)
ITID = OML_MY_THREAD()
LLomp = OML_IN_PARALLEL()
if (present(iold)) iold = current_method(ITID)
itmp = -1
if (present(inew)) then
  itmp = inew
else if (.not.LLomp) then ! Override the default method (only if outside the OpenMP)
  itmp = -1 ! no change
  if (LLfirst) then ! Do once per execution only
    inpes = MPL_NPROC()
    CALL ec_getenv('EC_SORTING_INFO',clenv) ! ../support/env.c
    if (clenv /= ' ') then
      itmp = nsinfo
      read(clenv,'(i20)',end=89,err=89) itmp
      goto 88
89    continue
      itmp = nsinfo ! no change
88    continue
      if (itmp <= 0) then
        nsinfo = 0
      else if (itmp >= 1 .and. itmp <= inpes) then
        nsinfo = itmp
      endif
    endif
    imyproc = MPL_MYRANK()
    CALL ec_getenv('EC_SORTING_METHOD',clenv) ! ../support/env.c
    if (clenv /= ' ') then
       if (imyproc == nsinfo) write(0,'(a)')'<EC_SORTING_METHOD='//trim(clenv)
       CALL toupper(clenv)
       select case (clenv)
       case ('RADIX', 'RADIXSORT')
          itmp = radixsort_method
       case ('HEAP', 'HEAPSORT')
          itmp = heapsort_method
       case ('QUICK', 'QUICKSORT', 'QSORT')
          itmp = quicksort_method
       case ('COUNT', 'COUNTINGSORT', 'COUNTSORT')
          itmp = countingsort_method
       case ('GNOME', 'GNOMESORT')
          itmp = gnomesort_method
       case ('DEFAULT', 'DEF')
          itmp = -1 ! no change
       case default
          read(clenv,'(i20)',end=99,err=99) itmp
          goto 98
99        continue
          itmp = -1 ! no change
98        continue
       end select
    endif

    if (itmp < min_method .or. itmp > max_method) itmp = default_method
    default_method = itmp
    current_method(:) = itmp
    if (imyproc == nsinfo) &
         & write(0,'(a,i1,a)')'>EC_SORTING_METHOD=',default_method,&
         & ' # '//method_name(default_method)

    CALL ec_getenv('EC_SORTING_NOMP',clenv)
    if (clenv /= ' ') then
       if (imyproc == nsinfo) write(0,'(a)')'<EC_SORTING_NOMP='//trim(clenv)
       read(clenv,'(i20)',end=199,err=199) itmp
       goto 198
199    continue
       itmp = nomp ! no change
198    continue
       nomp = itmp
    endif

    if (imyproc == nsinfo) then
      write(clenv,'(i20)') nomp
      write(0,'(a)')'>EC_SORTING_NOMP='//trim(adjustl(clenv))
    endif

    itmp = default_method
    LLfirst = .FALSE.
  endif
endif

if (itmp < min_method .or. itmp > max_method) itmp = default_method
if (LLomp) then ! Only this thread sees the change
  current_method(ITID) = itmp
else ! All threads see the change
  current_method(:) = itmp
endif
IF (LLhook_ok) CALL DR_HOOK('ECSORT_MIX:INT_SORTING_METHOD',1,ZHOOK_HANDLE)
END SUBROUTINE int_sorting_method


SUBROUTINE str_sorting_method(cdnew, iold)
character(len=*), intent(in) :: cdnew
INTEGER(KIND=JPIM), OPTIONAL, intent(out) :: iold
character(len=len(cdnew)) clnew
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:STR_SORTING_METHOD',0,ZHOOK_HANDLE)
clnew = cdnew
CALL toupper(clnew)
select case (clnew)
case ('RADIX', 'RADIXSORT')
  CALL sorting_method(radixsort_method, iold)
case ('HEAP', 'HEAPSORT')
  CALL sorting_method(heapsort_method, iold)
case ('QUICK', 'QUICKSORT', 'QSORT')
  CALL sorting_method(quicksort_method, iold)
case ('COUNT', 'COUNTINGSORT', 'COUNTSORT')
  CALL sorting_method(countingsort_method, iold)
case ('GNOME', 'GNOMESORT')
  CALL sorting_method(gnomesort_method, iold)
case ('DEFAULT', 'DEF')
  CALL sorting_method(default_method, iold)
case default
  CALL sorting_method(default_method, iold)
end select
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:STR_SORTING_METHOD',1,ZHOOK_HANDLE)
END SUBROUTINE str_sorting_method


SUBROUTINE init_index(index, index_adj)
INTEGER(KIND=JPIM), intent(out):: index(:)
INTEGER(KIND=JPIM), intent(in), OPTIONAL :: index_adj
INTEGER(KIND=JPIM) :: i, n
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:INIT_INDEX',0,ZHOOK_HANDLE)
n = size(index)
if (present(index_adj)) then
  do i=1,n
    index(i) = i + index_adj
  enddo
else
  do i=1,n
    index(i) = i
  enddo
endif
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:INIT_INDEX',1,ZHOOK_HANDLE)
END SUBROUTINE init_index


SUBROUTINE adjust_index(index, index_adj)
INTEGER(KIND=JPIM), intent(inout):: index(:)
INTEGER(KIND=JPIM), intent(in) :: index_adj
INTEGER(KIND=JPIM) :: i, n
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:ADJUST_INDEX',0,ZHOOK_HANDLE)
if (index_adj /= 0) then
  n = size(index)
  do i=1,n
    index(i) = index(i) + index_adj
  enddo
endif
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:ADJUST_INDEX',1,ZHOOK_HANDLE)
END SUBROUTINE adjust_index


SUBROUTINE get_rank(index, rank, index_adj)
INTEGER(KIND=JPIM), intent(in) :: index(:)
INTEGER(KIND=JPIM), intent(out):: rank(:)
INTEGER(KIND=JPIM), intent(in), OPTIONAL :: index_adj
INTEGER(KIND=JPIM) :: i, n
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:GET_RANK',0,ZHOOK_HANDLE)
n = min(size(index),size(rank))
if (present(index_adj)) then
  do i=1,n
    rank(index(i)+index_adj) = i
  enddo
else
  do i=1,n
    rank(index(i)) = i
  enddo
endif
IF (LHOOK) CALL DR_HOOK('ECSORT_MIX:GET_RANK',1,ZHOOK_HANDLE)
END SUBROUTINE get_rank


#undef INT_VERSION
#undef REAL_VERSION

!-- Create version for INTEGER(KIND=JPIM)
#define INT_VERSION   4
#define REAL_VERSION  0
#include "ecsort_shared.h"
#undef INT_VERSION
#undef REAL_VERSION

!-- Create version for INTEGER(KIND=JPIB)
#define INT_VERSION   8
#define REAL_VERSION  0
#include "ecsort_shared.h"
#undef INT_VERSION
#undef REAL_VERSION

!-- Create version for REAL(KIND=JPRM)
#define INT_VERSION   0
#define REAL_VERSION  4
#include "ecsort_shared.h"
#undef INT_VERSION
#undef REAL_VERSION

!-- Create version for REAL(KIND=JPRB)
#define INT_VERSION   0
#define REAL_VERSION  8
#include "ecsort_shared.h"
#undef INT_VERSION
#undef REAL_VERSION

END MODULE ecsort_mix
