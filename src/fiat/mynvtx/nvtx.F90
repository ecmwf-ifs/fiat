
!!mpif90 nvtx.f90 -Mpreprocess -c -O2

#ifndef NVTX_PROFILE
#define NVTX_PROFILE 1
#endif

module mynvtx
   use iso_c_binding
   implicit none
     interface
        subroutine mynvtxstart(name)
           use iso_c_binding
           character(kind=c_char,len=*) :: name
        end subroutine
        subroutine nvtxRangePop() bind(c,name="nvtxRangePop")
        end subroutine
     end interface
     PUBLIC :: PUSH_RANGE
     PUBLIC :: POP_RANGE
     contains
  subroutine PUSH_RANGE(fstr)
    character(kind=c_char,len=*), intent(in) :: fstr
#if NVTX_PROFILE != 0
    character(kind=c_char,len=1024) :: cstr
    !$omp master

    cstr=trim(fstr)//c_null_char

    call mynvtxstart(cstr)
    !$omp end master
#endif
  end subroutine PUSH_RANGE

  subroutine POP_RANGE(fstr)
    character(kind=c_char,len=*), intent(in) :: fstr
#ifdef NVTX_VERYVERBOSE
    character(kind=c_char,len=1024) :: cstr
#endif
    !$omp master
#if NVTX_PROFILE != 0
#ifdef NVTX_VERYVERBOSE
    cstr=trim(fstr)//c_null_char
    call mynvtxend(cstr)
#else
    call mynvtxend()
#endif

!!    call nvtxRangePop
#endif
!$omp end master
  end subroutine POP_RANGE
end module mynvtx

