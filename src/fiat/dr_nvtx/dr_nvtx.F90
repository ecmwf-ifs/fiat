module dr_nvtx

use iso_c_binding
implicit none

interface

subroutine dr_nvtx_start (name)
use iso_c_binding
character(kind=c_char,len=*) :: name
end subroutine

subroutine dr_nvtx_end (name)
use iso_c_binding
character(kind=c_char,len=*) :: name
end subroutine

end interface

public :: dr_nvtx_push_range
public :: dr_nvtx_pop_range

contains

subroutine dr_nvtx_push_range (fstr)
character(kind=c_char,len=*), intent(in) :: fstr
character(kind=c_char,len=1024) :: cstr

!$omp master

cstr=trim(fstr)//c_null_char
call dr_nvtx_start (cstr)

!$omp end master

end subroutine 

subroutine dr_nvtx_pop_range (fstr)
character(kind=c_char,len=*), intent(in) :: fstr
character(kind=c_char,len=1024) :: cstr

!$omp master

cstr=trim(fstr)//c_null_char
call dr_nvtx_end (cstr)

!$omp end master

end subroutine 

end module dr_nvtx

