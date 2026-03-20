! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_no_hw_fpe_basic

use yomhook, only : jphook, dr_hook

implicit none

real(jphook) :: zhook_handle, zhook_handle_a
real :: x
real, volatile :: y ! This is needed because some compilers are smart enough to perform constant folding at compile time, which prevents FPE flags being set a runtime
call dr_hook('drhook_no_hw_fpe_basic', 0, zhook_handle)


! DrHook can't see this, so it thinks FPEs are still enabled & gives the behaviour we want, but the test won't fail to an FPE
call silently_disable_all_fpes()

! This is needed because some compilers are smart enough to throw an error at compile time
y = 0.0
x = 1.0/y

call dr_hook('drhook_no_hw_fpe_basic', 1, zhook_handle)

print *, "Hello, x is ", x

end program drhook_no_hw_fpe_basic

