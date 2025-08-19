! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_roctx_mismatched_regions

use yomhook, only : jphook, dr_hook

implicit none

real(jphook) :: zhook_handle, zhook_handle_a
integer :: a, b
call dr_hook('drhook_roctx_mismatched_regions', 0, zhook_handle)

! Region A Start
call dr_hook('a', 0, zhook_handle_a)
a = 1

! Region A Stop
call dr_hook('this_is_not_a!', 1, zhook_handle_a)

call dr_hook('drhook_roctx_mismatched_regions', 1, zhook_handle)

end program drhook_roctx_mismatched_regions

