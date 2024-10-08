! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_nvtx_skip_spam_regions

use yomhook, only : jphook, dr_hook

implicit none

real(jphook) :: zhook_handle
call dr_hook('drhook_nvtx_skip_spam_regions', 0, zhook_handle)
call foo(0)
call dr_hook('drhook_nvtx_skip_spam_regions', 1, zhook_handle)

contains

  recursive subroutine foo (depth)
    integer, intent(in) :: depth
    real(jphook) :: zhook_handle

    call dr_hook('foo', 0, zhook_handle)

    if (depth < 10) then
      call foo(depth + 1)
    end if

    call dr_hook('foo', 1, zhook_handle)
  end subroutine

end program drhook_nvtx_skip_spam_regions

