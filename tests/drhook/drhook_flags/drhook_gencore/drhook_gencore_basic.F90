! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_gencore_basic
  use yomhook, only : jphook, dr_hook

  implicit none
  real(jphook) :: zhook_handle

  call dr_hook('drhook_gencore_basic',0,zhook_handle)

  call raise(11)

  call dr_hook('drhook_gencore_basic',1,zhook_handle)
end program drhook_gencore_basic
