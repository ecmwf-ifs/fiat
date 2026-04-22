! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_no_hw_fpe_Apple_Silicon_SIGILL

  use yomhook, only : jphook, dr_hook

  implicit none

  real(jphook) :: zhook_handle

  interface
    subroutine raise_sigill() bind(C, name="raise_sigill")
    end subroutine
  end interface

  call dr_hook('drhook_no_hw_fpe_Apple_Silicon_SIGILL', 0, zhook_handle)

  ! Ensure that DrHook still processes legitamate SIGILLs, as signal_drhook()
  ! converts SIGFPEs posing as SIGILLs to SIGFPEs.
  ! https://developer.apple.com/forums/thread/689159?answerId=733736022
  call raise_sigill()

  call dr_hook('drhook_no_hw_fpe_Apple_Silicon_SIGILL', 1, zhook_handle)

end program drhook_no_hw_fpe_Apple_Silicon_SIGILL

