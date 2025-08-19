! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program drhook_papi_mpi
  use mpl_module
  use yomhook, only : jphook, dr_hook
  use sdl_mod, only : sdl_traceback
  implicit none
  integer jpe, npes, mype, a
  character(len=256) arg, env
  real(jphook) :: zhook_handle

  call mpl_init(ldinfo=.false.)
  call dr_hook('drhook_papi_mpi',0,zhook_handle)

  npes = mpl_nproc()
  mype = mpl_myrank()

  do jpe=1,npes
    if (mype == jpe) then
      a = a + jpe
    endif
  enddo

  call mpl_barrier()
  call dr_hook('drhook_papi_mpi',1,zhook_handle)
  call mpl_end()
end program drhook_papi_mpi
