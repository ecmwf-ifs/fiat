! (C) Copyright 2024- ECMWF.
! (C) Copyright 2024- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

program fiat_test_drhook_nvtx_basic

use yomhook, only : jphook, dr_hook

implicit none

real(jphook) :: zhook_handle

call dr_hook('fiat_test_drhook_nvtx_basic',0,zhook_handle)

call sub (0)

call dr_hook('fiat_test_drhook_nvtx_basic',1,zhook_handle)

contains 

recursive subroutine sub (depth)

integer :: depth

character(len=128) :: clname
real(jphook) :: zhook_handle

integer :: i

if (depth > irand (2, 4)) return

do i = 1, len (clname)
  clname (i:i) = ' '
enddo

do i = 1, 16
  clname (i:i) = char (irand (ichar ('A'), ichar ('Z')))
enddo

do i = 1, depth
  write (*, '("  ")', advance='no') 
enddo

write (*, '(" - ",A)') clname (1:16)

call dr_hook(clname,0,zhook_handle)

call sleep (real (irand (10, 200)))


do i = 1, irand (1, 4)
  call sub (depth+1)
  call sleep (real (irand (10, 200)))
enddo

call dr_hook(clname,1,zhook_handle)

end subroutine

subroutine sleep (dt)

implicit none

real, intent (in) :: dt                

integer, dimension (8) :: t 
integer :: s1,s2,ms1,ms2  

call date_and_time(values=t)
ms1=(t(5)*3600+t(6)*60+t(7))*1000+t(8)

do 
  call date_and_time(values=t)
  ms2=(t(5)*3600+t(6)*60+t(7))*1000+t(8)
  if(ms2-ms1>=dt)exit
enddo

end subroutine sleep

integer function irand (k1, k2) 

integer :: k1, k2

integer*8, save :: x = 2713

x = modulo (16807_8 * x, 2147483647_8)

irand = k1 + modulo (x, int (k2-k1+1, 8)) 

end function

end program fiat_test_drhook_nvtx_basic

