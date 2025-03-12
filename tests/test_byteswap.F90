! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

!
! Simple Test program
!
subroutine fail_impl(msg,line)
  character(*) :: msg
  integer :: line
  write(0,'(A,I0,A)') "TEST FAILED in test_byteswap.F90 @ line ",line," :"
  write(0,*) msg
  stop 1
end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)

program byteswap_test
use byteswap_mod
use, intrinsic :: iso_c_binding
implicit none

call test_convert_to_big_endian_int32()
call test_convert_to_big_endian_int64()
call test_convert_to_big_endian_real32()
call test_convert_to_big_endian_real64()

call test_byteswap_int32()
call test_byteswap_int64()
call test_byteswap_real32()
call test_byteswap_real64()

write(0,'(A)') "SUCCESS"

CONTAINS

  subroutine test_convert_to_big_endian_int32()
    integer :: i
    integer(c_int32_t) :: buffer(3), buffer_BE(3)
    character(len=8) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_convert_to_big_endian_int32"
    buffer = [12345678,23456789,34567890]

    call convert_to_big_endian(buffer_BE, buffer)
    !call iswap(buffer_BE, buffer, 4, 3)

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z8.8)') transfer(buffer(i),0_c_int32_t)
        write(hex_BE(i),'(Z8.8)') transfer(buffer_BE(i),0_c_int32_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["4E61BC00","15EC6501","D2760F02"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_convert_to_big_endian_int64()
    integer :: i
    integer(c_int64_t) :: buffer(3), buffer_BE(3)
    character(len=16) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_convert_to_big_endian_int64"
    buffer = [123456789012345678_c_int64_t,&
              234567890123456789_c_int64_t,&
              345678901234567890_c_int64_t]

    call convert_to_big_endian(buffer_BE, buffer)
    !call iswap(buffer_BE, buffer, 8, 3)

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z16.16)') transfer(buffer(i),0_c_int64_t)
        write(hex_BE(i),'(Z16.16)') transfer(buffer_BE(i),0_c_int64_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["4EF330A64B9BB601","158185D6405A4103","D20A6F122119CC04"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_convert_to_big_endian_real32()
    integer :: i
    real(c_float) :: buffer(3), buffer_BE(3)
    character(len=8) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_convert_to_big_endian_real32"
    buffer = [1.234_c_float,2.345_c_float,3.456_c_float]

    call convert_to_big_endian(buffer_BE, buffer)
    !call iswap(buffer_BE, buffer, 4, 3)

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z8.8)') transfer(buffer(i),0_c_int32_t)
        write(hex_BE(i),'(Z8.8)') transfer(buffer_BE(i),0_c_int32_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["B6F39D3F","7B141640","1B2F5D40"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_convert_to_big_endian_real64()
    integer :: i
    real(c_double) :: buffer(3), buffer_BE(3)
    character(len=16) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_convert_to_big_endian_real64"
    buffer = [1.234567890123_c_double, &
              2.345678901234_c_double, &
              3.456789012345_c_double]

    call convert_to_big_endian(buffer_BE, buffer)
    !call iswap(buffer_BE, buffer, 8, 3)

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z16.16)') transfer(buffer(i),0_c_int64_t)
        write(hex_BE(i),'(Z16.16)') transfer(buffer_BE(i),0_c_int64_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["F2518C42CAC0F33F","EABCBD4CF3C30240","238D69FF80A70B40"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_byteswap_int32()
    integer :: i
    integer(c_int32_t) :: buffer(3), buffer_BE(3)
    character(len=8) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_byteswap_int32"
    buffer = [12345678,23456789,34567890]

    if (is_little_endian()) then
      call byteswap(buffer_BE, buffer)
      !call jswap(buffer_BE, buffer, 4, 3)
    else
      buffer_BE = buffer
    endif

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z8.8)') transfer(buffer(i),0_c_int32_t)
        write(hex_BE(i),'(Z8.8)') transfer(buffer_BE(i),0_c_int32_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["4E61BC00","15EC6501","D2760F02"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_byteswap_int64()
    integer :: i
    integer(c_int64_t) :: buffer(3), buffer_BE(3)
    character(len=16) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_byteswap_int64"
    buffer = [123456789012345678_c_int64_t,&
              234567890123456789_c_int64_t,&
              345678901234567890_c_int64_t]

    if (is_little_endian()) then
      call byteswap(buffer_BE, buffer)
      !call jswap(buffer_BE, buffer, 8, 3)
    else
      buffer_BE = buffer
    endif

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z16.16)') transfer(buffer(i),0_c_int64_t)
        write(hex_BE(i),'(Z16.16)') transfer(buffer_BE(i),0_c_int64_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["4EF330A64B9BB601","158185D6405A4103","D20A6F122119CC04"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_byteswap_real32()
    integer :: i
    real(c_float) :: buffer(3), buffer_BE(3)
    character(len=8) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_byteswap_real32"
    buffer = [1.234_c_float,2.345_c_float,3.456_c_float]

    if (is_little_endian()) then
      call byteswap(buffer_BE, buffer)
      !call jswap(buffer_BE, buffer, 4, 3)
    else
      buffer_BE = buffer
    endif

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z8.8)') transfer(buffer(i),0_c_int32_t)
        write(hex_BE(i),'(Z8.8)') transfer(buffer_BE(i),0_c_int32_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["B6F39D3F","7B141640","1B2F5D40"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine

  subroutine test_byteswap_real64()
    integer :: i
    real(c_double) :: buffer(3), buffer_BE(3)
    character(len=16) :: hex(3), hex_BE(3)
    write(0,'(A)') "test_byteswap_real64"
    buffer = [1.234567890123_c_double, &
              2.345678901234_c_double, &
              3.456789012345_c_double]

    if (is_little_endian()) then
      call byteswap(buffer_BE, buffer)
      !call jswap(buffer_BE, buffer, 8, 3)
    else
      buffer_BE = buffer
    endif

    if (is_little_endian()) then
      do i=1,3
        write(hex(i),'(Z16.16)') transfer(buffer(i),0_c_int64_t)
        write(hex_BE(i),'(Z16.16)') transfer(buffer_BE(i),0_c_int64_t)
        write(0,*) "   ", hex(i), " --> " , hex_BE(i)
      enddo
      if (any(hex_BE /= ["F2518C42CAC0F33F","EABCBD4CF3C30240","238D69FF80A70B40"])) then
        FAIL("conversion to big_endian failed")
      endif
    else
      if (any(buffer /= buffer_BE) ) then
        FAIL("buffer_BE should be copy of buffer on big_endian architecture")
      endif
    endif
  end subroutine


end program
