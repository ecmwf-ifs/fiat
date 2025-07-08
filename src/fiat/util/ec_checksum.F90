! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

module ec_checksum_mod

!**** Checksum of arrays
!
!     Purpose.
!     --------
!     Take the checksum of an array printed as 4-char string
!
!     An example Fortran programs:
!
!         program main
!             use ec_checksum_mod, only: fletcher16, fletcher16_hex
!             write(0,*) fletcher16(array(:,:))     ! as integer
!             write(0,*) fletcher16_hex(array(:,:)) ! as hex-string
!         end program
!
!     Multiple arrays can be checksummed together to a single hash:
!
!         program main
!             use ec_checksum_mod, only: fletcher16_type
!             type(fletcher16_type) :: checksum
!             call checksum%update(array1(:,:))
!             call checksum%update(array2(:,:,:))
!             write(0,*) checksum%digest_hex()
!         end program
!
!     Author.
!     -------
!        W.Deconinck, ECMWF
!
!     Modifications.
!     --------------
!        Original: 2025-07-04
!
!     ------------------------------------------------------------------
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t, c_int16_t, c_char
  implicit none

private
public :: fletcher16_type, fletcher16, fletcher16_hex

type :: fletcher16_type
  integer(c_int32_t) :: handle = 0
contains
  procedure, public  :: reset => fletcher16_reset
  procedure, private :: update_real32_r1 => fletcher16_update_real32_r1
  procedure, private :: update_real32_r2 => fletcher16_update_real32_r2
  procedure, private :: update_real32_r3 => fletcher16_update_real32_r3
  procedure, private :: update_real32_r4 => fletcher16_update_real32_r4
  procedure, private :: update_real32_r5 => fletcher16_update_real32_r5
  procedure, private :: update_real64_r1 => fletcher16_update_real64_r1
  procedure, private :: update_real64_r2 => fletcher16_update_real64_r2
  procedure, private :: update_real64_r3 => fletcher16_update_real64_r3
  procedure, private :: update_real64_r4 => fletcher16_update_real64_r4
  procedure, private :: update_real64_r5 => fletcher16_update_real64_r5
  procedure, private :: update_int32_r1  => fletcher16_update_int32_r1
  procedure, private :: update_int32_r2  => fletcher16_update_int32_r2
  procedure, private :: update_int32_r3  => fletcher16_update_int32_r3
  procedure, private :: update_int32_r4  => fletcher16_update_int32_r4
  procedure, private :: update_int32_r5  => fletcher16_update_int32_r5
  procedure, private :: update_int64_r1  => fletcher16_update_int64_r1
  procedure, private :: update_int64_r2  => fletcher16_update_int64_r2
  procedure, private :: update_int64_r3  => fletcher16_update_int64_r3
  procedure, private :: update_int64_r4  => fletcher16_update_int64_r4
  procedure, private :: update_int64_r5  => fletcher16_update_int64_r5
  generic, public :: update => &
    & update_real32_r1, update_real32_r2, update_real32_r3, update_real32_r4, update_real32_r5, &
    & update_real64_r1, update_real64_r2, update_real64_r3, update_real64_r4, update_real64_r5, &
    & update_int32_r1,  update_int32_r2,  update_int32_r3,  update_int32_r4,  update_int32_r5,  &
    & update_int64_r1,  update_int64_r2,  update_int64_r3,  update_int64_r4,  update_int64_r5

  procedure, public :: digest     => fletcher16__digest
  procedure, public :: digest_hex => fletcher16__digest_hex
end type

interface
    subroutine c_fletcher16_reset(checksum) bind(C,name="ec_checksum_fletcher16_reset")
      use, intrinsic :: iso_c_binding, only : c_int32_t
      integer(c_int32_t), intent(inout) :: checksum
    end subroutine

    subroutine c_ec_checksum_fletcher16_update(checksum, data, size) bind(C,name="ec_checksum_fletcher16_update")
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t
      integer(c_int32_t), intent(inout) :: checksum
      type(c_ptr), value, intent(in) :: data
      integer(c_size_t), value, intent(in) :: size
    end subroutine

    function c_ec_checksum_fletcher16_digest(checksum) bind(C,name="ec_checksum_fletcher16_digest") result(digest)
      use, intrinsic :: iso_c_binding, only : c_int32_t, c_int16_t
      integer(c_int16_t) :: digest
      integer(c_int32_t), intent(in) :: checksum
    end function
end interface

interface fletcher16
module procedure fletcher16_real32_r1
module procedure fletcher16_real32_r2
module procedure fletcher16_real32_r3
module procedure fletcher16_real32_r4
module procedure fletcher16_real32_r5
module procedure fletcher16_real64_r1
module procedure fletcher16_real64_r2
module procedure fletcher16_real64_r3
module procedure fletcher16_real64_r4
module procedure fletcher16_real64_r5
module procedure fletcher16_int32_r1
module procedure fletcher16_int32_r2
module procedure fletcher16_int32_r3
module procedure fletcher16_int32_r4
module procedure fletcher16_int32_r5
module procedure fletcher16_int64_r1
module procedure fletcher16_int64_r2
module procedure fletcher16_int64_r3
module procedure fletcher16_int64_r4
module procedure fletcher16_int64_r5
end interface

interface fletcher16_hex
module procedure fletcher16_hex_real32_r1
module procedure fletcher16_hex_real32_r2
module procedure fletcher16_hex_real32_r3
module procedure fletcher16_hex_real32_r4
module procedure fletcher16_hex_real32_r5
module procedure fletcher16_hex_real64_r1
module procedure fletcher16_hex_real64_r2
module procedure fletcher16_hex_real64_r3
module procedure fletcher16_hex_real64_r4
module procedure fletcher16_hex_real64_r5
module procedure fletcher16_hex_int32_r1
module procedure fletcher16_hex_int32_r2
module procedure fletcher16_hex_int32_r3
module procedure fletcher16_hex_int32_r4
module procedure fletcher16_hex_int32_r5
module procedure fletcher16_hex_int64_r1
module procedure fletcher16_hex_int64_r2
module procedure fletcher16_hex_int64_r3
module procedure fletcher16_hex_int64_r4
module procedure fletcher16_hex_int64_r5
end interface

interface to_hex
module procedure to_hex_16
module procedure to_hex_32
module procedure to_hex_64
end interface

contains

subroutine to_lower_inplace(string)
  character(len=*), intent(inout) :: string
  integer :: i
  do i = 1, len(string)
    string(i:i) = char_to_lower(string(i:i))
  end do
contains
  pure function char_to_lower(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1)             :: t
    integer, parameter :: wp= iachar('a')-iachar('A'), BA=iachar('A'), BZ=iachar('Z')
    integer :: k
    ! Check whether the integer equivalent is between BA=65 and BZ=90
    k = ichar(c)
    if (k>=BA.and.k<=BZ) k = k + wp
    t = char(k)
  end function
end subroutine

function to_hex_16(value) result(hex)
  use, intrinsic :: iso_c_binding, only : c_int16_t
  character(len=4) :: hex
  integer(c_int16_t), intent(in) :: value
  write(hex,'(z4.4)') value
  call to_lower_inplace(hex)
end function

function to_hex_32(value) result(hex)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=8) :: hex
  integer(c_int32_t), intent(in) :: value
  write(hex,'(z8.8)') value
  call to_lower_inplace(hex)
end function

function to_hex_64(value) result(hex)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=16) :: hex
  integer(c_int64_t), intent(in) :: value
  write(hex,'(z16.16)') value
  call to_lower_inplace(hex)
end function

subroutine fletcher16_reset(this)
  class(fletcher16_type), intent(inout) :: this
  this%handle = 0
end subroutine

subroutine fletcher16_update_real32_r1(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_float
  class(fletcher16_type), intent(inout) :: this
  real(c_float), contiguous, target, intent(in) :: array(:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1)), c_sizeof(array(1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real32_r2(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_float
  class(fletcher16_type), intent(inout) :: this
  real(c_float), contiguous, target, intent(in) :: array(:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1)), c_sizeof(array(1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real32_r3(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_float
  class(fletcher16_type), intent(inout) :: this
  real(c_float), contiguous, target, intent(in) :: array(:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1)), c_sizeof(array(1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real32_r4(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_float
  class(fletcher16_type), intent(inout) :: this
  real(c_float), contiguous, target, intent(in) :: array(:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1)), c_sizeof(array(1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real32_r5(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_float
  class(fletcher16_type), intent(inout) :: this
  real(c_float), contiguous, target, intent(in) :: array(:,:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1,1)), c_sizeof(array(1,1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real64_r1(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_double
  class(fletcher16_type), intent(inout) :: this
  real(c_double), contiguous, target, intent(in) :: array(:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1)), c_sizeof(array(1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real64_r2(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_double
  class(fletcher16_type), intent(inout) :: this
  real(c_double), contiguous, target, intent(in) :: array(:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1)), c_sizeof(array(1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real64_r3(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_double
  class(fletcher16_type), intent(inout) :: this
  real(c_double), contiguous, target, intent(in) :: array(:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1)), c_sizeof(array(1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real64_r4(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_double
  class(fletcher16_type), intent(inout) :: this
  real(c_double), contiguous, target, intent(in) :: array(:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1)), c_sizeof(array(1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_real64_r5(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_double
  class(fletcher16_type), intent(inout) :: this
  real(c_double), contiguous, target, intent(in) :: array(:,:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1,1)), c_sizeof(array(1,1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int32_r1(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_int32_t
  class(fletcher16_type), intent(inout) :: this
  integer(c_int32_t), contiguous, target, intent(in) :: array(:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1)), c_sizeof(array(1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int32_r2(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_int32_t
  class(fletcher16_type), intent(inout) :: this
  integer(c_int32_t), contiguous, target, intent(in) :: array(:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1)), c_sizeof(array(1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int32_r3(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_int32_t
  class(fletcher16_type), intent(inout) :: this
  integer(c_int32_t), contiguous, target, intent(in) :: array(:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1)), c_sizeof(array(1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int32_r4(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_int32_t
  class(fletcher16_type), intent(inout) :: this
  integer(c_int32_t), contiguous, target, intent(in) :: array(:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1)), c_sizeof(array(1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int32_r5(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof, c_int32_t
  class(fletcher16_type), intent(inout) :: this
  integer(c_int32_t), contiguous, target, intent(in) :: array(:,:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1,1)), c_sizeof(array(1,1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int64_r1(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof
  class(fletcher16_type), intent(inout) :: this
  integer(c_int64_t), contiguous, target, intent(in) :: array(:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1)), c_sizeof(array(1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int64_r2(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof
  class(fletcher16_type), intent(inout) :: this
  integer(c_int64_t), contiguous, target, intent(in) :: array(:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1)), c_sizeof(array(1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int64_r3(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof
  class(fletcher16_type), intent(inout) :: this
  integer(c_int64_t), contiguous, target, intent(in) :: array(:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1)), c_sizeof(array(1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int64_r4(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof
  class(fletcher16_type), intent(inout) :: this
  integer(c_int64_t), contiguous, target, intent(in) :: array(:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1)), c_sizeof(array(1,1,1,1)) * array_size)
  endif
end subroutine

subroutine fletcher16_update_int64_r5(this, array)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int64_t, c_loc, c_sizeof
  class(fletcher16_type), intent(inout) :: this
  integer(c_int64_t), contiguous, target, intent(in) :: array(:,:,:,:,:)
  integer(c_size_t) :: array_size
  array_size = size(array,kind=c_size_t)
  if (array_size > 0) then
    call c_ec_checksum_fletcher16_update(this%handle, c_loc(array(1,1,1,1,1)), c_sizeof(array(1,1,1,1,1)) * array_size)
  endif
end subroutine

function fletcher16__digest(this) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int16_t
  class(fletcher16_type), intent(in) :: this
  integer(c_int16_t) :: digest
  digest = c_ec_checksum_fletcher16_digest(this%handle)
end function

function fletcher16__digest_hex(this) result(digest)
  class(fletcher16_type), intent(in) :: this
  character(len=4) :: digest
  digest = to_hex(this%digest())
end function


function fletcher16_real32_r1(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_float
  integer(c_int16_t) :: digest
  real(c_float), intent(in) :: array(:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real32_r2(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_float
  integer(c_int16_t) :: digest
  real(c_float), intent(in) :: array(:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real32_r3(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_float
  integer(c_int16_t) :: digest
  real(c_float), intent(in) :: array(:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real32_r4(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_float
  integer(c_int16_t) :: digest
  real(c_float), intent(in) :: array(:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real32_r5(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_float
  integer(c_int16_t) :: digest
  real(c_float), intent(in) :: array(:,:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real64_r1(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_double
  integer(c_int16_t) :: digest
  real(c_double), intent(in) :: array(:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real64_r2(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_double
  integer(c_int16_t) :: digest
  real(c_double), intent(in) :: array(:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real64_r3(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_double
  integer(c_int16_t) :: digest
  real(c_double), intent(in) :: array(:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real64_r4(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_double
  integer(c_int16_t) :: digest
  real(c_double), intent(in) :: array(:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_real64_r5(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_double
  integer(c_int16_t) :: digest
  real(c_double), intent(in) :: array(:,:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int32_r1(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int16_t) :: digest
  integer(c_int32_t), intent(in) :: array(:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int32_r2(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int16_t) :: digest
  integer(c_int32_t), intent(in) :: array(:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int32_r3(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int16_t) :: digest
  integer(c_int32_t), intent(in) :: array(:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int32_r4(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int16_t) :: digest
  integer(c_int32_t), intent(in) :: array(:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int32_r5(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int16_t) :: digest
  integer(c_int32_t), intent(in) :: array(:,:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int64_r1(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  integer(c_int16_t) :: digest
  integer(c_int64_t), intent(in) :: array(:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int64_r2(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  integer(c_int16_t) :: digest
  integer(c_int64_t), intent(in) :: array(:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int64_r3(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  integer(c_int16_t) :: digest
  integer(c_int64_t), intent(in) :: array(:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int64_r4(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  integer(c_int16_t) :: digest
  integer(c_int64_t), intent(in) :: array(:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_int64_r5(array) result(digest)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  integer(c_int16_t) :: digest
  integer(c_int64_t), intent(in) :: array(:,:,:,:,:)
  type(fletcher16_type) :: checksum
  call checksum%update(array)
  digest = checksum%digest()
end function

function fletcher16_hex_real32_r1(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_float
  character(len=4) :: hex4
  real(c_float), intent(in) :: array(:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real32_r2(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_float
  character(len=4) :: hex4
  real(c_float), intent(in) :: array(:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real32_r3(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_float
  character(len=4) :: hex4
  real(c_float), intent(in) :: array(:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real32_r4(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_float
  character(len=4) :: hex4
  real(c_float), intent(in) :: array(:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real32_r5(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_float
  character(len=4) :: hex4
  real(c_float), intent(in) :: array(:,:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real64_r1(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_double
  character(len=4) :: hex4
  real(c_double), intent(in) :: array(:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real64_r2(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_double
  character(len=4) :: hex4
  real(c_double), intent(in) :: array(:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real64_r3(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_double
  character(len=4) :: hex4
  real(c_double), intent(in) :: array(:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real64_r4(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_double
  character(len=4) :: hex4
  real(c_double), intent(in) :: array(:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_real64_r5(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_double
  character(len=4) :: hex4
  real(c_double), intent(in) :: array(:,:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int32_r1(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=4) :: hex4
  integer(c_int32_t), intent(in) :: array(:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int32_r2(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=4) :: hex4
  integer(c_int32_t), intent(in) :: array(:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int32_r3(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=4) :: hex4
  integer(c_int32_t), intent(in) :: array(:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int32_r4(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=4) :: hex4
  integer(c_int32_t), intent(in) :: array(:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int32_r5(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  character(len=4) :: hex4
  integer(c_int32_t), intent(in) :: array(:,:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int64_r1(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=4) :: hex4
  integer(c_int64_t), intent(in) :: array(:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int64_r2(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=4) :: hex4
  integer(c_int64_t), intent(in) :: array(:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int64_r3(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=4) :: hex4
  integer(c_int64_t), intent(in) :: array(:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int64_r4(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=4) :: hex4
  integer(c_int64_t), intent(in) :: array(:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

function fletcher16_hex_int64_r5(array) result(hex4)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  character(len=4) :: hex4
  integer(c_int64_t), intent(in) :: array(:,:,:,:,:)
  hex4 = to_hex(fletcher16(array))
end function

end module
