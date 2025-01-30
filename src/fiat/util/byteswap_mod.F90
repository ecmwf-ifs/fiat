! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE BYTESWAP_MOD

!**** Module for byteswapping buffers

!     Purpose.
!     --------
!     Fortran 90 Interface to calling byteswap

!     Author.
!     -------
!        W.Deconinck     ECMWF

!     Modifications.
!     --------------
!        Original: 2025-01-30

!     ------------------------------------------------------------------

IMPLICIT NONE
PRIVATE

PUBLIC :: convert_to_big_endian, is_little_endian, byteswap

! ISO-C-BINDING C-interfaces
INTERFACE

    ! void iswap_ (char * a, const char * b, const int * _t, const int * _n);
    subroutine iswap_bind( &
      & ptr_out, &
      & ptr_in,  &
      & size_of_type_in_bytes, &
      & length) &
      & bind(c, name="iswap_")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value, intent(in)    :: ptr_out
      type(c_ptr), value, intent(in)    :: ptr_in
      integer(c_int), intent(in)        :: size_of_type_in_bytes
      integer(c_int), intent(in)        :: length
    end subroutine

    ! void jswap_ (char * a, const char * b, const int * _t, const int * _n);
    subroutine jswap_bind( &
      & ptr_out, &
      & ptr_in,  &
      & size_of_type_in_bytes, &
      & length) &
      & bind(c, name="jswap_")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value, intent(in)    :: ptr_out
      type(c_ptr), value, intent(in)    :: ptr_in
      integer(c_int), intent(in)        :: size_of_type_in_bytes
      integer(c_int), intent(in)        :: length
    end subroutine


    ! void iswap_isle_ (int * reqd);
    subroutine iswap_isle_bind(value) bind(c,name="iswap_isle_")
      use, intrinsic :: iso_c_binding, only : c_int
      integer(c_int), intent(out) :: value
    end subroutine

END INTERFACE

INTERFACE convert_to_big_endian
  MODULE PROCEDURE convert_to_big_endian_int32_r1
  MODULE PROCEDURE convert_to_big_endian_int64_r1
  MODULE PROCEDURE convert_to_big_endian_real32_r1
  MODULE PROCEDURE convert_to_big_endian_real64_r1
END INTERFACE

INTERFACE byteswap
  MODULE PROCEDURE byteswap_int32_r1
  MODULE PROCEDURE byteswap_int64_r1
  MODULE PROCEDURE byteswap_real32_r1
  MODULE PROCEDURE byteswap_real64_r1
END INTERFACE

CONTAINS

function is_little_endian()
  use, intrinsic :: iso_c_binding, only : c_int
  logical :: is_little_endian
  integer(c_int) :: isle
  call iswap_isle_bind(isle)
  if (isle == 0) then
    is_little_endian = .false.
  else
    is_little_endian = .true.
  endif
end function

subroutine convert_to_big_endian_int32_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int
  integer(c_int), target, intent(inout) :: array_out(:)
  integer(c_int), target, intent(in)    :: array_in(:)
  call iswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine convert_to_big_endian_int64_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_long
  integer(c_long), target, intent(inout) :: array_out(:)
  integer(c_long), target, intent(in)    :: array_in(:)
  call iswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine convert_to_big_endian_real32_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_float
  real(c_float), target, intent(inout) :: array_out(:)
  real(c_float), target, intent(in)    :: array_in(:)
  call iswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine convert_to_big_endian_real64_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_double
  real(c_double), target, intent(inout) :: array_out(:)
  real(c_double), target, intent(in)    :: array_in(:)
  call iswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine byteswap_int32_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int
  integer(c_int), target, intent(inout) :: array_out(:)
  integer(c_int), target, intent(in)    :: array_in(:)
  call jswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine byteswap_int64_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_long
  integer(c_long), target, intent(inout) :: array_out(:)
  integer(c_long), target, intent(in)    :: array_in(:)
  call jswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine byteswap_real32_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_float
  real(c_float), target, intent(inout) :: array_out(:)
  real(c_float), target, intent(in)    :: array_in(:)
  call jswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine

subroutine byteswap_real64_r1( array_out, array_in )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_int, c_double
  real(c_double), target, intent(inout) :: array_out(:)
  real(c_double), target, intent(in)    :: array_in(:)
  call jswap_bind(c_loc(array_out(1)), c_loc(array_in(1)), int(c_sizeof(array_in(1)),c_int), size(array_in))
end subroutine



END MODULE
