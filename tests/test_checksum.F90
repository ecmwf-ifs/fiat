! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

!
! Test program to test ec_checksum_mod

module fiat_test_fletcher16_type_mod
! A helper module to initialise arrays used in the tests
interface create_array
module procedure :: create_array_real32_r1
module procedure :: create_array_real32_r2
module procedure :: create_array_real32_r3
module procedure :: create_array_real32_r4
module procedure :: create_array_real64_r1
module procedure :: create_array_real64_r2
module procedure :: create_array_real64_r3
module procedure :: create_array_real64_r4
module procedure :: create_array_int32_r1
module procedure :: create_array_int32_r2
module procedure :: create_array_int32_r3
module procedure :: create_array_int32_r4
module procedure :: create_array_int64_r1
module procedure :: create_array_int64_r2
module procedure :: create_array_int64_r3
module procedure :: create_array_int64_r4
end interface

interface create_split_arrays
module procedure :: create_split_arrays_real32_r1
module procedure :: create_split_arrays_real64_r1
end interface

contains

subroutine create_array_real32_r1(array)
  use, intrinsic :: iso_c_binding
  real(c_float), allocatable :: array(:)
  integer :: i
  integer, parameter :: N = 100000
  allocate(array(N))
  do i=1,N
    array(i) = i
  enddo
end subroutine

subroutine create_array_real32_r2(array)
  use, intrinsic :: iso_c_binding
  real(c_float), allocatable :: array(:,:)
  integer :: i, j
  integer, parameter :: N = 100000
  integer, parameter :: Ni = N/200
  integer, parameter :: Nj = N/Ni 
  allocate(array(Ni,Nj))
  do j=1,Nj
    do i=1,Ni
      array(i,j) = i + (j-1) * Ni
    enddo
  enddo
end subroutine

subroutine create_array_real32_r3(array)
  use, intrinsic :: iso_c_binding
  real(c_float), allocatable :: array(:,:,:)
  integer :: i, j, k
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 500
  allocate(array(Ni,Nj,Nk))
  do k=1,Nk
    do j=1,Nj
      do i=1,Ni
        array(i,j,k) = i + (j-1) * Ni + (k-1) * (Ni * Nj)
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_real32_r4(array)
  use, intrinsic :: iso_c_binding
  real(c_float), allocatable :: array(:,:,:,:)
  integer :: i, j, k, l
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 10
  integer, parameter :: Nl = 50
  allocate(array(Ni,Nj,Nk,Nl))
  do l=1,Nl
    do k=1,Nk
      do j=1,Nj
        do i=1,Ni
          array(i,j,k,l) = i + (j-1) * Ni + (k-1) * (Ni * Nj) + (l-1) * (Ni * Nj * Nk)
        enddo
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_real64_r1(array)
  use, intrinsic :: iso_c_binding
  real(c_double), allocatable :: array(:)
  integer :: i
  integer, parameter :: N = 100000
  allocate(array(N))
  do i=1,N
    array(i) = i
  enddo
end subroutine

subroutine create_array_real64_r2(array)
  use, intrinsic :: iso_c_binding
  real(c_double), allocatable :: array(:,:)
  integer :: i, j
  integer, parameter :: N = 100000
  integer, parameter :: Ni = N/200
  integer, parameter :: Nj = N/Ni 
  allocate(array(Ni,Nj))
  do j=1,Nj
    do i=1,Ni
      array(i,j) = i + (j-1) * Ni
    enddo
  enddo
end subroutine

subroutine create_array_real64_r3(array)
  use, intrinsic :: iso_c_binding
  real(c_double), allocatable :: array(:,:,:)
  integer :: i, j, k
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 500
  allocate(array(Ni,Nj,Nk))
  do k=1,Nk
    do j=1,Nj
      do i=1,Ni
        array(i,j,k) = i + (j-1) * Ni + (k-1) * (Ni * Nj)
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_real64_r4(array)
  use, intrinsic :: iso_c_binding
  real(c_double), allocatable :: array(:,:,:,:)
  integer :: i, j, k, l
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 10
  integer, parameter :: Nl = 50
  allocate(array(Ni,Nj,Nk,Nl))
  do l=1,Nl
    do k=1,Nk
      do j=1,Nj
        do i=1,Ni
          array(i,j,k,l) = i + (j-1) * Ni + (k-1) * (Ni * Nj) + (l-1) * (Ni * Nj * Nk)
        enddo
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_int32_r1(array)
  use, intrinsic :: iso_c_binding
  integer(c_int32_t), allocatable :: array(:)
  integer :: i
  integer, parameter :: N = 100000
  allocate(array(N))
  do i=1,N
    array(i) = i
  enddo
end subroutine

subroutine create_array_int32_r2(array)
  use, intrinsic :: iso_c_binding
  integer(c_int32_t), allocatable :: array(:,:)
  integer :: i, j
  integer, parameter :: N = 100000
  integer, parameter :: Ni = N/200
  integer, parameter :: Nj = N/Ni 
  allocate(array(Ni,Nj))
  do j=1,Nj
    do i=1,Ni
      array(i,j) = i + (j-1) * Ni
    enddo
  enddo
end subroutine

subroutine create_array_int32_r3(array)
  use, intrinsic :: iso_c_binding
  integer(c_int32_t), allocatable :: array(:,:,:)
  integer :: i, j, k
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 500
  allocate(array(Ni,Nj,Nk))
  do k=1,Nk
    do j=1,Nj
      do i=1,Ni
        array(i,j,k) = i + (j-1) * Ni + (k-1) * (Ni * Nj)
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_int32_r4(array)
  use, intrinsic :: iso_c_binding
  integer(c_int32_t), allocatable :: array(:,:,:,:)
  integer :: i, j, k, l
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 10
  integer, parameter :: Nl = 50
  allocate(array(Ni,Nj,Nk,Nl))
  do l=1,Nl
    do k=1,Nk
      do j=1,Nj
        do i=1,Ni
          array(i,j,k,l) = i + (j-1) * Ni + (k-1) * (Ni * Nj) + (l-1) * (Ni * Nj * Nk)
        enddo
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_int64_r1(array)
  use, intrinsic :: iso_c_binding
  integer(c_int64_t), allocatable :: array(:)
  integer :: i
  integer, parameter :: N = 100000
  allocate(array(N))
  do i=1,N
    array(i) = i
  enddo
end subroutine

subroutine create_array_int64_r2(array)
  use, intrinsic :: iso_c_binding
  integer(c_int64_t), allocatable :: array(:,:)
  integer :: i, j
  integer, parameter :: N = 100000
  integer, parameter :: Ni = N/200
  integer, parameter :: Nj = N/Ni 
  allocate(array(Ni,Nj))
  do j=1,Nj
    do i=1,Ni
      array(i,j) = i + (j-1) * Ni
    enddo
  enddo
end subroutine

subroutine create_array_int64_r3(array)
  use, intrinsic :: iso_c_binding
  integer(c_int64_t), allocatable :: array(:,:,:)
  integer :: i, j, k
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 500
  allocate(array(Ni,Nj,Nk))
  do k=1,Nk
    do j=1,Nj
      do i=1,Ni
        array(i,j,k) = i + (j-1) * Ni + (k-1) * (Ni * Nj)
      enddo
    enddo
  enddo
end subroutine

subroutine create_array_int64_r4(array)
  use, intrinsic :: iso_c_binding
  integer(c_int64_t), allocatable :: array(:,:,:,:)
  integer :: i, j, k, l
  integer, parameter :: Ni = 25
  integer, parameter :: Nj = 8
  integer, parameter :: Nk = 10
  integer, parameter :: Nl = 50
  allocate(array(Ni,Nj,Nk,Nl))
  do l=1,Nl
    do k=1,Nk
      do j=1,Nj
        do i=1,Ni
          array(i,j,k,l) = i + (j-1) * Ni + (k-1) * (Ni * Nj) + (l-1) * (Ni * Nj * Nk)
        enddo
      enddo
    enddo
  enddo
end subroutine

subroutine create_split_arrays_real32_r1(array1, array2)
  use, intrinsic :: iso_c_binding
  real(c_float), allocatable :: array1(:), array2(:)
  integer :: i
  integer, parameter :: N = 100000
  integer :: N1, N2
  N1 = N / 3
  N2 = N - N1
  allocate(array1(N1))
  allocate(array2(N2))
  do i=1,N1
    array1(i) = i
  enddo
  do i=1,N2
    array2(i) = N1 + i
  enddo
end subroutine

subroutine create_split_arrays_real64_r1(array1, array2)
  use, intrinsic :: iso_c_binding
  real(c_double), allocatable :: array1(:), array2(:)
  integer :: i
  integer, parameter :: N = 100000
  integer :: N1, N2
  N1 = N / 3
  N2 = N - N1
  allocate(array1(N1))
  allocate(array2(N2))
  do i=1,N1
    array1(i) = i
  enddo
  do i=1,N2
    array2(i) = N1 + i
  enddo
end subroutine

end module fiat_test_fletcher16_type_mod

subroutine fail_impl(msg,line)
  character(*) :: msg
  integer :: line
  write(0,'(A,I0,A)') "TEST FAILED in test_byteswap.F90 @ line ",line," :"
  write(0,*) msg
  stop 1
end subroutine
#define FAIL(msg) call fail_impl(msg,__LINE__)

program test_checksum
use ec_checksum_mod
use fiat_test_fletcher16_type_mod
use, intrinsic :: iso_c_binding
implicit none

call test_fletcher16_type_real32_r1()
call test_fletcher16_type_real32_r2()
call test_fletcher16_type_real32_r3()
call test_fletcher16_type_real32_r4()
call test_fletcher16_type_real64_r1()
call test_fletcher16_type_real64_r2()
call test_fletcher16_type_real64_r3()
call test_fletcher16_type_real64_r4()
call test_fletcher16_type_int32_r1()
call test_fletcher16_type_int32_r2()
call test_fletcher16_type_int32_r3()
call test_fletcher16_type_int32_r4()
call test_fletcher16_type_int64_r1()
call test_fletcher16_type_int64_r2()
call test_fletcher16_type_int64_r3()
call test_fletcher16_type_int64_r4()
call test_fletcher16_type_split_real32_r1()
call test_fletcher16_type_split_real64_r1()
call test_fletcher16_real32_r1()
call test_fletcher16_real32_r2()
call test_fletcher16_real32_r3()
call test_fletcher16_real32_r4()
call test_fletcher16_real64_r1()
call test_fletcher16_real64_r2()
call test_fletcher16_real64_r3()
call test_fletcher16_real64_r4()
call test_fletcher16_int32_r1()
call test_fletcher16_int32_r2()
call test_fletcher16_int32_r3()
call test_fletcher16_int32_r4()
call test_fletcher16_int64_r1()
call test_fletcher16_int64_r2()
call test_fletcher16_int64_r3()
call test_fletcher16_int64_r4()

write(0,'(A)') "SUCCESS"

CONTAINS

  subroutine test_fletcher16_type_real32_r1()
    real(c_float), allocatable :: array(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_type_real32_r1"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real32_r2()
    real(c_float), allocatable :: array(:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_type_real32_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real32_r3()
    real(c_float), allocatable :: array(:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_type_real32_r3"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real32_r4()
    real(c_float), allocatable :: array(:,:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_type_real32_r4"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real64_r1()
    real(c_double), allocatable :: array(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_type_real64_r1"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real64_r2()
    real(c_double), allocatable :: array(:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_type_real64_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real64_r3()
    real(c_double), allocatable :: array(:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_type_real64_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_real64_r4()
    real(c_double), allocatable :: array(:,:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_type_real64_r4"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int32_r1()
    integer(c_int32_t), allocatable :: array(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_type_int32_r1"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int32_r2()
    integer(c_int32_t), allocatable :: array(:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_type_int32_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int32_r3()
    integer(c_int32_t), allocatable :: array(:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_type_int32_r3"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int32_r4()
    integer(c_int32_t), allocatable :: array(:,:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_type_int32_r4"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int64_r1()
    integer(c_int64_t), allocatable :: array(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_type_int64_r1"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int64_r2()
    integer(c_int64_t), allocatable :: array(:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_type_int64_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int64_r3()
    integer(c_int64_t), allocatable :: array(:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_type_int64_r2"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_int64_r4()
    integer(c_int64_t), allocatable :: array(:,:,:,:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_type_int64_r4"
    call create_array(array)
    call checksum%update(array)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_split_real32_r1()
    real(c_float), allocatable :: array1(:), array2(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_type_split_real32_r1"
    call create_split_arrays(array1, array2)
    call checksum%update(array1)
    call checksum%update(array2)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_type_split_real64_r1()
    real(c_double), allocatable :: array1(:), array2(:)
    type(fletcher16_type) :: checksum
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_type_split_real64_r1"
    call create_split_arrays(array1, array2)
    call checksum%update(array1)
    call checksum%update(array2)
    if (checksum%digest_hex() /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//checksum%digest_hex())
    endif
  end subroutine

  subroutine test_fletcher16_real32_r1()
    real(c_float), allocatable :: array(:)
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_real32_r1"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real32_r2()
    real(c_float), allocatable :: array(:,:)
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_real32_r2"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real32_r3()
    real(c_float), allocatable :: array(:,:,:)
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_real32_r3"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real32_r4()
    real(c_float), allocatable :: array(:,:,:,:)
    character(len=4) :: expected = '44c2'
    write(0,'(A)') "test_fletcher16_real32_r4"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real64_r1()
    real(c_double), allocatable :: array(:)
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_real64_r1"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real64_r2()
    real(c_double), allocatable :: array(:,:)
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_real64_r2"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real64_r3()
    real(c_double), allocatable :: array(:,:,:)
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_real64_r3"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_real64_r4()
    real(c_double), allocatable :: array(:,:,:,:)
    character(len=4) :: expected = 'ca21'
    write(0,'(A)') "test_fletcher16_real64_r4"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int32_r1()
    integer(c_int32_t), allocatable :: array(:)
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_int32_r1"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int32_r2()
    integer(c_int32_t), allocatable :: array(:,:)
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_int32_r2"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int32_r3()
    integer(c_int32_t), allocatable :: array(:,:,:)
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_int32_r3"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int32_r4()
    integer(c_int32_t), allocatable :: array(:,:,:,:)
    character(len=4) :: expected = 'e137'
    write(0,'(A)') "test_fletcher16_int32_r4"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int64_r1()
    integer(c_int64_t), allocatable :: array(:)
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_int64_r1"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int64_r2()
    integer(c_int64_t), allocatable :: array(:,:)
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_int64_r2"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int64_r3()
    integer(c_int64_t), allocatable :: array(:,:,:)
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_int64_r3"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

  subroutine test_fletcher16_int64_r4()
    integer(c_int64_t), allocatable :: array(:,:,:,:)
    character(len=4) :: expected = 'a037'
    write(0,'(A)') "test_fletcher16_int64_r4"
    call create_array(array)
    if (fletcher16_hex(array) /= expected) then
      FAIL('checksum failed. Expected: '//expected//' , Computed: '//fletcher16_hex(array))
    endif
  end subroutine

end program
