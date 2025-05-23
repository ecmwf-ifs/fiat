! (C) Copyright 2021- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

!
! Testing of ec_args; expected to be launched with 3 arguments:
!    <program> arg1 arg2 arg3
!

#define FAIL(msg) call fail_impl(msg,__LINE__)

program test_namelist
use namelist_mod
implicit none

logical,parameter          :: lvalue_check = .true.
integer,parameter          :: ivalue_check = 12
real,parameter             :: rvalue_check = 0.12
character(len=*),parameter :: cvalue_check = 'NONE'

integer :: kulnam = 4

logical           :: lvalue = .false.
integer           :: ivalue = 0
real              :: rvalue = 0.0
character(len=80) :: cvalue = ''

character(len=*),parameter :: cexisting = 'NAMBLOCK2'
character(len=*),parameter :: cnonexisting = 'nonexisting'
namelist/NAMBLOCK2/lvalue,ivalue,rvalue,cvalue
namelist/NAM_NONEXISTING/lvalue,ivalue,rvalue,cvalue

open(kulnam, file='../share/fiat/util/namelist_example')  ! cwd is build/tests, file is in build/share/...

call test_posnamef_existing()
call test_posnamef_nonexisting()
call test_posnam()

contains
subroutine test_posnamef_existing()
implicit none

if (posnamef(kulnam, cexisting, ldfatal=.false., ldverbose=.true.) == 0) read(kulnam, NAMBLOCK2)

if (lvalue .neqv. lvalue_check) FAIL("LVALUE")
if (ivalue /= ivalue_check) FAIL("IVALUE")
if (rvalue /= rvalue_check) FAIL("RVALUE")
if (trim(cvalue) /= trim(cvalue_check)) FAIL("CVALUE")
end subroutine test_posnamef_existing

subroutine test_posnamef_nonexisting()
implicit none

if (posnamef(kulnam, cnonexisting, ldfatal=.false., ldverbose=.true.) == 0) read(kulnam, NAM_NONEXISTING)

end subroutine test_posnamef_nonexisting

subroutine test_posnam()
implicit none

call posnam(kulnam, cexisting)
read(kulnam, NAMBLOCK2)
end subroutine test_posnam

subroutine fail_impl(msg,line)
    character(*) :: msg
    integer :: line
    write(0,'(A,I0,A)') "TEST FAILED in test_namelist.F90 @ line ",line," :"
    write(0,*) msg
    stop 1
end subroutine
end program
