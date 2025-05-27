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

character(len=*),parameter :: cfile = 'fort.4'
logical,parameter          :: lvalue_check = .true.
integer,parameter          :: ivalue_check = 12
real,parameter             :: rvalue_check = 0.12
character(len=*),parameter :: cvalue_check = 'NONE'

integer :: kulnam = 4

logical           :: lvalue = .false.
integer           :: ivalue = 0
real              :: rvalue = 0.0
character(len=80) :: cvalue = ''

character(len=*),parameter :: cpresent = 'NAMBLOCK2'
character(len=*),parameter :: cnonpresent = 'NAM_NONPRESENT'
namelist/NAMBLOCK2/lvalue,ivalue,rvalue,cvalue
namelist/NAM_NONPRESENT/lvalue,ivalue,rvalue,cvalue

call write_namelist()

open(kulnam, file=cfile)

call test_posnamef_present()
call test_posnamef_nonpresent()
call test_posnam_present()
call test_posnam_nonpresent()
close(kulnam)
! checking it also works with a closed kulnam,
! guessing filename (fort.kulnam) and opening it
call test_posnam_present()

contains

subroutine write_namelist()
implicit none
integer :: iunit = 4
logical :: lexist
inquire(file=cfile, exist=lexist)
if (lexist) then
  print*, "Removing", cfile
  call unlink(cfile)
endif
open(iunit, file=cfile, status='NEW')
write(iunit,*) '&NAMBLOCK1'
write(iunit,*) '/'
write(iunit,*) '&NAMBLOCK2'
write(iunit,*) '  LVALUE=.TRUE.,'
write(iunit,*) '  RVALUE=0.12,'
write(iunit,*) '  IVALUE=12,'
write(iunit,*) '  CVALUE="NONE",'
write(iunit,*) '/'
write(iunit,*) '&NAMBLOCK3'
write(iunit,*) '/'
close(iunit)
end subroutine write_namelist

subroutine test_posnamef_present()
implicit none

if (posnamef(kulnam, cpresent, ldfatal=.false., ldverbose=.true.) == 0) read(kulnam, NAMBLOCK2)

if (lvalue .neqv. lvalue_check) FAIL("LVALUE")
if (ivalue /= ivalue_check) FAIL("IVALUE")
if (rvalue /= rvalue_check) FAIL("RVALUE")
if (trim(cvalue) /= trim(cvalue_check)) FAIL("CVALUE")
end subroutine test_posnamef_present

subroutine test_posnamef_nonpresent()
implicit none
if (posnamef(kulnam, cnonpresent, ldfatal=.false., ldverbose=.true.) == 0) then
  read(kulnam, NAM_NONPRESENT)
else
  print*,'This should not be printed'
endif
end subroutine test_posnamef_nonpresent

subroutine test_posnam_present()
implicit none
call posnam(kulnam, cpresent)
read(kulnam, NAMBLOCK2)
end subroutine test_posnam_present

subroutine test_posnam_nonpresent()
implicit none
call posnam(kulnam, cnonpresent)  ! should call abor1
end subroutine test_posnam_nonpresent

subroutine fail_impl(msg,line)
    character(*) :: msg
    integer :: line
    write(0,'(A,I0,A)') "TEST FAILED in test_namelist.F90 @ line ",line," :"
    write(0,*) msg
    stop 1
end subroutine
end program
