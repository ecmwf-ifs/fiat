! (C) Copyright 2005- ECMWF.
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
  write(0,'(A,I0,A)') "TEST FAILED in test_mpl.F90 @ line ",line," :"
  write(0,*) msg
  stop 1
end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)

program test_mpl
use ec_parkind, only : jpim
use mpl_module, only: mpl_init, mpl_end, mpl_rank, linitmpi_via_mpl, mpl_alltoallv

implicit none

integer(jpim) :: nprocs
logical :: verbose = .false.
integer, allocatable :: sbuf(:), rbuf(:), scounts(:), rcounts(:)
integer i,j

call mpl_init(KPROCS=nprocs,ldinfo=verbose,ldenv=.true.)

if( nprocs <= 1 )            FAIL("nprocs must be > 1")
if( mpl_rank < 0 .or. mpl_rank > nprocs  )          FAIL("mpl_rank must be >= 1 and <= nprocs")
if( .not. linitmpi_via_mpl ) FAIL("linitmpi_via_mpl must be True")

allocate(sbuf((nprocs*(nprocs+1))/2),rbuf(nprocs*mpl_rank),scounts(nprocs),rcounts(nprocs))
do i=1,nprocs
  do j=1,i
    sbuf(i+j-1) = mpl_rank
  enddo
  scounts(i)=i
enddo
rcounts(:)=mpl_rank

call mpl_alltoallv(sbuf,scounts,rbuf,rcounts)

do i=1,nprocs,mpl_rank
  if ( any(rbuf(i:i+mpl_rank-1) /= i) ) then
    write(0,*) "test failed on mpl_rank", mpl_rank, rbuf
  endif
enddo

call mpl_end(ldmeminfo=verbose)
! Note that with mpi_serial meminfo will not be printed regardless of ldmeminfo

end program
