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
use mpl_module, only : mpl_abort
character(*) :: msg
integer :: line

write(0,'(A,I0,A)') "TEST FAILED in alltoallv @ line ",line," :"
write(0,*) msg

call mpl_abort()

end subroutine

#define FAIL(msg) call fail_impl(msg,__LINE__)
subroutine work1(r)
implicit none
integer, intent(out) :: r

real a(100000)
call  random_number(a)
if (any(a < 0.0) ) then
  r =1
else
  r =0
endif
end subroutine work1


program test_mpl_alltoallv
use ec_parkind, only : jpim, jprm, jprd
use mpl_module, only: mpl_init, mpl_end, mpl_rank, linitmpi_via_mpl, mpl_alltoallv, JP_NON_BLOCKING_STANDARD, mpl_wait

implicit none

integer(jpim) :: nprocs
logical :: verbose = .false.
integer(jpim), allocatable :: sbuf(:), rbuf(:), scounts(:), rcounts(:)
real(jprm), allocatable :: sbufr(:), rbufr(:)
real(jprd), allocatable :: sbufd(:), rbufd(:)
integer i,j,k
character(len=256) msg

call mpl_init(KPROCS=nprocs,ldinfo=verbose,ldenv=.true.)

if( nprocs <= 1 ) FAIL("nprocs must be > 1")
if( mpl_rank < 1 .or. mpl_rank > nprocs  ) FAIL("mpl_rank must be >= 1 and <= nprocs")
if( .not. linitmpi_via_mpl ) FAIL("linitmpi_via_mpl must be True")

allocate(sbuf((nprocs*(nprocs+1))/2),rbuf(nprocs*mpl_rank),&
  sbufr((nprocs*(nprocs+1))/2),rbufr(nprocs*mpl_rank), &
  sbufd((nprocs*(nprocs+1))/2),rbufd(nprocs*mpl_rank),&
  scounts(nprocs),rcounts(nprocs))

k=1
do i=1,nprocs
  do j=1,i
    sbuf (k) = mpl_rank
    sbufr(k) = mpl_rank
    sbufd(k) = mpl_rank
    k=k+1
  enddo
  scounts(i)=i
enddo
rcounts(:)=mpl_rank

 !call do_alltoallv("blocking")

call do_alltoallv("nonblocking")

call mpl_end(ldmeminfo=verbose)
! Note that with mpi_serial meminfo will not be printed regardless of ldmeminfo

contains

subroutine do_alltoallv(mode)
implicit none
character(len=*), intent(in) :: mode

character(len=256) :: msg

integer request_i, request_r, request_d, i, j, k, res
integer sdispl(nprocs), rdispl(nprocs), rqarray(3)

select case(mode)
 case("blocking")
  call mpl_alltoallv(sbuf,scounts,rbuf,rcounts)
  call mpl_alltoallv(sbufr,scounts,rbufr,rcounts)
  call mpl_alltoallv(sbufd,scounts,rbufd,rcounts)

  k=1
  do i=1,size(rbuf),mpl_rank
    if ( any(rbuf(i:i+mpl_rank-1) /= k) ) then
      write(msg,*) trim(mode)//" int alltoall test test failed on mpl_rank", mpl_rank, rbuf
      FAIL(msg)
    endif
    if ( any(nint(rbufr(i:i+mpl_rank-1)) /= k) ) then
      write(msg,*) trim(mode)//" real alltoall test test failed on mpl_rank", mpl_rank, rbufr
      FAIL(msg)
    endif
    if ( any(nint(rbufd(i:i+mpl_rank-1)) /= k) ) then
      write(msg,*) trim(mode)//" double alltoall test test failed on mpl_rank", mpl_rank, rbufd
      FAIL(msg)
    endif
    k=k+1
  enddo

 case("nonblocking")
  ! trying to get a random failure
  do j=1,1
    call mpl_alltoallv(sbuf,scounts,rbuf,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
    call mpl_alltoallv(sbufr,scounts,rbufr,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
    call mpl_alltoallv(sbufd,scounts,rbufd,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
    call work1(res)
    if ( res > 0 ) write(0,*) "error in  work1 non-blocking alltoallv" ! this should not happen ever !!!
    !call mpl_wait(request_r)
    call mpl_wait(request_d)
    !call mpl_wait(request_i)
    rqarray = [request_i, request_r, request_d]
    call mpl_wait(rqarray(1:2))
  enddo
  k = 1
  do i=1,size(rbuf),mpl_rank
    if ( any(rbuf(i:i+mpl_rank-1) /= k) ) then
      write(msg,*) trim(mode)//" int alltoall test failed on mpl_rank", mpl_rank, rbuf
      FAIL(msg)
    endif
    if ( any(nint(rbufr(i:i+mpl_rank-1)) /= k) ) then
      write(msg,*) trim(mode)//" real alltoall test failed on mpl_rank", mpl_rank, rbuf
      FAIL(msg)
    endif
    if ( any(nint(rbufd(i:i+mpl_rank-1)) /= k) ) then
      write(msg,*) trim(mode)//" double alltoall test failed on mpl_rank", mpl_rank, rbuf
      FAIL(msg)
    endif
    k = k+1
  enddo

  ! test with displacement arguments
  sdispl(1)=0
  rdispl(1)=0
  do i=2,nprocs
    sdispl(i)=sdispl(i-1)+scounts(i-1)
    rdispl(i)=rdispl(i-1)+rcounts(i-1)
  enddo

  call mpl_alltoallv(sbuf, scounts, rbuf, rcounts, sdispl, rdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
  call work1(res)
  if ( res > 0 ) write(0,*) "error in  work1 non-blocking alltoallv" ! this should not happen ever !!!
  call mpl_wait(request_i)

  k=1
  do i=1,nprocs,mpl_rank
    if ( any(rbuf(i:i+mpl_rank-1) /= k) ) then
      write(msg,*) trim(mode)//" int alltoall test with displ args failed on mpl_rank", mpl_rank, rbuf
      FAIL(msg)
    endif
    k=k+1
  enddo

end select

end subroutine do_alltoallv

end program test_mpl_alltoallv


