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
  
  write(0,'(A,I0,A)') "TEST FAILED in gatherv.F90 @ line ",line," :"
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


program test_mpl_gatherv
use ec_parkind, only : jpim, jprm, jprd
use mpl_module, only: mpl_init, mpl_end, mpl_rank, linitmpi_via_mpl, mpl_gatherv, JP_NON_BLOCKING_STANDARD, mpl_wait

implicit none

integer(jpim) :: nprocs
logical :: verbose = .false.
integer(jpim), allocatable :: sbuf(:), rbuf(:), rcounts(:)
integer(jpim) :: scounts
real(jprm), allocatable :: sbufr(:), rbufr(:)
real(jprd), allocatable :: sbufd(:), rbufd(:)
character(len=6) :: sbufc
character(len=6), allocatable ::  rbufc(:)
integer i,j,k, kroot
character(len=256) msg

call mpl_init(KPROCS=nprocs,ldinfo=verbose,ldenv=.true.)

if( nprocs <= 1 ) FAIL("nprocs must be > 1")
if( mpl_rank < 1 .or. mpl_rank > nprocs  ) FAIL("mpl_rank must be >= 1 and <= nprocs")
if( .not. linitmpi_via_mpl ) FAIL("linitmpi_via_mpl must be True")

allocate(sbuf(mpl_rank),rbuf((nprocs*(nprocs+1))/2+nprocs),&
    sbufr(mpl_rank),rbufr((nprocs*(nprocs+1))/2+nprocs), &
    sbufd(mpl_rank),rbufd((nprocs*(nprocs+1))/2+nprocs),&
    rcounts(nprocs))
!allocate(character(len=mpl_rank) :: sbufc)
allocate(rbufc(nprocs))

sbuf  = mpl_rank
sbufr = mpl_rank
sbufd = mpl_rank
do i=1,len(sbufc)
  sbufc(i:i) = char(ichar('a')+mod(mpl_rank-1,26))
  !print*,sbufc(i:i), mpl_rank
enddo
scounts=mpl_rank
do i=1,nprocs
  rcounts(i)=i
enddo

kroot=2

call do_gatherv("blocking")

call do_gatherv("nonblocking")

call mpl_end(ldmeminfo=verbose)
! Note that with mpi_serial meminfo will not be printed regardless of ldmeminfo

contains

  subroutine do_gatherv(mode)
    implicit none
    character(len=*), intent(in) :: mode

    character(len=128) :: msg
    
    integer request_i, request_r, request_d, request_c, i, j, k, res
    integer rdispl(nprocs), rcounts_c(nprocs)

    select case(mode)
    case("blocking")
      call mpl_gatherv(sbuf,kroot,rbuf,rcounts)
      call mpl_gatherv(sbufr,kroot,rbufr,rcounts)
      rcounts_c=6
      call mpl_gatherv(sbufc,kroot,rbufc,rcounts_c)
      if (mpl_rank == kroot) then
        call mpl_gatherv(sbufd,kroot,rbufd,rcounts)
      else
        call mpl_gatherv(sbufd,kroot)
      endif    
    case("nonblocking")
      ! trying to get a random failure
      do j=1,1
        if (mpl_rank == kroot ) then
          call mpl_gatherv(sbuf,kroot,rbuf,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
          call mpl_gatherv(sbufr,kroot,rbufr,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
          call mpl_gatherv(sbufd,kroot,rbufd,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
          rcounts_c=6 ! 
          call mpl_gatherv(sbufc,kroot,rbufc,rcounts_c, KMP_TYPE = JP_NON_BLOCKING_STANDARD,KREQUEST=request_c)
        else
          call mpl_gatherv(sbuf,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
          call mpl_gatherv(sbufr,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
          call mpl_gatherv(sbufd,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
          call mpl_gatherv(sbufc,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_c)
        endif
          call work1(res)
        if ( res > 0 ) write(0,*) "error in work1 non-blocking alltoallv" ! this should not happen ever !!!
        call mpl_wait(request_r)
        call mpl_wait(request_d)
        call mpl_wait(request_i)
        call mpl_wait(request_c)
      enddo
    end select
    ! test values
    if (mpl_rank == kroot) then
      k=1
      do i=1,nprocs
        if ( any(rbuf(k:k+i-1) /= i) ) then
          !write(0,*) 'send ', mpl_rank, scounts, sbuf
          !write(0,*) 'recv ', mpl_rank, rcounts, rdispl
          write(msg,*) trim(mode)//" int alltoall test test failed on mpl_rank", mpl_rank, rbuf
          FAIL(msg)
        endif
        if ( any(nint(rbufr(k:k+i-1)) /= i) ) then
          write(msg,*) trim(mode)//" real alltoall test test failed on mpl_rank", mpl_rank, rbufr
          FAIL(msg)
        endif
        if ( any(nint(rbufd(k:k+i-1)) /= i) ) then
          write(msg,*) trim(mode)//" double alltoall test test failed on mpl_rank", mpl_rank, rbufd
          FAIL(msg)
        endif
        if ( rbufc(i)(1:1) /= char(ichar('a')+mod(i-1,26)) ) then
              write(msg,*) trim(mode)//" char alltoall test test failed on mpl_rank", mpl_rank, rbufc(i)
            FAIL(msg)
        endif
        k=k+i
      enddo
    endif

      ! test with displacement arguments
      ! leaving a space of 1 between each block
      select case(mode)
      case("nonblocking")
        rdispl(1)=0
        rdispl(1)=1
        do i=2,nprocs
          rdispl(i)=rdispl(i-1)+(i-1)+1
        enddo
        rbuf = 0
        call mpl_gatherv(sbuf, kroot, rbuf, rcounts, krecvdispl=rdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
        call work1(res)
        if ( res > 0 ) write(0,*) "error in  work1 non-blocking alltoallv" ! this should not happen ever !!!
        call mpl_wait(request_i)
        
        if (mpl_rank == kroot) then
          !write(*,*) "rbuf", rbuf
          k=1
          do i=1,nprocs     
        if ( any(rbuf(k+1:k+1+i-1) /= i) .or. rbuf(k) /= 0 ) then
          write(msg,*) trim(mode)//" int alltoall test with displ args failed on mpl_rank", mpl_rank, rbuf
          FAIL(msg)
        endif
        k=k+i+1
          enddo
        endif
      end select
      
  end subroutine do_gatherv

end program test_mpl_gatherv
