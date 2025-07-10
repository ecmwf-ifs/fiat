!(C) Copyright 2005- ECMWF.
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
  
  write(0,'(A,I0,A)') "TEST FAILED in scatterv.F90 @ line ",line," :"
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


program test_mpl_scatterv
use ec_parkind, only : jpim, jprm, jprd
use mpl_module, only: mpl_init, mpl_end, mpl_rank, linitmpi_via_mpl, mpl_scatterv, JP_NON_BLOCKING_STANDARD, mpl_wait

implicit none

integer(jpim) :: nprocs


call mpl_init(KPROCS=nprocs,ldinfo=.false.,ldenv=.true.)

if( nprocs <= 1 ) FAIL("nprocs must be > 1")
if( mpl_rank < 1 .or. mpl_rank > nprocs  ) FAIL("mpl_rank must be >= 1 and <= nprocs")
if( .not. linitmpi_via_mpl ) FAIL("linitmpi_via_mpl must be True")

call do_scatterv("blocking")

call do_scatterv("nonblocking")

call mpl_end(ldmeminfo=.false.)
! Note that with mpi_serial meminfo will not be printed regardless of ldmeminfo

contains

  subroutine do_scatterv(mode)
    implicit none
    character(len=*), intent(in) :: mode

    character(len=128) :: msg
    integer request_i, request_r, request_d, request_c, i, j, k, res
    integer sdispl(nprocs), rcounts_c(nprocs)
    integer(jpim), allocatable :: sbuf(:), rbuf(:), scounts(:)
    integer(jpim) :: rcounts
    real(jprm), allocatable :: sbufr(:), rbufr(:)
    real(jprd), allocatable :: sbufd(:), rbufd(:)
    integer kroot

    allocate(rbuf(mpl_rank),sbuf((nprocs*(nprocs+1))/2+nprocs),&
        rbufr(mpl_rank),sbufr((nprocs*(nprocs+1))/2+nprocs), &
        rbufd(mpl_rank),sbufd((nprocs*(nprocs+1))/2+nprocs),&
        scounts(nprocs))
    k=1
    do i=1,nprocs
        do j=0,i-1
            sbuf(k+j)=i
            sbufr(k+j)=real(i)
            sbufd(k+j)=real(i,kind=jprd)
        enddo
        k=k+i
    enddo

    rcounts=mpl_rank
    do i=1,nprocs
      scounts(i)=i
    enddo

    kroot=2

    select case(mode)
    case("blocking")
      call mpl_scatterv(rbuf,kroot,sbuf,scounts)
      call mpl_scatterv(rbufr,kroot,sbufr,scounts)
      if (mpl_rank == kroot) then
        call mpl_scatterv(rbufd,kroot,sbufd,scounts)
      else
        call mpl_scatterv(rbufd,kroot)
      endif    
    case("nonblocking")
      ! trying to get a random failure
      do j=1,1
        if (mpl_rank == kroot ) then
          call mpl_scatterv(rbuf,kroot,sbuf,scounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
          call mpl_scatterv(rbufr,kroot,sbufr,scounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
          call mpl_scatterv(rbufd,kroot,sbufd,scounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
        else
          call mpl_scatterv(rbuf,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
          call mpl_scatterv(rbufr,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
          call mpl_scatterv(rbufd,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
        endif
          call work1(res)
        if ( res > 0 ) write(0,*) "error in work1 non-blocking alltoallv" ! this should not happen ever !!!
        call mpl_wait(request_r)
        call mpl_wait(request_d)
        call mpl_wait(request_i)
      enddo
    end select
    ! test values
    if ( any(rbuf /= mpl_rank) ) then
      !write(0,*) 'send ', mpl_rank, scounts, sbuf
      !write(0,*) 'recv ', mpl_rank, rcounts, rdispl
      write(msg,*) trim(mode)//" int scatterv test failed on mpl_rank", mpl_rank, rbuf
          FAIL(msg)
    endif
    if ( any(nint(rbufr) /= mpl_rank) ) then
      write(msg,*) trim(mode)//" real scatterv test failed on mpl_rank", mpl_rank, rbufr
      FAIL(msg)
    endif
    if ( any(nint(rbufd) /= mpl_rank) ) then
      write(msg,*) trim(mode)//" double scatterv test failed on mpl_rank", mpl_rank, rbufd
      FAIL(msg)
    endif

    ! test with displacement arguments
    k=1
    do i=1,nprocs
      sbuf(k)=-1
      sbufr(k)=-1.0
      sbufd(k)=-1.0
      do j=1,i
          sbuf(k+j)=i
          sbufr(k+j)=real(i)
          sbufd(k+j)=real(i,kind=jprd)
      enddo
      k=k+i+1
    enddo

  rcounts=mpl_rank
  do i=1,nprocs
    scounts(i)=i
    sdispl(i)=sum(scounts(1:i-1)) + i
  enddo

  kroot=1

  select case(mode)
  case("blocking")
    call mpl_scatterv(rbuf,kroot,sbuf,scounts,sdispl)
    call mpl_scatterv(rbufr,kroot,sbufr,scounts,sdispl)
    if (mpl_rank == kroot) then
      call mpl_scatterv(rbufd,kroot,sbufd,scounts,sdispl)
    else
      call mpl_scatterv(rbufd,kroot)
    endif    
  case("nonblocking")
    ! trying to get a random failure
    do j=1,1
      if (mpl_rank == kroot ) then
        call mpl_scatterv(rbuf,kroot,sbuf,scounts, sdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
        call mpl_scatterv(rbufr,kroot,sbufr,scounts, sdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
        call mpl_scatterv(rbufd,kroot,sbufd,scounts, sdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
      else
        call mpl_scatterv(rbuf,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
        call mpl_scatterv(rbufr,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
        call mpl_scatterv(rbufd,kroot, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
      endif
        call work1(res)
      if ( res > 0 ) write(0,*) "error in work1 non-blocking alltoallv" ! this should not happen ever !!!
      call mpl_wait(request_r)
      call mpl_wait(request_d)
      call mpl_wait(request_i)
    enddo
  end select
  ! test values
  if ( any(rbuf /= mpl_rank) ) then
    !write(0,*) 'send ', mpl_rank, scounts, sbuf
    !write(0,*) 'recv ', mpl_rank, rcounts, rdispl
    write(msg,*) trim(mode)//" int scatterv with displ test failed on mpl_rank", mpl_rank, rbuf
        FAIL(msg)
  endif
  if ( any(nint(rbufr) /= mpl_rank) ) then
    write(msg,*) trim(mode)//" real scatterv with displ test failed on mpl_rank", mpl_rank, rbufr
    FAIL(msg)
  endif
  if ( any(nint(rbufd) /= mpl_rank) ) then
    write(msg,*) trim(mode)//" double scatterv with displ test failed on mpl_rank", mpl_rank, rbufd
    FAIL(msg)
  endif

      
  end subroutine do_scatterv

end program test_mpl_scatterv
