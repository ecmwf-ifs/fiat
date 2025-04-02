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

   write(0,'(A,I0,A)') "TEST FAILED in allgatherv.F90 @ line ",line," :"
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


program test_mpl_allgatherv
   use ec_parkind, only : jpim, jprm, jprd
   use mpl_module, only: mpl_init, mpl_end, mpl_rank, linitmpi_via_mpl, mpl_allgatherv, JP_NON_BLOCKING_STANDARD, mpl_wait

   implicit none

   integer(jpim) :: nprocs
   logical :: verbose = .false.

   character(len=256) msg

   call mpl_init(KPROCS=nprocs,ldinfo=verbose,ldenv=.true.)

   if( nprocs <= 1 ) FAIL("nprocs must be > 1")
   if( mpl_rank < 1 .or. mpl_rank > nprocs  ) FAIL("mpl_rank must be >= 1 and <= nprocs")
   if( .not. linitmpi_via_mpl ) FAIL("linitmpi_via_mpl must be True")

   call do_allgatherv("blocking")

   call do_allgatherv("nonblocking")

   call mpl_end(ldmeminfo=verbose)
! Note that with mpi_serial meminfo will not be printed regardless of ldmeminfo

contains

   subroutine do_allgatherv(mode)
      use ec_parkind, only : jpim, jprm, jprd
      implicit none
      character(len=*), intent(in) :: mode

      integer(jpim), allocatable :: sbuf(:), rbuf(:), rcounts(:)
      integer(jpim) :: scounts
      real(jprm), allocatable :: sbufr(:), rbufr(:)
      real(jprd), allocatable :: sbufd(:), rbufd(:)
      character(len=256) :: msg

      integer request_i, request_r, request_d, i, j, k, res
      integer rdispl(nprocs)


      allocate(sbuf(mpl_rank),rbuf((nprocs*(nprocs+1))/2+nprocs),&
         sbufr(mpl_rank),rbufr((nprocs*(nprocs+1))/2+nprocs), &
         sbufd(mpl_rank),rbufd((nprocs*(nprocs+1))/2+nprocs),&
         rcounts(nprocs))


      sbuf    = mpl_rank
      sbufr   = mpl_rank
      sbufd   = mpl_rank
      scounts = mpl_rank
      rcounts = [ (i, i=1,nprocs) ]

      select case(mode)
       case("blocking")
         call mpl_allgatherv(sbuf,rbuf,rcounts)
         call mpl_allgatherv(sbufr,rbufr,rcounts)
         call mpl_allgatherv(sbufd,rbufd,rcounts)
       case("nonblocking")
         ! trying to get a random failure
         do j=1,1
            call mpl_allgatherv(sbuf,rbuf,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
            call mpl_allgatherv(sbufr,rbufr,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
            call mpl_allgatherv(sbufd,rbufd,rcounts, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
            call work1(res)
            if ( res > 0 ) write(0,*) "error in work1 non-blocking alltoallv" ! this should not happen ever !!!
            call mpl_wait(request_r)
            call mpl_wait(request_d)
            call mpl_wait(request_i)
         enddo
      end select
      ! test values
      k=1
      do i=1,nprocs
         if ( any(rbuf(k:k+i-1) /= i) ) then
            !write(0,*) 'send ', mpl_rank, scounts, sbuf
            !write(0,*) 'recv ', mpl_rank, rcounts, rdispl
            write(msg,*) trim(mode)//" int allgatherv test test failed on mpl_rank", mpl_rank, rbuf
            FAIL(msg)
         endif
         if ( any(nint(rbufr(k:k+i-1)) /= i) ) then
            write(msg,*) trim(mode)//" real allgatherv test test failed on mpl_rank", mpl_rank, rbufr
            FAIL(msg)
         endif
         if ( any(nint(rbufd(k:k+i-1)) /= i) ) then
            write(msg,*) trim(mode)//" double allgatherv test test failed on mpl_rank", mpl_rank, rbufd
            FAIL(msg)
         endif
         k=k+i
      enddo

      ! test with displacement arguments
      ! leaving a space of 1 between each block
      rdispl(1)=1
      do i=2,nprocs
         rdispl(i)=rdispl(i-1)+(i-1)+1
      enddo
      select case(mode)
       case("blocking")
         call mpl_allgatherv(sbuf,rbuf,rcounts, KRECVDISPL=rdispl)
         call mpl_allgatherv(sbufr,rbufr,rcounts, KRECVDISPL=rdispl)
         call mpl_allgatherv(sbufd,rbufd,rcounts, KRECVDISPL=rdispl)
       case("nonblocking")
         ! trying to get a random failure
         do j=1,1
            call mpl_allgatherv(sbuf,rbuf,rcounts,KRECVDISPL=rdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
            call mpl_allgatherv(sbufr,rbufr,rcounts,KRECVDISPL=rdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_r)
            call mpl_allgatherv(sbufd,rbufd,rcounts,KRECVDISPL=rdispl, KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_d)
            call work1(res)
            if ( res > 0 ) write(0,*) "error in work1 non-blocking alltoallv" ! this should not happen ever !!!
            call mpl_wait(request_r)
            call mpl_wait(request_d)
            call mpl_wait(request_i)
         enddo
      end select
      ! test values
      k=2
      do i=1,nprocs
         if ( any(rbuf(k:k+i-1) /= i) ) then
            !write(0,*) 'send ', mpl_rank, scounts, sbuf
            !write(0,*) 'recv ', mpl_rank, rcounts, rdispl
            write(msg,*) trim(mode)//" int allgatherv test failed on mpl_rank", mpl_rank, rbuf
            FAIL(msg)
         endif
         if ( any(nint(rbufr(k:k+i-1)) /= i) ) then
            write(msg,*) trim(mode)//" real allgatherv test failed on mpl_rank", mpl_rank, rbufr
            FAIL(msg)
         endif
         if ( any(nint(rbufd(k:k+i-1)) /= i) ) then
            write(msg,*) trim(mode)//" double allgatherv test failed on mpl_rank", mpl_rank, rbufd
            FAIL(msg)
         endif
         k=k+i+1
      enddo

      ! test for int_scalar
      do i=1,nprocs
         rcounts(i) = mod(i+1,2)
      enddo
      select case(mode)
       case("blocking")
         call mpl_allgatherv(mpl_rank,rbuf,rcounts,ksendcount=mod(mpl_rank+1,2))
       case("nonblocking")
         call mpl_allgatherv(mpl_rank,rbuf,rcounts,ksendcount=mod(mpl_rank+1,2), &
            KMP_TYPE = JP_NON_BLOCKING_STANDARD, KREQUEST=request_i)
         call mpl_wait(request_i)
      end select
      do i=1,nprocs/2
         if ( rbuf(i) /= 2*i) then
            write(msg,*) trim(mode)//" int_scalar allgatherv test failed on mpl_rank", mpl_rank, rbuf
            FAIL(msg)
         endif
      enddo

   end subroutine do_allgatherv

end program test_mpl_allgatherv
