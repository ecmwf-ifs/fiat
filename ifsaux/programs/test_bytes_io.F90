!
! Simple Test program
!

program bytes_io_test
use parkind1
use bytes_io_mod
implicit none

integer(jpim) :: unit, iret, ibuf, nbytes
integer(jpim) :: nbuffer(100)
integer(jpim), allocatable :: nrbuffer(:)
real(jprb) :: zbuffer(50)
real(jprb), allocatable :: zrbuffer(:)
integer(jpim) :: nrsize

do ibuf=1,size(nbuffer)
  nbuffer(ibuf)=ibuf
enddo

call bytes_io_open( unit, "testfile_bytes_io", "w", iret )
if( iret < 0 ) stop "open for write failed"

call bytes_io_write( unit, size(nbuffer), 4, iret  )
if( iret < 0 ) stop "writing failed"

call bytes_io_write( unit, nbuffer, size(nbuffer)*4, iret  )
if( iret < 0 ) stop "writing failed"

call bytes_io_close( unit, iret )
if( iret < 0 ) stop "close failed"

call bytes_io_open( unit, "testfile_bytes_io", "r", iret)
if( iret < 0 ) stop "open for read failed"

call bytes_io_read( unit, nrsize, 4, iret  )
if( iret < 0 ) stop "reading failed"

allocate( nrbuffer(nrsize) )
call bytes_io_read( unit, nrbuffer, nrsize*4, iret  )
if( iret < 0 ) stop "reading failed"

call bytes_io_close( unit, iret )
if( iret < 0 ) stop "close failed"


do ibuf=1,size(nbuffer)
  if( nrbuffer(ibuf) /= nbuffer(ibuf) ) then
    stop "rbuffer read not equal to nbuffer written"
  endif
enddo


!=============================


do ibuf=1,size(zbuffer)
  zbuffer(ibuf)=ibuf
enddo

call bytes_io_open( unit, "testfile_bytes_io", "w", iret )
if( iret < 0 ) stop "open for write failed"

call bytes_io_write( unit, size(zbuffer), 4, iret  )
if( iret < 0 ) stop "writing failed"

call bytes_io_write( unit, zbuffer, size(zbuffer)*4, iret  )
if( iret < 0 ) stop "writing failed"

call bytes_io_close( unit, iret )
if( iret < 0 ) stop "close failed"

call bytes_io_open( unit, "testfile_bytes_io", "r", iret)
if( iret < 0 ) stop "open for read failed"

call bytes_io_read( unit, nrsize, 4, iret  )
if( iret < 0 ) stop "reading failed"

allocate( zrbuffer(nrsize) )
call bytes_io_read( unit, zrbuffer, nrsize*4, iret  )
if( iret < 0 ) stop "reading failed"

call bytes_io_close( unit, iret )
if( iret < 0 ) stop "close failed"


do ibuf=1,size(nbuffer)
  if( zrbuffer(ibuf) /= zbuffer(ibuf) ) then
    stop "nrbuffer read not equal to nbuffer written"
  endif
enddo

write(0,'(A)') "SUCCESS"
end program
