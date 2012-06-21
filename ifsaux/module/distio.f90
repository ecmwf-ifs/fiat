!OPTIONS NOOPT
      MODULE distio
      IMPLICIT NONE
      PRIVATE

      INTERFACE COMM_ARRAY
      MODULE PROCEDURE real_COMM_ARRAY, int_COMM_ARRAY
      END INTERFACE

      PUBLIC :: 
     $     DIST_OPEN, DIST_CLOSE, 
     $     DIST_INQUIRE, 
     $     DIST_PBOPEN, DIST_PBCLOSE,
     $     GET_NEXT_UNIT, GET_MRFSDIR, GET_DIST_MAXFILESIZE,
     $     MRFSFILE

      integer  mpe_myrank, mpe_nproc
      external mpe_myrank, mpe_nproc
      integer myproc, nproc
      integer iret, ilen, itype, iroot, itag, icomm, iwords

      integer, parameter :: jpe_byte    = 0
      integer, parameter :: jpe_integer = 1
      integer, parameter :: jpe_real    = 2
      integer, parameter :: jpintbyt    = 4
      integer, parameter :: jpreabyt    = 8

      character(len=*), parameter :: notdef = 'NOT DEFINED'
      character(len=255), save :: MRFSDIR          = notdef
      character(len=12),  save :: DIST_MAXFILESIZE = notdef

      logical, save :: LL_has_MRFSDIR = .FALSE.
      integer, save :: maxfilesize    = 8 * 1024 * 1024 ! 8MB

      integer, parameter :: jp_maxunit = 99
      integer, parameter :: jp_minunit =  0

      CONTAINS

c=======================================================================
c---- Public routines ----
c=======================================================================

      SUBROUTINE DIST_CLOSE(unit, file, status, iostat)
      implicit none
      integer, intent(in)                    :: unit
      character(len=*), intent(in), optional :: file, status
      integer, intent(out),         optional :: iostat

      integer ierr, i
      logical opened
      character(len=255) cl_file
      character(len=80)  cl_status

      ierr = 0

      if (present(status)) then
         cl_status = adjustl(status)
         call TOUPPER(cl_status)
      else
         cl_status = 'KEEP'
      endif

      inquire(unit=unit, opened=opened)

      if (opened) then
         if (present(file) .and. .not. present(status)) then
c--   Try to remove the "file" if in MRFSDIR to conserve MRFS-space 
c     (after all, the "file" was supposed to be a tmp-file)

            CALL GET_MRFSDIR()
            if (LL_has_MRFSDIR) then
               cl_file = adjustl(file)
               i = index(trim(cl_file), trim(MRFSDIR))
               if (i > 0) cl_status = 'DELETE'
            endif
         endif

         close(unit, status=trim(cl_status), iostat=ierr)
      endif

      if (present(iostat)) then
         iostat = ierr
      endif
      END SUBROUTINE DIST_CLOSE

c=======================================================================

      SUBROUTINE DIST_PBCLOSE(unit, file, status, iostat)
      implicit none
      integer, intent(inout)                 :: unit
      character(len=*), intent(in), optional :: file, status
      integer, intent(out),         optional :: iostat

      integer ierr, i
      character(len=255) cl_file
      character(len=80)  cl_status

      ierr = 0

      if (present(status)) then
         cl_status = adjustl(status)
         call TOUPPER(cl_status)
      else
         cl_status = 'KEEP'
      endif

      call PBCLOSE(unit, ierr)

      if (ierr == 0 .and. present(file)) then
c--   Remove "file" if in MRFSDIR to conserve MRFS-space 
c     (after all, the "file" was supposed to be a tmp-file)

         cl_file = adjustl(file)

         if (.not. present(status)) then
            CALL GET_MRFSDIR()
            if (LL_has_MRFSDIR) then
               i = index(trim(cl_file), trim(MRFSDIR))
               if (i > 0) cl_status = 'DELETE'
            endif
         endif

         if (cl_status == 'DELETE') 
     $        call util_remove_file(trim(cl_file), ierr)
      endif

      if (present(iostat)) then
         iostat = ierr
      endif
      END SUBROUTINE DIST_PBCLOSE

c=======================================================================

      SUBROUTINE DIST_INQUIRE(file, exist)
      implicit none
      character(len=*), intent(in)   :: file
      logical,          intent(out)  :: exist
      integer i_exist

      i_exist = 0
      nproc  = MPE_NPROC()
      myproc = MPE_MYRANK()

      if (myproc == 1) then
         inquire(file=file, exist=exist)
         if (exist) i_exist = 1
      endif

      if (nproc > 1) then
c--   Broadcast the file existence status
         ilen  = 1
         itype = jpe_integer
         iroot = 1
         itag  = 500
         icomm = 0
         call MPE_BROADCAST(
     $        i_exist, ilen, itype, iroot, itag, 
     $        icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('DIST_INQUIRE: MPE_BROADCAST(tag=500)')
      endif

      exist = (i_exist == 1)
      END SUBROUTINE DIST_INQUIRE

c=======================================================================

      SUBROUTINE DIST_OPEN(
c--   Optional parameters (FORTRAN-OPEN style) --
     $     unit, file, iostat,
     $     status, form, action,
     $     access, recl, 
     $     fmt,
c--   Optional ARRAYs (real ARRAY has presedence over the integer ARRAY)
     $     ARRAY, IARRAY,
c--   The actual filename on which the I/O is (was) applied to
     $     localfile)

c
c..   A subroutine to open the same file only by the processor#1
c     and then distribute it to the other processors via
c     fast communication network.
c
c     Remote processors save the contents into their (preferably)
c     memory resident file system and open it there and return
c     a handle.
c
c     ... or ...
c
c     if one of the ARRAYs are provided, then data is read into it
c     by processor#1 and distributed to others
c
c     NOTE: Initially meant only for Read/Only -files
c
c     Algorithm(s) used:
c     ==================
c
c     (1) Processor#1 (raw-)reads the file
c         and sends it to all other processors (incl. itself)
c     (2) All processors store the bytes into their memory resident
c         file system and issue appropriate OPEN to that file
c     (3) File is silently closed if unit was not specified
c
c     except if:
c
c        If one of the arrays ARRAY or IARRAY exist, then the phase (2)
c        is skipped, but ARRAY/IARRAY is read in by proc#1 and xferred
c        to other PEs via network.
c
c        After this file is closed if no unit number was given
c
c     If environment-value MRFSDIR is not defined, then
c     all processors are forced to OPEN the same (shared) file.
c
c     Author: Sami Saarinen, ECMWF, 18/11/97
c

      implicit none

c--   unit : I/O-channel
      integer, intent(in), optional       :: unit

c--   file: Target file, that processor#1 reads
      character(len=*), intent(in), optional :: file

c--   iostat: An error code from the latest I/O operation
c             OR internal error from the DISTIO-routine (iostat < -10000)
c     -10001 : Both file name AND unit number were not given
c     -10002 : File is not read/only
c     -10004 : Free format, textual direct access read not allowed
c     -10008 : Direct access file has record length <= 0
c     -10016 : Neither unit number, nor ARRAY/IARRAY were supplied
c     -10032 : Format error
c     or combination of internal errors; find out via "mod(-ierr,10000)"
c
      integer, intent(out), optional :: iostat

c--   status, form, action, access, recl: FORTRAN-OPEN's keywords
      character(len=*), intent(in), optional :: 
     $     status, form, action, access
      integer, intent(in), optional :: recl

c--   fmt: The possible format to be used when reading an ARRAY/IARRAY
      character(len=*), intent(in), optional :: fmt

c--   ARRAY: Real array
      real, intent(out), optional :: ARRAY(:)

c--   IARRAY: Integer array
      integer, intent(out), optional :: IARRAY(:)

c--   localfile: The actual file name where the handle 'unit' (possibly)  refers to
c              An output parameter that (usually) has a value $MRFSDIR/file
      character(len=*), intent(out), optional :: localfile

! === END OF INTERFACE BLOCK ===

      integer ierr, itmp
      integer i_unit, i_recl
      logical LL_open, LL_close, LL_has_been_communicated
      logical LL_read_only, LL_formatted, LL_direct_access
      logical LL_real_array, LL_has_ARRAY
      character(len=255) cl_file, cl_tmpname
      character(len= 80) 
     $     cl_status, cl_action, cl_form, cl_access, cl_fmt 

c234567890-234567890-234567890-234567890-234567890-234567890-234567890--

      ierr = 0
      nproc  = MPE_NPROC()
      myproc = MPE_MYRANK()

      LL_open  = .FALSE.
      LL_close = .FALSE.

c--   unit
      if (present(unit)) then
         i_unit = unit
      else
         call GET_NEXT_UNIT(i_unit)
         LL_close = .TRUE.
      endif

c--   file
      if (present(file)) then
         cl_file = adjustl(file)
      else
         if (i_unit >= jp_minunit .and. i_unit <= jp_maxunit) then
            write(cl_file,"('fort.',i4)") i_unit
            call STRIP(cl_file,' ') ! File became "fort.<unit>"
         else
            cl_file = notdef
            ierr = ierr + 1
         endif
      endif

c--   status
      if (present(status)) then
         cl_status = adjustl(status)
         call TOUPPER(cl_status)
      else
         cl_status = 'OLD'
      endif

c--   action
      if (present(action)) then
         cl_action = adjustl(action)
         call TOUPPER(cl_action)
      else
         cl_action = 'READ'
      endif

      LL_read_only = (
     $     (cl_status == 'OLD' .or. cl_status == 'UNKNOWN')
     $     .and. 
     $     (cl_action == 'READ') )

      if (.not. LL_read_only) then
         ierr = ierr + 2
      endif

c--   fmt
      if (present(fmt)) then
         cl_fmt = fmt
      else
         cl_fmt = '*'
      endif

c--   form
      if (present(form)) then
         cl_form = adjustl(form)
         call TOUPPER(cl_form)
      else
         cl_form = 'FORMATTED'
      endif

      LL_formatted = (cl_form == 'FORMATTED')

      if (LL_formatted) then
         if (cl_fmt /= '*') then
            itmp = len_trim(cl_fmt)
            if (itmp <= 2) then
               ierr = ierr + 32
            else if (cl_fmt(1:1) /= '(' .and. 
     $               cl_fmt(itmp:itmp) /= ')') then
               ierr = ierr + 32
            endif
         endif
      endif

c--   access
      if (present(access)) then
         cl_access = adjustl(access)
         call TOUPPER(cl_access)
      else
         cl_access = 'SEQUENTIAL'
      endif

      LL_direct_access = (cl_access == 'DIRECT')

      if ( LL_direct_access .and. LL_formatted 
     $     .and. cl_fmt == '*') then
         ierr = ierr + 4
      endif

c--   recl
      if (present(recl)) then
         i_recl = recl
      else
         i_recl = 0
      endif

      if (LL_direct_access .and. i_recl <= 0) then
         ierr = ierr + 8
      endif

c--   ARRAY or IARRAY
      LL_real_array = .TRUE.

      if (present(ARRAY)) then
         LL_real_array = .TRUE.
      else if (present(IARRAY)) then
         LL_real_array = .FALSE.
      endif

      LL_has_ARRAY = (present(ARRAY) .or. present(IARRAY))

      if (.not. LL_has_array .and. .not. present(unit)) then
         ierr = ierr + 16
      endif

c=======================================================================
      if (ierr /= 0) ierr = -(ierr + 10000)
      if (ierr < 0) goto 9999
c=======================================================================

c--   Is the memory resident file system available ?
      CALL GET_MRFSDIR()

c--   Max file size that is allowed to go over the network
      CALL GET_DIST_MAXFILESIZE()

c=======================================================================
      if (.not. LL_has_ARRAY) then
c--   Communicate the file over the network
         call COMM_FILE(cl_file, LL_has_been_communicated)
         LL_open = .TRUE.
      else
         LL_has_been_communicated = .FALSE.
         LL_open  = (myproc == 1)
         LL_close = LL_open
      endif
c=======================================================================

      if (LL_has_been_communicated) then
         call MAKE_LOCAL_FILENAME(cl_tmpname, cl_file)
         cl_file = cl_tmpname
      endif

      if (present(localfile)) then
         if (scan(cl_file,'/') > 0) then
            localfile = adjustl(cl_file)
         else
            localfile = './'//adjustl(cl_file)
         endif
      endif

      if (LL_open) then
c--   Open the file via Fortran-OPEN

         if (LL_formatted) then

c--   Formatted file
            if (LL_direct_access) then
c---  .. Direct access
               OPEN(unit=i_unit, file=trim(cl_file),
     $              status=trim(cl_status), form='FORMATTED',
     $              access='DIRECT', recl=i_recl, action='READ',
     $              iostat=ierr, err=9999)
            else
c---  .. Sequential
               OPEN(unit=i_unit, file=trim(cl_file),
     $              status=trim(cl_status), form='FORMATTED',
     $              access='SEQUENTIAL', action='READ',
     $              position='REWIND',
     $              iostat=ierr, err=9999)
            endif

         else

c--   Unformatted file
            if (LL_direct_access) then
c---  .. Direct access
               OPEN(unit=i_unit, file=trim(cl_file),
     $              status=trim(cl_status), form='UNFORMATTED',
     $              access='DIRECT', recl=i_recl, action='READ',
     $              iostat=ierr, err=9999)
            else
c---  .. Sequential
               OPEN(unit=i_unit, file=trim(cl_file),
     $              status=trim(cl_status), form='UNFORMATTED',
     $              access='SEQUENTIAL', action='READ',
     $              position='REWIND',
     $              iostat=ierr, err=9999)
            endif

         endif

      endif
      
c=======================================================================
      if (LL_has_ARRAY) then
         if (LL_real_array) then
            call COMM_ARRAY(i_unit, ARRAY, 
     $           trim(adjustl(cl_fmt)), ierr,
     $           LL_direct_access, LL_formatted)
         else
            call COMM_ARRAY(i_unit, IARRAY, 
     $           trim(adjustl(cl_fmt)), ierr,
     $           LL_direct_access, LL_formatted)
         endif
      endif

      if (ierr /= 0) goto 9999
c=======================================================================

      if (LL_close) CLOSE(i_unit, iostat=ierr, err=9999)
         
 9999 continue
      if (present(iostat)) then
         iostat = ierr
      endif
      return
      END SUBROUTINE dist_open

c=======================================================================

      SUBROUTINE DIST_PBOPEN(
c--   Optional parameters (FORTRAN-OPEN style) --
     $     unit, file, iostat,
c--   Optional ARRAYs (real ARRAY has presedence over the integer ARRAY)
     $     ARRAY, IARRAY,
c--   The actual filename on which the I/O is (was) applied to
     $     localfile)

c
c..   A subroutine to open the same file only by the processor#1
c     and then distribute it to the other processors via
c     fast communication network.
c
c     Remote processors save the contents into their (preferably)
c     memory resident file system and open it there and return
c     a handle.
c
c     ... or ...
c
c     if one of the ARRAYs are provided, then data is read into it
c     by processor#1 and distributed to others
c
c     NOTE: Initially meant only for Read/Only -files
c
c     Algorithm(s) used:
c     ==================
c
c     (1) Processor#1 (raw-)reads the file
c         and sends it to all other processors (incl. itself)
c     (2) All processors store the bytes into their memory resident
c         file system and issue appropriate PBOPEN to that file
c     (3) File is silently closed if unit was not specified
c
c     except if:
c
c        If one of the arrays ARRAY or IARRAY exist, then the phase (2)
c        is skipped, but ARRAY/IARRAY is read in by proc#1 and xferred
c        to other PEs via network.
c
c        After this file is closed if no unit number was given
c
c     If environment-value MRFSDIR is not defined, then
c     all processors are forced to OPEN the same (shared) file.
c
c     Author: Sami Saarinen, ECMWF, 18/11/97
c

      implicit none

c--   unit : I/O-channel
      integer, intent(inout) , optional      :: unit

c--   file: Target file, that processor#1 reads
      character(len=*), intent(in) , optional:: file

c--   iostat: An error code from the latest I/O operation
c             OR internal error from the DISTIO-routine (iostat < -10000)
c     -10016 : Neither unit number, nor ARRAY/IARRAY were supplied
c     -10064 : File name was not given
c     or combination of internal errors; find out via "mod(-ierr,10000)"
c
      integer, intent(out), optional :: iostat

c--   ARRAY: Real array
      real, intent(out), optional :: ARRAY(:)

c--   IARRAY: Integer array
      integer, intent(out), optional :: IARRAY(:)

c--   localfile: The actual file name where the handle 'unit' (possibly)  refers to
c              An output parameter that (usually) has a value $MRFSDIR/file
      character(len=*), intent(out), optional :: localfile

! === END OF INTERFACE BLOCK ===

      integer ierr, i_unit
      logical LL_open, LL_close, LL_has_been_communicated
      logical LL_real_array, LL_has_ARRAY
      character(len=255) cl_file, cl_tmpname

c234567890-234567890-234567890-234567890-234567890-234567890-234567890--
      
      ierr = 0

      nproc  = MPE_NPROC()
      myproc = MPE_MYRANK()

      LL_open  = .FALSE.
      LL_close = .FALSE.

c--   unit
      if (.not. present(unit)) then
         LL_close = .TRUE.
      endif

c--   file
      if (present(file)) then
         cl_file = adjustl(file)
      else
         cl_file = notdef
         ierr = ierr + 64
      endif

c--   ARRAY or IARRAY
      LL_real_array = .TRUE.

      if (present(ARRAY)) then
         LL_real_array = .TRUE.
      else if (present(IARRAY)) then
         LL_real_array = .FALSE.
      endif

      LL_has_ARRAY = (present(ARRAY) .or. present(IARRAY))

      if (.not. LL_has_array .and. .not. present(unit)) then
         ierr = ierr + 16
      endif

c=======================================================================
      if (ierr /= 0) ierr = -(ierr + 10000)
      if (ierr < 0) goto 9999
c=======================================================================

c--   Is the memory resident file system available ?
      CALL GET_MRFSDIR()

c--   Max file size that is allowed to go over the network
      CALL GET_DIST_MAXFILESIZE()

c=======================================================================
      if (.not. LL_has_ARRAY) then
c--   Communicate the file over the network
         call COMM_FILE(cl_file, LL_has_been_communicated)
         LL_open = .TRUE.
      else
         LL_has_been_communicated = .FALSE.
         LL_open  = (myproc == 1)
         LL_close = LL_open
      endif
c=======================================================================

      if (LL_has_been_communicated) then
         call MAKE_LOCAL_FILENAME(cl_tmpname, cl_file)
         cl_file = cl_tmpname
      endif

      if (present(localfile)) then
         if (scan(cl_file,'/') > 0) then
            localfile = adjustl(cl_file)
         else
            localfile = './'//adjustl(cl_file)
         endif
      endif

      if (LL_open) then
c--   Open the file via PB I/O's PBOPEN
         call PBOPEN(i_unit, trim(cl_file), 'r', ierr)
      endif
      
c=======================================================================
      if (LL_has_ARRAY) then
         if (LL_real_array) then
            call COMM_ARRAY(i_unit, ARRAY, 
     $           'PBIO', ierr)
         else
            call COMM_ARRAY(i_unit, IARRAY, 
     $           'PBIO', ierr)
         endif
      endif

      if (ierr /= 0) goto 9999
c=======================================================================

      if (LL_close) then
         call PBCLOSE(i_unit, ierr)
      else 
         if (LL_open .and. present(unit)) unit = i_unit
      endif
         
 9999 continue
      if (present(iostat)) then
         iostat = ierr
      endif
      return
      END SUBROUTINE dist_pbopen

      SUBROUTINE GET_NEXT_UNIT(kunit)
      implicit none
      integer, intent(out) :: kunit
      integer j
      logical Lopened
      kunit = -1
      do j=jp_maxunit, jp_minunit, -1
         INQUIRE(unit=j, opened=Lopened)
         if (.not.Lopened) then
            kunit = j
            return
         endif
      enddo
      END SUBROUTINE GET_NEXT_UNIT

      SUBROUTINE GET_MRFSDIR(kout, cdout)
c--   Look for MRFSDIR (memory resident file system) environment variable
      implicit none
      integer, intent(in)          , optional :: kout
      character(len=*), intent(out), optional :: cdout
      integer i
      if (MRFSDIR == notdef) then
         call GETENV('MRFSDIR', MRFSDIR)
         MRFSDIR = adjustl(MRFSDIR)
         i = len_trim(MRFSDIR)
         LOOP: do while (i > 0)
c     Remove any trailing slashes '/'
            if (MRFSDIR(i:i) /= '/') exit LOOP
            MRFSDIR(i:i) = ' '
            i = i - 1
         enddo LOOP
         if (MRFSDIR == '.') MRFSDIR = ' '
         LL_has_MRFSDIR = (MRFSDIR /= ' ')

         if (present(kout)) then
            myproc = MPE_MYRANK()
            if (myproc == 1) then
               if (LL_has_MRFSDIR) then
                  write(kout,*)
     $                 'GET_MRFSDIR: MRFSDIR="'//trim(MRFSDIR)//'"'
               else
                  write(kout,*)
     $                 'GET_MRFSDIR: MRFSDIR not present'
               endif
            endif
         endif
      endif
      if (present(cdout)) then
         cdout = MRFSDIR
      endif
      END SUBROUTINE GET_MRFSDIR

      SUBROUTINE GET_DIST_MAXFILESIZE(kout, kmaxsize)
c--   Get the maximum permissible size of the file to be communicated
c     This option is to avoid excessively large files dealt
c     with this concept
      implicit none
      integer, intent(in) , optional :: kout
      integer, intent(out), optional :: kmaxsize
      if (DIST_MAXFILESIZE == notdef) then
         call GETENV('DIST_MAXFILESIZE', DIST_MAXFILESIZE)
         if (DIST_MAXFILESIZE /= ' ') then
            DIST_MAXFILESIZE = trim(adjustl(DIST_MAXFILESIZE))
            read(DIST_MAXFILESIZE,'(i12)',err=9999) maxfilesize
         endif
 9999    continue
         maxfilesize = max(-1,maxfilesize)

         if (present(kout)) then
            myproc = MPE_MYRANK()
            if (myproc == 1) then
               write(kout,'(1x,2a,i12,a)')
     $              'GET_DIST_MAXFILESIZE: ',
     $              'Largest file to be distributed: ',
     $              maxfilesize,' bytes'
            endif
         endif
      endif
      if (present(kmaxsize)) then
         kmaxsize = maxfilesize
      endif
      END SUBROUTINE GET_DIST_MAXFILESIZE

c=======================================================================
c---- Private routines ----
c=======================================================================

      SUBROUTINE toupper(cds)
      implicit none
c--   Converts lowercase letters to uppercase
      character(len=*), intent(inout) :: cds
      integer, parameter :: ich_A = ichar('A')
      integer, parameter :: icha  = ichar('a')
      integer, parameter :: ichz  = ichar('z')
      integer i, ich, new_ich, ilen
      character(len=1) ch
      ilen = len_trim(cds)
      do i=1,ilen
         ch = cds(i:i)
         ich = ichar(ch)
         if ( ich >= icha .and. ich <= ichz ) then
            new_ich = ich + (ich_A - icha)
            ch = char(new_ich)
            cds(i:i) = ch
         endif
      enddo
      END SUBROUTINE toupper

      SUBROUTINE strip(cds,cdwhat)
      implicit none
c--   Strips off all possible characters 'cdwhat'
      character(len=*), intent(inout) :: cds
      character(len=1), intent(in)    :: cdwhat
      character(len=len(cds)) cls
      integer i, j, ilen
      character(len=1) ch
      cls = ' '
      j = 0
      ilen = len_trim(cds)
      do i=1,ilen
         ch = cds(i:i)
         if (ch /= cdwhat) then
            j = j + 1
            cls(j:j) = ch
         endif
      enddo
      cds = trim(adjustl(cls))
      END SUBROUTINE strip

      SUBROUTINE make_local_filename(clfile, cdfile)
      implicit none
      character(len=*), intent(out) :: clfile
      character(len=*), intent(in)  :: cdfile
      logical, parameter :: LL_reverse = .TRUE.
      integer i
      clfile = adjustl(cdfile)
      i = scan(clfile,'/',LL_reverse) ! The basename after the path
      if (i > 0) clfile(1:i) = ' '
      clfile = trim(adjustl(clfile))
      clfile = trim(adjustl(MRFSDIR))//"/"//clfile ! $MRFSDIR/filename
      write(clfile,"(a,'.',i4.4)") trim(adjustl(clfile)),myproc
      END SUBROUTINE make_local_filename

      SUBROUTINE COMM_FILE(cdfile, ldstatus)
c--   Communicate the file
      implicit none
      character(len=*), intent(in) :: cdfile
      logical, intent(out)         :: ldstatus

      integer ifilesize
      integer, allocatable :: file_contents(:)
      logical LL_has_been_communicated
      character(len=255) cl_tmpname
      character(len=4)   cl_procid

      LL_has_been_communicated = .FALSE.

      if (nproc <= 1) goto 9999

      ifilesize = 0

      if (LL_has_MRFSDIR) then
         if (myproc == 1) then
            call UTIL_FILESIZE(trim(cdfile), ifilesize)
            if (maxfilesize /= -1) then
               if (ifilesize > maxfilesize) 
     $              ifilesize = -ifilesize
            endif
         endif
      endif

      ilen  = 1
      itype = jpe_integer
      iroot = 1
      itag  = 100
      icomm = 0
      call MPE_BROADCAST(
     $     ifilesize, ilen, itype, iroot, itag, 
     $     icomm, 0, 0, iret)
!      if (iret /= 0)
!     $call MPEI_ABORT('COMM_FILE: MPE_BROADCAST(tag=100)')
      
      if (ifilesize > 0) then
         iwords = (ifilesize + jpintbyt - 1)/jpintbyt
         ALLOCATE(file_contents(iwords))
         
c--   Only processor#1 reads it
         if (myproc == 1) then
            call UTIL_READRAW(
     $           trim(cdfile), file_contents, ifilesize, iret)
            if (iret /= ifilesize) then
               call MPEI_ABORT(
     $              'File "'//trim(cdfile)//'" read error at proc#1')
               stop '** Problems with UTIL_READRAW **'
            endif
         endif
         
c--   ... and then broadcasts to all
         
         ilen  = ifilesize
         itype = jpe_byte
         iroot = 1
         itag  = 200
         icomm = 0
         call MPE_BROADCAST(
     $        file_contents, ilen, itype, iroot, itag, 
     $        icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('COMM_FILE: MPE_BROADCAST(tag=200)')
         
c--   ... and finally store it into the *local* MRFSDIR
         
         call MAKE_LOCAL_FILENAME(cl_tmpname, cdfile)
         
         call UTIL_WRITERAW(
     $        trim(cl_tmpname), file_contents, ifilesize, iret)
         if (iret /= ifilesize) then
            write(cl_procid,'(i4)') myproc
            call MPEI_ABORT(
     $           'File "'//trim(cl_tmpname)//'" write error at proc#'
     $           //trim(adjustl(cl_procid)))
            stop '** Problems with UTIL_WRITERAW **'
         endif
         
         DEALLOCATE(file_contents)
         LL_has_been_communicated = .TRUE.
      endif

 9999 continue
      ldstatus = LL_has_been_communicated
      END SUBROUTINE COMM_FILE

      SUBROUTINE real_COMM_ARRAY(i_unit, ARRAY, 
     $           cdfmt, ierr,
     $           LL_direct_access, LL_formatted)
      implicit none
      integer, intent(in) :: i_unit
      real, intent(inout) :: ARRAY(:)
      logical, intent(in), optional ::
     $     LL_direct_access, LL_formatted
      character(len=*), intent(in) :: cdfmt
      integer, intent(out) :: ierr

      logical LL_free_format, LL_pbio
      integer i_size

      LL_free_format = (cdfmt == '*')
      LL_pbio        = (cdfmt == 'PBIO')
      i_size = size(ARRAY)

      if (myproc == 1) then
         if (LL_pbio) then
            call PBREAD(i_unit, ARRAY, i_size*jpreabyt, ierr)
            if (ierr == i_size*jpreabyt) ierr = 0
         else
            if (LL_formatted) then
               if (LL_direct_access) then
                  read(i_unit, fmt=cdfmt, iostat=ierr, err=9999,
     $                 rec=1) ARRAY
               else
                  if (LL_free_format) then
                     read(i_unit, fmt=*, iostat=ierr, err=9999,
     $                    end=9999) ARRAY
                  else
                     read(i_unit, fmt=cdfmt, iostat=ierr, err=9999,
     $                    end=9999) ARRAY
                  endif
               endif
            else
               if (LL_direct_access) then
                  read(i_unit, iostat=ierr, err=9999, rec=1) ARRAY
               else
                  read(i_unit, iostat=ierr, err=9999,
     $                 end=9999) ARRAY
               endif
            endif
         endif
      endif

 9999 continue

      if (nproc > 1) then

c--   Broadcast the error code
         ilen  = 1
         itype = jpe_integer
         iroot = 1
         itag  = 300
         icomm = 0
         call MPE_BROADCAST(
     $        ierr, ilen, itype, iroot, itag, 
     $        icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('REAL_COMM_ARRAY: MPE_BROADCAST(tag=300)')

         if (ierr == 0) then
c--   Broadcast the data itself if no errors
            ilen  = i_size
            itype = jpe_real
            iroot = 1
            itag  = 301
            icomm = 0
            call MPE_BROADCAST(
     $           ARRAY, ilen, itype, iroot, itag, 
     $           icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('REAL_COMM_ARRAY: MPE_BROADCAST(tag=301)')
         endif

      endif

      END SUBROUTINE real_COMM_ARRAY


      SUBROUTINE int_COMM_ARRAY(i_unit, IARRAY, 
     $           cdfmt, ierr,
     $           LL_direct_access, LL_formatted)
      implicit none
      integer, intent(in) :: i_unit
      integer, intent(inout) :: IARRAY(:)
      logical, intent(in), optional :: 
     $     LL_direct_access, LL_formatted
      character(len=*), intent(in) :: cdfmt
      integer, intent(out) :: ierr

      logical LL_free_format, LL_pbio
      integer i_size

      LL_free_format = (cdfmt == '*')
      LL_pbio        = (cdfmt == 'PBIO')
      i_size = size(IARRAY)

      if (myproc == 1) then
         if (LL_pbio) then
            call PBREAD(i_unit, IARRAY, i_size*jpintbyt, ierr)
            if (ierr == i_size*jpintbyt) ierr = 0
         else
            if (LL_formatted) then
               if (LL_direct_access) then
                  read(i_unit, fmt=cdfmt, iostat=ierr, err=9999,
     $                 rec=1) IARRAY
               else
                  if (LL_free_format) then
                     read(i_unit, fmt=*, iostat=ierr, err=9999,
     $                    end=9999) IARRAY
                  else
                     read(i_unit, fmt=cdfmt, iostat=ierr, err=9999,
     $                    end=9999) IARRAY
                  endif
               endif
            else
               if (LL_direct_access) then
                  read(i_unit, iostat=ierr, err=9999, rec=1) IARRAY
               else
                  read(i_unit, iostat=ierr, err=9999,
     $                 end=9999) IARRAY
               endif
            endif
         endif
      endif

 9999 continue

      if (nproc > 1) then

c--   Broadcast the error code
         ilen  = 1
         itype = jpe_integer
         iroot = 1
         itag  = 400
         icomm = 0
         call MPE_BROADCAST(
     $        ierr, ilen, itype, iroot, itag, 
     $        icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('INT_COMM_ARRAY: MPE_BROADCAST(tag=400)')

         if (ierr == 0) then
c--   Broadcast the data itself if no errors
            ilen  = i_size
            itype = jpe_integer
            iroot = 1
            itag  = 401
            icomm = 0
            call MPE_BROADCAST(
     $           IARRAY, ilen, itype, iroot, itag, 
     $           icomm, 0, 0, iret)
!         if (iret /= 0)
!     $   call MPEI_ABORT('INT_COMM_ARRAY: MPE_BROADCAST(tag=401)')
         endif

      endif

      END SUBROUTINE int_COMM_ARRAY

      SUBROUTINE mrfsfile(file_in, file_out)
!
!     A routine to prepend the "$MRFSDIR" in the front of the filename
!     If $MRFSDIR is not defined, then return original filename w/o changes.
!
!     Author: Sami Saarinen, ECMWF, 23/1/1998 for CY18R4
!


      implicit none

      character(len=*), intent(in)  :: file_in
      character(len=*), intent(out) :: file_out
! === END OF INTERFACE BLOCK ===

      character(len=255), save :: mrfsdir = ' '
      logical, save :: already_called = .FALSE.
      logical, save :: has_mrfsdir    = .FALSE.

!--   Cache the $MRFSDIR to avoid any further calls to GET_MRFSDIR()
      if (.not. already_called) then
         CALL GET_MRFSDIR(cdout = mrfsdir)
         has_mrfsdir = (mrfsdir /= ' ')
         already_called = .TRUE.
      endif

      if (has_mrfsdir) then
!--   Prepend "${MRFSDIR}/" and remove any leading/trailing blanks present
         file_out = 
     &   trim(adjustl(mrfsdir))//'/'//trim(adjustl(file_in))
      else
!--   No change
         file_out = file_in
      endif

      END SUBROUTINE mrfsfile

      END MODULE distio


