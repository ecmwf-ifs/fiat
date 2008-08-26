!OPTIONS NOOPT
MODULE strhandler_mod

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

PRIVATE

PUBLIC :: tolower, toupper, expand_string
PUBLIC :: sadjustl, sadjustr
PUBLIC :: stransfer

INTERFACE stransfer
MODULE PROCEDURE &
  stransfer_r8_to_str, &
  stransfer_str_to_r8
END INTERFACE


CONTAINS


FUNCTION stransfer_r8_to_str(source, mold) result(c)
REAL(KIND=JPRB) , intent(in) :: source
character(len=*), intent(in) :: mold
character(len=8) :: c
call ecmwf_transfer(c,MIN(8,len(mold)),source,8)
END FUNCTION stransfer_r8_to_str


FUNCTION stransfer_str_to_r8(source, mold) result(z)
character(len=*), intent(in) :: source
REAL(KIND=JPRB) , intent(in) :: mold
REAL(KIND=JPRB) :: z
call ecmwf_transfer(z,8,source,len(source))
END FUNCTION stransfer_str_to_r8


FUNCTION sadjustl(s) RESULT(c)
character(len=*), intent(in) :: s
character(len=max(1,len(s))) c
c = ' '
if (len(s) > 0) then
  if (s /= ' ') c = adjustl(s)
endif
END FUNCTION sadjustl


FUNCTION sadjustr(s) RESULT(c)
character(len=*), intent(in) :: s
character(len=max(1,len(s))) c
c = ' '
if (len(s) > 0) then
  if (s /= ' ') c = adjustr(s)
endif
END FUNCTION sadjustr


SUBROUTINE tolower(cds)
character(len=*), intent(inout) :: cds
INTEGER(KIND=JPIM), parameter :: ich_a = ichar('a')
INTEGER(KIND=JPIM), parameter :: ichA  = ichar('A')
INTEGER(KIND=JPIM), parameter :: ichZ  = ichar('Z')
INTEGER(KIND=JPIM) :: i, ich, new_ich
character(len=1) ch
do i=1,len(cds)
  ch = cds(i:i)
  ich = ichar(ch)
  if ( ich >= ichA .and. ich <= ichZ ) then
    new_ich = ich + (ich_a - ichA)
    ch = char(new_ich)
    cds(i:i) = ch
  endif
enddo
END SUBROUTINE tolower


SUBROUTINE toupper(cds)
character(len=*), intent(inout) :: cds
INTEGER(KIND=JPIM), parameter :: ich_A = ichar('A')
INTEGER(KIND=JPIM), parameter :: icha  = ichar('a')
INTEGER(KIND=JPIM), parameter :: ichz  = ichar('z')
INTEGER(KIND=JPIM) :: i, ich, new_ich
character(len=1) ch
do i=1,len(cds)
  ch = cds(i:i)
  ich = ichar(ch)
  if ( ich >= icha .and. ich <= ichz ) then
    new_ich = ich + (ich_A - icha)
    ch = char(new_ich)
    cds(i:i) = ch
  endif
enddo
END SUBROUTINE toupper


SUBROUTINE expand_string(&
     &myproc,               &! %p
     &nproc,                &! %n
     &timestep,             &! %t
     &max_timestep,&
     &s)                   ! %s

INTEGER(KIND=JPIM), intent(in)          :: myproc, nproc
INTEGER(KIND=JPIM), intent(in)          :: timestep, max_timestep
character(len=*), intent(inout) :: s(:)
character(len=2*len(s))  t
character(len=2*len(s)) tt
INTEGER(KIND=JPIM) :: i, j, jj, loc_p, len_t, n
INTEGER(KIND=JPIM) :: ndigs(4), num(4)
character(len=6) fmt(4)

n = size(s)

if (n < 1) return

!*    Setup output formats
num(1) = myproc
num(2) = max(nproc,myproc)
num(3) = n
num(4) = max(max_timestep,timestep)

!*    Count number of digits in each integer
do j=1,4
  ndigs(j) = 1
  if (num(j) /= 0) then
    ndigs(j) = 1 + log10(dble(abs(num(j))))
    if (num(j) < 0) ndigs(j) = ndigs(j) + 1 ! Room for minus sign
  endif
  ndigs(j) = min(ndigs(j),9)   ! Max 9 digits supported; i.e. '999999999'
  write(fmt(j),'("(i",i1,")")') ndigs(j)
enddo


!*    Expand fields '%s', '%p', '%n' and '%t' with their values


!*    A special treatment with the sequence numbering
if (n>1) then
  loc_p = index(s(1),'%s')
  if (loc_p > 0) then
    s(2:) = s(1)
  endif
endif

do i=1,n
  t = adjustl(s(i))//' '
  loc_p = index(t,'%')

  if (loc_p > 0) then
    len_t = len_trim(t)
    j = loc_p
    tt(:j-1) = t(:j-1)
    tt(j:) = ' '
    jj = j-1

    do while (j <= len_t)
      if (t(j:j) == '%') then
        j = j + 1
        if (j <= len_t) then
          select case ( t(j:j) )
          case ( 'p' )   ! myproc
          write(tt(jj+1:jj+ndigs(1)),fmt(1)) myproc
          jj = jj + ndigs(1)
          case ( 'n' )   ! nproc
          write(tt(jj+1:jj+ndigs(2)),fmt(2)) nproc
          jj = jj + ndigs(2)
          case ( 's' )   ! sequence number i=[1..n]
          write(tt(jj+1:jj+ndigs(3)),fmt(3)) i
          jj = jj + ndigs(3)
          case ( 't' )   ! timestep
          write(tt(jj+1:jj+ndigs(4)),fmt(4)) timestep
          jj = jj + ndigs(4)
          case default
          tt(jj+1:jj+2) = '%'//t(j:j)
          jj = jj + 2
          end select
        else
          tt(jj+1:jj+1) = '%'
          jj = jj + 1
        endif
      else
        tt(jj+1:jj+1) = t(j:j)
        jj = jj + 1
      endif
      j = j + 1
    enddo

    t = adjustl(tt)

!*   Get also rid of any blanks in the middle of the string

    len_t = len_trim(t)
    j = 1
    do while (j < len_t)
      if (t(j:j) == ' ') then
        t(j:) = t(j+1:)
        len_t = len_trim(t)
      else
        j = j + 1
      endif
    enddo

  endif

  s(i) = t
enddo

END SUBROUTINE expand_string

END MODULE strhandler_mod
