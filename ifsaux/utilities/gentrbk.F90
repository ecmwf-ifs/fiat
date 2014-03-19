!-- Generic traceback calls here

SUBROUTINE gentrbk_dummy
END SUBROUTINE gentrbk_dummy

#ifdef __INTEL_COMPILER
SUBROUTINE intel_trbk()
USE IFCORE
USE MPL_MODULE, ONLY : MPL_MYRANK
CHARACTER*80 MESSAGE
LOGICAL :: DONE_TRACEBACK = .false.
INTEGER :: MYPROC,MYTHREAD
INTEGER,EXTERNAL :: OMP_GET_THREAD_NUM

if(DONE_TRACEBACK) then
  write(0,*) "INTEL_TRBK already called"
  return
endif

MYPROC=MPL_MYRANK()
MYTHREAD=OMP_GET_THREAD_NUM() + 1

#ifndef BOM
  write(MESSAGE,'(A,I4,A,I2,A)') &
  &           "Process ",MYPROC," thread ",MYTHREAD, &
  &           " calling tracebackqq from intel_trbk()"
  CALL TRACEBACKQQ(MESSAGE, USER_EXIT_CODE=-1)
#endif
#ifdef LINUX
  write(0,*) "Process ",MYPROC," thread ",MYTHREAD, &
 &           " calling linux_trbk from intel_trbk()"
  CALL LINUX_TRBK() ! See ifsaux/utilities/linuxtrbk.c
#endif
DONE_TRACEBACK=.true.
END SUBROUTINE intel_trbk
#endif

#ifndef VPP
SUBROUTINE ERRTRA
END SUBROUTINE ERRTRA
#endif

#ifdef NECSX
subroutine necsx_trbk(cdmess)
implicit none
character(len=*), intent(in) :: cdmess
call mesput(cdmess, len(cdmess), 1)
call dbx_trbk()
end subroutine necsx_trbk

subroutine necsx_trbk_fl(cdmess, cdfilename, klineno)
USE PARKIND1  ,ONLY : JPIM
implicit none
character(len=*), intent(in) :: cdmess
character(len=*), intent(in) :: cdfilename
INTEGER(KIND=JPIM), intent(in) :: klineno
character(len=len(cdmess)+len(cdfilename)+30) clocal
write(clocal,'(a," at ",a,":",i6.6)') trim(cdmess),trim(cdfilename),klineno
call necsx_trbk(trim(clocal))
end subroutine necsx_trbk_fl
#endif
