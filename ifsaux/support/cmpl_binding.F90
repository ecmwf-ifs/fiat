subroutine cmpl_recv(kbuf,kcount,ktype,ksource,ktag,kcomm,&
 &ksync,kblock,krcount,krfrom,krtag,kerror)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_MODULE

implicit none
integer kcount,ktype,ksource,ktag,kcomm
integer krcount,krfrom,krtag,kerror,ksync,kblock
INTEGER(KIND=JPIM) :: kbuf(*)
INTEGER,EXTERNAL :: convin,convout

integer ilenb,ilen

ilenb=convin(kcount,ktype)
if(ksource > 0) then
  if(ktag /= -1) then
    call mpl_recv(kbuf(1:ilenb),KSOURCE=ksource,KTAG=ktag,&
     KFROM=krfrom,KRECVTAG=krtag,KOUNT=ilen,KERROR=kerror)
  else
    call mpl_recv(kbuf(1:ilenb),KSOURCE=ksource,&
     KFROM=krfrom,KRECVTAG=krtag,KOUNT=ilen,KERROR=kerror)
  endif
else
  if(ktag /= -1) then
    call mpl_recv(kbuf(1:ilenb),KTAG=ktag,&
     KFROM=krfrom,KRECVTAG=krtag,KOUNT=ilen,KERROR=kerror)
  else
    call mpl_recv(kbuf(1:ilenb),&
     KFROM=krfrom,KRECVTAG=krtag,KOUNT=ilen,KERROR=kerror)
  endif
endif
krcount=convout(ilen,ktype)

end subroutine cmpl_recv

subroutine cmpl_send(kbuf,kcount,ktype,kdest,ktag,kcomm,&
 &ksync,kblock,kerror)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_MODULE

implicit none
integer kcount,ktype,ktag,kcomm,ksync,kblock,kdest
integer kerror
integer kbuf(*)

INTEGER,EXTERNAL :: convin,convout
integer ilen

ilen=convin(kcount,ktype)

call mpl_send(kbuf(1:ilen),KDEST=kdest,KTAG=ktag,KERROR=kerror)

end subroutine cmpl_send

subroutine cmpl_broadcast(kbuf,kcount,ktype,kroot,ktag,kcomm,&
 &ksync,kblock,kerror)
USE MPL_MODULE

implicit none

integer kcount,ktype,ktag,kcomm,ksync,kblock,kroot
integer kerror
integer kbuf(*)

INTEGER,EXTERNAL :: convin,convout
integer ilen

ilen=convin(kcount,ktype)

call mpl_broadcast(kbuf(1:ilen),KROOT=kroot,KTAG=ktag,KERROR=kerror)

end subroutine cmpl_broadcast

subroutine cmpl_abort(cdmess)
USE MPL_MODULE

implicit none
character cdmess*(*)

call mpl_abort()

end subroutine cmpl_abort

subroutine cmpl_init(ldtrmsg,kerror)

USE MPL_MODULE

implicit none
logical ldtrmsg
integer kerror

call mpl_init(KERROR=kerror)

end subroutine cmpl_init

function cmpl_nproc()
USE MPL_MODULE

implicit none
integer cmpl_nproc

cmpl_nproc=mpl_nproc()

end function cmpl_nproc

function cmpl_myrank()
USE MPL_MODULE

implicit none
integer cmpl_myrank

cmpl_myrank=mpl_myrank()

end function cmpl_myrank

subroutine cmpl_barrier(kerror)
USE MPL_MODULE

implicit none
integer kerror

call mpl_barrier(KERROR=kerror)

end subroutine cmpl_barrier

subroutine cmpl_end(kerror)
USE MPL_MODULE

implicit none
integer kerror

call mpl_end(KERROR=kerror)

end subroutine cmpl_end

function mpe_myrank()
USE MPL_MODULE

implicit none
integer mpe_myrank

mpe_myrank=mpl_myrank()

end function mpe_myrank

subroutine mpei_abort(cdmess)
USE MPL_MODULE

implicit none
character cdmess*(*)

call mpl_abort()

end subroutine mpei_abort

subroutine coml_init_lockid(kmylock)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
INTEGER(KIND=JPIM), intent(inout) :: kmylock
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_INIT_LOCKID',0,ZHOOK_HANDLE)
!$ CALL OML_INIT_LOCK(kmylock)
!$ IF (LHOOK) CALL DR_HOOK('COML_INIT_LOCKID',1,ZHOOK_HANDLE)
end subroutine coml_init_lockid

subroutine coml_init_lock()
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_INIT_LOCK',0,ZHOOK_HANDLE)
!$ CALL OML_INIT_LOCK()
!$ IF (LHOOK) CALL DR_HOOK('COML_INIT_LOCK',1,ZHOOK_HANDLE)
end subroutine coml_init_lock

subroutine coml_set_lockid(kmylock)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
INTEGER(KIND=JPIM), intent(inout) :: kmylock
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_SET_LOCKID',0,ZHOOK_HANDLE)
!$ CALL OML_SET_LOCK(kmylock)
!$ IF (LHOOK) CALL DR_HOOK('COML_SET_LOCKID',1,ZHOOK_HANDLE)
end subroutine coml_set_lockid

subroutine coml_set_lock()
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_SET_LOCK',0,ZHOOK_HANDLE)
!$ CALL OML_SET_LOCK()
!$ IF (LHOOK) CALL DR_HOOK('COML_SET_LOCK',1,ZHOOK_HANDLE)
end subroutine coml_set_lock

subroutine coml_unset_lockid(kmylock)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
INTEGER(KIND=JPIM), intent(inout) :: kmylock
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_UNSET_LOCKID',0,ZHOOK_HANDLE)
!$ CALL OML_UNSET_LOCK(kmylock)
!$ IF (LHOOK) CALL DR_HOOK('COML_UNSET_LOCKID',1,ZHOOK_HANDLE)
end subroutine coml_unset_lockid

subroutine coml_unset_lock()
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
implicit none
!$ REAL(KIND=JPRB) :: ZHOOK_HANDLE
!$ IF (LHOOK) CALL DR_HOOK('COML_UNSET_LOCK',0,ZHOOK_HANDLE)
!$ CALL OML_UNSET_LOCK()
!$ IF (LHOOK) CALL DR_HOOK('COML_UNSET_LOCK',1,ZHOOK_HANDLE)
end subroutine coml_unset_lock
