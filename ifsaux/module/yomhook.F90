MODULE YOMHOOK

USE PARKIND1  ,ONLY : JPIM     ,JPRB

! Used by "hook" function
! LHOOK = true implies "hook" function will be called
! Altough initialized to TRUE it will be reset by first call to 
! DR_HOOK unless we really want to use the hook function

LOGICAL :: LHOOK=.TRUE.

CONTAINS 

SUBROUTINE DR_HOOK(CDNAME,KSWITCH,PKEY)
CHARACTER(LEN=*), INTENT(IN) :: CDNAME
INTEGER(KIND=JPIM),        INTENT(IN) :: KSWITCH
REAL(KIND=JPRB),        INTENT(INOUT) :: PKEY

CALL DR_HOOK_UTIL(CDNAME,KSWITCH,PKEY)

END SUBROUTINE DR_HOOK

END MODULE YOMHOOK
