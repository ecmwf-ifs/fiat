MODULE YOMHOOKSTACK

! Used by dr_hook_util to monitor thread stack usage 
! Need "export STACKCHECK=yes"

USE PARKIND1  ,ONLY : JPIM     ,JPRB,      JPIB

IMPLICIT NONE

SAVE

INTEGER(KIND=JPIB), ALLOCATABLE :: isave(:) 
INTEGER(KIND=JPIB), ALLOCATABLE :: imaxstack(:) 
LOGICAL,   ALLOCATABLE :: ll_thread_first(:)
CHARACTER(LEN=3)       :: cstack

END MODULE YOMHOOKSTACK

