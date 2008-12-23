FUNCTION get_thread_id() RESULT(tid)
USE PARKIND1  ,ONLY : JPIM
USE oml_mod, only : OML_MY_THREAD
implicit none
INTEGER(KIND=JPIM) :: tid
tid = 1
!$ tid = OML_MY_THREAD()
END FUNCTION get_thread_id
