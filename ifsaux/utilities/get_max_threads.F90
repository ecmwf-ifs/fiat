FUNCTION get_max_threads() RESULT(imaxt)
USE PARKIND1  ,ONLY : JPIM
USE oml_mod, only : OML_MAX_THREADS
implicit none
INTEGER(KIND=JPIM) :: imaxt
imaxt = 1
!$ imaxt = OML_MAX_THREADS()
END FUNCTION get_max_threads
