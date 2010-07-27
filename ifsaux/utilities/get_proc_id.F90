FUNCTION get_proc_id() RESULT(pid)

USE PARKIND1  ,ONLY : JPIM
use mpl_data_module, only : MPL_RANK
implicit none
INTEGER(KIND=JPIM) :: pid
pid = MPL_RANK

END FUNCTION get_proc_id
