subroutine dr_hook_procinfo(kmyproc, knproc)
#include "tsmbkind.h"
use mpl_data_module, only : MPL_RANK,MPL_NUMPROC
implicit none
INTEGER_M,intent(out) :: kmyproc, knproc
kmyproc = mpl_rank
knproc = mpl_numproc
end subroutine dr_hook_procinfo
