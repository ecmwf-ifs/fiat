MODULE MPL_DATA_MODULE

!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

!      variables controlling the execution of MPL

!  MPL_METHOD   : buffering type
!  MPL_MBX_SIZE : size of application mailbox, (bytes)
!                 used when MPL_METHOD=JP_BLOCKING_BUFFERED
!  MPL_COMM     : default communicator in use
!  MPL_UNIT     : Fortran I/O unit for messages
!  MPL_ERRUNIT  : Fortran I/O unit for error messages
!  MPL_OUTPUT   : controls contents of Output 
!  MPL_RANK     : rank of the process within MPL_COMM
!  MPL_NUMPROC  : number of processes in MPL_COMM
!  MPL_IDS      : array of processor numbers

#include "tsmbkind.h"

IMPLICIT NONE

SAVE

PUBLIC 

INTEGER_M :: MPL_METHOD, MPL_MBX_SIZE, MPL_COMM, MPL_UNIT, MPL_OUTPUT
INTEGER_M :: MPL_RANK=0,MPL_NUMPROC = -1,MPL_ERRUNIT=0
INTEGER_M,ALLOCATABLE :: MPL_IDS(:)
INTEGER_M,ALLOCATABLE :: MPL_OPPONENT(:)
!INTEGER_M,ALLOCATABLE :: MPL_ATTACHED_BUFFER(:)
!   needs to ge a TARGET for coexistence with MPE
INTEGER_M,ALLOCATABLE,TARGET     :: MPL_ATTACHED_BUFFER(:)
INTEGER_M,PARAMETER :: JP_ATTACHED_BUFFER_BYTES = 4
INTEGER_M,PARAMETER :: JP_BLOCKING_STANDARD        = 1
INTEGER_M,PARAMETER :: JP_BLOCKING_BUFFERED        = 2
INTEGER_M,PARAMETER :: JP_NON_BLOCKING_STANDARD    = 3
INTEGER_M,PARAMETER :: JP_NON_BLOCKING_READY       = 4
INTEGER_M,PARAMETER :: JP_NON_BLOCKING_SYNCHRONOUS = 5

END MODULE MPL_DATA_MODULE
