MODULE MPL_COMM_CREATE_MOD

!**** MPL_COMM_CREATE Create a new communicator

!     Purpose.
!     --------
!     Create a new communicator and set as default

!**   Interface.
!     ----------
!        CALL MPL_COMM_CREATE

!        Input required arguments :
!        -------------------------

!        Input optional arguments :
!        -------------------------

!        Output required arguments :
!        -------------------------

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_COMM_CREATE aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD

IMPLICIT NONE

PUBLIC MPL_COMM_CREATE

CONTAINS 

SUBROUTINE MPL_COMM_CREATE(KERROR)


INTEGER_M,OPTIONAL,INTENT(OUT) :: KERROR

!   this line to be replaced
MPL_COMM=MPI_COMM_WORLD
KERROR=0

RETURN
END SUBROUTINE MPL_COMM_CREATE

END MODULE MPL_COMM_CREATE_MOD
