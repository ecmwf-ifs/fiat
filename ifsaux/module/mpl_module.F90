MODULE MPL_MODULE
!
!    Message Passing Library (MPL)
!
!    Interface between parallel applications and the
!    Message Passing Interface (MPI standard) provided by the computer vendors
!    This version requires only MPI release 1.
!
!    designed and developed by
!      Mats Hamrud and David Dent,      ECMWF,      September 2000
!
!    all routines which wish to call MPL routines must contain:
!    USE MPL_MODULE

USE MPL_INIT_MOD
USE MPL_BUFFER_METHOD_MOD
USE MPL_SEND_MOD
USE MPL_RECV_MOD
USE MPL_WAIT_MOD
USE MPL_BARRIER_MOD
USE MPL_BROADCAST_MOD
USE MPL_PROBE_MOD
USE MPL_END_MOD
USE MPL_MESSAGE_MOD
USE MPL_ABORT_MOD
USE MPL_COMM_CREATE_MOD
USE MPL_MYRANK_MOD
USE MPL_NPROC_MOD

END MODULE MPL_MODULE
