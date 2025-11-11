! (C) Copyright 2025- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_MPL_SPLIT_COMM

USE EC_PARKIND, ONLY: JPIM
USE MPL_MODULE, ONLY: MPL_INIT, MPL_NPROC, MPL_MYRANK, MPL_COMM, MPL_COMM_SPLIT, MPL_MESSAGE, &
  &                   MPL_SETDFLT_COMM, MPL_ABORT, MPL_END

IMPLICIT NONE

INTEGER(JPIM), PARAMETER :: STDOUT = 6

INTEGER(JPIM) :: IGLOBAL_NPROC, IGLOBAL_RANK, ISPLIT_COLOUR, IERROR, ISPLIT_COMM, IDUMMY_COMM
INTEGER(JPIM) :: ISPLIT_RANK, ISPLIT_NPROC

CALL MPL_INIT

IGLOBAL_NPROC = MPL_NPROC()
IGLOBAL_RANK = MPL_MYRANK()

! First rank in group 0, others in group 1
ISPLIT_COLOUR = MERGE(0, 1, IGLOBAL_RANK == 1)

! Split world communicator according to rank colour
CALL MPL_COMM_SPLIT(MPL_COMM, ISPLIT_COLOUR, IGLOBAL_RANK, ISPLIT_COMM, IERROR)

IF (IERROR /= 0) THEN
  CALL MPL_MESSAGE("TEST_MPL_SPLIT_COMM", "MPL_COMM_SPLIT failed", IERROR, LDABORT=.TRUE.)
ENDIF

! Set new split communicator as default
CALL MPL_SETDFLT_COMM(ISPLIT_COMM, IDUMMY_COMM)

! Get rank and comm size in new split communicator
ISPLIT_RANK = MPL_MYRANK()
ISPLIT_NPROC = MPL_NPROC()

! Check all values are correct
IF (IGLOBAL_RANK == 1) THEN
  IF (ISPLIT_NPROC /= 1) THEN
    CALL MPL_ABORT("TEST_MPL_SPLIT_COMM: 1st split comm does not have 1 rank")
  ENDIF
  IF (ISPLIT_RANK /= 1) THEN
    CALL MPL_ABORT("TEST_MPL_SPLIT_COMM: 1st global rank is not 1st rank in 1st split comm")
  ENDIF
ELSE
  IF (ISPLIT_NPROC /= IGLOBAL_NPROC - 1) THEN
    CALL MPL_ABORT("TEST_MPL_SPLIT_COMM: 2nd split comm does not have correct # ranks")
  ENDIF
  IF (ISPLIT_RANK /= IGLOBAL_RANK - 1) THEN
    CALL MPL_ABORT("TEST_MPL_SPLIT_COMM: rank does not have correct number in 2nd split comm")
  ENDIF
ENDIF

CALL MPL_END(LDMEMINFO=.FALSE.)

END PROGRAM TEST_MPL_SPLIT_COMM
