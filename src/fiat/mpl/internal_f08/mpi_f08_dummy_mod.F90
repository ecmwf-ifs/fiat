! (C) Copyright 2024- ECMWF.
! (C) Copyright 2024- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPI_F08

INTEGER :: MPI_UNDEFINED, MPI_MAX_ERROR_STRING, MPI_ANY_TAG, MPI_ANY_SOURCE, &
 &         MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR, MPI_THREAD_SINGLE, MPI_THREAD_MULTIPLE, &
 &         MPI_MODE_RDONLY, MPI_MODE_WRONLY, MPI_MODE_CREATE

!! dummy type definitions
!!========================

TYPE MPI_COMM
  INTEGER :: MPI_VAL
END TYPE

TYPE MPI_DATATYPE
  INTEGER :: MPI_VAL
END TYPE

TYPE MPI_STATUS
  INTEGER :: MPI_TAG, MPI_SOURCE
END TYPE

TYPE MPI_REQUEST
  INTEGER :: MPI_VAL
END TYPE

TYPE MPI_FILE
  INTEGER :: MPI_VAL
END TYPE

TYPE MPI_OP
END TYPE

TYPE MPI_GROUP
END TYPE

TYPE MPI_INFO
END TYPE


!! dummy instances
!!=================

TYPE(MPI_DATATYPE) :: MPI_BYTE, MPI_LOGICAL, MPI_CHARACTER, MPI_INTEGER, MPI_INTEGER4, MPI_INTEGER8, MPI_REAL4, MPI_REAL8

TYPE(MPI_OP)       :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_BXOR

TYPE(MPI_REQUEST)  :: MPI_REQUEST_NULL

TYPE(MPI_COMM)     :: MPI_COMM_WORLD, MPI_COMM_NULL

TYPE(MPI_INFO)     :: MPI_INFO_NULL

END MODULE MPI_F08

