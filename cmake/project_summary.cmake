# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

ecbuild_info( "MPI (export MPI_HOME to correct MPI implementation)" )
ecbuild_info( "    MPI_Fortran_INCLUDE_DIRS  : [${MPI_Fortran_INCLUDE_DIRS}]" )
ecbuild_info( "    MPI_Fortran_LIBRARIES     : [${MPI_Fortran_LIBRARIES}]" )
ecbuild_info( "    MPIEXEC                   : [${MPIEXEC}]" )

if( HAVE_OMP )

    ecbuild_info( "OpenMP (following variable can be overwritten by user)" )
    ecbuild_info( "    OpenMP_Fortran_FLAGS      : [${OpenMP_Fortran_FLAGS}]" )

endif()

ecbuild_info( "FIAT_DEFINITIONS              : [${FIAT_DEFINITIONS}]" )

ecbuild_info( "---------------------------------------------------------" )