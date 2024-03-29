# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( HAVE_dp ${HAVE_DOUBLE_PRECISION} )
set( HAVE_sp ${HAVE_SINGLE_PRECISION} )

foreach( prec sp dp )
  if( HAVE_${prec} )
    set( target parkind_${prec} )
    ecbuild_add_library(
      TARGET  ${target}
      SOURCES parkind1.F90
              parkind2.F90
    )

    if( EC_OS_BITS EQUAL "64" )
      target_compile_definitions( ${target} PRIVATE ADDRESS64 )
    endif()

    fiat_target_fortran_module_directory(
      TARGET            ${target}
      MODULE_DIRECTORY  ${CMAKE_BINARY_DIR}/module/${target}
      INSTALL_DIRECTORY module/${target}
    )
  endif()
endforeach()

if( TARGET parkind_sp )
  target_compile_definitions( parkind_sp PRIVATE PARKIND1_SINGLE )
endif()
