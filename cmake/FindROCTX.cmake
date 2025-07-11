# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(HAVE_ROCTX 0)
set(ROCTX_REQUIRED_VARIABLES ROCTX_LIBRARIES)

find_package(rocprofiler-sdk-roctx CONFIG ${HIP_REQUIRED} PATHS ${ROCM_PATH}/lib)
if( NOT rocprofiler-sdk-roctx_FOUND )
    ecbuild_info("rocprofiler-sdk-roctx libraries not found: HAVE_ROCTX=0")
    set( HAVE_ROCTX 0 )
else()
    if( TARGET ${rocprofiler-sdk-roctx_LIBRARIES} )
        set(ROCTX_LIBRARIES ${rocprofiler-sdk-roctx_LIBRARIES})
        set(HAVE_ROCTX 1)
    endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ROCTX REQUIRED_VARIABLES ${ROCTX_REQUIRED_VARS} )
