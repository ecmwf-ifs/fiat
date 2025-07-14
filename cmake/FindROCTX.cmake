# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( HAVE_ROCTX 0 )
set( HAVE_ROCPROFILER_SDK_ROCTX 0)	
set(ROCTX_REQUIRED_VARIABLES ROCTX_LIBRARIES)

find_package(rocprofiler-sdk-roctx CONFIG REQUIRED PATHS ${ROCM_PATH}/lib)
if( NOT rocprofiler-sdk-roctx_FOUND )
    ecbuild_info("rocprofiler-sdk-roctx libraries not found")
    if (NOT DEFINED ROCM_PATH OR NOT ROCM_PATH_FOUND)
        find_path(ROCM_PATH
          NAMES include/roctracer/roctx.h
	  HINTS ENV ROCM_DIR ENV ROCM_PATH ENV HIP_PATH ENV ROCM_ROOT_DIR /opt/rocm)
        ecbuild_info("ROCM path: ${ROCM_PATH}")
    endif()

    find_path(ROCTX_INCLUDE_DIRS NAMES roctx.h HINTS ${ROCM_PATH}/include/roctracer/)
    list(APPEND ROCTX_REQUIRED_VARIABLES ROCTX_INCLUDE_DIRS)
    find_path(ROCTX_LIBRARY_PATH NAMES libroctx64.so HINTS ${ROCM_PATH}/lib/)

    if (ROCTX_LIBRARY_PATH)
        set(ROCTX_LIBRARIES ${ROCTX_LIBRARY_PATH}/libroctx64.so)
    endif()
    
    ecbuild_info("roctx libraries : ${ROCTX_LIBRARIES}")
    ecbuild_info("roctx include dirs : ${ROCTX_INCLUDE_DIRS}")
    if ( ROCTX_LIBRARIES AND ROCTX_INCLUDE_DIRS ) 
        set( HAVE_ROCTX 1 )
    endif()
else()
    if( TARGET ${rocprofiler-sdk-roctx_LIBRARIES} )
        set(ROCTX_LIBRARIES ${rocprofiler-sdk-roctx_LIBRARIES})
        set( HAVE_ROCTX 1 )
        set( HAVE_ROCPROFILER_SDK_ROCTX 1)
    endif()
endif()

if ( HAVE_ROCTX OR HAVE_ROCPROFILER_SDK_ROCTX )
    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(ROCTX REQUIRED_VARIABLES ${ROCTX_REQUIRED_VARS} )
endif()

