# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(HAVE_NVTX3 0)
set(NVTX_REQUIRED_VARIABLES NVTX_LIBRARIES)

if( ${CMAKE_VERSION} VERSION_LESS "3.20" )
    find_path(NVTX_ROOT
        NAMES include/nvToolsExt.h
        HINTS ENV NVTX_ROOT CUDA_ROOT ENV CUDA_ROOT ENV NVHPC_CUDA_HOME ENV CUDA_DIR)

    find_library(NVTX_LIBRARIES
        NAMES nvToolsExt
        HINTS ${NVTX_ROOT}
        PATH_SUFFIXES lib lib64)

    find_path(NVTX_INCLUDE_DIRS
        NAMES nvToolsExt.h
        HINTS ${NVTX_ROOT}/include)

    list(APPEND NVTX_REQUIRED_VARIABLES NVTX_INCLUDE_DIRS)

elseif( ${CMAKE_VERSION} VERSION_LESS "3.25" )

    find_package(CUDAToolkit COMPONENTS CUDA::nvToolsExt)
    if (TARGET CUDA::nvToolsExt)
        set(NVTX_LIBRARIES CUDA::nvToolsExt)
    endif()

else()

    find_package(CUDAToolkit COMPONENTS CUDA::nvtx3)
    if( TARGET CUDA::nvtx3 )
        set(NVTX_LIBRARIES CUDA::nvtx3)
        set(HAVE_NVTX3 1)
    else()
        find_package(CUDAToolkit COMPONENTS CUDA::nvToolsExt)
        if (TARGET CUDA::nvToolsExt)
            set(NVTX_LIBRARIES CUDA::nvToolsExt)
        endif()
   endif()

endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NVTX REQUIRED_VARIABLES ${NVTX_REQUIRED_VARS} )

