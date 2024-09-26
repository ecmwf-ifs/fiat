# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

if(CMAKE_C_COMPILER_ID STREQUAL "PGI" OR CMAKE_C_COMPILER_ID STREQUAL "NVHPC" )

    set (DEFAULT_DR_HOOK_NVTX ON)

    if( ${CMAKE_VERSION} VERSION_LESS "3.25" )
        find_package(CUDAToolkit REQUIRED COMPONENTS CUDA::nvToolsExt)

        find_path(NVTX_ROOT
                NAMES include/nvToolsExt.h
                HINTS ${CUDAToolkit_LIBRARY_DIR}/..
                )

        find_library(NVTX_LIBRARIES
                NAMES libnvToolsExt.so nvToolsExt
                HINTS ${NVTX_ROOT}/lib ${NVTX_ROOT}/lib64
                )

        find_path(NVTX_INCLUDE_DIRS
                NAMES nvToolsExt.h
                HINTS ${NVTX_ROOT}/include
                )

        include(FindPackageHandleStandardArgs)
        find_package_handle_standard_args(NVTX DEFAULT_MSG
                NVTX_LIBRARIES
                NVTX_INCLUDE_DIRS
                )

        mark_as_advanced(
                NVTX_LIBRARIES
                NVTX_INCLUDE_DIRS
        )
    else()
        find_package(CUDAToolkit REQUIRED COMPONENTS CUDA::nvtx3)

        find_path(NVTX_ROOT
                NAMES include/nvtx3/nvToolsExt.h
                HINTS ${CUDAToolkit_LIBRARY_DIR}/..
                )

        find_path(NVTX_INCLUDE_DIRS
                NAMES nvToolsExt.h
                HINTS ${NVTX_ROOT}/include/nvtx3
                )

        include(FindPackageHandleStandardArgs)
        find_package_handle_standard_args(NVTX DEFAULT_MSG
                NVTX_INCLUDE_DIRS
                )

        mark_as_advanced(
                NVTX_INCLUDE_DIRS
        )
    endif()


else ()
    set (DEFAULT_DR_HOOK_NVTX OFF)
endif ()