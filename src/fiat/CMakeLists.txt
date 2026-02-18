# (C) Copyright 2020- ECMWF.
# (C) Copyright 2024- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

### Assemble list of definitions

if( HAVE_FCKIT )
  list( APPEND FIAT_DEFINITIONS WITH_FCKIT )
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  list( APPEND FIAT_DEFINITIONS LINUX )
endif()

if( HAVE_DR_HOOK_MULTI_PRECISION_HANDLES )
   list( APPEND FIAT_DEFINITIONS DR_HOOK_MULTI_PRECISION_HANDLES )
endif()

if( FIAT_ATTRIBUTE_CONSTRUCTOR_SUPPORTED )
  list( APPEND FIAT_DEFINITIONS FIAT_ATTRIBUTE_CONSTRUCTOR_SUPPORTED )
endif()

### Compilation flags

if( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  ## To disable checking of argument correctness of dummy mpi symbols
  ecbuild_add_fortran_flags( -warn nointerfaces NO_FAIL )
endif()

if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  ecbuild_add_fortran_flags( -ffree-line-length-none NO_FAIL )
  if( CMAKE_Fortran_COMPILER_VERSION GREATER_EQUAL 10 )
      ecbuild_add_fortran_flags( -fallow-argument-mismatch NO_FAIL )
  endif()
endif()

if( NOT fiat_VERSION_PATCH )
    set( fiat_VERSION_PATCH 0 )
endif()
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/library/version.c.in ${CMAKE_CURRENT_BINARY_DIR}/version.c @ONLY )

ecbuild_list_add_pattern( LIST fiat_src GLOB
    drhook/*.c drhook/*.F*
    ecsort/*.c ecsort/*.F*
    gstats/*.F*
    library/*.c
    oml/*.F*
    system/*.c system/*.F* system/*.cc
    util/*.c util/*.F*
    )

# Two MPL interface styles
# - DEPRECATED F77 based interface, relies on mpif.h, can't take non-contiguous arrays
# - MPI_F08 based interface, relies on mpi_f08 module
# NB : both interfaces can be compiled without MPI, and then link to mpi_serial library
if( HAVE_MPI AND NOT MPI_Fortran_HAVE_F08_MODULE )
    ecbuild_info( "MPI present, but no MPI-F08 support so building F77 MPL" )
    set( HAVE_MPL_F77_DEPRECATED 1 )
endif()

if( HAVE_MPL_F77_DEPRECATED)
    ecbuild_list_add_pattern( LIST fiat_src GLOB
      "mpl/internal_deprecated/*.c" "mpl/internal_deprecated/*.F*"
      "mpl/mpl_module.F90" "mpl/mpl_bindc.F90" )
    ecbuild_info( "FIAT building with *deprecated* F77 (mpif.h) MPL interface" )
else()
    ecbuild_list_add_pattern( LIST fiat_src GLOB
      "mpl/internal_f08/*.c" "mpl/internal_f08/*.F*"
      "mpl/mpl_module.F90" "mpl/mpl_bindc.F90" )
    if( HAVE_MPI )
        ecbuild_info( "Not compiling mpi_f08_dummy_mod, because MPI is found" )
        ecbuild_list_exclude_pattern( LIST fiat_src REGEX mpl/internal_f08/mpi_f08_dummy_mod.F90 )
    endif()
    ecbuild_info( "FIAT building with MPI_F08 MPL interface" )
endif()
ecbuild_list_exclude_pattern( LIST fiat_src REGEX drhook/extensions/*)

set( fiat_src ${fiat_src} PARENT_SCOPE )

if( HAVE_MPI )
    set( FIAT_MPI_LIBRARIES MPI::MPI_Fortran )
else()
    set( FIAT_MPI_LIBRARIES ${MPI_SERIAL_LIBRARIES} )
endif()

# work around problem when linking privately to MPI libraries with NVHPC
# (see https://github.com/ecmwf-ifs/fiat/pull/79)
# NOTE matching change required in fiat-import.cmake.in to ensure
#      that downstream packages link to MPI::MPI_Fortran dependency.
set(LINK_FIAT_MPI_LIBRARIES PRIVATE_LIBS ${FIAT_MPI_LIBRARIES})
if (CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|IntelLLVM")
    set(LINK_FIAT_MPI_LIBRARIES PUBLIC_LIBS ${FIAT_MPI_LIBRARIES})
endif()

set( FIAT_DEPENDENCE )
if( ${BUILD_SHARED_LIBS} MATCHES "BOTH" )
    set( LIB_TYPES SHARED STATIC )
    set( FIAT_DEPENDENCE fiat )
elseif( BUILD_SHARED_LIBS )
    set( LIB_TYPES SHARED )
else()
    set( LIB_TYPES STATIC )
endif()

foreach( LIB_TYPE ${LIB_TYPES} )
    if( ${LIB_TYPE} MATCHES "STATIC" )
        set( fiatlib "fiat-static" )
    else()
        set( fiatlib "fiat")
    endif()
    ecbuild_add_library( TARGET ${fiatlib}
    LINKER_LANGUAGE Fortran
    SOURCES ${fiat_src} ${CMAKE_CURRENT_BINARY_DIR}/version.c
        TYPE ${LIB_TYPE}
    PRIVATE_LIBS 
        ${CMAKE_DL_LIBS}
        ${RT_LIB}
    ${LINK_FIAT_MPI_LIBRARIES}
    PRIVATE_INCLUDES
        ${CMAKE_CURRENT_SOURCE_DIR}/drhook/internal
        ${CMAKE_CURRENT_SOURCE_DIR}/ecsort/internal
        ${CMAKE_CURRENT_SOURCE_DIR}/util/internal
        ${CMAKE_CURRENT_SOURCE_DIR}/system/internal
    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include/fiat>
        $<INSTALL_INTERFACE:include>
        $<INSTALL_INTERFACE:include/fiat>
            ${MPI_Fortran_INCLUDE_DIRS}
        OUTPUT_NAME fiat
        DEPENDS ${FIAT_DEPENDENCE}
    )

    if (HAVE_DR_HOOK_NVTX)
        # Files from within DrHook
        ecbuild_list_add_pattern( LIST fiat_nvtx_src GLOB *.c SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/nvtx)
        target_sources(${fiatlib} PRIVATE ${fiat_nvtx_src})
        target_include_directories(${fiatlib} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/nvtx)
        target_compile_definitions(${fiatlib} PRIVATE DR_HOOK_HAVE_NVTX=1 HAVE_NVTX3=${HAVE_NVTX3} )

        # Link with NVTX
        target_link_libraries     (${fiatlib} PRIVATE ${NVTX_LIBRARIES})
        target_include_directories(${fiatlib} PRIVATE ${NVTX_INCLUDE_DIRS})
    endif()

    if (HAVE_DR_HOOK_ROCTX)
        # Files from within DrHook
        ecbuild_list_add_pattern( LIST fiat_roctx_src GLOB *.c SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/roctx)
        target_sources(fiat PRIVATE ${fiat_roctx_src})
        target_include_directories(${fiatlib} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/roctx)
        target_compile_definitions(${fiatlib} PRIVATE DR_HOOK_HAVE_ROCTX=1 HAVE_ROCPROFILER_SDK_ROCTX=${HAVE_ROCPROFILER_SDK_ROCTX})

        # Link with ROCTX
        target_link_libraries     (${fiatlib} PRIVATE ${ROCTX_LIBRARIES})
        target_include_directories(${fiatlib} PRIVATE ${ROCTX_INCLUDE_DIRS})
    endif()

    if (HAVE_DR_HOOK_PAPI)
        # Files from within DrHook
        ecbuild_list_add_pattern( LIST fiat_papi_src GLOB *.c SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/papi)
        target_sources(${fiatlib} PRIVATE ${fiat_papi_src})
        target_include_directories(${fiatlib} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/drhook/extensions/papi)
        target_compile_definitions(${fiatlib} PRIVATE DR_HOOK_HAVE_PAPI=1)

        # Files defined externally
        target_link_libraries      ( ${fiatlib} PRIVATE ${PAPI_LIBRARIES} )
        target_include_directories ( ${fiatlib} PRIVATE ${PAPI_INCLUDE_DIRS} )
    endif()

    if( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin" )
        # Following should not be necessary; 
        # Probably a bug in the M1 prerelease of gfortran 10.2.0.4
        target_compile_definitions( ${fiatlib} PRIVATE $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:__APPLE__>)
    endif()

    if( NOT HAVE_MPI )
        # define a preprocessor macro to improve messaging in dr_hook_init
        target_compile_definitions( ${fiatlib} PRIVATE NO_MPI_SUPPORT)
    endif()

    if( HAVE_MPL_F77_DEPRECATED)
        target_compile_definitions( ${fiatlib} PRIVATE MPL_F77_DEPRECATED )
    endif()

    if( HAVE_MPL_CHECK_CONTIG)
        target_compile_definitions( ${fiatlib} PRIVATE MPL_CHECK_CONTIG )
    endif()

    target_compile_definitions( ${fiatlib} PRIVATE ${FIAT_DEFINITIONS} )

    if( HAVE_FCKIT )
        target_link_libraries( ${fiatlib} PRIVATE fckit )
    endif()

    if( HAVE_OMP )
        target_link_libraries( ${fiatlib} PRIVATE OpenMP::OpenMP_Fortran )
    endif()

    fiat_target_ignore_missing_symbols( TARGET ${fiatlib} SYMBOLS
      _MallocExtension_GetNumericProperty
      _MPI_Get_version
      _MPI_Get_library_version
    )

    fiat_target_fortran_module_directory(
      TARGET ${fiatlib}
      MODULE_DIRECTORY  ${CMAKE_BINARY_DIR}/module/fiat
      INSTALL_DIRECTORY module/fiat
    )

endforeach()

## Install and Export ##

execute_process( COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BINARY_DIR}/include )
execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_SOURCE_DIR}/include/fiat ${PROJECT_BINARY_DIR}/include/fiat )
install(
  DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/include/fiat
  DESTINATION include
)
install(
  FILES       ${CMAKE_CURRENT_SOURCE_DIR}/version.h
  DESTINATION include/fiat
)

set( FIAT_DEFINITIONS ${FIAT_DEFINITIONS} PARENT_SCOPE )
