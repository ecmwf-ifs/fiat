# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

if( HAVE_TESTS )

set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

if( HAVE_MPI AND MPIEXEC )
  set (MPI_OPTS "" CACHE STRING "system depended srun or mpirun options (partition, time, account, ...)")
  set( LAUNCH ${MPIEXEC} ${MPI_OPTS} ${MPIEXEC_NUMPROC_FLAG} 1 )
else()
  set( LAUNCH ${CMAKE_CROSSCOMPILING_EMULATOR} )
endif()


if( HAVE_OMP )
  set( OMP OpenMP::OpenMP_Fortran )
endif()

if( HAVE_FCKIT )
  list( APPEND FCKIT_DEFINITIONS WITH_FCKIT )
  list( APPEND FCKIT_LIB fckit )
endif()

if( HAVE_FCKIT AND fckit_VERSION VERSION_GREATER_EQUAL 0.9.1 )
ecbuild_add_executable(
  TARGET  fiat-test-abort-exception-handler
  SOURCES test_abort_exception_handler.F90 test_abort_exception_handler.cc 
  LIBS    fiat fckit
  LINKER_LANGUAGE Fortran
  NOINSTALL)
add_test(NAME fiat_test_abort_exception_handler
  COMMAND ${CMAKE_COMMAND}
  "-DEXECUTABLE=$<TARGET_FILE:fiat-test-abort-exception-handler>"
  "-DLAUNCH=${LAUNCH}"
  "-DPASS_REGULAR_EXPRESSION=problem in function_2"
  -P ${CMAKE_CURRENT_SOURCE_DIR}/test_program_output.cmake )
endif()

add_subdirectory( drhook )

# ----------------------------------------------------------------------------------------
# Test: fiat_test_drhook_no_output
#
# Ensures that no output is generated with DR_HOOK_SILENT=1

ecbuild_add_executable(
    TARGET  fiat-test-drhook-no-output
    SOURCES test_drhook_no_output.c 
    LIBS    fiat
    LINKER_LANGUAGE C
    NOINSTALL )
add_test( NAME fiat_test_drhook_no_output
          COMMAND ${CMAKE_COMMAND}
          "-DLAUNCH=${LAUNCH}"
          "-DEXECUTABLE=$<TARGET_FILE:fiat-test-drhook-no-output>"
          -P ${CMAKE_CURRENT_SOURCE_DIR}/test_drhook_no_output.cmake )
set_tests_properties(fiat_test_drhook_no_output PROPERTIES
    ENVIRONMENT "DR_HOOK_ASSERT_MPI_INITIALIZED=0"
)


# ----------------------------------------------------------------------------------------
# Tests: fiat_test_drhook_abort_my_abort, fiat_test_drhook_abort_abor1fl
#
# Test that we can use drhook within C and register a custom abort

ecbuild_add_executable(
    TARGET  fiat-test-drhook-abort
    SOURCES test_drhook_abort.c
    LIBS    fiat
    LINKER_LANGUAGE C
    NOINSTALL )


add_test( NAME fiat_test_drhook_abort_abor1fl
          COMMAND fiat-test-drhook-abort )
set_tests_properties(fiat_test_drhook_abort_abor1fl PROPERTIES
    ENVIRONMENT "DR_HOOK_ASSERT_MPI_INITIALIZED=0"
    PASS_REGULAR_EXPRESSION "ABOR1.*EC_DRHOOK.*\[DrHookCallTree\]"
)

add_test( NAME fiat_test_drhook_abort_custom_abort
          COMMAND fiat-test-drhook-abort )
set_tests_properties(fiat_test_drhook_abort_custom_abort PROPERTIES
    ENVIRONMENT "CUSTOM_ABORT=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0"
    PASS_REGULAR_EXPRESSION "custom_abort.*EC_DRHOOK.*\[DrHookCallTree\]"
)

# ----------------------------------------------------------------------------------------

ecbuild_add_test(
    TARGET  fiat_test_drhook_fortran
    SOURCES test_drhook_fortran.F90 
    LIBS    fiat ${OMP} ${FCKIT_LIB}
    LINKER_LANGUAGE Fortran
    DEFINITIONS ${FCKIT_DEFINITIONS})
set_tests_properties(fiat_test_drhook_fortran PROPERTIES
    ENVIRONMENT "MPL=0;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=NOPROPAGATE_SIGNALS"
    PASS_REGULAR_EXPRESSION "EC_DRHOOK.*\[DrHookCallTree\]" )


# ----------------------------------------------------------------------------------------
# Tests: fiat_test_drhook_counters

if( HAVE_DR_HOOK_PAPI )
  ecbuild_add_test(TARGET fiat_test_drhook_counters
    SOURCES test_drhook_counters.F90
            test_drhook_counters_stream.F90
            test_drhook_counters_gemm.F90
    LIBS    fiat
    ENVIRONMENT "DR_HOOK_ASSERT_MPI_INITIALIZED=0;FIAT_UNIT_TEST=1"
    )
  target_compile_definitions( fiat_test_drhook_counters PRIVATE OMP )
  if( NOT HAVE_MPI )
    target_compile_definitions( fiat_test_drhook_counters PRIVATE NOMPI )
  endif()

  if( CMAKE_Fortran_COMPILER_ID MATCHES Intel )
    set_source_files_properties(test_drhook_counters_stream.F90
      PROPERTIES COMPILE_OPTIONS "-qopt-prefetch-distance=64,12;-qopt-streaming-cache-evict=0;-qopt-streaming-stores always;-qopt-zmm-usage=high")
  endif()

  find_package( OpenMP COMPONENTS Fortran )
  if( TARGET OpenMP::OpenMP_Fortran )
    target_link_libraries( fiat_test_drhook_counters OpenMP::OpenMP_Fortran )
  endif()
  if( NOT BLAS_LIBRARIES )
    find_package( MKL QUIET )
    if( MKL_LIBRARIES )
      set( BLAS_LIBRARIES ${MKL_LIBRARIES} )
    else()
      find_package( BLAS QUIET )
    endif()
  endif()
  if( BLAS_LIBRARIES )
    target_link_libraries( fiat_test_drhook_counters ${BLAS_LIBRARIES} )
    target_compile_definitions( fiat_test_drhook_counters PUBLIC HAVE_BLAS )
  endif()
endif()

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_ec_args_fortran

ecbuild_add_test(
    TARGET  fiat_test_ec_args_fortran 
    SOURCES test_ec_args_fortran.F90 
    ARGS arg1 arg2 arg3
    LIBS    fiat ${OMP}
    LINKER_LANGUAGE Fortran )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_bytes_io

ecbuild_add_test(
    TARGET  fiat_test_bytes_io 
    SOURCES test_bytes_io.F90 
    LIBS    fiat
    LINKER_LANGUAGE Fortran )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_byteswap

ecbuild_add_test(
    TARGET  fiat_test_byteswap
    SOURCES test_byteswap.F90
    LIBS    fiat
    LINKER_LANGUAGE Fortran )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_checksum_c

ecbuild_add_test(
    TARGET  fiat_test_checksum_c 
    SOURCES test_checksum.c
    LIBS    fiat )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_checksum

ecbuild_add_test(
    TARGET  fiat_test_checksum
    SOURCES test_checksum.F90
    LIBS    fiat
    LINKER_LANGUAGE Fortran )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_mpl_no_output
#   ensures that no output is generated in this test upon mpl_init() and mpl_end()

ecbuild_add_executable(
    TARGET  fiat-test-mpl-no-output
    SOURCES test_mpl_no_output.F90 
    LIBS    fiat
    LINKER_LANGUAGE Fortran
    NOINSTALL )

add_test(NAME fiat_test_mpl_no_output
         COMMAND ${CMAKE_COMMAND}
         "-DEXECUTABLE=$<TARGET_FILE:fiat-test-mpl-no-output>"
         "-DLAUNCH=${LAUNCH}"
         -P ${CMAKE_CURRENT_SOURCE_DIR}/test_mpl_no_output.cmake )

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_abor1
#
# Test abor1 function, and check the error message is printed

ecbuild_add_executable(
    TARGET  fiat-test-abor1
    SOURCES test_abor1.F90 
    LIBS    fiat ${OMP}
    LINKER_LANGUAGE Fortran
    NOINSTALL )
add_test(NAME fiat_test_abor1
      COMMAND ${CMAKE_COMMAND}
      "-DEXECUTABLE=$<TARGET_FILE:fiat-test-abor1>"
      "-DPASS_REGULAR_EXPRESSION=ABOR1.*aborting from OpenMP parallel region"
      -P ${CMAKE_CURRENT_SOURCE_DIR}/test_program_output.cmake )

#-----------------------------------------------------------------------------------------
# Tests:  fiat_test_alltoallv
#
#
ecbuild_add_test( TARGET fiat_test_alltoallv
    SOURCES alltoallv.F90
    LIBS     fiat
    LINKER_LANGUAGE Fortran
    MPI 2)
#-----------------------------------------------------------------------------------------
# Tests:  fiat_test_gatherv
#
#
ecbuild_add_test( TARGET fiat_test_gatherv
    SOURCES gatherv.F90 
    LIBS    fiat
    LINKER_LANGUAGE Fortran
    MPI 2)
#-----------------------------------------------------------------------------------------
# Tests:  fiat_test_allgatherv
#
#
ecbuild_add_test( TARGET fiat_test_allgatherv
    SOURCES allgatherv.F90 
    LIBS    fiat
    LINKER_LANGUAGE Fortran
    MPI 2)                  
#-----------------------------------------------------------------------------------------
# Tests:  fiat_test_scatterv
#
#
ecbuild_add_test( TARGET fiat_test_scatterv
    SOURCES scatterv.F90 
    LIBS    fiat
    LINKER_LANGUAGE Fortran
    MPI 2)
#-----------------------------------------------------------------------------------------
# Tests: fiat_test_displs_container
#
# test the linled_list needed for the non-blocking collectives 
ecbuild_add_test( TARGET fiat_test_displs_container
    SOURCES displs_container.F90
    LIBS    fiat
    LINKER_LANGUAGE Fortran)

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_namelist

ecbuild_add_executable(
    TARGET  fiat_test_namelist
    SOURCES test_namelist.F90
    LIBS    fiat
    LINKER_LANGUAGE Fortran
    NOINSTALL )
add_test(NAME fiat_test_namelist
      COMMAND ${CMAKE_COMMAND}
      "-DEXECUTABLE=$<TARGET_FILE:fiat_test_namelist>"
      "-DPASS_REGULAR_EXPRESSION=ABOR1.*POSNAM:CANNOT LOCATE NAM_NONPRESENT"
      -P ${CMAKE_CURRENT_SOURCE_DIR}/test_program_output.cmake )

# ----------------------------------------------------------------------------------------
# Test installation of fiat is working

configure_file( test-install.sh.in ${CMAKE_CURRENT_BINARY_DIR}/test-install.sh @ONLY )

unset( _test_args )
if( CMAKE_TOOLCHAIN_FILE )
  list( APPEND _test_args "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}" )
endif()
foreach( lang C CXX Fortran )
  if( CMAKE_${lang}_COMPILER )
    list( APPEND _test_args "-DCMAKE_${lang}_COMPILER=${CMAKE_${lang}_COMPILER}" )
  endif()
  if( CMAKE_${lang}_FLAGS )
    list( APPEND _test_args "-DCMAKE_${lang}_FLAGS=${CMAKE_${lang}_FLAGS}" )
  endif()
  if( CMAKE_EXE_LINKER_FLAGS )
    list( APPEND _test_args "-DCMAKE_EXE_LINKER_FLAGS=${CMAKE_EXE_LINKER_FLAGS}" )
  endif()
endforeach()

add_test( NAME fiat_test_install
          COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test-install.sh ${_test_args} )

# ----------------------------------------------------------------------------------------

endif(HAVE_TESTS)
