#
# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#

find_package(CMath)

# Do we need to emulate no hardware support, or is it natively unsupported?
try_run(DRHOOK_HW_FPE_NOT_SUPPORTED # This is the runtime result of the check
    DRHOOK_HW_FPE_SUPPORT_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/drhook_check_hw_fpe_support.c
    LINK_LIBRARIES ${CMATH_LIBRARIES} # Needed for fenv.h
    )

if ( CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM" )
  set( DRHOOK_HW_FPE_FFLAGS "-fpe-all=0")
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  set( DRHOOK_HW_FPE_FFLAGS "-Ktrap=fp" )
endif ()

# Test basic implementation
set (test_executable_name drhook_no_hw_fpe_basic)
set (test_name fiat_test_drhook_no_hw_fpe_native)

ecbuild_add_executable( TARGET ${test_executable_name}
  SOURCES drhook_no_hw_fpe_basic.F90 drhook_no_hw_fpe_fenv_util.c
  LIBS ${fiatlib} ${CMATH_LIBRARIES}
  LINKER_LANGUAGE Fortran
  FFLAGS ${DRHOOK_HW_FPE_FFLAGS}
  NOINSTALL )

if ( DRHOOK_HW_FPE_NOT_SUPPORTED )
  # If no hardware FPE support, then DrHook detects the FPE
  set( PASS_REGULAR_EXPRESSION "Dr.Hook has detected an FPE on exit of region 'drhook_no_hw_fpe_basic': FE_DIVBYZERO" )
else()
  # Else it's the OS sigfpe handler (but we silently disable it and instead get this output)
  set( PASS_REGULAR_EXPRESSION "Hello, x is" )
endif()
add_test(${test_name}
    ${CMAKE_COMMAND}
      "-DEXECUTABLE=${CMAKE_CURRENT_BINARY_DIR}/${test_executable_name}"
      "-DLAUNCH=${LAUNCH}"
      "-DPASS_REGULAR_EXPRESSION=${PASS_REGULAR_EXPRESSION}"
      -P ${CMAKE_CURRENT_SOURCE_DIR}/../../../test_program_output.cmake)
set_tests_properties(${test_name}
    PROPERTIES ENVIRONMENT "DR_HOOK=1;DR_HOOK_TRAPFPE=1;DR_HOOK_SILENT=0;DR_HOOK_ASSERT_MPI_INITIALIZED=0" )


################################################################################

# Test basic implementation, but with the opposite system behaviour faked

if ( DRHOOK_HW_FPE_NOT_SUPPORTED )
  set( DRHOOK_HW_FPE_MOCK_DEF "MOCK_HARDWARE_FPE_SUPPORT")
else ()
  set( DRHOOK_HW_FPE_NO "no_" )
  set( DRHOOK_HW_FPE_MOCK_DEF "MOCK_NO_HARDWARE_FPE_SUPPORT")
endif ()

set (test_executable_name drhook_no_hw_fpe_basic_faked_${DRHOOK_HW_FPE_NO}support)
set (test_name fiat_test_${test_executable_name})
ecbuild_add_executable( TARGET ${test_executable_name}
  SOURCES drhook_no_hw_fpe_basic.F90 drhook_no_hw_fpe_fenv_util.c
  LIBS ${fiatlib} ${CMATH_LIBRARIES}
  LINKER_LANGUAGE Fortran
  FFLAGS ${DRHOOK_HW_FPE_FFLAGS}
  DEFINITIONS ${DRHOOK_HW_FPE_MOCK_DEF}
  NOINSTALL )

if ( DRHOOK_HW_FPE_NOT_SUPPORTED )
  set( PASS_REGULAR_EXPRESSION "Hello, x is" )
else()
  set( PASS_REGULAR_EXPRESSION "Dr.Hook has detected an FPE on exit of region 'drhook_no_hw_fpe_basic': FE_DIVBYZERO" )
endif()
add_test(${test_name}
    ${CMAKE_COMMAND}
      "-DEXECUTABLE=${CMAKE_CURRENT_BINARY_DIR}/${test_executable_name}"
      "-DLAUNCH=${LAUNCH}"
      "-DPASS_REGULAR_EXPRESSION=${PASS_REGULAR_EXPRESSION}"
      -P ${CMAKE_CURRENT_SOURCE_DIR}/../../../test_program_output.cmake)
set_tests_properties(${test_name}
    PROPERTIES ENVIRONMENT "DR_HOOK=1;DR_HOOK_TRAPFPE=1;DR_HOOK_SILENT=0;DR_HOOK_ASSERT_MPI_INITIALIZED=0" )

################################################################################
