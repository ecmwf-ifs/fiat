#
# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#

# Do we need to emulate no hardware support, or is it natively unsupported?
try_run(DRHOOK_HW_FPE_NOT_SUPPORTED # This is the runtime result of the check
        DRHOOK_HW_FPE_SUPPORT_COMPILE_RESULT
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/drhook_check_hw_fpe_support.c
        LINK_LIBRARIES m # Needed for fenv.h
        )

if ( CMAKE_Fortran_COMPILER_ID MATCHES Intel )
  set( DRHOOK_HW_FPE_FFLAGS '-fpe-all=0')
endif ()

# Test basic implementation

ecbuild_add_executable( TARGET drhook_no_hw_fpe_basic
  SOURCES drhook_no_hw_fpe_basic.F90 drhook_no_hw_fpe_disabled_fpes.c
  LIBS ${fiatlib} m
  LINKER_LANGUAGE Fortran
  FFLAGS ${DRHOOK_HW_FPE_FFLAGS}
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_no_hw_fpe_native
        COMMAND drhook_no_hw_fpe_basic
        ENVIRONMENT DR_HOOK=1 DR_HOOK_TRAPFPE=1 DR_HOOK_SILENT=0 )

if ( TEST fiat_test_drhook_no_hw_fpe_native )
    if ( DRHOOK_HW_FPE_NOT_SUPPORTED )
        set_tests_properties(fiat_test_drhook_no_hw_fpe_native
            PROPERTIES WILL_FAIL TRUE PASS_REGULAR_EXPRESSION "Hello, x is" )
    else()
        set_tests_properties(fiat_test_drhook_no_hw_fpe_native
                PROPERTIES WILL_FAIL TRUE PASS_REGULAR_EXPRESSION "Dr.Hook has detected an FPE on exit of region 'drhook_no_hw_fpe_basic': FE_DIVBYZERO" )
    endif()
endif()

################################################################################

# Test basic implementation, but with the opposite system behaviour faked

if ( NOT APPLE ) # The faked libs don't work on macOS. They don't propagate to fiat/drhook
    if ( DRHOOK_HW_FPE_NOT_SUPPORTED )

        ecbuild_add_library( TARGET drhook_no_hw_fpe_basic_faked_support_lib
                SOURCES drhook_no_hw_fpe_fake_hardware_support_shim.c
                LINKER_LANGUAGE C
                NOINSTALL )

        ecbuild_add_executable( TARGET drhook_no_hw_fpe_basic_faked_support
          SOURCES drhook_no_hw_fpe_basic.F90 drhook_no_hw_fpe_disabled_fpes.c
          LIBS ${fiatlib} drhook_no_hw_fpe_basic_faked_support_lib
          LINKER_LANGUAGE Fortran m
          FFLAGS ${DRHOOK_HW_FPE_FFLAGS}
          NOINSTALL )

        ecbuild_add_test( TARGET fiat_test_drhook_no_hw_fpe_basic_faked_support
                COMMAND drhook_no_hw_fpe_basic
                ENVIRONMENT DR_HOOK=1 DR_HOOK_TRAPFPE=1 DR_HOOK_SILENT=0  LD_PRELOAD=${CMAKE_CURRENT_BINARY_DIR}/libdrhook_no_hw_fpe_basic_faked_support_lib.so )

        if ( TEST fiat_test_drhook_no_hw_fpe_basic_faked_support )
            set_tests_properties(fiat_test_drhook_no_hw_fpe_basic_faked_support
                    PROPERTIES PASS_REGULAR_EXPRESSION "Hello, x is" )
        endif()

    else()

        ecbuild_add_library( TARGET drhook_no_hw_fpe_basic_faked_no_support_lib
                SOURCES drhook_no_hw_fpe_fake_no_hardware_support_shim.c
                LINKER_LANGUAGE C
                NOINSTALL )

        ecbuild_add_executable( TARGET drhook_no_hw_fpe_basic_faked_no_support
          SOURCES drhook_no_hw_fpe_basic.F90 drhook_no_hw_fpe_disabled_fpes.c
          LIBS ${fiatlib} drhook_no_hw_fpe_basic_faked_no_support_lib m
          LINKER_LANGUAGE Fortran
          FFLAGS ${DRHOOK_HW_FPE_FFLAGS}
          NOINSTALL )

        ecbuild_add_test( TARGET fiat_test_drhook_no_hw_fpe_basic_faked_no_support
                COMMAND drhook_no_hw_fpe_basic
                ENVIRONMENT DR_HOOK=1 DR_HOOK_TRAPFPE=1 DR_HOOK_SILENT=0 LD_PRELOAD=${CMAKE_CURRENT_BINARY_DIR}/libdrhook_no_hw_fpe_basic_faked_no_support_lib.so )

        if ( TEST fiat_test_drhook_no_hw_fpe_basic_faked_no_support )
            set_tests_properties(fiat_test_drhook_no_hw_fpe_basic_faked_no_support
                    PROPERTIES PASS_REGULAR_EXPRESSION "Dr.Hook has detected an FPE on exit of region 'drhook_no_hw_fpe_basic': FE_DIVBYZERO" )
        endif()
    endif()
endif()

################################################################################
