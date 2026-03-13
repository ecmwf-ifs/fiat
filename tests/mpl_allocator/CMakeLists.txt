# (C) Copyright 2026- ECMWF.
# (C) Copyright 2026- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

set(___SEP___ ".")
if (POLICY CMP0110) # from 3.19, allows special characters in test names such as a space
    cmake_policy(SET CMP0110 NEW)
    set(___SEP___ " ")
endif()

ecbuild_add_executable(
    TARGET fiat_test_mpl_allocator
    SOURCES test_mpl_allocator.F90
    LIBS    ${fiatlib}
    LINKER_LANGUAGE Fortran
    NOINSTALL
)

ecbuild_add_executable(
    TARGET fiat_test_fiat_buddy
    SOURCES test_fiat_buddy.F90
    LIBS    ${fiatlib}
    LINKER_LANGUAGE Fortran
    NOINSTALL
)

add_test(
    NAME "fiat_test_fiat_buddy_capacity_uninitialized${___SEP___}ASSERT_ABORT"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_fiat_buddy>;fiat_test_fiat_buddy_capacity_uninitialized"
        "-DPASS_REGULAR_EXPRESSION=pool is null"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)

add_test(
    NAME "fiat_test_fiat_buddy_allocated_uninitialized${___SEP___}ASSERT_ABORT"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_fiat_buddy>;fiat_test_fiat_buddy_allocated_uninitialized"
        "-DPASS_REGULAR_EXPRESSION=pool is null"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)

add_test(
    NAME "fiat_test_fiat_buddy_reserve_uninitialized${___SEP___}ASSERT_ABORT"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_fiat_buddy>;fiat_test_fiat_buddy_reserve_uninitialized"
        "-DPASS_REGULAR_EXPRESSION=pool is null"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)

ecbuild_add_test(
    TARGET fiat_test_fiat_buddy_reserve_zero_then_allocate
    COMMAND fiat_test_fiat_buddy
    ARGS fiat_test_fiat_buddy_reserve_zero_then_allocate
)

ecbuild_add_test(
    TARGET fiat_test_fiat_buddy_remove_middle_node
    COMMAND fiat_test_fiat_buddy
    ARGS fiat_test_fiat_buddy_remove_middle_node
)

ecbuild_add_test(
    TARGET fiat_test_fiat_buddy_remove_head_node
    COMMAND fiat_test_fiat_buddy
    ARGS fiat_test_fiat_buddy_remove_head_node
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_no_init
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_no_init
    ENVIRONMENT "MPL=0"
)

set( DEFAULT_MPL_ALLOCATOR_RESOURCE "FIAT_BUDDY" )
set( MPL_PLUTO_RESOURCE "HOST_POOL" )

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE}" "MPL_ALLOCATOR_GROW=1"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_multiple
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_multiple
    ENVIRONMENT "MPL=0"
)

add_test(
    NAME "fiat_test_mpl_allocator_init_multiple${___SEP___}ASSERT_ABORT_ALLOCATED"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_multiple_allocated_abort"
        "-DPASS_REGULAR_EXPRESSION=MPL_ABORT.*outstanding allocations"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_multiple${___SEP___}ASSERT_ABORT_ALLOCATED" PROPERTIES
    ENVIRONMENT "MPL=0"
)

add_test(
    NAME "fiat_test_mpl_allocator_final${___SEP___}ASSERT_ABORT_ALLOCATED"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_final_allocated_abort"
        "-DPASS_REGULAR_EXPRESSION=MPL_ABORT.*outstanding allocations"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_final${___SEP___}ASSERT_ABORT_ALLOCATED" PROPERTIES
    ENVIRONMENT "MPL=0"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FORTRAN" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size0
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size0
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE}" "MPL_ALLOCATOR_GROW=1"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size0${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size0
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FORTRAN" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size0${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size0
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size0${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size0
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)

# Test of MPL_ALLOCATOR_INIT(SIZE) which sets an initial size.
# The FIAT_BUDDY resource is currently expected to be used when MPL_ALLOCATOR_INIT(SIZE) is used,
# and the allocator is not allowed to grow beyond its initial size by default in this case.
# This is tested with the test marked with "ASSERT_ABORT_FAILED_TO_GROW".

if (DEFAULT_MPL_ALLOCATOR_RESOURCE STREQUAL "FIAT_BUDDY")
add_test(
    NAME "fiat_test_mpl_allocator_init_size${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_size"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_size${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE}"
)
ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_GROW=1}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE}" "MPL_ALLOCATOR_GROW=1"
)
else()
ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE}"
)
endif()

add_test(
    NAME "fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_size"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY;MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY"
)
ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY,MPL_ALLOCATOR_GROW=1}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "MPL_ALLOCATOR_GROW=1"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FORTRAN" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)

add_test(
    NAME "fiat_test_mpl_allocator_init_size${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_size_RESOURCE;FIAT_BUDDY"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_size${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY"
)
ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}${___SEP___}ENV={MPL_ALLOCATOR_GROW=1}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size_RESOURCE FIAT_BUDDY
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "MPL_ALLOCATOR_GROW=1"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ARGS={RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size_RESOURCE FORTRAN
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_size${___SEP___}ARGS={RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_size_RESOURCE ${MPL_PLUTO_RESOURCE}
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)

# Test of MPL_ALLOCATOR_INIT() followed by MPL_ALLOCATOR_RESERVE(SIZE)
# The allocator is now allowed to grow, unless MPL_ALLOCATOR_GROW=0 is set.
# This is tested with the FIAT_BUDDY backend for tests marked with "ASSERT_ABORT_FAILED_TO_GROW".

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${EXPECTED_MPL_ALLOCATOR_RESOURCE}"
)

if (EXPECTED_MPL_ALLOCATOR_RESOURCE STREQUAL "FIAT_BUDDY")
add_test(
    NAME "fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_and_reserve"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=${DEFAULT_MPL_ALLOCATOR_RESOURCE};MPL_ALLOCATOR_GROW=0"
)
endif()

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY"
)

add_test(
    NAME "fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY,MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_and_reserve"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY,MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY;MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY;MPL_ALLOCATOR_GROW=0"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=FORTRAN" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ENV={MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve
    ENVIRONMENT "MPL=0" "MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve_RESOURCE FIAT_BUDDY
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY" "MPL_ALLOCATOR_GROW=1"
)

add_test(
    NAME "fiat_test_mpl_allocator_init_and_reserve${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}${___SEP___}ENV={MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW"
    COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_mpl_allocator>;fiat_test_mpl_allocator_init_and_reserve_RESOURCE;FIAT_BUDDY"
        "-DPASS_REGULAR_EXPRESSION=ABOR1.*Failed to grow"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../test_program_output.cmake
)
set_tests_properties("fiat_test_mpl_allocator_init_and_reserve${___SEP___}ARGS={RESOURCE=FIAT_BUDDY}${___SEP___}ENV={MPL_ALLOCATOR_GROW=0}${___SEP___}ASSERT_ABORT_FAILED_TO_GROW" PROPERTIES
    ENVIRONMENT "MPL=0;EXPECTED_MPL_ALLOCATOR_RESOURCE=FIAT_BUDDY;MPL_ALLOCATOR_GROW=0"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ARGS={RESOURCE=FORTRAN}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve_RESOURCE FORTRAN
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=FORTRAN"
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_init_and_reserve${___SEP___}ARGS={RESOURCE=${MPL_PLUTO_RESOURCE}}
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_init_and_reserve_RESOURCE ${MPL_PLUTO_RESOURCE}
    ENVIRONMENT "MPL=0" "EXPECTED_MPL_ALLOCATOR_RESOURCE=${MPL_PLUTO_RESOURCE}"
    CONDITION HAVE_PLUTO
)
