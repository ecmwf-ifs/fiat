# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#


ecbuild_add_executable( TARGET fiat_test_drhook_fn_process_options_dummy
        SOURCES dummy.F90
        LIBS ${fiatlib}
        LINKER_LANGUAGE Fortran
        NOINSTALL )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_CANNONICAL_NAMES ALL MEMORY TIMES HEAP STACK RSS PAGING WALLTIME CPUTIME CALLS MEMPROF WALLPROF COUNTERS CPUPROF TRIM SELF NOSELF NOPROPAGATE_SIGNALS NOSIZEINFO CLUSTERINFO CALLPATH NONE)
list( JOIN DRHOOK_OPTS_CANNONICAL_NAMES "," DRHOOK_OPTS_CANNONICAL_NAMES_STRING )

add_test( NAME fiat_test_drhook_fn_process_options_cannon_names
        COMMAND ${CMAKE_COMMAND}
                "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
# ${LAUNCH} is inherited from tests/CMakeLists.txt
                "-DLAUNCH=${LAUNCH}"
                "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"${DRHOOK_OPTS_CANNONICAL_NAMES_STRING}\""
                "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
                -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_cannon_names PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_CANNONICAL_NAMES_STRING}"
        )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_SHORT1_NAMES MEM TIME HWM STK PAG WALL CPU COUNT PROF NOPROP NOSIZE CLUSTER)
list( JOIN DRHOOK_OPTS_SHORT1_NAMES "," DRHOOK_OPTS_SHORT1_NAMES_STRING )

set(DRHOOK_OPTS_SHORT1_CANNONICAL_NAMES MEMORY TIMES HEAP STACK PAGING WALLTIME CPUTIME CALLS WALLPROF NOPROPAGATE_SIGNALS NOSIZEINFO CLUSTERINFO)
list( JOIN DRHOOK_OPTS_SHORT1_CANNONICAL_NAMES "," DRHOOK_OPTS_SHORT1_CANNONICAL_NAMES_STRING )

add_test( NAME fiat_test_drhook_fn_process_options_short_names1
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"${DRHOOK_OPTS_SHORT1_CANNONICAL_NAMES_STRING}\""
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_short_names1 PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_SHORT1_NAMES_STRING}"
        )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_SHORT2_NAMES CYCLES NOPROPAGATE)
list( JOIN DRHOOK_OPTS_SHORT2_NAMES "," DRHOOK_OPTS_SHORT2_NAMES_STRING )

set(DRHOOK_OPTS_SHORT2_CANNONICAL_NAMES WALLPROF NOPROPAGATE_SIGNALS)
list( JOIN DRHOOK_OPTS_SHORT2_CANNONICAL_NAMES "," DRHOOK_OPTS_SHORT2_CANNONICAL_NAMES_STRING )

add_test( NAME fiat_test_drhook_fn_process_options_short_names2
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"${DRHOOK_OPTS_SHORT2_CANNONICAL_NAMES_STRING}\""
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_short_names2 PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_SHORT2_NAMES_STRING}"
        )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_DELIMITED_STRING "ALL,MEMORY TIMES\tHEAP/STACK")

add_test( NAME fiat_test_drhook_fn_process_options_delimiters
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"ALL,MEMORY,TIMES,HEAP,STACK\""
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_delimiters PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_DELIMITED_STRING}"
        )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_ONLY_INVALID_STRING "foo,bar, ,,")

add_test( NAME fiat_test_drhook_fn_process_options_only_invalid
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DPASS_REGULAR_EXPRESSION_STDERR=Warning - no match for DR_HOOK_OPT=\"FOO,BAR\""
        "-DFAIL_REGULAR_EXPRESSION_STDERR=] DR_HOOK_OPT=\""
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_only_invalid PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_ONLY_INVALID_STRING}"
        )


# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_INVALID_STRING "foo,all,bar, ,,NONE")

add_test( NAME fiat_test_drhook_fn_process_options_invalid
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"ALL,NONE\"\n.*Warning - no match for DR_HOOK_OPT=\"FOO,BAR\""
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_invalid PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_INVALID_STRING}"
        )

# ------------------------------------------------------------------------------

set(DRHOOK_OPTS_EMPTY_STRING ",, ,")

add_test( NAME fiat_test_drhook_fn_process_options_empty
        COMMAND ${CMAKE_COMMAND}
        "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
        # ${LAUNCH} is inherited from tests/CMakeLists.txt
        "-DLAUNCH=${LAUNCH}"
        "-DFAIL_REGULAR_EXPRESSION_STDERR=(DR_HOOK_OPT=)|(Warning - no match for DR_HOOK_OPT=)"
        "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_empty PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_ASSERT_MPI_INITIALIZED=0;DR_HOOK_OPT=${DRHOOK_OPTS_EMPTY_STRING}"
        )
