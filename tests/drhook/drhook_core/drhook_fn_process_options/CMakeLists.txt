# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#

set(DRHOOK_OPTS_CANNONICAL_NAMES ALL MEMORY TIMES HEAP STACK RSS PAGING WALLTIME CPUTIME CALLS MEMPROF WALLPROF COUNTERS CPUPROF TRIM SELF NOSELF NOPROPAGATE_SIGNALS NOSIZEINFO CLUSTERINFO CALLPATH NONE)

ecbuild_add_executable( TARGET fiat_test_drhook_fn_process_options_dummy
        SOURCES dummy.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

list( JOIN DRHOOK_OPTS_CANNONICAL_NAMES "," DRHOOK_OPTS_CANNONICAL_NAME_STRING )


add_test( NAME fiat_test_drhook_fn_process_options_cannon_names
        COMMAND ${CMAKE_COMMAND}
                "-DEXECUTABLE=$<TARGET_FILE:fiat_test_drhook_fn_process_options_dummy>"
# ${LAUNCH} is inherited from tests/CMakeLists.txt
                "-DLAUNCH=${LAUNCH}"
                "-DPASS_REGULAR_EXPRESSION_STDERR=DR_HOOK_OPT=\"${DRHOOK_OPTS_CANNONICAL_NAME_STRING}\""
                "-DFAIL_REGULAR_EXPRESSION_STDOUT=.+"
                -P ${CMAKE_CURRENT_SOURCE_DIR}/drhook_separate_stream_regex.cmake
        )

set_tests_properties(fiat_test_drhook_fn_process_options_cannon_names PROPERTIES
        ENVIRONMENT "DR_HOOK=1;DR_HOOK_OPT=${DRHOOK_OPTS_CANNONICAL_NAME_STRING}"
        )
