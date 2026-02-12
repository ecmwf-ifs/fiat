# (C) Copyright 2026 ECMWF.
#
# This file is covered by the LICENSING file in the root of this project.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

function( run_command COMMAND )
    set( COMMAND ${ARGV} )
    string(REPLACE ";" " " PRINT_COMMAND "${COMMAND}" )
    message( "${PRINT_COMMAND}" )
    execute_process(
            COMMAND ${COMMAND}
            RESULT_VARIABLE res
            OUTPUT_VARIABLE stdout
            ERROR_VARIABLE  stderr
    )

    if( DEFINED PASS_REGULAR_EXPRESSION_STDOUT )
        string(REGEX MATCH "${PASS_REGULAR_EXPRESSION_STDOUT}" stdout_pass_match "${stdout}" )
    endif()
    if( DEFINED FAIL_REGULAR_EXPRESSION_STDOUT )
        string(REGEX MATCH "${FAIL_REGULAR_EXPRESSION_STDOUT}" stdout_fail_match "${stdout}" )
    endif()

    if( DEFINED PASS_REGULAR_EXPRESSION_STDERR )
        string(REGEX MATCH "${PASS_REGULAR_EXPRESSION_STDERR}" stderr_pass_match "${stderr}" )
    endif()
    if( DEFINED FAIL_REGULAR_EXPRESSION_STDERR )
        string(REGEX MATCH "${FAIL_REGULAR_EXPRESSION_STDERR}" stderr_fail_match "${stderr}" )
    endif()

    if ( DEFINED PASS_REGULAR_EXPRESSION_STDOUT AND NOT stdout_pass_match )
        message( FATAL_ERROR "Test failed: Could not find pass regex [${PASS_REGULAR_EXPRESSION_STDOUT}] in program stdout [${stdout}]")
    endif()
    if ( DEFINED PASS_REGULAR_EXPRESSION_STDERR AND NOT stderr_pass_match )
        message( FATAL_ERROR "Test failed: Could not find pass regex [${PASS_REGULAR_EXPRESSION_STDERR}] in program stderr [${stderr}]")
    endif()

    if ( DEFINED FAIL_REGULAR_EXPRESSION_STDOUT AND stdout_fail_match )
        message( FATAL_ERROR "Test failed: Found fail regex [${FAIL_REGULAR_EXPRESSION_STDOUT}] in program stdout [${stdout}]")
    endif()
    if ( DEFINED FAIL_REGULAR_EXPRESSION_STDERR AND stderr_fail_match )
        message( FATAL_ERROR "Test failed: Found fail regex [${FAIL_REGULAR_EXPRESSION_STDERR}] in program stderr [${stderr}]")
    endif()

    if( (stdout_pass_match AND stderr_pass_match) AND NOT (stdout_fail_match OR stderr_fail_match))
        message( "Test succeeded: All regexes matched")
    endif()
endfunction()

message( "Running test ${EXECUTABLE} ... ")
run_command( ${LAUNCH} ${EXECUTABLE} )
message( "Running test ${EXECUTABLE} ... done")