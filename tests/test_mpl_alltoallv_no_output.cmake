# (C) Copyright 2019 ECMWF.
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
  if(res)
    set( error TRUE )
    message("Test failed. This could be because executable is not launched with correctly (aprun, srun, mpirun, ...)")
  endif()
  if( stdout OR stderr )
    set( error TRUE )
    message("${stdout}")
    message("${stderr}")
    if( $ENV{FIAT_TEST_IGNORE_MPI_OUTPUT} )
      set( error FALSE )
    else()
      message("Test failed. Executable should have no output. Set FIAT_TEST_IGNORE_MPI_OUTPUT=1 in environment if the output is not from MPL.")
    endif()
  endif()

  if( error )
    message(FATAL_ERROR "Test failed.")
  endif()

endfunction()

message( "Running test ${EXECUTABLE} ... ")
run_command( ${LAUNCH} ${EXECUTABLE} )
message( "Running test ${EXECUTABLE} ... done")
