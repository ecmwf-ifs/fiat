#
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#

# Test basic implementation

ecbuild_add_executable( TARGET drhook_papi_basic
  SOURCES drhook_papi_basic.F90
  LIBS fiat
  LINKER_LANGUAGE Fortran
  CONDITION HAVE_DR_HOOK_PAPI
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_papi_basic
        COMMAND drhook_papi_basic
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS
        CONDITION HAVE_DR_HOOK_PAPI )

set_tests_properties(fiat_test_drhook_papi_basic
        PROPERTIES PASS_REGULAR_EXPRESSION "Writing counter information of proc#1 into file" )


ecbuild_add_test( TARGET fiat_test_drhook_papi_basic_valid_csv
        TYPE SCRIPT
        # Just making sure it's not an empty file
        COMMAND "find"
        ARGS "." "-name" "drhook.prof.0.csv" "-type" "f" "-size" "+100c"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS
        CONDITION HAVE_DR_HOOK_PAPI )

set_tests_properties(fiat_test_drhook_papi_basic_valid_csv
        PROPERTIES DEPENDS fiat_test_drhook_papi_basic
        FAIL_REGULAR_EXPRESSION "no matches found" )

# Test MPI implementation

ecbuild_add_executable( TARGET drhook_papi_mpi
  SOURCES drhook_papi_mpi.F90
  LIBS fiat
  LINKER_LANGUAGE Fortran
  CONDITION HAVE_DR_HOOK_PAPI AND HAVE_MPI
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_papi_mpi
        COMMAND drhook_papi_mpi
        MPI 5
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS
        CONDITION HAVE_DR_HOOK_PAPI AND HAVE_MPI )


ecbuild_add_test( TARGET fiat_test_drhook_papi_mpi_valid_csv
        TYPE SCRIPT
        # Just making sure it's not an empty file
        # We have to do this weird thing with bash so that we can
        # use a redirect. CMake tests are really basic...
        COMMAND "bash"
        ARGS "-c" "find . -name 'drhook.prof.[1-5].csv' -type f -size +100c | wc -l"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS
        CONDITION HAVE_DR_HOOK_PAPI AND HAVE_MPI )

set_tests_properties(fiat_test_drhook_papi_mpi_valid_csv
        PROPERTIES DEPENDS fiat_test_drhook_papi_mpi
        PASS_REGULAR_EXPRESSION "5" )

# Test user specified output file names

ecbuild_add_executable( TARGET drhook_papi_user_filename
        SOURCES drhook_papi_user_filename.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        CONDITION HAVE_DR_HOOK_PAPI
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_papi_user_filename
        COMMAND drhook_papi_user_filename
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS DR_HOOK_PROFILE=fiat_test_drhook_papi_user_filename
        CONDITION HAVE_DR_HOOK_PAPI )

ecbuild_add_test( TARGET fiat_test_drhook_papi_user_filename_valid_csv
        TYPE SCRIPT
        # Just making sure it's not an empty file
        COMMAND "find"
        ARGS "." "-name" "fiat_test_drhook_papi_user_filename.1.csv" "-type" "f"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS
        CONDITION HAVE_DR_HOOK_PAPI )

set_tests_properties(fiat_test_drhook_papi_user_filename_valid_csv
        PROPERTIES DEPENDS fiat_test_drhook_papi_user_filename
        FAIL_REGULAR_EXPRESSION "no matches found" )

# Test user specified counters

ecbuild_add_executable( TARGET drhook_papi_user_counters
  SOURCES drhook_papi_user_counters.F90
  LIBS fiat
  LINKER_LANGUAGE Fortran
  CONDITION HAVE_DR_HOOK_PAPI
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_papi_user_counters
        COMMAND drhook_papi_user_counters
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS DR_HOOK_PAPI_COUNTERS=PAPI_TOT_INS DR_HOOK_PROFILE=fiat_test_drhook_papi_user_counters
        CONDITION HAVE_DR_HOOK_PAPI )