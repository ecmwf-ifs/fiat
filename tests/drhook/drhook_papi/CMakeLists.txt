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

if (TEST fiat_test_drhook_papi_basic)
    set_tests_properties(fiat_test_drhook_papi_basic
            PROPERTIES PASS_REGULAR_EXPRESSION "Writing counter information of proc#1 into file" )
endif()


ecbuild_add_test( TARGET fiat_test_drhook_papi_basic_valid_csv
        TYPE SCRIPT
        # Just making sure it's not an empty file
        COMMAND "find"
        ARGS "." "-name" "drhook.prof.1.csv" "-type" "f" "-size" "+100c"
        CONDITION HAVE_DR_HOOK_PAPI )

if (TEST fiat_test_drhook_papi_basic_valid_csv)
    set_tests_properties(fiat_test_drhook_papi_basic_valid_csv
            PROPERTIES DEPENDS fiat_test_drhook_papi_basic
            PASS_REGULAR_EXPRESSION "drhook.prof.1.csv" )
endif()

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
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS DR_HOOK_PROFILE=fiat_test_drhook_papi_mpi
        CONDITION HAVE_DR_HOOK_PAPI AND HAVE_MPI )


ecbuild_add_test( TARGET fiat_test_drhook_papi_mpi_valid_csv
        TYPE SCRIPT
        # Just making sure it's not an empty file
        # We have to do this weird thing with bash so that we can
        # use a redirect. CMake tests are really basic...
        COMMAND "bash"
        ARGS "-c" "find . -name 'fiat_test_drhook_papi_mpi.[1-5].csv' -type f -size +100c | wc -l"
        CONDITION HAVE_DR_HOOK_PAPI AND HAVE_MPI )

if (TEST fiat_test_drhook_papi_mpi_valid_csv)
    set_tests_properties(fiat_test_drhook_papi_mpi_valid_csv
            PROPERTIES DEPENDS fiat_test_drhook_papi_mpi
            PASS_REGULAR_EXPRESSION "5" )
endif()

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
        CONDITION HAVE_DR_HOOK_PAPI )

if (TEST fiat_test_drhook_papi_user_filename_valid_csv)
    set_tests_properties(fiat_test_drhook_papi_user_filename_valid_csv
            PROPERTIES DEPENDS fiat_test_drhook_papi_user_filename
            PASS_REGULAR_EXPRESSION "fiat_test_drhook_papi_user_filename.1.csv" )
endif()

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

if (TEST fiat_test_drhook_papi_user_counters)
    set_tests_properties(fiat_test_drhook_papi_user_counters
            PROPERTIES PASS_REGULAR_EXPRESSION "PAPI_TOT_INS" )
endif()

# Test user specified counters going over max allowed

ecbuild_add_executable( TARGET drhook_papi_user_counters_more_than_max
  SOURCES drhook_papi_user_counters_more_than_max.F90
  LIBS fiat
  LINKER_LANGUAGE Fortran
  CONDITION HAVE_DR_HOOK_PAPI
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_papi_user_counters_more_than_max
        COMMAND drhook_papi_user_counters_more_than_max
        ENVIRONMENT DR_HOOK=1 DR_HOOK_OPT=COUNTERS DR_HOOK_PAPI_COUNTERS=PAPI_TOT_CYC,PAPI_FP_OPS,PAPI_L1_DCA,PAPI_L2_DCM,PAPI_TOT_INS DR_HOOK_PROFILE=drhook_papi_user_counters_more_than_max
        CONDITION HAVE_DR_HOOK_PAPI )

if (TEST fiat_test_drhook_papi_user_counters_more_than_max)
    set_tests_properties(fiat_test_drhook_papi_user_counters_more_than_max
            PROPERTIES FAIL_REGULAR_EXPRESSION "PAPI_TOT_INS" )
endif()
