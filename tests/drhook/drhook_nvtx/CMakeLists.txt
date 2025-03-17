#
# (C) Copyright 2024- ECMWF.
# (C) Copyright 2024- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
#

# Test basic implementation

ecbuild_add_executable( TARGET drhook_nvtx_basic
  SOURCES drhook_nvtx_basic.F90
  LIBS fiat
  LINKER_LANGUAGE Fortran
  CONDITION HAVE_DR_HOOK_NVTX
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_nvtx_basic
        TYPE SCRIPT
        COMMAND "nsys"
        ARGS "profile" "--force-overwrite=true" "--trace=nvtx" "--kill=none" "--output=nsys.drhook_nvtx_basic.qdrep" "./drhook_nvtx_basic"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_NVTX=1
        CONDITION HAVE_DR_HOOK_NVTX )

# Test basic implementation in C

ecbuild_add_executable( TARGET drhook_nvtx_basic_c
  SOURCES drhook_nvtx_basic_c.c
  LIBS fiat
  LINKER_LANGUAGE C
  CONDITION HAVE_DR_HOOK_NVTX
  NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_nvtx_basic_c
        TYPE SCRIPT
        COMMAND "nsys"
        ARGS "profile" "--force-overwrite=true" "--trace=nvtx" "--kill=none" "--output=nsys.drhook_nvtx_basic_c.qdrep" "./drhook_nvtx_basic_c"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_NVTX=1
        CONDITION HAVE_DR_HOOK_NVTX )

# Test abort on mismatched region names

ecbuild_add_executable( TARGET drhook_nvtx_mismatched_regions
        SOURCES drhook_nvtx_mismatched_regions.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        CONDITION HAVE_DR_HOOK_NVTX
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_nvtx_mismatched_regions
        TYPE SCRIPT
        COMMAND "nsys"
        ARGS "profile" "--force-overwrite=true" "--trace=nvtx" "--kill=none" "--output=nsys.drhook_nvtx_mismatched_regions.qdrep" "./drhook_nvtx_mismatched_regions"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_NVTX=1
        CONDITION HAVE_DR_HOOK_NVTX )

if (TEST fiat_test_drhook_nvtx_mismatched_regions)
    set_tests_properties(fiat_test_drhook_nvtx_mismatched_regions
            PROPERTIES WILL_FAIL TRUE )
endif()

# Test skip on spammy regions

ecbuild_add_executable( TARGET drhook_nvtx_skip_spam_regions
        SOURCES drhook_nvtx_skip_spam_regions.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        CONDITION HAVE_DR_HOOK_NVTX
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_nvtx_skip_spam_regions
        TYPE SCRIPT
        COMMAND "nsys"
        ARGS "profile" "--force-overwrite=true" "--trace=nvtx" "--kill=none" "--output=nsys.drhook_nvtx_skip_spam_regions.qdrep" "./drhook_nvtx_skip_spam_regions"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_NVTX=1 DR_HOOK_SILENT=0
        CONDITION HAVE_DR_HOOK_NVTX )

if (TEST fiat_test_drhook_nvtx_skip_spam_regions)
    set_tests_properties(fiat_test_drhook_nvtx_skip_spam_regions
            PROPERTIES PASS_REGULAR_EXPRESSION "DRHOOK:NVTX: Skipping closing of region foo" PASS_REGULAR_EXPRESSION "DRHOOK:NVTX: Skipping opening of region foo" )
endif()

# Test not to skip on spammy regions with long runtimes

ecbuild_add_executable( TARGET drhook_nvtx_no_skip_spam_regions
        SOURCES drhook_nvtx_no_skip_spam_regions.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        CONDITION HAVE_DR_HOOK_NVTX
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_nvtx_no_skip_spam_regions
        TYPE SCRIPT
        COMMAND "nsys"
        ARGS "profile" "--force-overwrite=true" "--trace=nvtx" "--kill=none" "--output=nsys.drhook_nvtx_no_skip_spam_regions.qdrep" "./drhook_nvtx_no_skip_spam_regions"
        ENVIRONMENT DR_HOOK=1 DR_HOOK_NVTX=1 DR_HOOK_SILENT=0
        CONDITION HAVE_DR_HOOK_NVTX )

if (TEST fiat_test_drhook_nvtx_no_skip_spam_regions)
    set_tests_properties(fiat_test_drhook_nvtx_no_skip_spam_regions
            PROPERTIES FAIL_REGULAR_EXPRESSION "DRHOOK:NVTX: Skipping closing of region foo" FAIL_REGULAR_EXPRESSION "DRHOOK:NVTX: Skipping opening of region foo" )
endif()
