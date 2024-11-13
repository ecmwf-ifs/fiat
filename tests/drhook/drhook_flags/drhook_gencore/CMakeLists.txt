# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# ------------------------------------------------------------------------------
#  Dr Hook Gencore Tests
# ------------------------------------------------------------------------------

# Test single threaded basic gencore
ecbuild_add_executable( TARGET drhook_gencore_basic
        SOURCES drhook_gencore_basic.F90 drhook_gencore_utilities.c
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_gencore_basic
        COMMAND gencore_test_runner.sh
        ARGS drhook_gencore_basic 1 "DR_HOOK_GENCORE=1" "DR_HOOK_ALLOW_COREDUMP=1" "Received signal#11 and now calling abort"
        RESOURCES gencore_test_runner.sh
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 DR_HOOK_GENCORE_SIGNALS=11
        )

# Test allow_coredump respects existing soft limits
ecbuild_add_executable( TARGET drhook_gencore_existing_soft_limits
        SOURCES drhook_gencore_existing_soft_limits.F90
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

# ulimit -S -c 555 is set in the runner
ecbuild_add_test( TARGET fiat_test_drhook_gencore_existing_soft_limits
        COMMAND gencore_test_runner.sh
        #       The soft limit is set in increments of 1024, so 555*1024=568320
        ARGS drhook_gencore_existing_soft_limits 0 "DR_HOOK_GENCORE=1" "DR_HOOK_ALLOW_COREDUMP=1" "Hardlimit for core file is now 568320"
        RESOURCES gencore_test_runner.sh
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 )

# Test user specified processes for allowing coredumps
ecbuild_add_executable( TARGET drhook_gencore_user_procs
        SOURCES drhook_gencore_user_procs.F90 drhook_gencore_utilities.c
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_gencore_user_procs
        COMMAND gencore_test_runner.sh
        ARGS drhook_gencore_user_procs 2 "DR_HOOK_GENCORE_PROCS=0:0, 1:1, 2:0, 3:1, 4:0"
        RESOURCES gencore_test_runner.sh
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 DR_HOOK_GENCORE_SIGNALS=11 DR_HOOK_GENCORE_PROCS=1,3
        CONDITION HAVE_MPI )

# Test super secret flag for allowing coredumps
ecbuild_add_executable( TARGET drhook_gencore_super_secret
        SOURCES drhook_gencore_super_secret.F90 drhook_gencore_utilities.c
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_gencore_super_secret
        COMMAND gencore_test_runner.sh
        ARGS drhook_gencore_super_secret 5 "DR_HOOK_SECRET" "WARNING: The following option can easily bring down entire file systems on its own."
        RESOURCES gencore_test_runner.sh
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 DR_HOOK_GENCORE_SIGNALS=11 DR_HOOK_SECRET=1
        CONDITION HAVE_MPI )

# Test default only creates a coredump for the first process
ecbuild_add_executable( TARGET drhook_gencore_first_only
        SOURCES drhook_gencore_first_only.F90 drhook_gencore_utilities.c
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_gencore_first_only
        COMMAND gencore_test_runner.sh
        ARGS drhook_gencore_first_only 1
        RESOURCES gencore_test_runner.sh
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 DR_HOOK_GENCORE_SIGNALS=11
        CONDITION HAVE_MPI )

# Test user specified signals for allowing coredumps
ecbuild_add_executable( TARGET drhook_gencore_user_sigs
        SOURCES drhook_gencore_user_sigs.F90 drhook_gencore_utilities.c
        LIBS fiat
        LINKER_LANGUAGE Fortran
        NOINSTALL )

ecbuild_add_test( TARGET fiat_test_drhook_gencore_user_sigs
        COMMAND drhook_gencore_user_sigs
        ENVIRONMENT DR_HOOK=1 DR_HOOK_GENCORE=1 DR_HOOK_GENCORE_SIGNALS=11 )
set_tests_properties( fiat_test_drhook_gencore_user_sigs
        PROPERTIES PASS_REGULAR_EXPRESSION "Received signal#8 and now calling abort()"
        WILL_FAIL TRUE
        )
