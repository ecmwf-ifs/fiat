# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# ----------------------------------------------------------------------------------------
# Tests: fiat_test_gstats_csv_output (only gstats print csv so far)
ecbuild_add_test(
    TARGET  fiat_test_gstats_csv_output
    SOURCES gstats_csv_basic.F90
    LIBS    ${fiatlib}
    LINKER_LANGUAGE Fortran
    )
# ----------------------------------------------------------------------------------------