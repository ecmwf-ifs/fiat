# (C) Copyright 2026- ECMWF.
# (C) Copyright 2026- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

# ----------------------------------------------------------------------------------------
# fiat_test_mpl_allocator_tests
#   test that MPL allocator functionality works

ecbuild_add_executable(
    TARGET fiat_test_mpl_allocator
    SOURCES test_mpl_allocator.F90
    LIBS    ${fiatlib}
    LINKER_LANGUAGE Fortran
    NOINSTALL
)

ecbuild_add_test(
    TARGET fiat_test_mpl_allocator_no_init
    COMMAND fiat_test_mpl_allocator
    ARGS fiat_test_mpl_allocator_no_init
    ENVIRONMENT "MPL=0"
)

foreach( fiat_test_mpl_allocator_test
                fiat_test_mpl_allocator_init
                fiat_test_mpl_allocator_init_size
                fiat_test_mpl_allocator_init_and_reserve )
    if (HAVE_PLUTO)
        set( EXPECTED_MPL_RESOURCE "MPI_POOL" )
    else ()
        set( EXPECTED_MPL_RESOURCE "BUDDY_ALLOC" )
    endif()

    ecbuild_add_test(
        TARGET ${fiat_test_mpl_allocator_test}
        COMMAND fiat_test_mpl_allocator
        ARGS ${fiat_test_mpl_allocator_test}
        ENVIRONMENT "MPL=0" "EXPECTED_MPL_RESOURCE=${EXPECTED_MPL_RESOURCE}"
    )

    ecbuild_add_test(
        TARGET ${fiat_test_mpl_allocator_test}_with_changed_default_resource
        COMMAND fiat_test_mpl_allocator
        ARGS ${fiat_test_mpl_allocator_test}
        ENVIRONMENT "MPL=0" "MPL_RESOURCE=FORTRAN" "EXPECTED_MPL_RESOURCE=FORTRAN"
    )

    unset( RESOURCES )
    list (APPEND RESOURCES FORTRAN BUDDY_ALLOC )
    if (HAVE_PLUTO)
      list(APPEND RESOURCES MPI_POOL)
    endif()

    foreach( resource ${RESOURCES} )
        ecbuild_add_test(
            TARGET ${fiat_test_mpl_allocator_test}_${resource}
            COMMAND fiat_test_mpl_allocator
            ARGS ${fiat_test_mpl_allocator_test}_${resource}
            ENVIRONMENT "MPL=0"
        )
    endforeach()
endforeach()
