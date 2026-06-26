# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

#[=======================================================================[.rst:
fiat_find_fypp
--------------

Locate the ``fypp`` preprocessor used by FIAT.

.. command:: fiat_find_fypp()

    This macro preserves an existing ``FYPP`` setting when one is already
    available. Otherwise it resolves ``FYPP`` in the following order:

    #. ``${FETCHCONTENT_BASE_DIR}/fypp/bin``
    #. the normal system search path via ``find_program()``
    #. a local ``FetchContent`` checkout of ``https://github.com/aradi/fypp``

    When ``fypp`` is downloaded, the fetched script is patched in place to lower
    its minimum Python requirement from 3.7 to 3.6.

    On success, ``FYPP`` is set to the path of the executable script and progress
    is reported with ``ecbuild_info()`` messages.
#]=======================================================================]

macro(fiat_find_fypp)
    if (fckit_FOUND)
        ecbuild_info("fiat FOUND fypp via fckit: ${FYPP}}")
    endif()
    include(FetchContent)
    if (NOT FYPP)
        find_program( FYPP NAMES fypp PATHS ${FETCHCONTENT_BASE_DIR}/fypp/bin NO_DEFAULT_PATH )
        if (FYPP)
            ecbuild_info("fiat FOUND fypp in build-dir: ${FYPP}")
        endif()
    endif()
    if (NOT FYPP)
    find_program( FYPP NAMES fypp )
        if (FYPP)
            ecbuild_info("fiat FOUND fypp: ${FYPP}")
        endif()
    endif()
    if (NOT FYPP)
        set( fypp_SOURCE_DIR ${FETCHCONTENT_BASE_DIR}/fypp )
        set( FYPP ${fypp_SOURCE_DIR}/bin/fypp )
        ecbuild_info("fypp not found, downloading fypp in build-dir: ${FYPP}")
        cmake_policy(PUSH)
        if (POLICY CMP0174)
            cmake_policy(SET CMP0174 OLD)
        endif()
        FetchContent_Declare(
            fypp
            GIT_REPOSITORY https://github.com/aradi/fypp
            GIT_TAG fc255be56ee2b0ad5833ecb567a477db2cc61167 # 2026-03-03
            SOURCE_DIR ${fypp_SOURCE_DIR}
            CONFIGURE_COMMAND ""  # Disables the configure step (requires policy CMP0174 OLD)
            BUILD_COMMAND     ""  # Disables the build step (requires policy CMP0174 OLD)
            INSTALL_COMMAND   ""  # Disables the install step (requires policy CMP0174 OLD)
        )
        FetchContent_MakeAvailable(fypp)
        cmake_policy(POP)

        # PATCH: Adapt minimum required version of Python to 3.6, as 3.7 may not be default available, and 3.6 seems to work fine.
        file(READ ${FYPP} FILE_CONTENTS)
        string(REPLACE "MIN_PYTHON_VERSION = (3, 7)" "MIN_PYTHON_VERSION = (3, 6)" OUT_CONTENTS "${FILE_CONTENTS}")
        file(WRITE ${FYPP} "${OUT_CONTENTS}")
    endif()
    if (NOT FYPP)
        ecbuild_info("Could NOT find fypp")
    endif()
endmacro()
