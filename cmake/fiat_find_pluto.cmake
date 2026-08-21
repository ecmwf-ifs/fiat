# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

#[=======================================================================[.rst:
fiat_find_pluto
---------------

Locate the optional ``pluto`` dependency used by FIAT.

.. command:: fiat_find_pluto()

  This macro initialises ``HAVE_PLUTO`` to ``0`` and then resolves the
  optional ``pluto`` package according to the user configuration.

  If neither ``FIAT_ENABLE_PLUTO`` nor ``ENABLE_PLUTO`` is defined, the
  package is searched quietly. If either option is defined and enabled,
  ``pluto`` becomes mandatory and configuration stops with ``ecbuild_error()``
  when it cannot be found.

  On success, ``HAVE_PLUTO`` is set to ``1`` and status messages report the
  detected ``pluto`` version and installation directory.
#]=======================================================================]

macro(fiat_find_pluto)
  set(HAVE_PLUTO 0)
  if (NOT DEFINED FIAT_ENABLE_PLUTO AND NOT DEFINED ENABLE_PLUTO)
    find_package(pluto QUIET)
  elseif (DEFINED FIAT_ENABLE_PLUTO)
    if (FIAT_ENABLE_PLUTO)
      find_package(pluto)
      if (NOT pluto_FOUND)
        ecbuild_error("pluto not found, but FEATURE PLUTO is ON (FIAT_ENABLE_PLUTO=ON). Please install pluto or disable FEATURE PLUTO")
      endif()
    endif()
  elseif (DEFINED ENABLE_PLUTO)
    if (ENABLE_PLUTO)
      find_package(pluto)
      if (NOT pluto_FOUND)
        ecbuild_error("pluto not found, but FEATURE PLUTO is ON (ENABLE_PLUTO=ON). Please install pluto or disable FEATURE PLUTO")
      endif()
    endif()
  endif()
  if (pluto_FOUND)
    message(STATUS "fiat found pluto (version ${pluto_VERSION}): ${pluto_DIR})")
    message(STATUS "Feature PLUTO enabled")
    set( HAVE_PLUTO 1 )
  else()
    message(STATUS "Feature PLUTO was not enabled (also not requested) -- following packages weren't found: pluto")
  endif()
endmacro()

