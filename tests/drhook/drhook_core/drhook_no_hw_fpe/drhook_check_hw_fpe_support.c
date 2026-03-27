/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <fenv.h>

#ifdef __APPLE__
/* Needed to pass compilation and pretend no hardware support */
int feenableexcept(int mask) {
  return -1;
}
#endif

int main() {
  return feenableexcept(FE_ALL_EXCEPT) == -1;
}
