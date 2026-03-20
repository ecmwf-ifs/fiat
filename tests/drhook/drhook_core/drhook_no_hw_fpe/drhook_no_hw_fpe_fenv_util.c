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

void silently_disable_all_fpes_() {
  fenv_t envp;
  feholdexcept(&envp);
}

#ifdef MOCK_HARDWARE_FPE_SUPPORT

int feenableexcept(int excepts) {
  return excepts;
}

#elif defined(MOCK_NO_HARDWARE_FPE_SUPPORT)

int feenableexcept(int excepts) {
  return -1;
}

#endif