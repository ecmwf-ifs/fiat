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

#if defined(__APPLE__) && defined(__arm64__)
/*
 * There is this implementation for Apple Silicon in DrHook.
 * We can't link fiat to make this available because it
 * won't have been built yet!
 */
static unsigned long long fenv_to_fpcr(unsigned int fenv_flag) {
  unsigned long long fpcr_flags = 0;
  if (fenv_flag & FE_INEXACT ) {
    fpcr_flags |= __fpcr_trap_inexact;
  }
  if (fenv_flag & FE_UNDERFLOW ) {
    fpcr_flags |= __fpcr_trap_underflow;
  }
  if (fenv_flag & FE_OVERFLOW ) {
    fpcr_flags |= __fpcr_trap_overflow;
  }
  if (fenv_flag & FE_DIVBYZERO ) {
    fpcr_flags |= __fpcr_trap_divbyzero;
  }
  if (fenv_flag & FE_INVALID ) {
    fpcr_flags |= __fpcr_trap_invalid;
  }
  if (fenv_flag & FE_FLUSHTOZERO ) {
    fpcr_flags |= __fpcr_flush_to_zero;
  }
  // Better to assume nothing and be explicit...
  if (fenv_flag == FE_ALL_EXCEPT ) {
    fpcr_flags |= __fpcr_trap_inexact & __fpcr_trap_underflow & __fpcr_trap_overflow &
      __fpcr_trap_divbyzero & __fpcr_trap_invalid & __fpcr_flush_to_zero;
  }
  return fpcr_flags;
}

static unsigned int fpcr_to_fenv(unsigned long long fpcr_flags) {
  unsigned int fenv_flag = 0;
  if (fpcr_flags & __fpcr_trap_inexact) {
    fenv_flag |= FE_INEXACT;
  }
  if (fpcr_flags & __fpcr_trap_underflow) {
    fenv_flag |= FE_UNDERFLOW;
  }
  if (fpcr_flags & __fpcr_trap_overflow) {
    fenv_flag |= FE_OVERFLOW;
  }
  if (fpcr_flags & __fpcr_trap_divbyzero) {
    fenv_flag |= FE_DIVBYZERO;
  }
  if (fpcr_flags & __fpcr_trap_invalid) {
    fenv_flag |= FE_INVALID;
  }
  if (fpcr_flags & __fpcr_flush_to_zero) {
    fenv_flag |= FE_FLUSHTOZERO;
  }
  unsigned long long all_fpcr_flags = __fpcr_trap_inexact & __fpcr_trap_underflow &
    __fpcr_trap_overflow & __fpcr_trap_divbyzero & __fpcr_trap_invalid & __fpcr_flush_to_zero;
  if (fenv_flag == all_fpcr_flags ) {
    return FE_ALL_EXCEPT;
  }
  return fenv_flag;
}

__attribute__((weak)) int feenableexcept (int excepts) {
  fenv_t fenv;
  unsigned int new_excepts = excepts & FE_ALL_EXCEPT;
  unsigned int old_excepts; // previous masks

  if (fegetenv(&fenv)) {
    return -1;
  }

  old_excepts = fpcr_to_fenv(fenv.__fpcr) & FE_ALL_EXCEPT;

  fenv.__fpcr |= fenv_to_fpcr(new_excepts);

  return fesetenv(&fenv) ? -1 : old_excepts;
}
#elif defined(__APPLE__)
/* Needed to pass compilation and pretend no hardware support */
int feenableexcept(int mask) {
  return -1;
}
#endif

int main() {
  return feenableexcept(FE_INVALID & FE_DIVBYZERO & FE_OVERFLOW) == -1;
}
