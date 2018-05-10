#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

extern void abor1_(const char msg[], int msglen);

#pragma weak abor1_

void tabort_()
{
  static volatile sig_atomic_t irecur = 0;
  if (++irecur == 1) {
    const int sig = SIGINT;
    raise(sig); /* We get better DrHook & LinuxTrbk's with this than abort() aka SIGABRT */
    // abort(); -- essentially raise(SIGABRT) but with messier output (and may bypass DrHook)
    exit(128 + sig);
  }
}

void abort_()
{
  if (abor1_) { // Call only if available
    static volatile sig_atomic_t irecur = 0;
    if (++irecur == 1) {
      const char msg[] = "Fortran ABORT()";
      abor1_(msg,strlen(msg));
    }
  }
  tabort_();
}

void _gfortran_abort()
{
  abort_();
}
