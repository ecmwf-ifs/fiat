#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

extern void abor1_(const char msg[], int msglen);

#pragma weak abor1_

void abort_()
{
  if (abor1_) { // Call only if available
    static volatile int irecur = 0;
    if (++irecur == 1) {
      const char msg[] = "Fortran ABORT()";
      abor1_(msg,strlen(msg));
    }
  }
  {
    const int sig = SIGINT;
    raise(sig); /* We get better DrHook & LinuxTrbk's with this than abort() aka SIGABRT */
    // abort(); -- essentially raise(SIGABRT)
    exit(128 + sig);
  }
}

void _gfortran_abort()
{
  abort_();
}
