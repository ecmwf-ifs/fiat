#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

extern void abor1_(const char msg[], int msglen);

#pragma weak abor1_

void batch_kill_()
{
#ifdef __INTEL_COMPILER
  {
    // Fixes (?) hangs Intel MPI
    char *env = getenv("SLURM_JOBID");
    if (env) {
      static char cmd[128] = "set -x; sleep 10; scancel --signal=TERM ";
      //static char cmd[128] = "set -x; sleep 10; scancel ";
      strcat(cmd,env);
      system(cmd);
    }
  }
#endif
}

void _brexit(int errcode)
{
  batch_kill_();
  _exit(errcode);
}

void tabort_()
{
  int ret = -1;
  const int sig = SIGABRT;
  static volatile sig_atomic_t irecur = 0;
  if (++irecur == 1) {
    ret = raise(sig); /* We get better DrHook & LinuxTrbk's with this than abort() aka SIGABRT */
    // abort(); -- essentially raise(SIGABRT) but with messier output (and may bypass DrHook)
    if (ret == 0) { // Means raise() was okay and tracebacks etc. DrHooks took place
      exit(128 + sig);
    }
  }
  // Still here ?? get the hell out of here ... now !!
  if (ret != 0) _brexit(128 + sig);
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
