/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "drhook.h"
#include "mpl.h"

extern void ec_microsleep(int usecs); // from ec_env.c

extern void abor1_(const char msg[], int msglen);

#pragma weak abor1_

#if 0
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
#endif

#if 0
void _brexit(int errcode)
{
  batch_kill_();
  _exit(errcode);
}
#endif

// Forward declarations
void LinuxTraceBack(const char *prefix, const char *timestr, void *sigcontextptr);
void fortran_mpi_abort(int rc);

void tabort_()
{
  const int sig = SIGABRT;
  int rc = 128 + sig;
  static volatile sig_atomic_t irecur = 0;
  if (++irecur == 1) { // only one thread per task ever gets in here
#if 1
    drhook_calltree();
    {
      // Only the fastest MPI task calls LinuxTraceBack -- avoids messy outputs
      int nfirst = 0;
      const char tabort_lockfile[] = "tabort_lock";
      int fd = open(tabort_lockfile,O_CREAT|O_TRUNC|O_EXCL,S_IRUSR|S_IWUSR);
      if (fd >= 0) {
        close(fd);
        nfirst = 1;
      }
      if (nfirst) {
	LinuxTraceBack(NULL,NULL,NULL);
      }
      else {
	const int usecs = 10 * 1000000; // micro-seconds
	ec_sleep(usecs);
      }
    }
    fortran_mpi_abort(rc); // see ifsaux/parallel/cmpl_binding.F90 : calls MPI_ABORT with MPI_COMM_WORLD
#else
    ret = raise(sig); /* We get better DrHook & LinuxTrbk's with this than abort() aka SIGABRT */
    // abort(); -- essentially raise(SIGABRT) but with messier output (and may bypass DrHook)
    if (ret == 0) { // Means raise() was okay and tracebacks etc. DrHooks took place
      exit(128 + sig);
    }
#endif
  }
  // Still here ?? get the hell out of here ... now !!
  _exit(rc);
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
