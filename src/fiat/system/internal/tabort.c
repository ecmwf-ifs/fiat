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

extern void abor1_(const char msg[], int msglen);

#pragma weak abor1_

// Forward declarations
void LinuxTraceBack(const char *prefix, const char *timestr, void *sigcontextptr);
void ec_microsleep(int usecs); // from ec_env.c
void fortran_mpi_abort(int rc);

void tabort_()
{
  const int sig = SIGABRT;
  int rc = 128 + sig;
  static volatile sig_atomic_t irecur = 0;
  if (++irecur == 1) { // only one thread per task ever gets in here
    // Only the fastest MPI task calls LinuxTraceBack -- avoids messy outputs
    int nfirst = 0;
    const char tabort_lockfile[] = "tabort_lock";
    int fd = open(tabort_lockfile,O_CREAT|O_TRUNC|O_EXCL,S_IRUSR|S_IWUSR);
    if (fd >= 0) {
      close(fd);
      nfirst = 1;
    }
    if (nfirst) {
      drhook_calltree();
      LinuxTraceBack(NULL,NULL,NULL);
    }
    else {
      const int usecs = 100 * 1000000; // 100M micro-seconds i.e. 100 secs
      ec_sleep(usecs);
    }
    fortran_mpi_abort(rc); // calls MPI_ABORT with MPI_COMM_WORLD
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
