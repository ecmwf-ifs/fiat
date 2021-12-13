/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#if defined(__GNUC__)

/* pthread_attr_init() interception to reset guard region size 
   between thread stacks, by S.Saarinen, 30-sep-2016 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <time.h>
#include <pthread.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/syscall.h>

#undef RNDUP_DIV
#define RNDUP_DIV(i,n) (( (i) + (n) - 1 ) / (n))

#undef RNDUP
#define RNDUP(i,n) ( RNDUP_DIV(i,n) * (n))

#if defined(RTLD_NEXT)
#define PTR_LIBC RTLD_NEXT
#else
#define PTR_LIBC ((void*) -1L)
#endif

#ifndef SYS_gettid
#define SYS_gettid __NR_gettid
#endif

static pid_t gettid() {
#if defined(__APPLE__)
  uint64_t tid64;
  pthread_threadid_np(NULL, &tid64);
  pid_t tid = (pid_t)tid64;
#else
  pid_t tid = syscall(SYS_gettid);
#endif
  return tid;
}

static int GetMe()
{
  int me = -1; /* MPI task id >= 0 && <= NPES - 1 */
  /* Trying to figure out MPI task id since are potentially doing this *before* MPI_Init*() */
  char *env_procid = getenv("ALPS_APP_PE");
  if (!env_procid) env_procid = getenv("EC_FARM_ID");
  if (!env_procid) env_procid = getenv("PMI_RANK");
  if (!env_procid) env_procid = getenv("OMPI_COMM_WORLD_RANK");
  if (env_procid) me = atoi(env_procid);
  return me;
}

static int (*ptr_pthread_attr_init)(pthread_attr_t *attr) = NULL;
int pthread_attr_init(pthread_attr_t *attr)
{
  int rc;
  static int done = 0;
  FILE *fp = NULL;
  int me = GetMe();
  pid_t pid = getpid();
  pid_t tid = gettid();
  int master = (pid == tid) ? 1 : 0;
  if (!ptr_pthread_attr_init) {
    ptr_pthread_attr_init = (int (*)(pthread_attr_t *a))dlsym(PTR_LIBC, "pthread_attr_init");
    if (!ptr_pthread_attr_init) {
      fprintf(stderr,"***Error: Dynamic linking to pthread_attr_init() failed : errno = %d\n",errno);
      abort();
    }
    /* We intend to output only from MPI-task 0, master thread */
    if (!done && me == 0 && master) fp = stderr;
    done = 1;
  }
  rc = ptr_pthread_attr_init(attr);
  {
    char *env_gs = getenv("THREAD_GUARDSIZE");
    if (env_gs) {
      size_t pgsize = getpagesize();
      size_t guardsize = atoll(env_gs);
      if (strchr(env_gs,'G')) guardsize *= 1073741824; /* hence, in GiB */
      else if (strchr(env_gs,'M')) guardsize *= 1048576; /* hence, in MiB */
      else if (strchr(env_gs,'K')) guardsize *= 1024; /* hence, in KiB */
      guardsize = RNDUP(guardsize,pgsize);
      if (guardsize > pgsize) { /* Now we *do* bother */
        char *env_omp = getenv("OMP_STACKSIZE");
        size_t omp_stacksize = env_omp ? atoll(env_omp) : 0;
        size_t stacksize = 0;
        int iret = pthread_attr_getstacksize(attr,&stacksize);
#if 0
        if (fp) fprintf(fp,
          "[%s@%s:%d] [pid=%ld:tid=%ld]: Requesting guard region size "
          "between thread stacks : %lld bytes (%s PAGESIZE = %d)\n",
          __FUNCTION__,__FILE__,__LINE__,
          (long int)pid,(long int)tid,
          (long long int)guardsize,
          (guardsize > pgsize) ? ">" : "<=",
          pgsize);
#endif
        if (env_omp) {
          if (strchr(env_omp,'G')) omp_stacksize *= 1073741824; /* hence, in GiB */
          else if (strchr(env_omp,'M')) omp_stacksize *= 1048576; /* hence, in MiB */
          else if (strchr(env_omp,'K')) omp_stacksize *= 1024; /* hence, in KiB */
        }
        if (fp) fprintf(fp,
          "[%s@%s:%d] [pid=%ld:tid=%ld]: Stack size(s) : %lld bytes (def), %lld bytes (OMP) : [iret=%d]\n",
          __FUNCTION__,__FILE__,__LINE__,
          (long int)pid,(long int)tid,
          (long long int)stacksize,
          (long long int)omp_stacksize,
          iret);
        if (iret == 0 && omp_stacksize > guardsize) {
          iret = pthread_attr_setguardsize(attr,guardsize);
          (void) pthread_attr_getguardsize(attr,&guardsize);
          if (fp) fprintf(fp,
            "[%s@%s:%d] [pid=%ld:tid=%ld]: Guard region size now : %lld bytes : [iret=%d]\n",
            __FUNCTION__,__FILE__,__LINE__,
            (long int)pid,(long int)tid,
	          (long long int)guardsize,iret);
        }
      }
    }
  }
  if (fp) fflush(fp);
  return rc;
}

#if 0
/* Opting out for now */
static void MemInfoBeforeMain() __attribute__((constructor));
static void MemInfoBeforeMain()
{
  static int done = 0;
  int me = GetMe();
  if (!done && me == 0) {
    extern void meminfo_(const int *, const int *);
    const int kout = 0;
    const int kstep = -1;
    pid_t pid = getpid();
    pid_t tid = gettid();
    int master = (pid == tid) ? 1 : 0;
    if (me == 0 && master) meminfo_(&kout, &kstep); /* utilities/ec_meminfo.F90 */
    done = 1;
  }
}
#endif

#endif /* defined(__GNUC__) */
