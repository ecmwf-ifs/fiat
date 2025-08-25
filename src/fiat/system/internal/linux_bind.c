/*
 * (C) Copyright 2005- ECMWF.
 * (C) Copyright 2005- Meteo France.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#if defined(LINUX) && !defined(_CRAYC)

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sched.h>

#include "oml.h"

static char * getcpumask (char *buffer, size_t size)
{
  cpu_set_t mask;
  unsigned int ncpu;
  unsigned int icpu;

  ncpu = sysconf (_SC_NPROCESSORS_CONF);

  sched_getaffinity (0, sizeof (mask), &mask);

  for (icpu = 0; icpu < ncpu; icpu++)
    buffer[icpu] = CPU_ISSET (icpu, &mask) ? '1' : '0';

  buffer[ncpu] = '\0';

  return buffer;
}

static void function_linux_bind_dump_parallel( void* args ) {
  // This function must be called within an OMP PARALLEL region

  // Unpack args
  FILE* fp = (FILE*)args;

  char buffer[1048576]; /* 1 megabyte */
  int nomp = oml_get_max_threads();
  int iomp = oml_get_thread_num();
  for (int i = 0; i < nomp; i++) {
    if (i == iomp) {
      // OMP CRITICAL REGION implemented with locks
      oml_set_lock();
      fprintf (fp, "\n                                                    mask = %s iomp = %2d",
               getcpumask (buffer, sizeof (buffer)), iomp);
      oml_unset_lock();
    }
    oml_barrier();
  }
  oml_barrier();
}

void linux_bind_dump_ (int * prank, int * psize)
{
  const int rank = *prank;
  const int size = *psize;
  FILE* fp = NULL;
  char f[256];
  char host[255];
  char buffer[1024];
  const int nomp = oml_get_max_threads();
  const unsigned int ncpu = sysconf (_SC_NPROCESSORS_CONF);

  sprintf (f, "linux_bind.%6.6d.txt", rank);
  fp = fopen (f, "w");

  if (gethostname (host, 255) != 0) {
       strcpy (host, "unknown");
  }

  fprintf (fp, " rank = %6d", rank);
  fprintf (fp, " host = %9s", host);
  fprintf (fp, " ncpu = %2d", ncpu);
  fprintf (fp, " nomp = %2d", nomp);
  fprintf (fp, " mask = %s", getcpumask (buffer, sizeof (buffer)));

  oml_init_lock();
  oml_run_parallel (function_linux_bind_dump_parallel, fp);
  oml_destroy_lock();

  fprintf (fp, "\n");
  fclose (fp);
}

#define LINUX_BIND_TXT "linux_bind.txt"

typedef struct {
  const char* linux_bind_txt;
  const char* buf;
  const int rank;
} function_linux_bind_parallel_args_t;

static void function_linux_bind_parallel(void* args) {
  // This function must be called within an OMP PARALLEL region

  // Unpack args
  function_linux_bind_parallel_args_t* function_args = (function_linux_bind_parallel_args_t*)args;
  const char* linux_bind_txt = function_args->linux_bind_txt;
  const char* buf = function_args->buf;
  const int rank = function_args->rank;
  const int iomp = oml_get_thread_num();

  const char* c = buf;
  // Move c to position for this omp thread
  for (int jomp = 0; jomp < iomp; jomp++) {
    while (*c && isdigit (*c)) {
      c++;
    }
    while (*c && (! isdigit (*c))) {
      c++;
    }
    if (*c == '\0') {
      fprintf (stderr, "Unexpected end of line while reading '%s' on rank %d, thread %d (linux_bind.c:%d)\n", linux_bind_txt, rank, iomp, __LINE__);
      return;
    }
  }

  cpu_set_t mask;
  CPU_ZERO (&mask);

  for (int icpu = 0; isdigit (*c); icpu++, c++) {
    if (*c != '0') {
      CPU_SET (icpu, &mask);
    }
  }

  sched_setaffinity (0, sizeof (mask), &mask);
}

void linux_bind_ (int * prank, int * psize)
{
  const int rank = *prank;
  const int size = *psize;
  const char* linux_bind_txt = getenv ("EC_LINUX_BIND");

  if (linux_bind_txt == NULL) {
    linux_bind_txt = LINUX_BIND_TXT;
  }

  FILE* fp = fopen (linux_bind_txt, "r");

  if (fp == NULL) {
      // Willem Deconinck: Commented out as this pollutes logs
      // fprintf (stderr, "`%s' was not found\n", EC_LINUX_BIND);
      return;
  }

  size_t len = 1024;
  char* buf = (char*) malloc( len );

  for (int i = 0; i < rank+1; i++) {
    if (getline (&buf, &len, fp) == -1) {
      fprintf (stderr, "Unexpected end of file while reading '%s' on rank %d (linux_bind.c:%d)\n", linux_bind_txt, rank, __LINE__);
      fclose (fp);
      free (buf);
      return;
    }
  }

  function_linux_bind_parallel_args_t args = {.linux_bind_txt=linux_bind_txt, .buf=buf, .rank=rank};
  oml_run_parallel (function_linux_bind_parallel, &args);

  fclose (fp);
  free (buf);
}

#else

void linux_bind_ () { }
void linux_bind_dump_ () { }

#endif

