/*
 * (C) Copyright 2021- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <utmpx.h>
#include <sched.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <limits.h>

#if !defined(HOST_NAME_MAX)
#define HOST_NAME_MAX 512
#endif
#define HOSTNAME_SIZE HOST_NAME_MAX+1

#ifndef NOMPI
#include <mpi.h>
#else
#warning Not compiled with MPI support
#endif

#ifdef _OPENMP
#include <omp.h>
#else
#warning Not compiled with OpenMP support
int omp_get_max_threads(void) {
  return 1;
}
int omp_get_thread_num(void) {
  return 0;
}
#endif


#if defined(__APPLE__)
  // These variables and functions are Linux specific
  typedef struct {} cpu_set_t;
  static int CPU_ISSET(int cpu, const cpu_set_t* cpu_set) { return 0; }
  static void CPU_ZERO(cpu_set_t* cpu_set) {}
  static int sched_getaffinity(pid_t pid, size_t cpu_set_size, cpu_set_t* cpu_set) { return 0; }
#endif


void printbind( int rank, int nthreads, int maxthreads, int* counts, int* procs, const char* hostname ) {
  /* Print out the binding for one rank */

  // "procs" represents multidimensional array with extents {nproc, maxthreads}
#define PROCS(P,T) procs[(P) * maxthreads + (T)]
#define this_proc PROCS(iproc,thread)
#define next_proc PROCS(iproc+1,thread)

  printf("Rank %4d on %16s has %3d threads on cores: ",rank, hostname, nthreads);

  for(int thread=0;thread<nthreads; thread++) {
    int nprocs = counts[thread];
    int in_range=0;
    int iproc=0;
    printf("(%d",this_proc);
    if(nprocs>1) {
      if(next_proc == this_proc+1) {
         in_range=1;
         printf("-");
      }
    }
    for(iproc=1; iproc<nprocs-1; iproc++) {
      if(in_range) {
        if(next_proc != this_proc+1) {
          printf("%d",this_proc);
          in_range=0;
        }
      }
      else {
        printf(",%d",this_proc);
        if(next_proc == this_proc+1) {
          in_range=1;
          printf("-");
        }
      }
    }
    if(nprocs>1) {
      if(!in_range) {
        printf(",");
      }
      printf("%d",this_proc);
    }
    printf(")");
  }
  printf("\n");
  return;
}

int main( int argc , char **argv )
{
  int mpi_size = 1;
  int mpi_rank = 0;
  int nthreads = omp_get_max_threads();
  int maxthreads = nthreads;
  int nprocs = sysconf(_SC_NPROCESSORS_ONLN);
  int maxprocs = nprocs;

#ifndef NOMPI
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
  MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
  MPI_Allreduce(&nthreads, &maxthreads, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
  MPI_Allreduce(&nprocs, &maxprocs, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
#endif

  int* counts  = calloc(maxthreads, sizeof(int));
  int* myprocs = calloc(maxprocs*maxthreads, sizeof(int));

  for(int i=0; i<nthreads; i++) {
      counts[i] = 0;
  }
  for(int i=0; i<maxprocs*nthreads; i++) {
      myprocs[i] = -1;
  }

  char hostname[HOSTNAME_SIZE];
  gethostname(hostname, HOSTNAME_SIZE);

  /* Create list of procs for this rank & thread */
#ifdef _OPENMP
  #pragma omp parallel default(none) shared(myprocs, counts, maxthreads, nprocs)
#endif
  {
    /* Each thread creates a list of cores that it is bound to */
    int tid=omp_get_thread_num();

#define MYPROCS(proc,thread) myprocs[proc * maxthreads + thread]

    /* Initialise */
    cpu_set_t myset;
    CPU_ZERO(&myset);

    sched_getaffinity(0, sizeof(cpu_set_t), &myset);
    for(int i=0; i<nprocs; i++) {
      if(CPU_ISSET(i, &myset) == 1) {
        MYPROCS(counts[tid],tid) = i;
        counts[tid]++;
      }
    }
  }

#ifndef NOMPI
  if(mpi_rank==0) {
    /* Rank 0 prints its own information then receives and prints the binding of every other process in turn */
    MPI_Status status;
    int* rprocs  = calloc(maxprocs * maxthreads, sizeof(int));
    int* rcounts = calloc(maxthreads, sizeof(int));
    int rnthreads;
    char rhostname[HOSTNAME_SIZE];

    printbind( mpi_rank, nthreads, maxthreads, counts, myprocs, hostname );

    for(int i=1; i<mpi_size; i++) {
      MPI_Recv( &rnthreads, 1, MPI_INT, i, 1, MPI_COMM_WORLD, &status );
      MPI_Recv( rcounts, maxthreads, MPI_INT, i, 2, MPI_COMM_WORLD, &status );
      MPI_Recv( rprocs, maxprocs*maxthreads, MPI_INT, i, 3, MPI_COMM_WORLD, &status );
      MPI_Recv( rhostname, HOSTNAME_SIZE, MPI_CHAR, i, 4, MPI_COMM_WORLD, &status );
      printbind ( i, rnthreads, maxthreads, rcounts, rprocs, rhostname );
    }

    printf("\n");
    free(rprocs);
    free(rcounts);
  }
  else {
    /* All other ranks send their binding information to rank 0 */
    MPI_Send( &nthreads, 1, MPI_INT, 0, 1, MPI_COMM_WORLD );
    MPI_Send( counts, maxthreads, MPI_INT, 0, 2, MPI_COMM_WORLD );
    MPI_Send( myprocs, maxprocs*maxthreads, MPI_INT, 0, 3, MPI_COMM_WORLD );
    MPI_Send( hostname, HOSTNAME_SIZE, MPI_CHAR, 0, 4, MPI_COMM_WORLD );
  }

  MPI_Finalize();

#else
  printbind( 0, nthreads, maxthreads, counts, myprocs, hostname );
#endif

  free(myprocs);
  free(counts);
  return 0;
}

