/*
 * Fortran wrappers for opfla_perfmon
 */

#ifdef PAPI

#include <stdlib.h>



#define BIND_FTN(upper, lower, su, du, f, sign, params)		\
	void upper sign { f params; }				\
	void lower sign { f params; }				\
	void su sign { f params; }				\
	void du sign { f params; }

typedef int ftn_int;

static void
mpi_init_f(int *ierr)
{
	int argc = 0;
	char **argv = NULL;

	*ierr = __wrap_MPI_Init(&argc, &argv);
}

static void
mpi_init_thread_f(int *ierr)
{
	int argc = 0;
	char **argv = NULL;
	int required = 0;
	int provided = 0;

	*ierr = __wrap_MPI_Init_thread(&argc, &argv, required, &provided);
}

static void
mpi_finalize_f(int *ierr)
{
	*ierr = __wrap_MPI_Finalize();
}

BIND_FTN(__wrap_MPI_INIT,
         __wrap_mpi_init,
         __wrap_mpi_init_,
         __wrap_mpi_init__,
         mpi_init_f,
         (ftn_int *ierr), (ierr))

BIND_FTN(__wrap_MPI_INIT_THREAD,
         __wrap_mpi_init_thread,
         __wrap_mpi_init_thread_,
         __wrap_mpi_init_thread__,
         mpi_init_thread_f,
         (ftn_int *ierr), (ierr))

BIND_FTN(__wrap_MPI_FINALIZE,
         __wrap_mpi_finalize,
         __wrap_mpi_finalize_,
         __wrap_mpi_finalize__,
         mpi_finalize_f,
         (ftn_int *ierr), (ierr))

#else

void dummy_Wrap_FTN() { }

#endif
