/*
 * Philippe Marguinaud: Feb-2013: Bind threads to cores
 */

#ifdef LINUX

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <mpi.h>

#include <sched.h>

static char * getcpumask (char *buffer, size_t size)
{
  cpu_set_t mask;
  unsigned int ncpu;
  int icpu;
  
  ncpu = sysconf (_SC_NPROCESSORS_CONF);
  
  sched_getaffinity (0, sizeof (mask), &mask);

  for (icpu = 0; icpu < ncpu; icpu++) 
    buffer[icpu] = CPU_ISSET (icpu, &mask) ? '1' : '0';

  buffer[ncpu] = '\0';

  return buffer;
}

void linux_bind_dump_ ()
{
  int rank;
  int size;
  int icpu;
  unsigned int ncpu;
  FILE * fp = NULL;
  char f[256];
  char host[255];
  int nomp = omp_get_max_threads ();

  ncpu = sysconf (_SC_NPROCESSORS_CONF);

  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  MPI_Comm_size (MPI_COMM_WORLD, &size);

  sprintf (f, "linux_bind.%6.6d.txt", rank);
  fp = fopen (f, "w");

  if (gethostname (host, 255) != 0)
       strcpy (host, "unknown");

  fprintf (fp, " rank = %6d", rank);
  fprintf (fp, " host = %9s", host);
  fprintf (fp, " ncpu = %2d", ncpu);
  fprintf (fp, " nomp = %2d", nomp);

  {
    char buffer[1024];
    fprintf (fp, " mask = %s", getcpumask (buffer, sizeof (buffer)));
  }

#pragma omp parallel 
  {
    char buffer[1024];
    int iomp = omp_get_thread_num ();
    int i;
    for (i = 0; i < nomp; i++)
      {
        if (i == iomp)
          {
#pragma omp critical
            fprintf (fp, "\n                                                    mask = %s iomp = %2d", 
                     getcpumask (buffer, sizeof (buffer)), iomp);
          }
#pragma omp barrier
      }
#pragma omp barrier
  }

  fprintf (fp, "\n");

  fclose (fp);

}

#define LINUX_BIND_TXT "linux_bind.txt"

void linux_bind_ ()
{
  FILE * fp = fopen (LINUX_BIND_TXT, "r");
  int size, rank;
  int i;
  size_t len  = 256;
  char * buf = (char*)malloc (len);

  if (fp == NULL)
    {
      goto end;
    }

  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  MPI_Comm_size (MPI_COMM_WORLD, &size);

  for (i = 0; i < rank+1; i++)
    {
      if (getline (&buf, &len, fp) == -1)
        {
          fprintf (stderr, "Unexpected EOF while reading `" LINUX_BIND_TXT "'\n");
          goto end;
        }
    }

#pragma omp parallel 
  {
    char * c;
    cpu_set_t mask;
    int iomp = omp_get_thread_num ();
    int jomp, icpu;

    for (jomp = 0, c = buf; jomp < iomp; jomp++)
      {
        while (*c && isdigit (*c))
          c++;
        while (*c && (! isdigit (*c)))
          c++;
        if (*c == '\0')
          {
            fprintf (stderr, "Unexpected end of line while reading `" LINUX_BIND_TXT "'\n");
            goto end_parallel;
          }
      }

    CPU_ZERO (&mask);

    for (icpu = 0; isdigit (*c); icpu++, c++)
      if (*c != '0')
        CPU_SET (icpu, &mask);
     
    sched_setaffinity (0, sizeof (mask), &mask);

end_parallel:

    c = NULL;

  }

end:

  if (fp != NULL)
    fclose (fp);

  free (buf);
}

#else

void linux_bind_ () { }
void linux_bind_dump_ () { }

#endif
