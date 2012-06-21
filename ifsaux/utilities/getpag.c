#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if defined(CRAY)
#define getpag GETPAG
#elif defined(HPPA)
#else
#define getpag getpag_
#endif

#ifdef RS6K
#include <sys/resource.h>
long long int
getpag()
{
#if defined(__64BIT__)
  struct rusage64 r;
  long long int rc = getrusage64(RUSAGE_SELF, &r);
#else
  struct rusage r;
  long long int rc = getrusage(RUSAGE_SELF, &r);
#endif
  rc = (rc == 0) ? (long long int) r.ru_majflt : 0;
  return rc;
}

#else


long long int getpag()
{
  return 0L;
}

#endif

