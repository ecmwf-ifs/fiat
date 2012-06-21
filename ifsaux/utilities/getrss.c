#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if defined(CRAY)
#define getrss GETRSS
#elif defined(HPPA)
#else
#define getrss getrss_
#endif

#ifdef RS6K
#include <sys/resource.h>
long long int
getrss()
{
  const long long int scaler = 1024; /* in kilobytes */
#if defined(__64BIT__)
  struct rusage64 r;
  long long int rc = getrusage64(RUSAGE_SELF, &r);
#else
  struct rusage r;
  long long int rc = getrusage(RUSAGE_SELF, &r);
#endif
  rc = (rc == 0) ? (long long int) r.ru_maxrss * scaler : 0;
  return rc;
}

#else


long long int getrss()
{
  long long int rc = (long long int)sbrk(0);
  return rc;
}

#endif

