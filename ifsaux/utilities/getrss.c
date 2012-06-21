#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef  long long int  ll_t;

#if defined(CRAY) && !defined(SV2)
#define getrss GETRSS
#elif defined(HPPA)
#else
#define getrss getrss_
#endif

#if defined(RS6K) || defined(LINUX) || defined(SGI)
#include <sys/resource.h>
ll_t
getrss()
{
  const ll_t scaler = 1024; /* in kilobytes */
#if defined(__64BIT__)
  struct rusage64 r;
  ll_t rc = getrusage64(RUSAGE_SELF, &r);
#else
  struct rusage r;
  ll_t rc = getrusage(RUSAGE_SELF, &r);
#endif
  rc = (rc == 0) ? (ll_t) r.ru_maxrss * scaler : 0;
  return rc;
}

#else

ll_t getrss()
{
  ll_t rc = (ll_t)((char *)sbrk(0) - (char *)0);
  return rc;
}

#endif

