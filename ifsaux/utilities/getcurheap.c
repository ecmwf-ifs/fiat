#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

typedef  long long int  ll_t;

static ll_t maxcurheap = 0;

#if defined(CRAY) && !defined(SV2)
#define getcurheap GETCURHEAP
#define getmaxcurheap GETMAXCURHEAP
#elif defined(HPPA)
#else
#define getcurheap getcurheap_
#define getmaxcurheap getmaxcurheap_
#endif

#ifdef RS6K

#if defined(__64BIT__)
/* Assume AIX >= 5.1 with 64-bit addressing */

#if defined(INTERCEPT_ALLOC)
#include <pthread.h>
pthread_mutex_t getcurheap_lock = PTHREAD_MUTEX_INITIALIZER;

static ll_t curalloc = 0;

static int profile_heap = -1; /* Profiling:  -1 = UNDEF, 0 = OFF, 1 = ON */

#define NPROFILE 9 /* byte ranges: 10**1 .. 10^9 */
static ll_t malloc_hits[NPROFILE+1]; /* +1 for ranges >= 10^9 */
static ll_t free_hits[NPROFILE+1];
static ll_t alloc_amount[NPROFILE+1];

#define WORDLEN   sizeof(ll_t)
#define RNDUP(i,n) (( ( (i) + (n) - 1 ) / (n) ) * (n))

static void
Check_curalloc() /* Normally not called */
{
  const ll_t big = (ll_t) 1000000000000L; /* 1,000,000 million bytes */
  if (curalloc < 0 || curalloc > big) {
    extern void xl__trbk_();
    fprintf(stderr,"Check_curalloc(): curalloc has gone crazy => %lld\n",curalloc);
    xl__trbk_();
    raise(SIGABRT);
  }
}

static void
Profile_heap_put(ll_t size, int is_malloc)
{
  if (profile_heap == -1) { /* First time */
    char *env = getenv("EC_PROFILE_HEAP");
    if (env) profile_heap = atoi(env);
    if (profile_heap != 0) profile_heap = 1; /* OFF by export EC_PROFILE_HEAP=0 */
  }
  if (profile_heap == 1) {
    int j;
    ll_t n = 1; /* initial byte range */
    ll_t *p = is_malloc ? malloc_hits : free_hits;
    for (j=0; j<NPROFILE; j++) { 
      n *= 10; /* increment byte range by 10X */
      /* BTW: Don't want log10() overhead here !! */
      if (size < n) { /* i.e. size < pow(10,j+1) */
	alloc_amount[j] += is_malloc ? size : -size;
	p[j]++;
	return;
      }
    }
    j = NPROFILE;
    alloc_amount[j] += is_malloc ? size : -size;
    p[j]++;
  } /* if (profile_heap == 1) */
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Icase,
		  int *nret)
     /* Fortran callable */
{
  int icase = *Icase;
  int nval = *Nval;
  int j;
  if (nval < 0) nval = 0;
  if (nval > NPROFILE+1) nval = NPROFILE+1;
  if (icase == 0) { /* free() hits */
    for (j=0; j<nval; j++) val[j] = free_hits[j];
  }
  else if (icase == 1) { /* malloc() hits */
    for (j=0; j<nval; j++) val[j] = malloc_hits[j];
  }
  else if (icase == 2) { /* outstanding allocs (malloc minus free) */
    for (j=0; j<nval; j++) val[j] = malloc_hits[j] - free_hits[j];
  }
  else if (icase == 3) { /* allocation amount left per range; in bytes */
    for (j=0; j<nval; j++) val[j] = alloc_amount[j];
  }
  else if (icase == 4) { /* average allocation chunk left per range; in bytes */
    for (j=0; j<nval; j++) {
      ll_t tmp = malloc_hits[j] - free_hits[j];
      val[j] = (tmp > 0) ? RNDUP(alloc_amount[j],tmp)/tmp : 0;
    }
  }
  else {
    nval = 0;
  }
  *nret = nval;
}

void __free(void *vptr)
{
  if (vptr) {
    ll_t *p = vptr;
    ll_t size = *--p;
    free(p);
    pthread_mutex_lock(&getcurheap_lock);
    curalloc -= size;
    /* Check_curalloc(); */
    Profile_heap_put(size, 0);
    pthread_mutex_unlock(&getcurheap_lock);
  }
}

void *__malloc(ll_t size)
{
  double *d = NULL;
  void *vptr = NULL;
  ll_t adjsize = size;
  if (adjsize < 0) adjsize = 0;
  adjsize = RNDUP(adjsize,WORDLEN);
  d = (double *)malloc(WORDLEN + adjsize);
  vptr = d;
  if (vptr) {
    ll_t *p = vptr;
    extern ll_t getstk_();
    (void) getstk_(); /* to gather near up to date stack statistics */
    *p++ = adjsize;
    pthread_mutex_lock(&getcurheap_lock);
    curalloc += adjsize;
    if (curalloc > maxcurheap) maxcurheap = curalloc;
    /* Check_curalloc(); */
    Profile_heap_put(adjsize, 1);
    pthread_mutex_unlock(&getcurheap_lock);
    vptr = p;
  }
  else {
    extern void xl__trbk_();
    pthread_mutex_lock(&getcurheap_lock);
    fprintf(stderr,
	    "__malloc(size=%lld => adjsize=%lld bytes) failed in file=%s, line=%d\n",
	    size, adjsize, __FILE__, __LINE__);
    xl__trbk_(); /* Oops !! */
    raise(SIGABRT);
    pthread_mutex_unlock(&getcurheap_lock);
    exit(1); /* Just in case, but shouldn't end up here at all */
  }
  return vptr;
}

void *__calloc(ll_t nelem, ll_t elsize)
{
  ll_t totbytes = nelem * elsize;
  void *p = __malloc(totbytes);
  if (p) memset(p, 0, totbytes);
  return p;
}

void *__realloc(void *vptr, ll_t size)
{
  ll_t *pnew = NULL;
  if (vptr) {
    ll_t *p = vptr;
    ll_t oldsize = p[-1];
    if (oldsize < size) {
      pnew = __malloc(size);
      if (pnew) {
	memcpy(pnew, p, oldsize);
	__free(p);
      }
    }
    else { /* the old allocation size was already sufficient */
      pnew = p;
    }
  }
  else { /* Revert to malloc() */
    pnew = __malloc(size);
  }
  return pnew;
}

char *__strdup(const char *s)
{
  ll_t totbytes = sizeof(*s) * (strlen(s) + 1);
  char *p = __malloc(totbytes);
  if (p) memcpy(p,s,totbytes);
  return p;
}

#else

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif /* defined(INTERCEPT_ALLOC) */

ll_t
getcurheap()
{
#if defined(INTERCEPT_ALLOC)
  ll_t curvalue;
  pthread_mutex_lock(&getcurheap_lock);
  /* Check_curalloc(); */
  curvalue = curalloc;
  pthread_mutex_unlock(&getcurheap_lock);
  return curvalue;
#else
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
#endif
}


#else /* non-defined(__64BIT__) [but still RS6K] */

ll_t
getcurheap() 
{ 
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif /* defined(__64BIT__) */

#else  /* non-RS6K */

ll_t getcurheap()
{
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif

/* Maximum current (virtual mem) allocation encountered */

ll_t
getmaxcurheap()
{
  return maxcurheap;
}
