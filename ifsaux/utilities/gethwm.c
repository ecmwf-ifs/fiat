#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef  long long int  ll_t;

#if defined(CRAY) && !defined(SV2)
#define gethwm GETHWM
#elif defined(HPPA)
#else
#define gethwm gethwm_
#endif

#ifdef RS6K

#if defined(__64BIT__)
/* Assume AIX >= 5.1 with 64-bit addressing */
#include <fcntl.h>
#include <sys/procfs.h>
ll_t
gethwm()
{
  static int fd = -9999;
  static char *heapbase = NULL;
  ll_t heapsize = 0;

  if (fd == -9999) {
    pstatus_t pstatus;
    char procfile[80];
    int pid = getpid();
    sprintf(procfile,"/proc/%d/status",pid);
    fd = open(procfile, O_RDONLY);
    if (read(fd, &pstatus, sizeof(pstatus)) == sizeof(pstatus)) {
      heapbase = (char *)pstatus.pr_brkbase;
      close(fd);
      fd = 0;
    }
  }

  if (fd == 0 && heapbase != NULL) {
    heapsize = (ll_t)((char *)sbrk(0) - heapbase);
  }

  return heapsize;
}

#else
ll_t
gethwm() 
{ 
  extern ll_t getrss_();
  return getrss_();
}
#endif /* defined(__64BIT__) */

#else  /* non-RS6K */


ll_t gethwm()
{
  ll_t rc = (ll_t)((char *)sbrk(0) - (char *)0);
  return rc;
}

#endif

#if defined(SV2)
int getpid_()
{
  return getpid();
}

unsigned int sleep_(unsigned int seconds)
{
  return sleep(seconds);
}

#endif
