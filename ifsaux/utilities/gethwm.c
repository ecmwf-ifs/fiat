#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if defined(CRAY)
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
long long int
gethwm()
{
  static int fd = -9999;
  static long long int heapbase = 0;
  long long int heapsize = 0;

  if (fd == -9999) {
    pstatus_t pstatus;
    char procfile[80];
    int pid = getpid();
    sprintf(procfile,"/proc/%d/status",pid);
    fd = open(procfile, O_RDONLY);
    if (read(fd, &pstatus, sizeof(pstatus)) == sizeof(pstatus)) {
      heapbase = (long long int)pstatus.pr_brkbase;
      close(fd);
      fd = 0;
    }
  }

  if (fd == 0 && heapbase > 0) {
    heapsize = (long long int)sbrk(0) - heapbase;
  }

  return heapsize;
}

#else
long long int
gethwm() 
{ 
  extern long long int getrss_();
  return getrss_();
}
#endif /* defined(__64BIT__) */

#else  /* non-RS6K */


long long int gethwm()
{
  long long int rc = (long long int)sbrk(0);
  return rc;
}

#endif

