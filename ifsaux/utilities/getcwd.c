#if defined(__uxppx__) || defined(VPP)

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* A quick getcwd() to get rid of original getcwd() 
   timeout (or something) problems in Fortran OPEN 

   Tested with command "vcc -c getcwd.c" */

char *
getcwd(char *buf, 
       int size)
{
  char *env = getenv("PWD");

  if (env) {
    if (buf && size > 0) {
      strncpy(buf,env,size);
    }
    else {
      buf = strdup(env);
    }
  }
  else {
    buf = NULL;
  }

  return buf;
}

#else
void dummy_getcwd() { }
#endif
