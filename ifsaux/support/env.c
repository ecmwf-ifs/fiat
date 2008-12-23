
/* env.c */

/* Implement Fortran-callable ec_getenv and ec_putenv,
   since not all environments have getenv & putenv,
   but Unix/C library always have them */

/* Author: Sami Saarinen, ECMWF, 15-Mar-2006 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include "raise.h"

extern char **environ; /* Global Unix var */
static int numenv = 0;

void
ec_numenv_(int *n)
{ /* Returns the number of environment variables currently active */
  int j=0;
  if (environ) {
    for (; environ[j]; j++) { }
  }
  if (n) *n = j;
  numenv = j; /* Not thread-safe */
}


void
ec_numenv(int *n)
{
  ec_numenv_(n);
}


void
ec_overwrite_env_(int *do_overwrite)
{
  if (do_overwrite) {
    char *env = getenv("EC_OVERWRITE_ENV");
    if (env) {
      *do_overwrite = atoi(env);
    }
    else {
      *do_overwrite = 0;
    }
  }
}


void
ec_overwrite_env(int *do_overwrite)
{
  ec_overwrite_env_(do_overwrite);
}


void
ec_strenv_(const int *i,
	   char *value,
	   /* Hidden arguments */
	   const int valuelen)
{ /* Returns (*i)'th environment number; 
     Note: "Fortran", not "C" range between [1..numenv] */
  int j = (i && environ) ? (*i) : 0;
  memset(value, ' ', valuelen);
  if (j >= 1 && j <= numenv) {
    char *p = environ[--j];
    if (p) {
      int len = strlen(p);
      if (valuelen < len) len = valuelen;
      memcpy(value,p,len);
    }
  }
}


void
ec_strenv(const int *i,
	  char *value,
	  /* Hidden arguments */
	  const int valuelen)
{
  ec_strenv_(i, value, valuelen);
}


void
ec_getenv_(const char *s,
	   char *value,
	   /* Hidden arguments */
	   int slen,
	   const int valuelen)
{
  char *env = NULL;
  char *p = malloc(slen+1);
  if (!p) {
    fprintf(stderr,"ec_getenv_(): Unable to allocate %d bytes of memory\n", slen+1);
    ABOR1("ec_getenv_(): Unable to allocate memory");
  }
  memcpy(p,s,slen);
  p[slen]='\0';
  memset(value, ' ', valuelen);
  env = getenv(p);
  if (env) {
    int len = strlen(env);
    if (valuelen < len) len = valuelen;
    memcpy(value,env,len);
  }
  free(p);
}


void
ec_getenv(const char *s,
	   char *value,
	   /* Hidden arguments */
	   int slen,
	   const int valuelen)
{
  ec_getenv_(s, value, slen, valuelen);
}


void
ec_putenv_(const char *s,
	   /* Hidden argument */
	   int slen)
{
  const char *x = &s[slen-1];
  /* strip trailing blanks first */
  while (slen > 0 && *x == ' ') { --slen; --x; }
  /* now go ahead */
  if (slen > 0) {
    char *p = malloc(slen+1);
    if (!p) {
      fprintf(stderr,"ec_putenv_(): Unable to allocate %d bytes of memory\n", slen+1);
      ABOR1("ec_putenv_(): Unable to allocate memory");
    }
    memcpy(p,s,slen);
    p[slen]='\0';
    putenv(p);
    /* Cannot free(p); , since putenv() uses this memory area for good ;-( */
  }
}


void
ec_putenv(const char *s,
	  /* Hidden argument */
	  int slen)
{
  ec_putenv_(s,slen);
}


void
ec_putenv_nooverwrite_(const char *s,
		       /* Hidden argument */
		       int slen)
{
  const char *x = &s[slen-1];
  /* strip trailing blanks first */
  while (slen > 0 && *x == ' ') { --slen; --x; }
  /* now go ahead */
  if (slen > 0) {
    char *eq = NULL;
    char *p = malloc(slen+1);
    if (!p) {
      fprintf(stderr,"ec_putenv_nooverwrite_(): Unable to allocate %d bytes of memory\n", slen+1);
      ABOR1("ec_putenv_nooverwrite_(): Unable to allocate memory");
    }
    memcpy(p,s,slen);
    p[slen]='\0';
    eq = strchr(p,'=');
    if (eq) {
      char *env = NULL;
      *eq = '\0';
      env = getenv(p);
      if (env) {
	/* Already found ==> do not overwrite */
	free(p);
	return;
      }
      else {
	/* Reset '=' back and continue with putenv() */
	*eq = '=';
      }
    }
    putenv(p);
    /* Cannot free(p); , since putenv() uses this memory area for good ;-( */
  }
}


void
ec_putenv_nooverwrite(const char *s,
		      /* Hidden argument */
		      int slen)
{
  ec_putenv_nooverwrite_(s,slen);
}


unsigned int
ec_sleep_(const int *nsec)
{
  return sleep((nsec && *nsec > 0) ? *nsec : 0);
}


unsigned int
ec_sleep(const int *nsec)
{
  return ec_sleep_(nsec);
}


/* Microsecond-sleep, by S.Saarinen, 25-jan-2008 */

void  /* Global, C-callable, too */
ec_microsleep(int usecs) {
  if (usecs > 0) {
    struct timeval t;
    t.tv_sec =  usecs/1000000;
    t.tv_usec = usecs%1000000;
    (void) select(0, NULL, NULL, NULL, &t);
  }
}


void
ec_usleep_(const int *usecs)
{
  if (usecs && *usecs > 0) ec_microsleep(*usecs);
}


void
ec_usleep(const int *usecs)
{
  ec_usleep_(usecs);
}
