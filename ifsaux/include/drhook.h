#ifndef _DRHOOK_H_
#define _DRHOOK_H_

#ifdef _DRHOOK_C_
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <math.h>

#ifdef RS6K
#include <fptrap.h>
#endif

#ifdef VPP
#include <ucontext.h>
#endif

int drhook_lhook = 1;
#else
extern int drhook_lhook;
#endif

/* drhook.c external interfaces */

extern void
c_drhook_getenv_(const char *s,
		 char *value,
		 /* Hidden arguments */
		 int slen,
		 const int valuelen);

extern void
c_drhook_memcounter_(const int *thread_id,
		     const long long int *size,
		     long long int *keyptr_addr);

extern void
c_drhook_raise_(const int *sig);

extern void
c_drhook_print_(const int *ftnunitno,
		const int *thread_id,
		const int *print_option, /* 
					    1=raw call counts 
					    2=calling tree
					    3=profiling info
					 */
		int *level);

extern void
c_drhook_init_signals_(const int *enforce);

extern void
c_drhook_set_lhook_(const int *lhook);

extern void 
c_drhook_init_(const char *progname,
	       const int *num_threads
	       /* Hidden length */
	       ,int progname_len);

extern void
c_drhook_start_(const char *name, 
		const int *thread_id, 
		double *key,
		const char *filename,
		const int *sizeinfo
		/* Hidden length */
		,int name_len, int filename_len);

extern void
c_drhook_end_(const char *name,
	      const int *thread_id,
	      const double *key,
	      const char *filename,
	      const int *sizeinfo
	      /* Hidden length */
	      ,int name_len, int filename_len);

/**** C-interface to Dr.Hook ****/

extern void
Dr_Hook(const char *name, int option, double *handle, 
	const char *filename, int sizeinfo,
	int name_len, int filename_len);

#define DRHOOK_START(name) \
  static const char *drhook_name = #name; \
  static const int drhook_name_len = sizeof(#name) - 1; /* Compile time eval */ \
  static const char *drhook_filename = __FILE__; \
  static const int drhook_filename_len = sizeof(__FILE__) - 1; /* Compile time eval */ \
  double zhook_handle; \
  if (drhook_lhook) Dr_Hook(drhook_name, 0, &zhook_handle, \
			    drhook_filename, 0, \
			    drhook_name_len, drhook_filename_len)

#define DRHOOK_START_BY_STRING(name) \
  static const char *drhook_name = name; \
  static const int drhook_name_len = sizeof(name) - 1; /* Compile time eval */ \
  static const char *drhook_filename = __FILE__; \
  static const int drhook_filename_len = sizeof(__FILE__) - 1; /* Compile time eval */ \
  double zhook_handle; \
  if (drhook_lhook) Dr_Hook(drhook_name, 0, &zhook_handle, \
			    drhook_filename, 0, \
			    drhook_name_len, drhook_filename_len)

#define DRHOOK_END(sizeinfo) \
  if (drhook_lhook) Dr_Hook(drhook_name, 1, &zhook_handle, \
			    drhook_filename, sizeinfo, \
			    drhook_name_len, drhook_filename_len)


/* Fortran routines */

extern void
dr_hook_prt_(const int *ftnunitno,
	     const char *s
	     /* Hidden arguments */
	     , int s_len);

extern void
dr_hook_procinfo_(int *myproc, int *nproc);

#endif  /* _DRHOOK_H_ */
