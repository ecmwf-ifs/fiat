
/* linuxtrbk.c : Print traceback on linux */

/* 
   Author: Sami Saarinen, ECMWF, 28-Apr-2006
   The code "nicked" from ifsaux/support/drhook.c

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include "cargs.h"

#ifdef CRAYXT
/* Cray XT3/XT4 with catamount microkernel */
#define system(cmd) (-1)
#endif

#define PRETOSTR(x) #x
#define TOSTR(x) PRETOSTR(x)

#if defined(SUN4) && !defined(PSTACKTRACE)
#define PSTACKTRACE /bin/pstack
#endif

#define strequ(s1,s2)     ((void *)s1 && (void *)s2 && strcmp(s1,s2) == 0)
#define strnequ(s1,s2,n)  ((void *)s1 && (void *)s2 && memcmp(s1,s2,n) == 0)

typedef struct {
  const char *func;
  const char *file;
  unsigned int lineno;
} BFD_t;

static int ResolveViaBFD(void *address, BFD_t *b, const char *str);
static void InitBFD();

#if (defined(LINUX) || defined(SUN4)) && !defined(XT3) && !defined(XD1) 

#ifndef LINELEN
#define LINELEN 1024
#endif

#if defined(__GNUC__)

#define _GNU_SOURCE
#include <pthread.h>
#include <sys/time.h>
#include <sys/resource.h>

#if defined(LINUX) && !defined(CYGWIN) && !defined(DARWIN) && !defined(CRAYXT)
#include <execinfo.h>
#elif defined(DARWIN)
#include <errno.h>
#include <limits.h>
#undef HANDLE
#define HANDLE pthread_t *
#endif

#define GNUC_BTRACE 64
#define __USE_GNU
#define _XOPEN_SOURCE

#if !defined(CYGWIN)
#include <ucontext.h>
#endif

#endif /* defined(__GNUC__) */

#if !defined(ADDR2LINE)
#define ADDR2LINE /usr/bin/addr2line
#endif

#ifdef BFDLIB
#include <bfd.h>
static const int has_bfd = 1;
#else
static const int has_bfd = 0;
#endif

#if defined(__GNUC__) && defined(__INTEL_COMPILER)
static void SetMasterThreadsStackSizeBeforeMain() __attribute__((constructor));

static void SetMasterThreadsStackSizeBeforeMain()
{
  pthread_attr_t Attributes;
  void *StackAddress;
  size_t StackSize = 0;
  int StackMult = 4; /* Multiply slave stacksize by this number to get master stacksize; the default = 4 */
  long long int value = 0;
  char *env_ignore = getenv("IGNORE_STACKSIZE");
  int ignore = 0;
  char *envstksz = getenv("STACKSIZE"); /* Master thread's stacksize -- according to Intel */
  char *envompstksz = getenv("OMP_STACKSIZE"); /* slaves' stacksize, not master's. Master gets its default value from ulimit -s */
  char *envmult = getenv("STACKMULT"); /* Becomes new StackMult, if defined and >= 1. Not used in case "envstksz" is defined. */

  if (env_ignore) ignore = atoi(env_ignore);
  if (!envstksz && !envompstksz) ignore = 1;

  if (envstksz || envompstksz) {
    struct rlimit rlim;
    char *s = envstksz ? strdup(envstksz) : strdup(envompstksz);
    int slen = strlen(s);
    char *last = s + slen - 1;
    long long int mult = 1024;
    if (*last == 'b' || *last == 'B') {
      mult = 1;
      *last = 0;
    }
    else if (*last == 'k' || *last == 'K') {
      mult = 1024;
      *last = 0;
    }
    else if (*last == 'm' || *last == 'M') {
      mult = 1048576;
      *last = 0;
    }
    else if (*last == 'g' || *last == 'G') {
      mult = 1073741824;
      *last = 0;
    }
    value = atoll(s) * mult;
    free(s);
    if (envmult) {
      StackMult = atoi(envmult);
      if (StackMult < 1) StackMult = 1;
    }
    if (envstksz) StackMult = 1;
    if (getrlimit(RLIMIT_STACK, &rlim) == 0) {
      rlim.rlim_cur = value * StackMult;
      setrlimit(RLIMIT_STACK, &rlim);
    }
  }

  /* Get the pthread attributes using pthread-routines */
  memset(&Attributes, 0x0, sizeof (Attributes));
  pthread_getattr_np(pthread_self(), &Attributes);
  pthread_attr_getstack(&Attributes, &StackAddress, &StackSize);
  pthread_attr_destroy(&Attributes);

  if (ignore) {
    fprintf(stderr,
	    "SetMasterThreadsStackSizeBeforeMain() : Master thread's stack size = %llu bytes [setrlimit() was not called]\n",
	    (unsigned long long int)StackSize);
  }
  else if (envstksz) {
    fprintf(stderr,
	    "SetMasterThreadsStackSizeBeforeMain() : Master thread's stack size = %llu bytes, (STACKSIZE %lld bytes)\n",
	    (unsigned long long int)StackSize,
	    value);
  }
  else if (envompstksz) {
    fprintf(stderr,
	    "SetMasterThreadsStackSizeBeforeMain() : Master thread's stack size = %llu bytes, (slave's OMP_STACKSIZE %lld bytes, STACKMULT = %d)\n",
	    (unsigned long long int)StackSize,
	    value,
	    StackMult);
  }
}

#endif /* defined(__GNUC__) && defined(__INTEL_COMPILER) */

void
LinuxTraceBack(void *sigcontextptr)
{
  int sigcontextptr_given = sigcontextptr ? 1 : 0;
  extern void gdb_trbk_();
  extern void dbx_trbk_();
#if defined(__GNUC__) && defined(LINUX) && !defined(CYGWIN)
  ucontext_t ctx;
#endif
  static int recur = 0;
  pid_t pid = getpid();
  unsigned long long int pth = pthread_self();
  const char *a_out = ec_GetArgs(0);
#if defined(__GNUC__) && defined(LINUX) && !defined(CYGWIN) && !defined(DARWIN)
  if (!sigcontextptr) {
      sigcontextptr = (getcontext(&ctx) == 0) ? &ctx : NULL;
  }
#endif
  fprintf(stderr,"[LinuxTraceBack]: Backtrace(s) for program '%s' (pid=%d.%llu) : sigcontextptr=%p\n", 
	  a_out ? a_out : "/dev/null", pid, pth, sigcontextptr);

  if (++recur > 1) {
    fprintf(stderr,
	    "[LinuxTraceBack]: I don't handle recursive calls very well (recursion level = %d)\n",
	    recur);
    if (recur > 10) {
      fprintf(stderr,"[LinuxTraceBack]: Recursion too deep. Exiting immediately with _exit(%d)\n",
	      recur);
      _exit(recur); /* Exit immediately */
    }
  }

#if defined(__GNUC__) && defined(LINUX) && !defined(CYGWIN) && !defined(DARWIN)
  //fflush(NULL);

  if (sigcontextptr) {
    /* To have a desired effect, 
       compile with -g (and maybe -O1 or greater to get some optimization)
       and link with -g -Wl,-export-dynamic */
    void *trace[GNUC_BTRACE];
    ucontext_t *uc = (ucontext_t *)sigcontextptr;
    int fd = fileno(stderr);
    int trace_size = backtrace(trace, GNUC_BTRACE);
    int len_addr2linecmd = sizeof(TOSTR(ADDR2LINE)) + 80 + strlen(a_out) + trace_size * 30;
    char *addr2linecmd = (has_bfd || (access(TOSTR(ADDR2LINE),X_OK) != 0)) ? NULL : malloc(len_addr2linecmd);
    char **strings = NULL;
#if defined(REG_EIP)
    if (trace_size > 0) {
      /* overwrite sigaction with caller's address */
      trace[1] = (void *) uc->uc_mcontext.gregs[REG_EIP]; /* Help!! REG_EIP only available in 32-bit mode ? */
    }
#endif
    strings = backtrace_symbols(trace, trace_size);
    fprintf(stderr,"[LinuxTraceBack]: Backtrace (size = %d) with %s\n",trace_size,
	    addr2linecmd ? "addr2line-cmd" : has_bfd ? "BFD-method" : "plain hex-dump");
    if (strings && trace_size > 0) {
      int i;
      FILE *fp = NULL;
      if (addr2linecmd) {
	/* Use ADDR2LINE to obtain source file & line numbers for each trace-address */
	snprintf(addr2linecmd, len_addr2linecmd, "%s -fs -e '%s'", TOSTR(ADDR2LINE), a_out);
	for (i = 0; i < trace_size; i++) {
	  char s[30];
	  if (trace[i])
	    snprintf(s,sizeof(s)," %p",trace[i]);
	  else
	    snprintf(s,sizeof(s)," 0x0");
	  strcat(addr2linecmd,s);
	}
	if (getenv("LD_PRELOAD")) {
	  static char ld_preload[] = "LD_PRELOAD=";
	  putenv(ld_preload);
	}
	fp = popen(addr2linecmd,"r");
	free(addr2linecmd);
      }
      if (fp) {
	for (i = 0; i < trace_size; i++) {
	  int ok = 0;
	  char func[LINELEN];
	  if (!feof(fp) && fgets(func, LINELEN, fp)) {
	    char line[LINELEN];
	    if (!feof(fp) && fgets(line, LINELEN, fp)) {
	      char *nl = strchr(func,'\n');
	      const char *last_slash = strrchr(strings[i],'/');
	      if (last_slash) last_slash++; else last_slash = strings[i];
	      if (nl) *nl = '\0';
	      nl = strchr(line,'\n');
	      if (nl) *nl = '\0';
	      fprintf(stderr, "(pid=%d.%llu) [%d]: %s : %s() at %s\n", pid, pth, i, last_slash, func, line);
	      ok = 1;
	    }
	  }
	  if (!ok) fprintf(stderr, "(pid=%d.%llu) [%d]: %s\n", pid, pth, i, strings[i]);
	} /* for (i = 0; i < trace_size; i++) */
	fflush(stderr);
	pclose(fp);
      } /* if (fp) */
      else {
	int rc;
	InitBFD();
	for (i = 0 ; i < trace_size; ++i) {
	  BFD_t b;
	  const char *last_slash = strrchr(strings[i],'/');
	  if (last_slash) last_slash++; else last_slash = strings[i];
	  rc = ResolveViaBFD(trace[i], &b, last_slash);
	  if (rc == 0) {
	    fprintf(stderr,"(pid=%d.%llu) [%d]: %s : %s() at %s:%u\n",pid,pth,i,last_slash,b.func,b.file,b.lineno);
	  }
	  else {
	    fprintf(stderr,"(pid=%d.%llu) [%d]: %s : %p\n",pid,pth,i,last_slash,trace[i]);
	  }
	}
      }
    }
    else {
      /* Print traceback directly to fd=2 (stderr) */
      backtrace_symbols_fd(trace, trace_size, fd);
    } /* if (addr2linecmd) else ... */
    if (strings) free(strings);
  }
#endif /* __GNUC__ */

  if (!sigcontextptr_given) goto finish;

#if defined(PSTACKTRACE)
  /* This is normally available on Sun/Solaris ("SUN4") platforms */
  if (access(TOSTR(PSTACKTRACE),X_OK) == 0) {
    char cmd[sizeof(TOSTR(PSTACKTRACE)) + 20];
    snprintf(cmd,sizeof(cmd),"%s %d", TOSTR(PSTACKTRACE), pid);
    fflush(NULL);
    system(cmd);
    fflush(NULL);
  }
#endif /* defined(PSTACKTRACE) */

  gdb_trbk_();
  dbx_trbk_();

 finish:
  fprintf(stderr,"[LinuxTraceBack] : End of backtrace(s)\n");
  recur--;
}
 
void linux_trbk_(void)
{
  LinuxTraceBack(NULL);
}

#else

/* Non-Linux: A dummy call which does nothing */

void LinuxTraceBack(void *sigcontextptr) { }

void linux_trbk_(void) { }

#endif

void linux_trbk(void)
{
  linux_trbk_();
}

/* GNU-debugger traceback */

#if !defined(GNUDEBUGGER)
#define GNUDEBUGGER /usr/bin/gdb
#endif

void gdb_trbk_()
{
  char *gdb = getenv("GNUDEBUGGER");
  if (gdb && 
      (access(TOSTR(GNUDEBUGGER),X_OK) == 0) && /* GNUDEBUGGER was set */
      (strequ(gdb,"1")    || 
       strequ(gdb,"true") || 
       strequ(gdb,"TRUE"))) {
    char gdbcmd[65536];
    pid_t pid = getpid();
    const char *a_out = ec_GetArgs(0);
    fprintf(stderr,
	    "[gdb_trbk] : Invoking %s ...\n",
	    TOSTR(GNUDEBUGGER));
    snprintf(gdbcmd,sizeof(gdbcmd),
	     "set +e; /bin/echo '"
	     "set watchdog 1\n"
	     "set confirm off\n"
	     "set pagination off\n"
	     "set print elements 16\n"
	     "set print repeats 3\n"
	     "set print sevenbit-strings on\n"
	     "where\n"
	     "quit\n' > ./gdb_drhook.%d ; "
	     "%s -x ./gdb_drhook.%d -q -n -f -batch %s %d < /dev/null ; "
	     "/bin/rm -f ./gdb_drhook.%d"
	     , pid
	     , TOSTR(GNUDEBUGGER), pid, a_out, pid
	     , pid);
    
    /* fprintf(stderr,"%s\n",gdbcmd); */
    fflush(NULL);
    system(gdbcmd);
    fflush(NULL);
  }
}

void gdb_trbk() { gdb_trbk_(); }


/* DBX-debugger traceback */

#if !defined(DBXDEBUGGER)
#define DBXDEBUGGER /usr/bin/dbx
#endif

void dbx_trbk_()
{
  char *dbx = getenv("DBXDEBUGGER");
  if (dbx && 
      (access(TOSTR(DBXDEBUGGER),X_OK) == 0) && /* DBXDEBUGGER was set */
      (strequ(dbx,"1")    || 
       strequ(dbx,"true") || 
       strequ(dbx,"TRUE"))) {
    pid_t pid = getpid();
    const char *a_out = ec_GetArgs(0);
    char dbxcmd[65536];
#if defined(SUN4)
    const char *qopt = " -q";
#else
    const char *qopt = "";
#endif
    fprintf(stderr,
	    "[dbx_trbk] : Invoking %s ...\n",
	    TOSTR(DBXDEBUGGER));
    if (a_out && (access(a_out,X_OK|R_OK) == 0)) {
      snprintf(dbxcmd,sizeof(dbxcmd),
	       "set +e; /bin/echo 'where; quit; '"
	       " | %s%s %s %d ",
	       TOSTR(DBXDEBUGGER), qopt, a_out, pid);
    }
    else {
      snprintf(dbxcmd,sizeof(dbxcmd),
	       "set +e; /bin/echo 'where; quit; '"
	       " | %s%s - %d ",
	       TOSTR(DBXDEBUGGER), qopt, pid);
    }
    
    /* fprintf(stderr,"%s\n",dbxcmd); */
    fflush(NULL);
    system(dbxcmd);
    fflush(NULL);
  }
}

void dbx_trbk() { dbx_trbk_(); }

#ifdef BFDLIB
static bfd *abfd = 0;
static asymbol **syms = 0;
static asection *text = 0;
#endif

static void InitBFD()
{
#ifdef BFDLIB
  if (!abfd) {
    const char *a_out = ec_GetArgs(0);
 
    bfd_init();
 
    abfd = bfd_openr(a_out, 0);
    if (!abfd) {
      perror("bfd_openr failed: ");
      return;
    }
 
    bfd_check_format(abfd,bfd_object);
 
    unsigned int storage_needed = bfd_get_symtab_upper_bound(abfd);
    syms = (asymbol **) malloc(storage_needed);
    unsigned int cSymbols = bfd_canonicalize_symtab(abfd, syms);
 
    text = bfd_get_section_by_name(abfd, ".text");
  }
#endif
}
 
static int ResolveViaBFD(void *address, BFD_t *b, const char *str)
{
  int rc = -1;
#ifdef BFDLIB
  if (!abfd) InitBFD();
  if (b) {
    long offset = (long)(address - text->vma);
    if (offset > 0) {
      memset(b,0x0,sizeof(*b));
      if (bfd_find_nearest_line(abfd, text, syms, offset, &b->file, &b->func, &b->lineno) && b->file && b->func) {
	const char *last_slash = strrchr(b->file,'/');
	if (last_slash) b->file = last_slash + 1;
	rc = 0;
      }
      else if (str) {
	static const char qmarks[] = "??";
	if (!b->func) {
	  static char *s = NULL; // Not thread safe
	  char *loc_lbr;
	  if (s) free(s);
	  s = strdup(str);
	  loc_lbr = strchr(s,'(');
	  if (loc_lbr) {
	    char *loc_add = NULL;
	    *loc_lbr++ = 0;
	    loc_add = strchr(loc_lbr,'+');
	    if (loc_add) *loc_add = 0;
	    b->func = loc_lbr;
	  }
	  else {
	    b->func = qmarks;
	  }
	}
	if (!b->file) b->file = qmarks;
	rc = 0;
      } /* if (bfd_find_nearest_line(...)) else if (str) ... */
    }
  }
#endif
  return rc;
}

