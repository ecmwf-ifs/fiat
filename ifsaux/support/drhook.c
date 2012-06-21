/* 
   drhook.c

   Author: Sami Saarinen, ECMWF, 14..24-Nov-2003

*/

#ifdef RS6K
#pragma options opt=3 halt=e
#endif

/* === This doesn't handle recursive calls correctly (yet) === */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#ifdef RS6K
#include <fptrap.h>
#include <pthread.h>
#endif
#ifdef VPP
#include <ucontext.h>
#endif

static int any_memstat = 0;
static int opt_gethwm = 0;
static int opt_getstk = 0;
static int opt_getrss = 0;
static int opt_getpag = 0;
static int opt_walltime = 0;
static int opt_cputime = 0;
static int opt_wallprof = 0;
static int opt_cpuprof = 0;
static int opt_trim = 0;
static int opt_calls = 0;

#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#define SIG_EXTRA_ARGS       /* empty */
#define SIG_PASS_EXTRA_ARGS  /* empty */
#else 
#define SIG_EXTRA_ARGS       , siginfo_t *sigcode, void *sigcontextptr
#define SIG_PASS_EXTRA_ARGS  , sigcode, sigcontextptr
#endif

#undef MIN
#define MIN(a,b) ( (a) < (b) ? (a) :  (b) )

#undef MAX
#define MAX(a,b) ( (a) > (b) ? (a) :  (b) )

#undef ABS
#define ABS(x)   ( (x) >= 0  ? (x) : -(x) )

#define strequ(s1,s2)     (strcmp(s1,s2) == 0)
/*#define strnequ(s1,s2,n)  (strncmp(s1,s2,n) == 0)*/
#define strnequ(s1,s2,n)  (memcmp(s1,s2,n) == 0)

extern long long int getstk_();
extern long long int gethwm_();
extern long long int getrss_();
extern long long int getcurheap_();
extern long long int getpag_();

#ifdef RS6K
static long long int irtc_start = 0;
extern long long int irtc();
#define WALLTIME() ((double)(irtc() - irtc_start)*1.0e-9)
extern double util_cputime_();
#define CPUTIME() util_cputime_()
#else
extern double util_walltime_();
#define WALLTIME() util_walltime_()
extern double util_cputime_();
#define CPUTIME() util_cputime_()
#endif

extern void c_drhook_raise_(const int *sig);
#define RAISE(x) { int tmp = x; c_drhook_raise_(&tmp); }

extern void c_drhook_print_(const int *ftnunitno,
			    const int *thread_id,
			    const int *print_option, /* 
							1=raw call counts 
							2=calling tree
							3=profiling info
						     */
			    int *level);

extern void dr_hook_prt_(const int *ftnunitno,
			 const char *s
			 /* Hidden arguments */
			 , int s_len);

extern void dr_hook_procinfo_(int *myproc, int *nproc);
extern int get_thread_id_();

/*** typedefs ***/

typedef struct drhook_key_t {
  char *name;
  unsigned short name_len;
  unsigned short status; /* 0=inactive, >1 active */
  unsigned long long int calls;
  long long int hwm, maxrss, rssnow, stack, maxstack, paging;
  double wall_in, delta_wall_all, delta_wall_child;
  double cpu_in, delta_cpu_all, delta_cpu_child;
  struct drhook_key_t *next;
} drhook_key_t;

typedef union {
  drhook_key_t *keyptr;
  double d;
} equivalence_t;

typedef struct drhook_calltree_t {
  int active;
  drhook_key_t *keyptr;
  struct drhook_calltree_t *next;
  struct drhook_calltree_t *prev;
} drhook_calltree_t;

typedef struct drhook_sig_t {
  int active;
  char name[32];
  struct sigaction new;
  struct sigaction old;
} drhook_sig_t;

typedef union {
  void (*func1args)(int sig);
  void (*func3args)(int sig SIG_EXTRA_ARGS);
} drhook_sigfunc_t;

typedef struct drhook_prof_t {
  double pc;
  double total;
  double self;
  unsigned long long int calls;
  double percall_ms_self;
  double percall_ms_total;
  int index;
  int tid;
  int cluster;
  double *maxval;
  unsigned char is_max;
  char *name;
} drhook_prof_t;

/*** static (local) variables ***/

static int numthreads = 0;
static int myproc = -1;
static int nproc = -1;
static drhook_key_t      **keydata  = NULL;
static drhook_calltree_t **calltree = NULL;
static drhook_calltree_t **thiscall = NULL;
static int signals_set = 0;
static int signal_handler_calls = 0;
static drhook_sig_t siglist[1+NSIG] = { 0 };
static char *a_out = NULL;
static char *mon_out = NULL;
static int mon_out_procs = -1;
static double percent_limit = 0; /* Lowest percentage accepted into the printouts */
static drhook_key_t **keyself = NULL; /* pointers to itself (per thread) */

#define HASHSIZE(n) ((unsigned int)1<<(n))
#define HASHMASK(n) (HASHSIZE(n)-1)

#define NHASH    15
#define NHASHMAX 24
static int nhash = NHASH;
unsigned int hashsize = HASHSIZE(NHASH);
unsigned int hashmask = HASHMASK(NHASH);

/*--- malloc_drhook ---*/

static void *
malloc_drhook(size_t size)
{
  void *p = malloc(size);
  if (!p) {
    fprintf(stderr,"***Error in malloc_drhook(): Unable to allocate space for %d bytes\n", size);
    RAISE(SIGABRT);
  }
  return p;
}

/*--- calloc_drhook ---*/

static void *
calloc_drhook(size_t nmemb, size_t size)
{
  size_t n = nmemb * size;
  void *p = malloc_drhook(n);
  memset(p,0,n);
  return p;
}

/*--- free_drhook ---*/

#define free_drhook(x) { if (x) { free(x); x = NULL; } }

/*--- strdup_drhook ---*/

static char *
strdup_drhook(const char *s)
{
  int n = strlen(s);
  char *p = malloc_drhook(n+1);
  memcpy(p,s,n);
  p[n] = 0;
  return p;
}

/*--- strdup2_drhook ---*/

static char *
strdup2_drhook(const char *s, int s_len)
{
  int n = s_len;
  char *p = malloc_drhook(n+1);
  memcpy(p,s,n);
  p[n] = 0;
  return p;
}

/*--- hashfunc ---*/

unsigned int
hashfunc(const char *s, int s_len)
{
  unsigned int hashval;
  if (opt_trim) {
    for (hashval = 0; s_len>0 ; s++, s_len--) {
      unsigned char c = islower(*s) ? toupper(*s) : *s;
      hashval = (hashval<<4)^(hashval>>28)^(c);
    }
  }
  else {
    for (hashval = s_len; s_len>0 ; s_len--) {
      hashval = (hashval<<4)^(hashval>>28)^(*s++);
    }
  }
  hashval = (hashval ^ (hashval>>10) ^ (hashval>>20)) & hashmask;
  return hashval;
}

/*--- insert_calltree ---*/

static void
insert_calltree(int tid, drhook_key_t *keyptr)
{
  if (tid >= 1 && tid <= numthreads) {
    drhook_calltree_t *treeptr = thiscall[--tid];
    while (treeptr->active) {
      if (!treeptr->next) {
	treeptr->next = calloc_drhook(1,sizeof(drhook_calltree_t));
	treeptr->next->prev = treeptr;
      }
      treeptr = treeptr->next;
    }
    treeptr->keyptr = keyptr;
    treeptr->active = 1;
    thiscall[tid] = treeptr;
  }
}

/*--- remove_calltree ---*/

static void 
remove_calltree(int tid, drhook_key_t *keyptr, 
		const double *delta_wall_child, const double *delta_cpu_child)
{
  if (tid >= 1 && tid <= numthreads) {
    drhook_calltree_t *treeptr = thiscall[--tid];
    if (treeptr->active && treeptr->keyptr == keyptr) {
      treeptr->active = 0;
      if (treeptr->prev) {
	if (opt_walltime) treeptr->prev->keyptr->delta_wall_child += (*delta_wall_child);
	if (opt_cputime)  treeptr->prev->keyptr->delta_cpu_child  += (*delta_cpu_child);
	thiscall[tid] = treeptr->prev;
      }
      else {
	thiscall[tid] = calltree[tid];
      }
    }
  }
}

/*--- memstat ---*/

static void
memstat(drhook_key_t *keyptr)
{
  if (any_memstat && keyptr) {
    if (opt_gethwm) keyptr->hwm = gethwm_();
    if (opt_getrss) {
      keyptr->maxrss = getrss_();
      keyptr->rssnow = getcurheap_();
    }
    if (opt_getstk) {
      long long int stk = getstk_();
      keyptr->stack = stk;
      keyptr->maxstack = MAX(keyptr->maxstack,stk);
    }
    if (opt_getpag) keyptr->paging = getpag_();
  }
}

/*--- flptrap ---*/

/*
  -----------------------------------------------------------------------
  If we are trapping Floating-Point Error, then set the processor in SYNC
  modes and enable TRP_INVALID, TRP_DIV_BY_ZERO and TRP_OVERFLOW.
  -----------------------------------------------------------------------
*/

#ifdef RS6K
static void
flptrap(int sig)
{
  if (sig == SIGFPE) {
    /* From John Hague, IBM, UK (--> thanks a lot, John !!)*/
    int ret = fp_trap(FP_TRAP_FASTMODE);
    if ((ret == FP_TRAP_UNIMPL) || (ret == FP_TRAP_ERROR)) {
      char errmsg[4096];
      sprintf(errmsg, 
      "flptrap(): Call to 'fp_trap' in signal_trap failed (return code = %d)\n (line %d in file %s)\n",
      ret, __LINE__, __FILE__);
      perror(errmsg);
      RAISE(SIGABRT);
      exit(1); /* Never ending up here */
    }
    fp_enable(TRP_INVALID | TRP_DIV_BY_ZERO | TRP_OVERFLOW);
  }
}
#else
void
static flptrap(int sig)
{
  return; /* A dummy */
}
#endif

/*--- catch_signals ---*/

static void signal_drhook(int sig SIG_EXTRA_ARGS);

#define CATCHSIG(x) {\
  drhook_sig_t *sl = &siglist[x];\
  if (sl->active == 0) {\
    drhook_sigfunc_t u;\
    u.func3args = signal_drhook;\
    sl->active = 1;\
    sigemptyset(&sl->new.sa_mask);\
    sl->new.sa_handler = u.func1args;\
    sl->new.sa_flags = SA_SIGINFO;\
    sigaction(x,&sl->new,&sl->old);\
    flptrap(x);\
    if (myproc == 1) {\
      fprintf(stderr,\
	      ">>%s(): DR_HOOK also catches signal#%d; new handler installed at 0x%x; old preserved at 0x%x\n",\
              "catch_signals", x, sl->new.sa_handler, sl->old.sa_handler);\
    }\
  }\
}

static void
catch_signals()
{
  char *env = getenv("DR_HOOK_CATCH_SIGNALS");
  if (env) {
    const char delim[] = ", \t/";
    char *p, *s = strdup_drhook(env);
    p = strtok(s,delim);
    while (p) {
      int sig = atoi(p);
      if (sig >= 1 && sig <= NSIG) {
	CATCHSIG(sig);
      }
      else if (sig == -1) { /* Makes ALL (catchable) signals available to DR_HOOK */
	int j;
	for (j=1; j<=NSIG; j++) {
	  CATCHSIG(j);
	} /* for (j=1; j<=NSIG; j++) */
	break;
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
  }
}

/*--- ignore_signals ---*/

static void
ignore_signals()
{
  char *env = getenv("DR_HOOK_IGNORE_SIGNALS");
  if (env) {
    const char delim[] = ", \t/";
    char *p, *s = strdup_drhook(env);
    p = strtok(s,delim);
    while (p) {
      int sig = atoi(p);
      if (sig >= 1 && sig <= NSIG) {
	drhook_sig_t *sl = &siglist[sig];
	if (myproc == 1) {
	  fprintf(stderr,
		  ">>>ignore_signals(): DR_HOOK will ignore signal#%d altogether\n", sig);
	}
	sl->active = -1;
      }
      else if (sig == -1) { /* Switches off ALL signals from DR_HOOK */
	int j;
	for (j=1; j<=NSIG; j++) {
	  drhook_sig_t *sl = &siglist[j];
	  if (myproc == 1) {
	    fprintf(stderr,
		    ">>>ignore_signals(): DR_HOOK will ignore signal#%d altogether\n", j);
	  }
	  sl->active = -1;
	} /* for (j=1; j<=NSIG; j++) */
	break;
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
  }
}

/*--- signal_drhook ---*/

#define SETSIG(x) {\
  drhook_sig_t *sl = &siglist[x];\
  if (sl->active == 0) {\
    drhook_sigfunc_t u;\
    u.func3args = signal_drhook;\
    sl->active = 1;\
    strcpy(sl->name,#x);\
    sigemptyset(&sl->new.sa_mask);\
    sl->new.sa_handler = u.func1args;\
    sl->new.sa_flags = SA_SIGINFO;\
    sigaction(x,&sl->new,&sl->old);\
    flptrap(x);\
    if (myproc == 1) {\
      fprintf(stderr,"%s(%s=%d): New handler installed at 0x%x; old preserved at 0x%x\n",\
              "signal_drhook", sl->name, x, sl->new.sa_handler, sl->old.sa_handler);\
    }\
  }\
}

static void 
signal_drhook(int sig SIG_EXTRA_ARGS)
{
  /* signal(sig, SIG_IGN); */
  if (signals_set && sig >= 1 && sig <= NSIG) { 
    /* Signal catching */
    drhook_sig_t *sl = &siglist[sig];
    drhook_sigfunc_t u;
    sigset_t newmask, oldmask;
    int tid;
    pid_t pid = getpid();
    long long int hwm = gethwm_();
    long long int stk = getstk_();
    hwm /= 1024;
    stk /= 1024;
    tid = get_thread_id_();

    fprintf(stderr,"[myproc#%d,tid#%d,pid#%d]: Received signal#%d (%s) ; Memory: %lldK (heap), %lldK (stack)\n",
	    myproc,tid,pid,sig,sl->name, hwm, stk);

    u.func3args = signal_drhook;

    sigfillset(&newmask);
    /*
    sigemptyset(&newmask);
    sigaddset(&newmask, sig);
    */

    /* Start critical region (we don't want any signals to interfere while doing this) */
    sigprocmask(SIG_BLOCK, &newmask, &oldmask);
    signal_handler_calls++;
    { /* Print Dr. HOOK traceback */
      const int ftnunitno = 0; /* stderr */
      const int print_option = 2; /* calling tree */
      int level = 0;
      c_drhook_print_(&ftnunitno, &tid, &print_option, &level);
      fflush(NULL);
#ifdef RS6K
      xl__sigdump(sig SIG_PASS_EXTRA_ARGS); /* Can't use xl__trce(...), since it also stops */
#endif
      fflush(NULL);
    }
    sigprocmask(SIG_SETMASK, &oldmask, 0);
    /* End critical region : the original signal state restored */

    if (sl->old.sa_handler != SIG_DFL && 
	sl->old.sa_handler != SIG_IGN && 
	sl->old.sa_handler != u.func1args) {
      fprintf(stderr,
	      ">>%s(at 0x%x): Calling previous signal handler in chain at 0x%x (if possible)\n",
	      "signal_drhook",signal_drhook,sl->old.sa_handler); 
      u.func1args = sl->old.sa_handler;
      u.func3args(sig SIG_PASS_EXTRA_ARGS);
    }
    else {
      fprintf(stderr,
	      "[myproc#%d,tid#%d,pid#%d]: Error exit due to signal#%d (%s)\n",
	      myproc,tid,pid,sig,sl->name);
      fflush(NULL);
      exit(1);
    }
  }
  else {
    fprintf(stderr,
	    "%s(at 0x%x): Invalid signal#%d or signals/this signal not set (%d)\n",
	    "signal_drhook",signal_drhook,sig,signals_set);
#ifdef RS6K
    xl__sigdump(sig SIG_PASS_EXTRA_ARGS);
#endif
    fflush(NULL);
    exit(1);
  }
}

/*--- signal_drhook_init ---*/

static void 
signal_drhook_init()
{
  int j;
  if (signals_set) return; /* Extra safety */
  dr_hook_procinfo_(&myproc, &nproc);
  if (myproc < 1 || nproc < 0) return; /* Signals not (yet) set, since MPI not initialized */
  for (j=1; j<=NSIG; j++) {
    drhook_sig_t *sl = &siglist[j];
    sl->active = 0;
    sprintf(sl->name, "DR_HOOK_SIG#%d", j);
  }
  ignore_signals(); /* These signals will not be seen by DR_HOOK */
  SETSIG(SIGABRT); /* Good to be first */
  SETSIG(SIGBUS);
  SETSIG(SIGSEGV);
  SETSIG(SIGILL);
#if !defined(LINUX)
  SETSIG(SIGEMT);
#endif
  SETSIG(SIGFPE);
  SETSIG(SIGTRAP); /* should be switched off when used with debuggers */
  SETSIG(SIGINT);
  SETSIG(SIGQUIT);
  SETSIG(SIGTERM);
  SETSIG(SIGIO);
  SETSIG(SIGXCPU);
  SETSIG(SIGSYS);
  /* SETSIG(SIGCHLD); we may not want to catch this either; may interfere parallel processing */
  /* -- not active
  SETSIG(SIGCHLD);
  SETSIG(SIGHUP);
  SETSIG(SIGCONT);
  */
  catch_signals(); /* Additional signals to be seen by DR_HOOK */
  signals_set = 1; /* Signals are set now */
}

/*--- get_mon_out ---*/

static char *
get_mon_out(int me)
{
  char *s = NULL;
  if (mon_out_procs == me || (mon_out_procs == -1 && me >= 1 && me <= nproc)) {
    if (!mon_out) mon_out = strdup_drhook("drhook.prof.%d");
    s = malloc_drhook((strlen(mon_out) + 20) * sizeof(*s));
    sprintf(s,mon_out,me);
  }
  return s;
}

/*--- process_options ---*/

static void do_prof();

static void
process_options()
{
  char *env;

  env = getenv("DR_HOOK_PROFILE");
  if (env) {
    char *s = calloc_drhook(strlen(env) + 10, sizeof(*s));
    strcpy(s,env);
    if (!strchr(env,'%')) strcat(s,".%d");
    mon_out = strdup_drhook(s);
    free(s);
  }

  env = getenv("DR_HOOK_PROFILE_PROC");
  if (env) {
    mon_out_procs = atoi(env);
  }

  env = getenv("DR_HOOK_PROFILE_LIMIT");
  if (env) {
    percent_limit = atof(env);
  }

  env = getenv("DR_HOOK_HASHBITS");
  if (env) {
    int value = atoi(env);
    if (value < 1) value = 1;
    else if (value > NHASHMAX) value = NHASHMAX;
    nhash = value;
    hashsize = HASHSIZE(nhash);
    hashmask = HASHMASK(nhash);
  }

  env = getenv("DR_HOOK_OPT");
  if (env) {
    const char delim[] = ", \t/";
    char *s = strdup_drhook(env);
    char *p = s;
    while (*p) {
      if (islower(*p)) *p = toupper(*p);
      p++;
    } 
    p = strtok(s,delim);
    while (p) {
      /* Assume that everything is OFF by default */
      if (strequ(p,"ALL")) { /* all except profiler data */
	opt_gethwm = opt_getstk = opt_getrss = opt_getpag = opt_walltime = opt_cputime = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"MEM") || strequ(p,"MEMORY")) {
	opt_gethwm = opt_getstk = opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"TIME") || strequ(p,"TIMES")) {
	opt_walltime = opt_cputime = 1;
	opt_calls = 1;
      }
      else if (strequ(p,"HWM") || strequ(p,"HEAP")) {
	opt_gethwm = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"STK") || strequ(p,"STACK")) {
	opt_getstk = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"RSS")) {
	opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"PAG") || strequ(p,"PAGING")) {
	opt_getpag = 1;
	opt_calls = 1;
	any_memstat++;
      }
      else if (strequ(p,"WALL") || strequ(p,"WALLTIME")) {
	opt_walltime = 1;
	opt_calls = 1;
      }
      else if (strequ(p,"CPU") || strequ(p,"CPUTIME")) {
	opt_cputime = 1;
	opt_calls = 1;
      }
      else if (strequ(p,"CALLS") || strequ(p,"COUNT")) {
	opt_calls = 1;
      }
      else if (strequ(p,"PROF") || strequ(p,"WALLPROF")) {
	opt_wallprof = 1;
	opt_walltime = 1;
	opt_cpuprof = 0;
	opt_calls = 1;
      }
      else if (strequ(p,"CPUPROF")) {
	opt_cpuprof = 1;
	opt_cputime = 1;
	opt_wallprof = 0;
	opt_calls = 1;
      }
      else if (strequ(p,"TRIM")) {
	opt_trim = 1;
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
    if (opt_wallprof || opt_cpuprof) {
      atexit(do_prof);
    }
  }
}

/*--- trim ---*/

static const char *
trim(const char *name, int *n)
{
  const char *from;
  int len;
  int name_len = *n;
  while (*name && isspace(*name) && name_len > 0) {
    /* skip leading blanks */
    name++;
    name_len--;
  }
  len = 0;
  from = name;
  while (*from && !isspace(*from) && name_len > 0) {
    /* find first space point, if any */
    from++;
    len++;
    name_len--;
  }
  *n = len;
  return name;
}

/*--- getkey ---*/

static drhook_key_t *
getkey(int tid, const char *name, int name_len)
{
  drhook_key_t *keyptr = NULL;
  if (tid >= 1 && tid <= numthreads) {
    unsigned int hash;
    if (opt_trim) name = trim(name, &name_len);
    hash = hashfunc(name, name_len);
    keyptr = &keydata[tid-1][hash];
    for (;;) {
      int found = 0;
      if (!keyptr->name) {
	keyptr->name = malloc_drhook((name_len+1)*sizeof(*name));
	keyptr->name_len = name_len;
	if (opt_trim) {
	  const char *from = name;
	  char *to = keyptr->name;
	  int len = name_len;
	  for (; len>0; from++, len--) {
	    *to++ = islower(*from) ? toupper(*from) : *from;
	  }
	  *to = 0;
	}
	else {
	  memcpy(keyptr->name, name, name_len);
	  keyptr->name[name_len] = 0;
	}
	found = 1;
      }
      if (found || 
	  (keyptr->name_len == name_len &&
	   ((!opt_trim && *keyptr->name == *name && strnequ(keyptr->name, name, name_len)) ||
	    (opt_trim && strncasecmp(keyptr->name, name, name_len) == 0)))) {
	keyptr->wall_in = opt_walltime ? WALLTIME() : 0;
	keyptr->cpu_in  = opt_cputime  ? CPUTIME()  : 0;
	if (any_memstat) memstat(keyptr);
	if (opt_calls) {
	  keyptr->calls++;
	  keyptr->status++;
	}
	insert_calltree(tid, keyptr);
	break; /* for (;;) */
      }
      else {
	if (!keyptr->next) {
	  keyptr->next = calloc_drhook(1, sizeof(drhook_key_t)); /* chaining */
	}
	keyptr = keyptr->next;
      }  /* if (found ...) else ... */
    } /* for (;;) */
  } /* if (tid >= 0 && tid < numthreads) */
  return keyptr;
}

/*--- putkey ---*/

static void
putkey(int tid, drhook_key_t *keyptr, const char *name, int name_len)
{
  drhook_calltree_t *treeptr = (tid >= 1 && tid <= numthreads) ? thiscall[tid-1] : NULL;
  if (!treeptr || !treeptr->active || !(treeptr->keyptr == keyptr)) {
    pid_t pid = getpid();
    char *s;
    if (opt_trim) name = trim(name,&name_len);
    s = strdup2_drhook(name,name_len);
    if (opt_trim) {
      char *p = s;
      while (*p) {
	if (islower(*p)) *p = toupper(*p);
	p++;
      }
    }
    fprintf(stderr,
	    "[myproc#%d,tid#%d,pid#%d]: Dr.Hook has detected an invalid key-pointer/handle while leaving routine '%s'\n",
	    myproc,tid,pid,s);
    if (treeptr) {
      equivalence_t u;
      u.keyptr = treeptr->keyptr;
      fprintf(stderr,
	      "[myproc#%d,tid#%d,pid#%d]: Expecting key-pointer=0x%x (%.20g) and treeptr->active-flag == 1\n",
	      myproc,tid,pid,u.keyptr,u.d);
      u.keyptr = keyptr;
      fprintf(stderr,
	      "[myproc#%d,tid#%d,pid#%d]: Got a key-pointer=0x%x (%.20g) and treeptr->active-flag = %d\n",
	      myproc,tid,pid,u.keyptr,u.d,treeptr->active);
    }
    fprintf(stderr,"[myproc#%d,tid#%d,pid#%d]: Aborting...\n",myproc,tid,pid);
    free_drhook(s);
    RAISE(SIGABRT);
    exit(1); /* Never ending up here */
  }
  else if (tid >= 1 && tid <= numthreads) {
    double delta_wall = opt_walltime ? (WALLTIME() - keyptr->wall_in) : 0;
    double delta_cpu  = opt_cputime  ? (CPUTIME()  - keyptr->cpu_in ) : 0;
    if (any_memstat) memstat(keyptr);
    if (opt_walltime) keyptr->delta_wall_all += delta_wall;
    if (opt_cputime)  keyptr->delta_cpu_all  += delta_cpu;
    if (opt_calls)    keyptr->status--;
    remove_calltree(tid, keyptr, &delta_wall, &delta_cpu);
  }
}
    
/*--- init_drhook ---*/

static void
init_drhook(int ntids)
{
  if (numthreads == 0 || !keydata || !calltree || !keyself) {
    int j;
#ifdef RS6K
    irtc_start = irtc();
#endif
    process_options();
    if (!keydata) {
      keydata = malloc_drhook(sizeof(**keydata) * ntids);
      for (j=0; j<ntids; j++) {
	keydata[j] = calloc_drhook(hashsize, sizeof(drhook_key_t));
      }
    }
    if (!calltree) {
      calltree = malloc_drhook(sizeof(**calltree) * ntids);
      thiscall = malloc_drhook(sizeof(**thiscall) * ntids);
      for (j=0; j<ntids; j++) {
	thiscall[j] = calltree[j] = calloc_drhook(1,sizeof(drhook_calltree_t));
      }
    }
    if (!keyself) {
      const char *name = "$drhook";
      int name_len = strlen(name);
      keyself = malloc_drhook(sizeof(**keyself) * ntids);
      for (j=0; j<ntids; j++) {
	drhook_key_t *keyptr = keyself[j] = calloc(1,sizeof(drhook_key_t));
	keyptr->name = strdup_drhook(name);
	keyptr->name_len = name_len;
      }
    }
    numthreads = ntids;
    signal_drhook_init();
  }
}

/*--- itself ---*/

#define ITSELF_0 \
  drhook_key_t *keyptr = NULL; \
  if (opt_wallprof || opt_cpuprof) keyptr = itself(*thread_id,0,NULL)

#define ITSELF_1 \
  if (keyptr) { \
    double delta = 0; \
    (void) itself(*thread_id,1,&delta); \
    if (opt_wallprof) u.keyptr->delta_wall_child += delta; \
    else              u.keyptr->delta_cpu_child  += delta; \
  }

static drhook_key_t *
itself(int tid, int opt, double *delta_time) 
{
  drhook_key_t *keyptr = NULL;
  if (keyself && (opt_wallprof || opt_cpuprof)) {
    double tim = opt_wallprof ? WALLTIME() : CPUTIME();
    keyptr = keyself[--tid];
    if (opt == 0) {
      if (opt_wallprof) keyptr->wall_in = tim;
      else              keyptr->cpu_in = tim;
      keyptr->calls++;
    }
    else if (opt = 1) {
      double delta = 0;
      if (opt_wallprof) delta = (tim - keyptr->wall_in), keyptr->delta_wall_all += delta;
      else              delta = (tim - keyptr->cpu_in),  keyptr->delta_cpu_all += delta;
      if (delta_time) *delta_time = delta;
    }
  }
  return keyptr;
}

/*--- profiler output ---*/

static void
do_prof()
{
  if (opt_wallprof || opt_cpuprof) {
    const int ftnunitno = 0;
    const int master = 1;
    const int print_option = 3;
    int initlev = 0;
    c_drhook_print_(&ftnunitno, &master, &print_option, &initlev);
  }
}

/*** PUBLIC ***/

/*=== c_drhook_init_ ===*/

void 
c_drhook_init_(const char *progname,
	       const int *num_threads
	       /* Hidden length */
	       ,int progname_len)
{
  init_drhook(*num_threads);
  progname = trim(progname, &progname_len);
  a_out = calloc_drhook(progname_len+1,sizeof(*progname));
  memcpy(a_out, progname, progname_len);
}

/*=== c_drhook_start_ ===*/

void 
c_drhook_start_(const char *name, 
		const int *thread_id, 
		double *key
		/* Hidden length */
		,int name_len)
{
  equivalence_t u;
  ITSELF_0;
  if (!signals_set) {
    dr_hook_procinfo_(&myproc, &nproc);
    if (myproc >= 1 && nproc >= 1) signal_drhook_init();
  }
  u.keyptr = getkey(*thread_id, name, name_len);
  *key = u.d;
  ITSELF_1;
}

/*=== c_drhook_end_ ===*/

void 
c_drhook_end_(const char *name,
	      const int *thread_id,
	      const double *key
	      /* Hidden length */
	      ,int name_len)
{
  equivalence_t u;
  ITSELF_0;
  u.d = *key;
  putkey(*thread_id, u.keyptr, name, name_len);
  ITSELF_1;
}

/*=== c_drhook_print_ ===*/

#define PRINT_HWM() \
if (opt_gethwm) { sprintf(s,",hwm=%lldK",keyptr->hwm/1024); s += strlen(s); }

#define PRINT_RSS() \
if (opt_getrss) { \
  sprintf(s,",rss/max=%lldK/%lldK",keyptr->rssnow/1024, keyptr->maxrss/1024); \
  s += strlen(s); \
}

#define PRINT_STK() \
if (opt_getstk) { \
  sprintf(s,",stack/max=%lldK/%lldK",keyptr->stack/1024, keyptr->maxstack/1024); \
  s += strlen(s); \
}

#define PRINT_PAG() \
if (opt_getpag) { \
  sprintf(s,",pag=%lld",keyptr->paging); \
  s += strlen(s); \
}

#define PRINT_WALL() \
if (opt_walltime) { \
  double self = keyptr->delta_wall_all-keyptr->delta_wall_child; \
  if (self < 0) self = 0; \
  sprintf(s,",wall=%.3fs/%.3fs", \
	  keyptr->delta_wall_all, self); \
  s += strlen(s); \
}

#define PRINT_CPU() \
if (opt_cputime) { \
  double self = keyptr->delta_cpu_all-keyptr->delta_cpu_child; \
  if (self < 0) self = 0; \
  sprintf(s,",cpu=%.3fs/%.3fs", \
	  keyptr->delta_cpu_all, self); \
  s += strlen(s); \
}

#define PRINT_CALLS() \
if (opt_calls) { \
  sprintf(s,",#%llu,st=%d",keyptr->calls,keyptr->status); \
  s += strlen(s); \
}

static int
prof_name_comp(const void *v1, const void *v2)
{
  const drhook_prof_t *p1 = v1;
  const drhook_prof_t *p2 = v2;
  return strcmp(p1->name,p2->name);
}

static int
prof_pc_comp_desc(const void *v1, const void *v2)
{
  const drhook_prof_t *p1 = v1;
  const drhook_prof_t *p2 = v2;
  if (p1->pc < p2->pc) return 1;
  else if (p1->pc > p2->pc) return -1;
  else return 0;
}

void 
c_drhook_print_(const int *ftnunitno,
		const int *thread_id,
		const int *print_option, /* 
					    1=raw call counts 
					    2=calling tree
					    3=profiling info
					 */
		int *level
		)
{
  int tid = (thread_id && (*thread_id > 0)) ? *thread_id : get_thread_id_();
  if (keydata && calltree && tid >= 1 && tid <= numthreads) {
    char line[4096];
    int j;
    pid_t pid = getpid();

    if (*print_option == 1) { /* raw call counts */
      for (j=0; j<hashsize; j++) {
	int nestlevel = 0;
	drhook_key_t *keyptr = &keydata[tid-1][j];
	while (keyptr) {
	  if (keyptr->name) {
	    char *s = line;
	    sprintf(s,
		    "[myproc#%d,tid#%d,pid#%d,hash#%d,nest=%d]: '%s'",
		    myproc,tid,pid,j,nestlevel,keyptr->name);
	    s += strlen(s);
	    PRINT_CALLS();
	    PRINT_HWM();
	    PRINT_RSS();
	    PRINT_STK();
	    PRINT_PAG();
	    PRINT_WALL();
	    PRINT_CPU();
	    *s = 0;
	    dr_hook_prt_(ftnunitno, line, strlen(line));
	  }
	  keyptr = keyptr->next;
	  nestlevel++;
	} /* while (keyptr) */
      } /* for (j=0; j<hashsize; j++) */
    }

    else if (*print_option == 2) { /* current calling tree */
      drhook_calltree_t *treeptr = calltree[tid-1];
      if (tid > 1) { /* I'm not master thread, but my master has the beginning of the calltree */
	int initlev = 0;
	const int master = 1;
	c_drhook_print_(ftnunitno, &master, print_option, &initlev);
	*level += initlev;
      }
      while (treeptr && treeptr->active) {
	drhook_key_t *keyptr = treeptr->keyptr;
	char *s = line;
	sprintf(s,"[myproc#%d,tid#%d,pid#%d]: ",myproc,tid,pid);
	s += strlen(s);
	(*level)++;
	for (j=0; j<(*level); j++) *s++ = ' ';
	sprintf(s,"%s ",keyptr->name);
	s += strlen(s);
	PRINT_CALLS();
	PRINT_HWM();
	PRINT_RSS();
	PRINT_STK();
	PRINT_PAG();
	PRINT_WALL();
	PRINT_CPU();
	*s = 0;
	dr_hook_prt_(ftnunitno, line, strlen(line));
	treeptr = treeptr->next;
      } /* while (treeptr && treeptr->active) */
    }

    else if (*print_option == 3) { /* profiling */
      int t;
      double cumul;
      double tottime = 0;
      double *tot = NULL;
      int nprof = 0;
      drhook_prof_t *prof = NULL;
      drhook_prof_t *p;

      if (!opt_wallprof && !opt_cpuprof) return; /* no profiling info available */
      /* if (signal_handler_calls) return; */ /* signal handler called => profiling info unreliable */
      if (tid > 1) return; /* just master thread allowed ; takes care of siblings, too */
      if (numthreads<=0) return;
      if (!keyself) return;

      tot = calloc_drhook(numthreads, sizeof(*tot));

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && keyptr->status == 0) {
	      double self;
	      if (opt_wallprof) {
		self = keyptr->delta_wall_all - keyptr->delta_wall_child;
	      }
	      else {
		self = keyptr->delta_cpu_all - keyptr->delta_cpu_child;
	      }
	      if (self < 0) self = 0;
	      tot[t] += self;
	      nprof++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      if (opt_wallprof) { /* unreliable; will be recalculated */
	tottime = tot[0] + keyself[0]->delta_wall_all;
	for (t=1; t<numthreads; t++) {
	  double tmp = tot[t] + keyself[t]->delta_wall_all;
	  tottime = MAX(tottime,tmp);
	}
      }
      else { /* ok & reliable (for cpuprof) */
	tottime = 0;
	for (t=0; t<numthreads; t++) tottime += (tot[t] + keyself[t]->delta_cpu_all);
      }

      if (tottime <= 0) tottime = 1e-10;

      nprof += numthreads; /* Account for itself times # of threads */
      p = prof = calloc_drhook(nprof, sizeof(*prof)); /* Make sure there is at least one element */

      for (t=0; t<numthreads; t++) {
	drhook_key_t *keyptr = keyself[t];
	if (keyptr) { /* itself for this thread */
	  p->self = opt_wallprof ? keyptr->delta_wall_all : keyptr->delta_cpu_all;
	  p->total = p->self;
	  p->calls = keyptr->calls;
	  p->name = keyptr->name;
	  p->pc = (p->self/tottime) * 100.0;
	  if (p->calls > 0) {
	    p->percall_ms_self = (p->self/p->calls) * 1000.0;
	    p->percall_ms_total = (p->total/p->calls) * 1000.0;
	  }
	  p->tid = t+1;
	  p->index = p - prof;
	  p++;
	}
	for (j=0; j<hashsize; j++) {
	  keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && keyptr->status == 0) {
	      p->self = opt_wallprof ?
		keyptr->delta_wall_all - keyptr->delta_wall_child :
		keyptr->delta_cpu_all - keyptr->delta_cpu_child;
	      p->total = opt_wallprof ?
		keyptr->delta_wall_all :
		keyptr->delta_cpu_all;
	      p->calls = keyptr->calls;
	      p->name = keyptr->name;
	      p->pc = (p->self/tottime) * 100.0;
	      if (p->calls > 0) {
		p->percall_ms_self = (p->self/p->calls) * 1000.0;
		p->percall_ms_total = (p->total/p->calls) * 1000.0;
	      }
	      p->tid = t+1;
	      p->index = p - prof;
	      p++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      do {
	int numroutines = 0;
	int cluster;
	double *maxval = calloc_drhook(nprof, sizeof(*maxval)); /* Make sure there is at least one element */
	int *clusize = calloc_drhook(nprof, sizeof(*clusize)); /* Make sure there is at least one element */
	char *prevname = NULL;
	const char *fmt = "%5d %8.2f %11.2f %11.2f %11.2f %14llu %11.2f %11.2f   %s%s@%d [%d,%d]\n";
	char *filename = get_mon_out(myproc);
	FILE *fp = NULL;

	if (!filename) break;

	if ((myproc == 1 && mon_out_procs == -1) || mon_out_procs == myproc) {
	  fprintf(stderr,"Writing profiling information of proc#%d into file '%s'\n",myproc,filename);
	}

	fp = fopen(filename,"w");
	if (!fp) goto finish_3;
	
	/* alphanumerical sorting to find out clusters of the same routine but on different threads */
	/* also find out total wall clock time */
	/* calculate percentage values */
	
	p = prof;
	qsort(p, nprof, sizeof(*p), prof_name_comp);

	cluster = 0;
	maxval[cluster] = p->self;
	clusize[cluster] = 1;
	prevname = p->name;
	p++;
	for (j=1; j<nprof; j++) {
	  if (!strequ(prevname,p->name)) {
	    (p-1)->cluster = cluster;
	    (p-1)->maxval = &maxval[cluster];
	    prevname = p->name;
	    cluster++;
	  }
	  if (p->self > maxval[cluster]) maxval[cluster] = p->self;
	  p->cluster = cluster;
	  p->maxval = &maxval[cluster];
	  clusize[cluster]++;
	  p++;
	} /* for (j=1; j<nprof; j++) */

	numroutines = (nprof > 0) ? (cluster + 1) : 0; /* Active no. of routines */

	if (opt_wallprof) tottime = 0;
	p = prof;
	for (j=0; j<nprof; j++) {
	  int use_this = 0;
	  cluster = p->cluster;
	  if (clusize[cluster] > 1) { /* multiple threads <= numthreads indeed called this routine */
	    p->is_max = (p->self == *p->maxval);
	    if (p->is_max) { /* first max found will be used for total time */
	      clusize[cluster] = -clusize[cluster]; /* ensures that max has been found for this cluster */
	      use_this = opt_wallprof;
	    }
	  }
	  else if (clusize[cluster] == 1) {
	    use_this = opt_wallprof;
	  }
	  if (use_this && opt_wallprof) tottime += p->self;
	  p++;
	}

        if (tottime <= 0) tottime = 1e-10;

	if (opt_wallprof) { /* use re-calculated tottime to define percentages */
	  p = prof;
	  for (j=0; j<nprof; j++) {
	    p->pc = (p->self/tottime) * 100.0;
	    p++;
	  }
	}

	/* sorting with respect to percentage value */

	p = prof;
	qsort(p, nprof, sizeof(*p), prof_pc_comp_desc);

	fprintf(fp,
		"Profiling information for program='%s' (# of instrumented routines called = %d):\n\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		a_out, numroutines, opt_wallprof ? "Wall" : "Total CPU", tottime, myproc, nproc, numthreads); 

	if (myproc == 1) {
	  fprintf(stderr,
		  "Profiling information for program='%s' (# of instrumented routines called = %d):\n\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		  a_out, numroutines, opt_wallprof ? "Wall" : "Total CPU", tottime, myproc, nproc, numthreads); 
	}

	for (t=0; t<numthreads; t++) {
	  double tmp = 100.0*(tot[t]/tottime);
	  fprintf(    fp,"\tThread#%d: %11.2f sec (%.2f%%)\n",t+1,tot[t],tmp);
	  if (myproc == 1) {
	    fprintf(stderr,"\tThread#%d: %11.2f sec (%.2f%%)\n",t+1,tot[t],tmp);
	  }
	}

	fprintf(fp,"\n");
	fprintf(fp,"    #  %% Time        Cumul        Self       Total     # of calls        Self       Total    Routine@<tid> [Cluster:(id,size)]\n");
	fprintf(fp, "        (self)       (sec)       (sec)       (sec)                    ms/call     ms/call\n");
	fprintf(fp,"\n");

	cumul = 0;
	for (j=0; j<nprof; ) {
	  int cluster_size = clusize[p->cluster];
	  if (p->pc < percent_limit) break;
	  if (opt_cputime) {
	    cumul += p->self;
	  }
	  else {
	    if (p->is_max || cluster_size == 1) cumul += p->self;
	  }
	  fprintf(fp, fmt,
		  ++j, p->pc, cumul, p->self, p->total, p->calls,
		  p->percall_ms_self, p->percall_ms_total, 
		  p->is_max ? "*" : " ",
		  p->name, p->tid,
		  p->cluster, ABS(cluster_size));
	  p++;
	} /* for (j=0; j<nprof; ) */
	
	fclose(fp);
      finish_3:
	free_drhook(filename);
	free_drhook(maxval);
	free_drhook(clusize);
      } while (0);

      free_drhook(tot);
      free_drhook(prof);
    }
  }
}

/*=== c_drhook_init_signals_ ===*/

void
c_drhook_init_signals_()
{
  signal_drhook_init();
}

/*=== c_drhook_raise_ ===*/

/* 
   Just a convenience function for Fortran90 which may not have raise()-signal function
   CALL c_drhook_raise(10)  ! Raise signal#10
*/

void 
c_drhook_raise_(const int *sig) 
{ 
  fflush(NULL);
  raise(*sig);
} 

/**** C-interface to Dr.Hook ****/

void
Dr_Hook(const char *name, int option, double *handle, int name_len)
{
  static int first_time = 1;
  static int value = 1; /* ON by default */
  if (value == 0) return; /* Immediate return if OFF */
  if (first_time) {
    char *env = getenv("DR_HOOK");
    value = atoi(env);
    first_time = 0;
  }
  if (value != 0) {
    int tid = get_thread_id_();
    if (option == 0) {
      c_drhook_start_(name, &tid, handle, name_len > 0 ? name_len : strlen(name));
    }
    else if (option == 1) {
      c_drhook_end_(name, &tid, handle, name_len > 0 ? name_len : strlen(name));
    }
  }
}
