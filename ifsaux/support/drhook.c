#define _DRHOOK_C_   1

/* 
   drhook.c

   Author: Sami Saarinen, ECMWF, 14..24-Nov-2003

*/

#ifdef RS6K
#pragma options opt=3 halt=e
#endif

/* === This doesn't handle recursive calls correctly (yet) === */

#include "drhook.h"

static char *start_stamp = NULL;
static char *end_stamp = NULL;

static int any_memstat = 0;
static int opt_gethwm = 0;
static int opt_getstk = 0;
static int opt_getrss = 0;
static int opt_getpag = 0;
static int opt_walltime = 0;
static int opt_cputime = 0;
static int opt_wallprof = 0;
static int opt_cpuprof = 0;
static int opt_hpmprof = 0;
static int opt_trim = 0;
static int opt_calls = 0;
static int opt_self = 1; /* 0=exclude drhook altogether, 1=include, but don't print, 2=also print */

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

#define RAISE(x) { int tmp = x; c_drhook_raise_(&tmp); }

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
#ifdef HPM
  long long int *counter_in, *counter_sum;
#endif
  char *filename;         /* the filename where the 1st call (on this routine-name) 
			     to dr_hook() occurred */
  long long int sizeinfo; /* # of data elements, bytes, etc. */
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
  int ignore_atexit;
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
  double mipsrate, mflops, divpc;
  int index;
  int tid;
  int cluster;
  double *maxval;
  unsigned char is_max;
  char *name;
  char *filename;
  unsigned long long int sizeinfo;
  double sizespeed, sizeavg;
} drhook_prof_t;

/*** static (local) variables ***/

static int numthreads = 0;
static int myproc = -1;
static int nproc = -1;
static pid_t pid = -1;
static drhook_key_t      **keydata  = NULL;
static drhook_calltree_t **calltree = NULL;
static drhook_calltree_t **thiscall = NULL;
static int signals_set = 0;
static int signal_handler_called = 0;
static int signal_handler_ignore_atexit = 0;
static drhook_sig_t siglist[1+NSIG] = { 0 };
static char *a_out = NULL;
static char *mon_out = NULL;
static int mon_out_procs = -1;
static double percent_limit = -10; /* Lowest percentage accepted into the printouts */
static drhook_key_t **keyself = NULL; /* pointers to itself (per thread) */

#define HASHSIZE(n) ((unsigned int)1<<(n))
#define HASHMASK(n) (HASHSIZE(n)-1)

#define NHASH    15
#define NHASHMAX 24
static int nhash = NHASH;
unsigned int hashsize = HASHSIZE(NHASH);
unsigned int hashmask = HASHMASK(NHASH);

#ifdef HPM
/* HPM-specific (static) protos */

static void stopstart_hpm(int tid, drhook_key_t *pstop, drhook_key_t *pstart);
static void init_hpm(int tid);
static double mflops_hpm(const drhook_key_t *keyptr);
static double mips_hpm(const drhook_key_t *keyptr);
static double divpc_hpm(const drhook_key_t *keyptr);
static double mflop_count(const drhook_key_t *keyptr);
static double mip_count(const drhook_key_t *keyptr);

#else
/* Dummies for HPM as macros that do nothing */

#define stopstart_hpm(tid, pstop, pstart)
#define init_hpm(tid)
#define mflops_hpm(keyptr)  0
#define mips_hpm(keyptr)    0
#define divpc_hpm(keyptr)   0
#define mflop_count(keyptr) 0
#define mip_count(keyptr)   0

#endif

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

/*--- timestamp ---*/

static char *
timestamp()
{
  time_t tp;
  const int bufsize = 80;
  char *buf = malloc_drhook(bufsize+1);
  time(&tp);
  strftime(buf, bufsize, "%Y%m%d %H%M%S", localtime(&tp));
  return buf;
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
    if (opt_hpmprof) {
      stopstart_hpm(tid+1,
		    treeptr->prev ? treeptr->prev->keyptr : NULL, /* stop current (i.e. my parent) */
		    treeptr->keyptr);                             /* start to gather for me */
    }
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
      if (opt_hpmprof) {
	stopstart_hpm(tid+1, treeptr->keyptr, thiscall[tid]->keyptr); /* stop current, (re-)start previous */
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

#define SETSIG(x,ignore_flag) {\
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
    sl->ignore_atexit = ignore_flag;\
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

    if (sl->ignore_atexit) signal_handler_ignore_atexit++;

    if (signal_handler_called++) {
      /*
	-------------------------------------------------------------------------
	This is the actual signal-handler that is called by the Kernel when one
	of the trapped SIGNALs occurs. All but the first thread that cause it to
	be called are put to sleep for about 5 mins, giving the first thread time
	to print out the traceback and optionally dump the memory before exiting.
	-------------------------------------------------------------------------
      */
      sleep(5*60);
    }

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
signal_drhook_init(int enforce)
{
  int j;
  if (signals_set) return; /* Extra safety */
  dr_hook_procinfo_(&myproc, &nproc);
  /* Signals not (yet) set, since MPI not initialized 
     Only enforce-parameter can enforce to set these => no output on myproc=1 */
  if (!enforce && (myproc < 1 || nproc < 0)) return; 
  for (j=1; j<=NSIG; j++) { /* Initialize */
    drhook_sig_t *sl = &siglist[j];
    sl->active = 0;
    sprintf(sl->name, "DR_HOOK_SIG#%d", j);
    sl->ignore_atexit = 0;
  }
  ignore_signals(); /* These signals will not be seen by DR_HOOK */
  SETSIG(SIGABRT,0); /* Good to be first */
  SETSIG(SIGBUS,0);
  SETSIG(SIGSEGV,0);
  SETSIG(SIGILL,0);
#if !defined(LINUX)
  SETSIG(SIGEMT,0);
#endif
  SETSIG(SIGFPE,0);
  SETSIG(SIGTRAP,0); /* should be switched off when used with debuggers */
  SETSIG(SIGINT,0);
  SETSIG(SIGQUIT,0);
  SETSIG(SIGTERM,0);
  SETSIG(SIGIO,0);
  SETSIG(SIGXCPU,1); /* ignore_atexit == 1 i.e. no profile info via atexit() */
  SETSIG(SIGSYS,0);
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
    fprintf(stderr,">>>process_options(): DR_HOOK_PROFILE=%s\n",mon_out);
    free(s);
  }

  env = getenv("DR_HOOK_PROFILE_PROC");
  if (env) {
    mon_out_procs = atoi(env);
    fprintf(stderr,">>>process_options(): DR_HOOK_PROFILE_PROCS=%d\n",mon_out_procs);
  }

  env = getenv("DR_HOOK_PROFILE_LIMIT");
  if (env) {
    percent_limit = atof(env);
    fprintf(stderr,">>>process_options(): DR_HOOK_PROFILE_LIMIT=%.3f\n",percent_limit);
  }

  env = getenv("DR_HOOK_HASHBITS");
  if (env) {
    int value = atoi(env);
    if (value < 1) value = 1;
    else if (value > NHASHMAX) value = NHASHMAX;
    nhash = value;
    hashsize = HASHSIZE(nhash);
    hashmask = HASHMASK(nhash);
    fprintf(stderr,">>>process_options(): DR_HOOK_HASHBITS=%d\n",nhash);
  }

  env = getenv("DR_HOOK_OPT");
  if (env) {
    const char delim[] = ", \t/";
    char *comma = ">>>process_options(): DR_HOOK_OPT=\"";
    char *s = strdup_drhook(env);
    char *p = s;
    while (*p) {
      if (islower(*p)) *p = toupper(*p);
      p++;
    } 
    p = strtok(s,delim);
    /* if (p) fprintf(stderr,">>>process_options(): DR_HOOK_OPT=\""); */
    while (p) {
      /* Assume that everything is OFF by default */
      if (strequ(p,"ALL")) { /* all except profiler data */
	opt_gethwm = opt_getstk = opt_getrss = opt_getpag = opt_walltime = opt_cputime = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"ALL"); comma = ",";
      }
      else if (strequ(p,"MEM") || strequ(p,"MEMORY")) {
	opt_gethwm = opt_getstk = opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"MEMORY"); comma = ",";
      }
      else if (strequ(p,"TIME") || strequ(p,"TIMES")) {
	opt_walltime = opt_cputime = 1;
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"TIMES"); comma = ",";
      }
      else if (strequ(p,"HWM") || strequ(p,"HEAP")) {
	opt_gethwm = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"HEAP"); comma = ",";
      }
      else if (strequ(p,"STK") || strequ(p,"STACK")) {
	opt_getstk = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"STACK"); comma = ",";
      }
      else if (strequ(p,"RSS")) {
	opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"RSS"); comma = ",";
      }
      else if (strequ(p,"PAG") || strequ(p,"PAGING")) {
	opt_getpag = 1;
	opt_calls = 1;
	any_memstat++;
	fprintf(stderr,"%s%s",comma,"PAGING"); comma = ",";
      }
      else if (strequ(p,"WALL") || strequ(p,"WALLTIME")) {
	opt_walltime = 1;
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"WALLTIME"); comma = ",";
      }
      else if (strequ(p,"CPU") || strequ(p,"CPUTIME")) {
	opt_cputime = 1;
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"CPUTIME"); comma = ",";
      }
      else if (strequ(p,"CALLS") || strequ(p,"COUNT")) {
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"CALLS"); comma = ",";
      }
      else if (strequ(p,"PROF") || strequ(p,"WALLPROF")) {
	opt_wallprof = 1;
	opt_walltime = 1;
	opt_cpuprof = 0; /* Note: Switches cpuprof OFF */
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"WALLPROF"); comma = ",";
      }
      else if (strequ(p,"CPUPROF")) {
	opt_cpuprof = 1;
	opt_cputime = 1;
	opt_wallprof = 0; /* Note: Switches walprof OFF */
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"CPUPROF"); comma = ",";
      }
      else if (strequ(p,"HPM") || strequ(p,"HPMPROF") || strequ(p,"MFLOPS")) {
	opt_hpmprof = 1;
	opt_wallprof = 1; /* Note: Implies wallprof (or prof), not cpuprof */
	opt_walltime = 1;
	opt_cpuprof = 0;  /* Note: Switches cpuprof OFF */
	opt_calls = 1;
	fprintf(stderr,"%s%s",comma,"HPMPROF"); comma = ",";
      }
      else if (strequ(p,"TRIM")) {
	opt_trim = 1;
	fprintf(stderr,"%s%s",comma,"TRIM"); comma = ",";
      }
      else if (strequ(p,"SELF")) {
	opt_self = 2;
	fprintf(stderr,"%s%s",comma,"SELF"); comma = ",";
      }
      else if (strequ(p,"NOSELF")) {
	opt_self = 0;
	fprintf(stderr,"%s%s",comma,"NOSELF"); comma = ",";
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
    if (*comma == ',') fprintf(stderr,"\"\n");
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

/*--- insertkey ---*/

static drhook_key_t *
insertkey(int tid, const drhook_key_t *keyptr_in)
{
  drhook_key_t *keyptr = NULL;
  if (tid >= 1 && tid <= numthreads) {
    /* no trimming available for this; just raw eval & insert */
    unsigned int hash = hashfunc(keyptr_in->name, keyptr_in->name_len);
    keyptr = &keydata[tid-1][hash];
    for (;;) {
      if (!keyptr->name) { /* A free slot */
	memcpy(keyptr,keyptr_in,sizeof(*keyptr));
	keyptr->next = NULL;
	break;
      }
      else {
	if (!keyptr->next) {
	  keyptr->next = calloc_drhook(1, sizeof(drhook_key_t)); /* chaining */
	}
	keyptr = keyptr->next;
      }  /* if (!keyptr->name) ... else ... */
    } /* for (;;) */
  } /* if (tid >= 0 && tid < numthreads) */
  return keyptr;
}

/*--- getkey ---*/

static drhook_key_t *
getkey(int tid, const char *name, int name_len,
       const char *filename, int filename_len,
       const double *walltime, const double *cputime)
{
  drhook_key_t *keyptr = NULL;
  if (tid >= 1 && tid <= numthreads) {
    unsigned int hash;
    if (opt_trim) name = trim(name, &name_len);
    hash = hashfunc(name, name_len);
    keyptr = &keydata[tid-1][hash];
    for (;;) {
      int found = 0;
      if (!keyptr->name) { /* A free slot */
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
	if (filename && *filename && filename_len > 0) {
	  char *psave = NULL;
	  char *p = psave = malloc_drhook((filename_len+1)*sizeof(*filename));
	  memcpy(p, filename, filename_len);
	  p[filename_len] = 0;
	  { /* Strip out dirname */
	    char *s = strrchr(p,'/');
	    if (s) p = s+1;
	  }
	  keyptr->filename = strdup_drhook(p);
	  free_drhook(psave);
	}
	found = 1;
      }
      if (found || 
	  (keyptr->name_len == name_len &&
	   ((!opt_trim && *keyptr->name == *name && strnequ(keyptr->name, name, name_len)) ||
	    (opt_trim && strncasecmp(keyptr->name, name, name_len) == 0)))) {
	if (opt_walltime) keyptr->wall_in = walltime ? *walltime : WALLTIME();
	if (opt_cputime) keyptr->cpu_in  = cputime ? *cputime : CPUTIME();
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
putkey(int tid, drhook_key_t *keyptr, const char *name, int name_len,
       int sizeinfo,
       double *walltime, double *cputime)
{
  drhook_calltree_t *treeptr = (tid >= 1 && tid <= numthreads) ? thiscall[tid-1] : NULL;
  if (!treeptr || !treeptr->active || !(treeptr->keyptr == keyptr)) {
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
    double delta_wall = 0;
    double delta_cpu  = 0;
    if (any_memstat) memstat(keyptr);
    if (opt_calls)   keyptr->status--;
    if (opt_cputime && cputime) {
      *cputime = CPUTIME();
      delta_cpu = *cputime - keyptr->cpu_in;
    }
    if (opt_walltime && walltime) {
      *walltime = WALLTIME();
      delta_wall = *walltime - keyptr->wall_in;
    }
    if (opt_walltime) keyptr->delta_wall_all += delta_wall;
    if (opt_cputime)  keyptr->delta_cpu_all  += delta_cpu;
    if (sizeinfo > 0) keyptr->sizeinfo += sizeinfo;
    remove_calltree(tid, keyptr, &delta_wall, &delta_cpu);
  }
}
    
/*--- init_drhook ---*/

static void
init_drhook(int ntids)
{
  if (numthreads == 0 || !keydata || !calltree || !keyself) {
    int j;
    if (pid == -1) { /* Ensure that called just once */
#ifdef RS6K
      irtc_start = irtc();
#endif
      start_stamp = timestamp();
      pid = getpid();
      process_options();
      drhook_lhook = 1;
    }
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
    if (!keyself && opt_self && (opt_wallprof || opt_cpuprof || opt_hpmprof)) {
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
    signal_drhook_init(0);
    init_hpm(1); /* First thread */
  }
}

/*--- itself ---*/

#define ITSELF_0 \
  drhook_key_t *keyptr_self = keyself ? itself(NULL,*thread_id,0,NULL,&walltime,&cputime) : NULL;

#define ITSELF_1 \
  if (keyptr_self) { \
    double delta = 0; \
    (void) itself(keyptr_self,*thread_id,1,&delta,&walltime,&cputime); \
    if (opt_wallprof) u.keyptr->delta_wall_child += delta; \
    else              u.keyptr->delta_cpu_child  += delta; \
  }

static drhook_key_t *
itself(drhook_key_t *keyptr_self, 
       int tid, int opt, double *delta_time, 
       const double *walltime, const double *cputime) 
{
  drhook_key_t *keyptr = NULL;
  if (keyself) {
    keyptr = keyptr_self ? keyptr_self : keyself[--tid];
    if (opt == 0) {
      if (opt_wallprof) keyptr->wall_in = walltime ? *walltime : WALLTIME();
      else              keyptr->cpu_in = cputime ? *cputime : CPUTIME();
      keyptr->calls++;
    }
    else if (opt == 1) {
      double delta = 0;
      if (opt_wallprof) {
	delta = walltime ? (*walltime - keyptr->wall_in) : (WALLTIME() - keyptr->wall_in);
	keyptr->delta_wall_all += delta;
      }
      else {
	delta = cputime ? (*cputime - keyptr->cpu_in) : (CPUTIME() - keyptr->cpu_in);
	keyptr->delta_cpu_all += delta;
      }
      if (delta_time) *delta_time = delta;
    }
  }
  return keyptr;
}

/*--- profiler output ---*/

static int do_prof_off = 0;

static void
do_prof()
{

  /* to avoid recursive signals while atexit() (e.g. SIGXCPU) */
  if (signal_handler_ignore_atexit) return; 

  if (!do_prof_off && (opt_wallprof || opt_cpuprof)) {
    const int ftnunitno = 0;
    const int master = 1;
    const int print_option = 3;
    int initlev = 0;
    c_drhook_print_(&ftnunitno, &master, &print_option, &initlev);
  }
}

/*** PUBLIC ***/

#define TIMERS \
double walltime = opt_walltime ? WALLTIME() : 0; \
double cputime  = opt_walltime ? CPUTIME()  : 0

/*=== c_drhook_set_lhook_ ===*/

void
c_drhook_set_lhook_(const int *lhook)
{
  if (lhook) drhook_lhook = *lhook;
}

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
		double *key,
		const char *filename,
		const int *sizeinfo
		/* Hidden length */
		,int name_len, int filename_len)
{
  TIMERS;
  equivalence_t u;
  ITSELF_0;
  if (!signals_set) {
    dr_hook_procinfo_(&myproc, &nproc);
    if (myproc >= 1 && nproc >= 1) signal_drhook_init(0);
  }
  u.keyptr = getkey(*thread_id, name, name_len, 
		    filename, filename_len,
		    &walltime, &cputime);
  *key = u.d;
  ITSELF_1;
}

/*=== c_drhook_end_ ===*/

void 
c_drhook_end_(const char *name,
	      const int *thread_id,
	      const double *key,
	      const char *filename,
	      const int *sizeinfo
	      /* Hidden length */
	      ,int name_len, int filename_len)
{
  TIMERS;
  equivalence_t u;
  ITSELF_0;
  u.d = *key;
  putkey(*thread_id, u.keyptr, name, name_len, 
	 *sizeinfo,
	 &walltime, &cputime);
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
      double overhead, maxcumul;
      double tottime = 0;
      double *tot = NULL;
      int nprof = 0;
      drhook_prof_t *prof = NULL;
      drhook_prof_t *p;
      double flop_tot = 0, instr_tot = 0;
      double *flop = NULL, *instr = NULL;

      if (!opt_wallprof && !opt_cpuprof) return; /* no profiling info available */
      if (tid > 1) return; /* just master thread allowed ; takes care of siblings, too */
      if (numthreads<=0) return;
      if (do_prof_off) return;
      do_prof_off = 1;

      /* Insert "$drhook" */
      if (keyself && opt_self > 1) {
	for (t=0; t<numthreads; t++) (void) insertkey(t+1,keyself[t]);
      }

      flop = calloc_drhook(numthreads, sizeof(*flop));
      instr = calloc_drhook(numthreads, sizeof(*instr));
      tot = calloc_drhook(numthreads, sizeof(*tot));

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {
	      double self;
	      if (opt_wallprof) {
		self = keyptr->delta_wall_all - keyptr->delta_wall_child;
	      }
	      else {
		self = keyptr->delta_cpu_all - keyptr->delta_cpu_child;
	      }
	      /* if (self < 0) self = 0; */
	      tot[t] += self;
	      flop[t] += mflop_count(keyptr);
	      instr[t] += mip_count(keyptr);
	      nprof++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      if (opt_wallprof) { /* a bit unreliable; had not taken max. value of threads wall yet; will be recalculated */
	tottime = tot[0] + ((keyself && opt_self > 1) ? keyself[0]->delta_wall_all : 0);
	for (t=1; t<numthreads; t++) {
	  double tmp = tot[t] + ((keyself && opt_self > 1) ? keyself[t]->delta_wall_all : 0);
	  tottime = MAX(tottime,tmp);
	}
      }
      else { /* ok & reliable (for cpuprof) */
	tottime = 0;
	for (t=0; t<numthreads; t++) tottime += (tot[t] + ((keyself && opt_self > 1) ? keyself[t]->delta_cpu_all : 0));
      }

      if (tottime <= 0) tottime = 1e-10;

      p = prof = calloc_drhook(nprof + 1, sizeof(*prof)); /* Make sure there is at least one entry */

      maxcumul = 0;
      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {
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
	      if (opt_hpmprof) {
		p->mflops = mflops_hpm(keyptr);
		p->mipsrate = mips_hpm(keyptr);
		p->divpc = divpc_hpm(keyptr);
	      }
	      maxcumul = MAX(maxcumul, p->total);
	      p->filename = keyptr->filename;
	      p->sizeinfo = keyptr->sizeinfo;
	      p->sizespeed = (p->self > 0 && p->sizeinfo > 0) ? p->sizeinfo/p->self : 0;
	      p->sizeavg = (p->calls > 0 && p->sizeinfo > 0) ? p->sizeinfo/p->calls : 0;
	      p++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      do {
	double mflop_rate = 0;
	double mip_rate = 0;
	int numroutines = 0;
	int cluster;
	double *maxval = calloc_drhook(nprof+1, sizeof(*maxval)); /* make sure at least 1 element */
	int *clusize = calloc_drhook(nprof+1, sizeof(*clusize)); /* make sure at least 1 element */
	char *prevname = NULL;
	const char *fmt1 = "%5d %8.2f %12.3f %12.3f %12.3f %14llu %11.2f %11.2f   %s%s@%d%s%s [%d,%d]";
	const char *fmt2 = "%5d %8.2f %12.3f %12.3f %12.3f %14llu %7.0f %7.0f %7.1f   %s%s@%d%s%s [%d,%d]";
	const char *fmt = opt_hpmprof ? fmt2 : fmt1;
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

	flop_tot = 0;
	instr_tot = 0;
	for (t=0; t<numthreads; t++) {
	  flop_tot += flop[t];
	  instr_tot += instr[t];
	}

	if (maxcumul <= 0) maxcumul = 1e-10;
	overhead = 100.0*(1 - tottime/maxcumul);

	fprintf(fp,
		"Profiling information for program='%s', proc#%d:\n",a_out, myproc);
	fprintf(fp,"\tNo. of instrumented routines called : %d\n", numroutines);
	fprintf(fp,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	end_stamp = timestamp();
	fprintf(fp,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	fprintf(fp,"\tInstrumentation overhead: %.2f%%\n",overhead);
	if (opt_hpmprof) {
	  mflop_rate = flop_tot / tottime;
	  mip_rate = instr_tot /tottime;
	  fprintf(fp,
		  "\t%s-time is %.2f sec on proc#%d, %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6) (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  mflop_rate, flop_tot, mip_rate, instr_tot,
		  nproc, numthreads);
	}
	else {
	  fprintf(fp,
		  "\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  nproc, numthreads);
	}

	if (myproc == 1) {
	  fprintf(stderr,
		  "Profiling information for program='%s', proc#%d:\n",a_out, myproc);
	  fprintf(stderr,"\tNo. of instrumented routines called : %d\n", numroutines);
	  fprintf(stderr,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	  fprintf(stderr,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	  fprintf(stderr,"\tInstrumentation overhead: %.2f%%\n",overhead);
	  if (opt_hpmprof) {
	    fprintf(stderr,
		  "\t%s-time is %.2f sec on proc#%d, %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6) (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  mflop_rate, flop_tot, mip_rate, instr_tot,
		  nproc, numthreads);
	  }
	  else {
	    fprintf(stderr,
		    "\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		    opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		    nproc, numthreads);
	  }
	} /* if (myproc == 1) */

	free_drhook(end_stamp);

	for (t=0; t<numthreads; t++) {
	  double tmp = 100.0*(tot[t]/tottime);
	  if (opt_hpmprof && tot[t] > 0) {
	    mflop_rate = flop[t]/tot[t];
	    mip_rate = instr[t]/tot[t];
	  }
	  else {
	    mflop_rate = 0;
	    mip_rate = 0;
	  }
	  fprintf(    fp,"\tThread#%d: %11.2f sec (%.2f%%)",t+1,tot[t],tmp);
	  if (opt_hpmprof) fprintf(    fp,", %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6)", mflop_rate, flop[t], mip_rate, instr[t]);
	  fprintf(    fp,"\n");
	  if (myproc == 1) {
	    fprintf(stderr,"\tThread#%d: %11.2f sec (%.2f%%)",t+1,tot[t],tmp);
	    if (opt_hpmprof) fprintf(stderr,", %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6)", mflop_rate, flop[t], mip_rate, instr[t]);
	    fprintf(stderr,"\n");
	  }
	}

	fprintf(fp,"\n");
	if (opt_hpmprof) {
	  fprintf(fp,"    #  %% Time         Cumul         Self        Total     # of calls    MIPS  MFlops   Div-%%    Routine@<tid> [Cluster:(id,size)] (Size,Size/sec,AvgSize)\n");
	  fprintf(fp, "        (self)        (sec)        (sec)        (sec)                                       \n");
	}
	else {
	  fprintf(fp,"    #  %% Time         Cumul         Self        Total     # of calls        Self       Total    Routine@<tid> [Cluster:(id,size)] (Size,Size/sec,AvgSize)\n");
	  fprintf(fp, "        (self)        (sec)        (sec)        (sec)                    ms/call     ms/call\n");
	}
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
	  if (opt_hpmprof) {
	    fprintf(fp, fmt,
		    ++j, p->pc, cumul, p->self, p->total, p->calls,
		    p->mipsrate, p->mflops, p->divpc,
		    p->is_max ? "*" : " ",
		    p->name, p->tid,
		    p->filename ? ":" : "",
		    p->filename ? p->filename : "",
		    p->cluster, ABS(cluster_size));
	  }
	  else {
	    fprintf(fp, fmt,
		    ++j, p->pc, cumul, p->self, p->total, p->calls,
		    p->percall_ms_self, p->percall_ms_total, 
		    p->is_max ? "*" : " ",
		    p->name, p->tid,
		    p->filename ? ":" : "",
		    p->filename ? p->filename : "",
		    p->cluster, ABS(cluster_size));
	  }
	  if (p->sizeinfo > 0) fprintf(fp," (%lld,%.0f,%.0f)",p->sizeinfo,p->sizespeed,p->sizeavg);
	  fprintf(fp,"\n");
	  p++;
	} /* for (j=0; j<nprof; ) */
	
	fclose(fp);
      finish_3:
	free_drhook(filename);
	free_drhook(maxval);
	free_drhook(clusize);
      } while (0);

      free_drhook(instr);
      free_drhook(flop);
      free_drhook(tot);
      free_drhook(prof);
      do_prof_off = 0;
    }
  }
}

/*=== c_drhook_init_signals_ ===*/

void
c_drhook_init_signals_(const int *enforce)
{
  signal_drhook_init(*enforce);
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
Dr_Hook(const char *name, int option, double *handle, 
	const char *filename, int sizeinfo,
	int name_len, int filename_len)
{
  static int first_time = 1;
  static int value = 1; /* ON by default */
  if (value == 0) return; /* Immediate return if OFF */
  if (first_time) {
    char *env = getenv("DR_HOOK");
    if (env) {
      char *p = env;
      while (*p) { 
	if (isspace(*p)) p++; 
	else break; 
      }
      if (isdigit(*p)) {
	value = atoi(p);
      }
      else {
	value = (strequ(p,"false") || strequ(p,"FALSE")) ? 0 : 1;
      }
    }
    first_time = 0;
  }
  if (value != 0) {
    int tid = get_thread_id_();
    if (option == 0) {
      c_drhook_start_(name, &tid, handle, 
		      filename, &sizeinfo,
		      name_len > 0 ? name_len : strlen(name),
		      filename_len > 0 ? filename_len : strlen(filename));
    }
    else if (option == 1) {
      c_drhook_end_(name, &tid, handle, 
		    filename, &sizeinfo,
		    name_len > 0 ? name_len : strlen(name),
		    filename_len > 0 ? filename_len : strlen(filename));
    }
  }
}


/**** Interface to HPM ****/

/*<<< experimental >>>*/

#ifdef HPM

#ifdef RS6K
/**** Interface to HPM (RS6K) ****/

#include <pmapi.h>
#include <pthread.h>

static pthread_mutex_t hpm_lock = PTHREAD_MUTEX_INITIALIZER;

static int *hpm_tid_init = NULL;
static double cycles = 1300000000.0; /* 1.3GHz */

#define MCYCLES (cycles * 1e-6)

#define TEST_PM_ERROR(name, rc) \
  if (rc != 0) { \
    fprintf(stderr,"PM_ERROR(tid#%d, pthread_self()=%d): rc=%d at %s(), line=%d, file=%s\n",\
	    tid+1,pthread_self(),rc,name,__LINE__,__FILE__); \
    pm_error((char *)name, rc); \
    sleep(tid+1); \
    raise(SIGABRT); \
  }

static void
init_hpm(int tid)
{
  const char *name = "init_hpm";
  int rc;

  --tid;

  if (!hpm_tid_init) {
    hpm_tid_init = calloc_drhook(numthreads, sizeof(*hpm_tid_init));
    cycles = pm_cycles();
  }

  if (!hpm_tid_init[tid]) {
    pm_info_t pminfo;
    pm_groups_info_t pmgroupsinfo;
    
    /*------------------------------------*/
    /* initialize the performance monitor */
    /*------------------------------------*/
    rc = pm_init(PM_VERIFIED | PM_UNVERIFIED | PM_CAVEAT | PM_GET_GROUPS, 
		 &pminfo, &pmgroupsinfo);
    TEST_PM_ERROR((char *)name, rc);

    if (myproc <= 1) fprintf(stderr,">>>pm_init() for ECMWF/OpenMP-tid#%d, pthread_self()=%d\n",tid+1,pthread_self());
  }

  if (!hpm_tid_init[tid]) {
    const int group = 60; /* pm_hpmcount2 */
    /*-- counters --
     case 60:
       strcpy(group_label, "pm_hpmcount2, Hpmcount group for computation intensity analysis");
       strcpy(label[0], "FPU executed FDIV instruction (PM_FPU_FDIV)");
       strcpy(label[1], "FPU executed multiply-add instruction (PM_FPU_FMA)");
       strcpy(label[2], "FPU0 produced a result (PM_FPU0_FIN)");
       strcpy(label[3], "FPU1 produced a result (PM_FPU1_FIN)");
       strcpy(label[4], "Processor cycles (PM_CYC)");
       strcpy(label[5], "FPU executed store instruction (PM_FPU_STF)");
       strcpy(label[6], "Instructions completed (PM_INST_CMPL)");
       strcpy(label[7], "LSU executed Floating Point load instruction (PM_LSU_LDF)");
    */
    pm_prog_t pmprog;
    pm_data_t pmdata;
    int i;

    /*---------------------*/
    /* set a default group */
    /*---------------------*/
    for (i=0; i<MAX_COUNTERS; i++) {
      pmprog.events[i] = COUNT_NOTHING;
    }
    pmprog.events[0] = group;
    
    /*-------------------------------------------------------------*/
    /* set the mode for user (not kernel) and thread (not process) */
    /*-------------------------------------------------------------*/
    pmprog.mode.w = 0;
    pmprog.mode.b.user = 1;
    pmprog.mode.b.process = 0;
    /* pmprog.mode.b.process = 1; */
    
    /*------------------------------------------*/
    /* for power-4 you have to use event groups */
    /*------------------------------------------*/
    pmprog.mode.b.is_group = 1;
    
    /*---------------------------------------------------*/
    /* set the mode to not to start counting immediately */
    /*---------------------------------------------------*/
    /* pmprog.mode.b.count = 1; */
    pmprog.mode.b.count = 0;
    
    /*-----------------------------------------*/
    /* initialize the group and start counting */
    /*-----------------------------------------*/
    hpm_tid_init[tid] = pthread_self(); /* Always > 0 */

    rc = pm_set_program_mythread(&pmprog); 
    TEST_PM_ERROR((char *)name, rc);

    rc = pm_start_mythread();
    TEST_PM_ERROR((char *)name, rc);
  }
}

static void
stopstart_hpm(int tid, drhook_key_t *pstop, drhook_key_t *pstart)
{
  const char *name = "stopstart_hpm";
  pm_data_t pmdata;
  int i, rc;

  /* if (numthreads > 1) pthread_mutex_lock(&hpm_lock); */

  if (!hpm_tid_init || !hpm_tid_init[tid-1]) init_hpm(tid);
  --tid;

  /*
  rc = pm_stop_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  rc = pm_get_data_mythread(&pmdata);
  TEST_PM_ERROR((char *)name, rc);

  if (pstop && pstop->counter_in) {
    for (i=0; i<MAX_COUNTERS; i++) {
      pstop->counter_sum[i] += (pmdata.accu[i] - pstop->counter_in[i]);
    }
  }

  if (pstart) {
    if (!pstart->counter_in ) pstart->counter_in  = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_in ));
    if (!pstart->counter_sum) pstart->counter_sum = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_sum));
     for (i=0; i<MAX_COUNTERS; i++) {
       pstart->counter_in[i] = pmdata.accu[i];
     }
  }

  /*
  rc = pm_start_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  /* if (numthreads > 1) pthread_mutex_unlock(&hpm_lock); */
}

static double
mflops_hpm(const drhook_key_t *keyptr)
{
  double mflops = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[4] > 0) {
    long long int sum = keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5];
    if (sum > 0)
      mflops = (sum * MCYCLES)/keyptr->counter_sum[4];
  }
  return mflops;
}

static double
mips_hpm(const drhook_key_t *keyptr)
{
  double mipsrate = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[4] > 0) {
    mipsrate = (keyptr->counter_sum[6] * MCYCLES)/keyptr->counter_sum[4];
  }
  return mipsrate;
}

static double
divpc_hpm(const drhook_key_t *keyptr)
{
  double divpc = 0;
  if (keyptr && keyptr->counter_sum) {
    long long int sum = keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5];
    if (sum > 0) divpc = (keyptr->counter_sum[0]*100.0)/sum;
  }
  return divpc;
}

static double
mflop_count(const drhook_key_t *keyptr)
{
  double sum = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[4] > 0) {
    sum = (keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5]) * 1e-6;
    if (sum < 0) sum = 0;
  }
  return sum;
}

static double
mip_count(const drhook_key_t *keyptr)
{
  double sum = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[4] > 0) {
    sum = keyptr->counter_sum[6] * 1e-6;
  }
  return sum;
}

#endif /* RS6K */

#endif /* HPM */
