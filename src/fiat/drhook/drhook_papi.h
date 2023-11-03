#ifndef DRHOOK_PAPI
#define DRHOOK_PAPI
#ifdef HKPAPI
#warning Compiling with PAPI

#ifdef _OPENMP
#include <omp.h>
#endif

#include <papi.h>

#define  NPAPICNTRS 2

int drhook_papi_init();
int drhook_papi_num_counters();
char * drhook_papi_counter_name(int c,int t);
long_long drhook_papi_read(int counterId);
int drhook_papi_readAll(long_long * counterArray);

/* a = b - c  
if  b or c == NULL means use current readings
 */      
void drhook_papi_subtract(long_long* a, long_long* b, long_long* c);

/* a = b + c 
if a==NULL,  b=b+c */
void drhook_papi_add(long_long* a,long_long* b,long_long* c);

/* a = b */
void drhook_papi_cpy(long_long* a,long_long* b);

/* a=0 */
void drhook_papi_bzero(long_long* a);

void drhook_papi_print(char * s,long_long* a,int header);

#endif
#endif
