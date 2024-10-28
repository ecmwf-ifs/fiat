/*
 * (C) Copyright 2024- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "drhook_papi.h"
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "oml.h"

#define STD_MSG_LEN 4096

static int silent = 0;

int*  drhook_papi_event_set=NULL;
enum {
  drhook_papi_notstarted,
  drhook_papi_running,
  drhook_papi_failed
};
int    drhook_papi_state=drhook_papi_notstarted;
int    drhook_papi_rank=0; /* C style! */

static int papi_counter_event_codes[MAXNPAPICNTRS];
static char* papi_counter_names[MAXNPAPICNTRS];
static int papi_counters_count = 0;

/* function to use for thread id 
   - it should be better than omp_get_thread_num!
*/
unsigned long papi_safe_thread_num(){
  return oml_my_thread()-1;
}

int drhook_papi_max_num_counters() {
  return MAXNPAPICNTRS;
}

int drhook_papi_max_name_len() {
  return PAPI_MAX_STR_LEN;
}

void drhook_papi_counter_name(int c, char* event_name){
  PAPI_event_code_to_name(papi_counter_event_codes[c], event_name);
}

void drhook_papi_cpy(long_long* a,long_long* b){
  for (int i=0;i<drhook_papi_num_counters();i++){
    a[i]=b[i];
  }
}

void drhook_papi_bzero(long_long* a){
  for (int i=0;i<drhook_papi_num_counters();i++){
    a[i]=0;
  }
}

void drhook_papi_print(char* s, long_long* a, int header){
  char msg[STD_MSG_LEN];
  if (header>0){
    char fmt[STD_MSG_LEN];
    sprintf(fmt,"%%%lds",strlen(s));
    sprintf(msg,fmt," ");
    for (int i=0;i<drhook_papi_num_counters();i++) {
      char event_name[PAPI_MAX_STR_LEN];

      PAPI_event_code_to_name(papi_counter_event_codes[i], event_name);
      sprintf(&msg[strlen(msg)]," %16s",event_name);
    }
    printf("%s\n",msg);
  }
        
  sprintf(msg,"%s",s);
  for (int i=0;i<drhook_papi_num_counters();i++){
    sprintf(&msg[strlen(msg)]," %16lld",a[i]);
  }
  printf("%s\n",msg);
}

/* a = b - c  (b or c == NULL means use current readings) */      
void drhook_papi_subtract(long_long* a, long_long* b, long_long* c){
  if (b==NULL || c==NULL){
    long_long * tmp=alloca(drhook_papi_num_counters() * sizeof(long_long));
    drhook_papi_readAll(tmp);
    if (b==NULL)b=tmp;
    if (c==NULL)c=tmp;
  }
  for (int i=0;i<drhook_papi_num_counters();i++){
    /*    printf("papi_subtract_from_current: %d %lld - %lld = %lld\n",i,b[i],c[i],b[i]-c[i]);*/
    a[i]=b[i]-c[i];
  }
}

/* a = b + c 
if a==NULL,  b=b+c */
void drhook_papi_add(long_long* a,long_long* b, long_long* c){
  if (a==NULL) a=b;
  for (int i=0;i<drhook_papi_num_counters();i++){
    /*printf("papi_add: %d %lld + %lld = %lld \n",i,b[i],c[i],b[i]+c[i]);*/
    a[i]=b[i]+c[i];
  }
}

// number of counters available to read
int drhook_papi_num_counters(){
  return papi_counters_count;
}

void drhook_papi_add_counter_name(const char* counter_name) {
  papi_counter_names[papi_counters_count] = counter_name;
  papi_counters_count++;
}

long_long drhook_papi_read(int counterId){
  long_long * tmp=alloca(drhook_papi_num_counters() * sizeof (long_long *));
  int err=drhook_papi_readAll(tmp);
  return tmp[counterId];
}

int drhook_papi_readAll(long_long * counterArray){
  if (drhook_papi_state!=drhook_papi_running){
    printf("DRHOOK:PAPI: Error reading counters, papi is not running\n");
    exit (1);
  }
  if (!drhook_papi_event_set){
    printf("DRHOOK:PAPI: Error reading counters, eventset\n");
    exit (1);
  }
  int err=PAPI_read(drhook_papi_event_set[papi_safe_thread_num()],counterArray);
  if (err!=PAPI_OK){
    printf("DRHOOK:PAPI:PAPI_read: Error reading counters, thread=%ld es=%d %s\n",papi_safe_thread_num(),drhook_papi_event_set[papi_safe_thread_num()],PAPI_strerror(err));
  }
#if defined(DEBUG)
  drhook_papi_print("readAll:",counterArray,0);
#endif
  return err;
}

void drhook_papi_readall_(long_long * counterArray){
  drhook_papi_readAll(counterArray);
}

extern void *malloc_drhook(size_t size);

/* return 1 if papi can be used after the call */
int drhook_papi_init(int rank){
  int lib_version;
  char pmsg[STD_MSG_LEN];
  int paperr=-1;
  char *env;

  if (drhook_papi_state==drhook_papi_running) return 1;
  if (drhook_papi_state==drhook_papi_failed) return 0;

  drhook_papi_rank=rank;

  if (oml_in_parallel()){
    printf("DRHOOK:PAPI: Error, tried to initialise from a parallel region :-(\n");
    return 0;
  }

  env = getenv("DR_HOOK_SILENT");
  silent = env ? atoi(env) : silent;
  
  paperr=PAPI_library_init(PAPI_VER_CURRENT);
  if (paperr != PAPI_VER_CURRENT){
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI:PAPI_library_init: ret code=%d version loaded =%d ", paperr,PAPI_VER_CURRENT);
    printf("%s\n",pmsg);
    if (paperr > 0) {
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, library version mismatch between compilation and run!\n");
      printf("%s\n",pmsg);
      return 0;
    }
    if (paperr == PAPI_EINVAL){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, PAPI_EINVAL\n");
      printf("%s\n",pmsg);
      return 0;
    }
    if (paperr == PAPI_ENOMEM){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, PAPI_ENOMEM\n");
      printf("%s\n",pmsg);
      return 0;
    }
    if (paperr == PAPI_ESBSTR){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, PAPI_ESBSTR\n");
      printf("%s\n",pmsg);
      return 0;
    }
    if (paperr == PAPI_ESYS){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, PAPI_ESYS\n");
      printf("%s\n",pmsg);
      return 0;
    }
    else {
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, unknown error code: %d\n", paperr);
      printf("%s\n",pmsg);
      return 0;
    }
  }
    
  lib_version = PAPI_get_opt( PAPI_LIB_VERSION, NULL );

  int nthreads=oml_get_max_threads();

  paperr=PAPI_thread_init(papi_safe_thread_num);
  
  if( paperr != PAPI_OK ){
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, thread init failed (%s)",PAPI_strerror(paperr));
    printf("%s\n",pmsg);
    return 0;
  }

  snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Version %d.%d.%d initialised with %d threads",
           PAPI_VERSION_MAJOR( lib_version ),
           PAPI_VERSION_MINOR( lib_version ),
           PAPI_VERSION_REVISION( lib_version ),
           nthreads);
  
  if (drhook_papi_rank==0 && !silent) printf("%s\n",pmsg);

  drhook_papi_event_set=malloc_drhook(nthreads*sizeof(int));

  int rcout;
  drhook_run_omp_parallel_papi_startup(drhook_papi_event_set, nthreads, &rcout);
  if (rcout)
    return 0;

  for (int i=0; i < drhook_papi_max_num_counters(); i++)
    free(papi_counter_names[i]);

  drhook_papi_state=drhook_papi_running;
  if (drhook_papi_rank==0 && !silent) printf("DRHOOK:PAPI: Initialisation sucess\n");
  return 1;
}

int drhook_papi_start_threads(int* events){
  int thread=papi_safe_thread_num();
  int papiErr;
  char pmsg[STD_MSG_LEN];

  events[thread]=PAPI_NULL;
  papiErr=PAPI_create_eventset(&events[thread]);
  if (papiErr != PAPI_OK){
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, create event set failed (%s) \n",PAPI_strerror(papiErr));
    printf("%s\n",pmsg);
    return 0;
  }
  
  if (!silent) printf("DRHOOK:PAPI: Event set %d created for thread %d\n",events[thread],thread);
  
  if (!silent && drhook_papi_rank==0 && thread==0)
    printf("DRHOOK:PAPI: Attempting to add events to event set:\n");

  for (int counter=0; counter < drhook_papi_num_counters(); counter ++){
    int eventCode;

    if (!silent && drhook_papi_rank==0 && thread==0) {
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: %s", papi_counter_names[counter]);
      printf("%s\n",pmsg);
    }
    
    papiErr=PAPI_event_name_to_code(papi_counter_names[counter], &eventCode);
    if (papiErr != PAPI_OK){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, event name to code failed for %s (%s)", papi_counter_names[counter], PAPI_strerror(papiErr));
      printf("%s\n",pmsg);
      PAPI_perror("initPapi");
      return 0;
    }

    papi_counter_event_codes[counter] = eventCode;
    papiErr=PAPI_add_event(events[thread],eventCode);
    if (papiErr!=PAPI_OK){
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, add_event failed: %d (%s)",papiErr,PAPI_strerror(papiErr));
      printf("%s\n",pmsg);
      if (papiErr == PAPI_EINVAL)
        printf("Invalid argument\n");
      else if (papiErr == PAPI_ENOMEM)
        printf("Out of memory\n");
      else if (papiErr == PAPI_ENOEVST)
        printf("EventSet does not exist\n");
      else if (papiErr == PAPI_EISRUN)
        printf("EventSet is running\n");
      else if (papiErr == PAPI_ECNFLCT)
        printf("Conflict\n");
      else if (papiErr == PAPI_ENOEVNT)
        printf("Preset not available\n");
      return 0;
    }
    else {
#if defined(DEBUG)
      snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Added code=%d to Event set %d",eventCode, events[thread]);
      if (thread==0) {
        printf("%s\n",pmsg);
      }
#endif
    }
  }
  
  int number = drhook_papi_num_counters();
  int* checkEvents=malloc(drhook_papi_num_counters()*sizeof(int));
  papiErr = PAPI_list_events(events[thread], checkEvents, &number);
  if (papiErr != PAPI_OK){
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error querying events - %d=%s",papiErr,PAPI_strerror(papiErr));
    printf("%s\n",pmsg);
    return 0;
  }
#if defined(DEBUG)
  for (int counter=0;counter<number;counter++) {
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Ev: %d=%d",counter,events[counter]);
    printf("%s\n",pmsg);
  }
#endif
  
  if (number != drhook_papi_num_counters()){
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error checking events - expected=%d got=%d",drhook_papi_num_counters(),number);
    printf("%s\n",pmsg);
  }
  
  papiErr=PAPI_start(events[thread]);
  
  if (papiErr != PAPI_OK) {
    snprintf(pmsg,STD_MSG_LEN,"DRHOOK:PAPI: Error, starting counters failed (%d=%s)",papiErr,PAPI_strerror(papiErr));
    printf("%s\n",pmsg);
    return 0;
  }

#if defined(DEBUG)
  printf("DRHOOK:PAPI: Started counting for thread %d \n",thread);
#endif
  
  return 1;
}
