/*
 * (C) Copyright 2024- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#ifndef DRHOOK_PAPI
#define DRHOOK_PAPI

#include <papi.h>

#define  MAXNPAPICNTRS 4

int drhook_papi_init(int rank);
int drhook_papi_num_counters();
int drhook_papi_max_num_counters();
int drhook_papi_max_name_len();
void drhook_papi_counter_name(int c, char* event_name);
void drhook_papi_add_counter_name(const char* counter_name);
long_long drhook_papi_read(int counterId);
int drhook_papi_readAll(long_long* counterArray);

/* implemented in fortran */
int drhook_run_omp_parallel_papi_startup(int* drhook_papi_event_set, int nthreads, int* rcout);

/* a = b - c  
if  b or c == NULL means use current readings
 */      
void drhook_papi_subtract(long_long* a, long_long* b, long_long* c);

/* a = b + c 
if a==NULL,  b=b+c */
void drhook_papi_add(long_long* a, long_long* b, long_long* c);

/* a = b */
void drhook_papi_cpy(long_long* a, long_long* b);

/* a=0 */
void drhook_papi_bzero(long_long* a);

void drhook_papi_print(char* s, long_long* a, int header);

#endif
