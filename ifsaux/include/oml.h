/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#ifndef _OML_H_
#define _OML_H_

/* oml.h */

typedef long long int oml_lock_t; /* i.e. 64-bit integer */

extern int  oml_my_thread();         // Equivalent to omp_get_thread_num() + 1
extern int  oml_get_max_threads();   // Equivalent to omp_get_max_threads()
extern int  oml_get_num_threads();   // Equivalent to omp_get_num_threads()
extern int  oml_in_parallel();

extern void oml_set_debug(int);
extern int  oml_get_debug();
extern void oml_init_lock();
extern void oml_init_lockid(oml_lock_t*);
extern void oml_init_lockid_with_name(oml_lock_t*, const char* name);
extern void oml_set_lock();
extern void oml_set_lockid(oml_lock_t*);
extern void oml_unset_lock();
extern void oml_unset_lockid(oml_lock_t*);
extern int  oml_test_lock();
extern int  oml_test_lockid(oml_lock_t*);

#endif /* _OML_H_ */
