/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/* fnec.c */

#include <stdlib.h>

#ifdef __NEC__
void exit_(const int *exit_code) { exit(exit_code ? *exit_code : 0 ); }
#endif

#ifdef __NEC__
void getenv_(const char *s, char *value,
	/* Hidden arguments */
	int slen,
	const int valuelen)
{
  ec_getenv_(s, value, slen, valuelen);
}
#endif

#ifdef __NEC__
void sleep_(const int *nsec) { (void)ec_sleep_(nsec); }

void flush_(const int *io) { } /* temporary fix */
#endif


#ifdef __NEC__
int hostnm_(char a[], int alen) { ec_gethostname_(a,alen); return 0; }
#endif
