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
void exit_(const int *exit_code) {
  exit(exit_code ? *exit_code : 0 );
}

void getenv_(const char *s, char *value,
             /* Hidden arguments */
             int slen,
             const int valuelen)
{
  extern void ec_getenv(const char *s, char *value, /* Hidden arguments */ int slen, const int valuelen);
  ec_getenv(s, value, slen, valuelen);
}

void sleep_(const int *nsec) {
	extern unsigned int ec_sleep(const int *nsec);
	(void)ec_sleep(nsec);
}

void flush_(const int *io) {
	/* temporary fix */
}

int hostnm_(char a[], int alen) {
	extern void ec_gethostname(char a[],  /* Hidden argument */ int alen);
	ec_gethostname(a,alen);
	return 0;
}

#endif
