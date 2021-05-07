/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/* ec_cargs.h */

/* Author: Sami Saarinen, ECMWF, 27-Apr-2006 */

#if defined(__cplusplus)
extern "C" {
#endif

/* The following two as in C-main : "int main(int argc, char *argv[])" */

int ec_argc(void);
char **ec_argv(void);

#if defined(__cplusplus)
}
#endif
