/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*

Author: Dr. Umberto Modigliani, User Support.

*/

#ifndef __datecmd_H__
#define __datecmd_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#if ! defined(__alpha) && !defined(LINUX) && !defined(_AIX43) && !defined(linux) && !defined(CYGWIN) && !defined(MACOSX) && !defined(DARWIN)
#include <pfmt.h> 
#endif
#include <limits.h>


#include "myhdr.h"






/*******************************
 * Macros
 *******************************/



/*
#define	   DAY_PARSE_STRING		"%04ld%02ld%02ld"
#define	   HOUR_PARSE_STRING		"%04ld%02ld%02ld%02ld"
#define	   MIN_PARSE_STRING		"%04ld%02ld%02ld%02ld%02ld"
#define	   SEC_PARSE_STRING		"%04ld%02ld%02ld%02ld%02ld%02ld"
#define	   STR_LENGTH			80
*/




/*******************************
 *   Constants
 *******************************/

#if defined(_ABI64) 
static char *DAY_PARSE_STRING             =     "%04d%02d%02d";
static char *HOUR_PARSE_STRING            =     "%04d%02d%02d%02d";
static char *MIN_PARSE_STRING             =  	"%04d%02d%02d%02d%02d";
static char *SEC_PARSE_STRING             =     "%04d%02d%02d%02d%02d%02d";
static char *INT32FRM                     =     "%d\n";
#else
static char *DAY_PARSE_STRING             =     "%04ld%02ld%02ld";
static char *HOUR_PARSE_STRING            =     "%04ld%02ld%02ld%02ld";
static char *MIN_PARSE_STRING             =  	"%04ld%02ld%02ld%02ld%02ld";
static char *SEC_PARSE_STRING             =     "%04ld%02ld%02ld%02ld%02ld%02ld";
static char *INT32FRM                     =     "%ld\n";
#endif

static const _int32_t 	ARGC_VALUE        =	3; 


static const _int32_t 	DAY_DATE_LENGTH   =	8;
static const _int32_t 	HOUR_DATE_LENGTH  =	10;
static const _int32_t 	MIN_DATE_LENGTH   =	12;
static const _int32_t 	SEC_DATE_LENGTH   =	14;


static const _int32_t 	DAY_PARSE_ARGS	  =	3;
static const _int32_t 	HOUR_PARSE_ARGS	  =	4;
static const _int32_t 	MIN_PARSE_ARGS	  =	5;
static const _int32_t 	SEC_PARSE_ARGS	  =	6;


enum { FLAG_DAY=0, FLAG_HOUR=1, FLAG_MIN=2, FLAG_SEC=3 };



/*

enum { VALUE_DAY=0, VALUE_HOUR=1, VALUE_MIN=2, VALUE_SEC=3 };

static const _int32_t   FLAG_DAY        = VALUE_DAY;
static const _int32_t   FLAG_HOUR       = VALUE_HOUR;
static const _int32_t   FLAG_MIN        = VALUE_MIN;
static const _int32_t   FLAG_SEC        = VALUE_SEC;

*/



#endif /* __datecmd_H__ */
