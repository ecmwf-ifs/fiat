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


#include "julian.h"
#include "datecmd.h"
#include "julian_lib.c"


/*******************************
 *  Prototypes
 *******************************/ 


static void           usage(void);
static exit_t         check_length(const char *const, const _int32_t length);
static exit_t         is_number(const char *);
static exit_t         is_signed_number(const char *);
static exit_t         check_args(const char *const date, const char *const incr, const _int32_t *const flag, yyyymmdd_t *const date_ptr, hhmmss_t *const hms_ptr, _int32_t *const incr_num);

static exit_t         main_function(const char *const date, const char  *const incr, const _int32_t *const flag);



/*******************************
 * Externals
 *******************************/

/* None */


/*******************************
 *   Globals
 *******************************/
 
static char rcsid[] = "$Header$ Author: Dr. Umberto Modigliani, User Support.";

static char *exec_name = "dateincr";





/*******************************
 *  Functions
 *******************************/


static void 
usage(void)
{
	err_msg("\n");
	err_msg("Usage: %s [switch]  basedate        increment", exec_name);
	err_msg("");
	err_msg("       %s           YYYYMMDD        [+|-]days" , exec_name);
	err_msg("       %s -d        YYYYMMDD        [+|-]days" , exec_name);
	err_msg("       %s -h        YYYYMMDDhh      [+|-]hours" , exec_name);
	err_msg("       %s -m        YYYYMMDDhhmm    [+|-]minutes" , exec_name);
	err_msg("       %s -s        YYYYMMDDhhmmss  [+|-]seconds" , exec_name);
	err_msg("");
	err_msg("       Input restrictions:");
	err_msg("");
	err_msg("                    0000 <= YYYY < 9999");
	err_msg("                    00 <= hh < 24, 00 <= mm < 60, 00 <= ss < 60");
	err_msg("                    %+ld <= days, hours, minutes, seconds <= %+ld", LONG_MIN, LONG_MAX);
	err_msg("");
	return ;


} /* end of usage */



static exit_t
is_signed_number(const char *s)
/*************************************************
?  Check if the string given is a signed number.
=  EC_OK if this is a signed number, EC_NAN otherwise
**************************************************/
{
	if( !s ) return (EC_NAN);
	if( !*s ) return (EC_NAN);


	/*
	 *	In case of a number prefixed by a sign
	 */  
	if( (*s=='+' || *s=='-' ) && isdigit(s[1]) ) s++; 


	while( *s && isdigit(*s) ) 
  		s++;

	return (EC_OK);


} /* end of is_signed_number */



static exit_t
is_number(const char *s)
/*************************************************
?  Check if the string given is a pure number.
=  EC_OK if this is a pure number, EC_NAN otherwise
**************************************************/
{
	if( !s ) return (EC_NAN);
	if( !*s ) return (EC_NAN);


	while( *s && isdigit(*s) ) 
  		s++;

	return (EC_OK);

  
} /* end of is_number */



static exit_t
check_length(const char *const string, const _int32_t length)
{
	if ( strlen(string) == length)
		return (EC_OK);
	else
		return (EC_WRGLEN);


} /* end of check_length */




static exit_t
check_args(const char *const date,  const char *const incr, const _int32_t *const flag, yyyymmdd_t *const date_ptr, hhmmss_t *const hms_ptr, _int32_t *const incr_num_ptr)
{
	_int64_t       test = 0;


	if ( is_number(date) != EC_OK ) {
		err_msg("%s: Invalid date string (%s) given: it is not a number", exec_name, date);
		return (EC_NAN);
	}
	
        /*
	 *
	 * According to flag apply the appropriate check
	 *
	 */
	switch (*flag) {

	case FLAG_DAY:
		if ( check_length(date, DAY_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date, DAY_DATE_LENGTH);
			return (EC_DATELEN);
		}
		break;

	case FLAG_HOUR:
		if ( check_length(date, HOUR_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date, HOUR_DATE_LENGTH);
			return (EC_DATELEN);
		}
		break;

	case FLAG_MIN:
		if ( check_length(date, MIN_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date, MIN_DATE_LENGTH);
			return (EC_DATELEN);
		}
		break;

	case FLAG_SEC:
		if ( check_length(date, SEC_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date, SEC_DATE_LENGTH);
			return (EC_DATELEN);
		}
		break;
	}



        /*
	 *
	 * Check the correctness of incr
	 *
	 */	
	if ( is_signed_number(incr) != EC_OK ) {
		err_msg("%s: Invalid increment string (%s) given: it is not a number", exec_name, incr);
		return (EC_NAN);
	}
	

        /*
	 * check if incr is in the allowed range 
	 */
#if defined(_ABI64)
	if ( sscanf(incr,"%ld", &test) != 1 )
#else
	if ( sscanf(incr,"%lld", &test) != 1 )
#endif
		err_quit("%s: sscanf error", exec_name);
			

	if ( test > LONG_MAX || test < LONG_MIN) {
		err_msg("%s: Increment %s out of range", exec_name, incr);
		return (EC_RANGE);
	}

        /* Convert to the appropriate type */
	*incr_num_ptr = (_int32_t) test;


        /*
	 *
	 * According to flag set the appropriate structures and check thier correctness
	 *
	 */
	switch (*flag) {

	case FLAG_DAY:
		
		if ( sscanf(date, DAY_PARSE_STRING, &(date_ptr->year), &(date_ptr->month), &(date_ptr->day)) != DAY_PARSE_ARGS  ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date);
			return (EC_DATEFMT);
		}
		if ( is_date(date_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date);
			return (EC_DATEINV);
		}
		break;

	case FLAG_HOUR:
		if ( sscanf(date, HOUR_PARSE_STRING, &(date_ptr->year), &(date_ptr->month), &(date_ptr->day), &(hms_ptr->hour)) != HOUR_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date);
		      	return (EC_TIMEINV);	
		}
		break;

	case FLAG_MIN:
		if ( sscanf(date, MIN_PARSE_STRING, &(date_ptr->year), &(date_ptr->month), &(date_ptr->day), &(hms_ptr->hour), &(hms_ptr->min)) != MIN_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date);
		      	return (EC_TIMEINV);	
		}
		break;

	case FLAG_SEC:
		if ( sscanf(date, SEC_PARSE_STRING, &(date_ptr->year), &(date_ptr->month), &(date_ptr->day), &(hms_ptr->hour), &(hms_ptr->min), &(hms_ptr->sec)) != SEC_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date);
		      	return (EC_TIMEINV);	
		}
		break;

	}

	
	return (EC_OK);


} /* end of check_args */




static exit_t
main_function(const char *const date, const char *const incr, const _int32_t *const flag)
{
	yyyymmdd_t        date_str = {0,0,0}, new_date_str = {0,0,0};
	hhmmss_t          hms_str = {0,0,0}, new_hms_str = {0,0,0};
	_int32_t          incr_num = 0;
	exit_t            exit_status = 0;




	/*
	 *    Check arguments
	 */
	if (  (exit_status = check_args(date, incr, flag, &date_str, &hms_str, &incr_num)) != EC_OK ) {
		usage();
		return (exit_status);
	}





	switch (*flag) {

	case FLAG_DAY:
		if ( (exit_status = addDays(&date_str, incr_num, &new_date_str)) == EC_OK) {
			if ( new_date_str.year >= 0 )
#if defined(_ABI64)
				(void) printf("%04d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day);
#else
			        (void) printf("%04ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day);
#endif
			else
#if defined(_ABI64)
				(void) printf("%+05d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day);
#else
	 		        (void) printf("%+05ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day);
#endif
		} else
			return (exit_status);
		
		break;

	case FLAG_HOUR:
		if ( (exit_status = addHours(&date_str, &hms_str, incr_num, &new_date_str, &new_hms_str)) == EC_OK) {
			if ( new_date_str.year >= 0 )
#if defined(_ABI64)
				(void) printf("%04d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour);	
#else
			        (void) printf("%04ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour);
#endif
			else
#if defined(_ABI64)
				(void) printf("%+05d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour);
#else
			        (void) printf("%+05ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour);
#endif	
		} else
			return (exit_status);
		
		break;

	case FLAG_MIN:
		if ( (exit_status = addMinutes(&date_str, &hms_str, incr_num, &new_date_str, &new_hms_str)) == EC_OK) {
			if ( new_date_str.year >= 0 )
#if defined(_ABI64)				
				(void) printf("%04d%02d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour, new_hms_str.min);
#else
			        (void) printf("%04ld%02ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour, new_hms_str.min);
#endif
			else
#if defined(_ABI64)		
				(void) printf("%+05d%02d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour, new_hms_str.min);
#else
			        (void) printf("%+05ld%02ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour, new_hms_str.min);
#endif	
		} else
			return (exit_status);
		
		break;

	case FLAG_SEC:
		
		if ( (exit_status = addSeconds(&date_str, &hms_str, incr_num, &new_date_str, &new_hms_str)) == EC_OK) {
			if ( new_date_str.year >= 0 )
#if defined(_ABI64)
				(void) printf("%04d%02d%02d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour,  new_hms_str.min,  new_hms_str.sec);	
#else
			        (void) printf("%04ld%02ld%02ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour,  new_hms_str.min,  new_hms_str.sec);
#endif
			else
#if defined(_ABI64)
				(void) printf("%+05d%02d%02d%02d%02d%02d\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour,  new_hms_str.min,  new_hms_str.sec);
#else
			        (void) printf("%+05ld%02ld%02ld%02ld%02ld%02ld\n", new_date_str.year, new_date_str.month, new_date_str.day, new_hms_str.hour,  new_hms_str.min,  new_hms_str.sec);
#endif
		} else
			return (exit_status);
		
		break;
	}
	
	return (EC_OK);


} /* end of main_function */



/*******************************
 *  Main
 *******************************/

static int
xmain(int argc, char *argv[])
{

	_int32_t        flag = FLAG_DAY; /* Default: Day */
	_int32_t        opt_ch = 0;
	extern int      optind;
	extern int      opterr;
	exit_t          exit_status = 0;
        char            *date = NULL;
        char            *incr = NULL;


	opterr = 0;
#if ! defined(__alpha) && !defined(LINUX) && !defined(_AIX43) && !defined(linux)
	if ( setlabel(exec_name) !=0 )
		err_quit("setlabel error");
#endif
        /*
	 *
	 * putenv calls malloc and therefore a block is dynamically allocated
	 *
	 */
	if ( putenv("NOMSGSEVERITY=1") != 0 )
		err_quit("putenv error");
		
#if defined(LINUX) || defined(linux)
        /*
       *
       * defines POSIXLY_CORRECT to get the correct getopt behaviour
       *
       */
      if ( putenv("POSIXLY_CORRECT=1") != 0 )
            err_quit("putenv error");
#endif


	while( ( opt_ch = getopt(argc, argv, "dhms")) != -1)

		switch (opt_ch) {
		
		case 'd':       /* Day  */
			flag = FLAG_DAY;
			break;
		
		case 'h':	/* Hour  */
			flag = FLAG_HOUR;
			break;
		case 'm':      /* Minute */
			flag = FLAG_MIN;
			break;
		case 's':      /* Second */
			flag = FLAG_SEC;
			break;

		default:
			usage();
			return (EC_WRGOPT);
		}



        argc -= (optind - 1);
        argv += (optind - 1);


	if ( argc != ARGC_VALUE ) {
		err_msg("%s: Wrong number of arguments", exec_name);
		usage();
		return (EC_WRGPAR);
	}


	/*
	 *	Define main parameters
	 */       
   
	date = strdup(argv[1]);
	incr = strdup(argv[2]);
	

	
	/*
	 *	Call the main routine
	 */	
	if ( ( exit_status = main_function(date, incr, &flag)) != EC_OK) {
		return (exit_status);
	} else {
		return (EC_OK);
	}



} /* end of main */

