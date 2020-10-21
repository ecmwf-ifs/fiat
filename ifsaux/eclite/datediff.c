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
static exit_t         check_args(const char *const date1, const char *const date2 , const _int32_t *const flag, yyyymmdd_t *const date1_ptr, hhmmss_t *const hms1_ptr, yyyymmdd_t *const date2_ptr, hhmmss_t *const hms2_ptr);
static exit_t         main_function(const char *const date1, const char  *const date2, const _int32_t *const flag);




/*******************************
 * Externals
 *******************************/

/* None */




/*******************************
 *   Globals
 *******************************/
 
static char rcsid[] = "$Header$ Author: Dr. Umberto Modigliani, User Support.";

static char *exec_name = "datediff";




/*******************************
 *  Functions
 *******************************/


static void 
usage(void)
{
	err_msg("\n");
	err_msg("Usage: %s [switch]  first_date      second_date", exec_name);
	err_msg("");
	err_msg("       %s           YYYYMMDD        YYYYMMDD" , exec_name);
	err_msg("       %s -d        YYYYMMDD        YYYYMMDD" , exec_name);
	err_msg("       %s -h        YYYYMMDDhh      YYYYMMDDhh" , exec_name);
	err_msg("       %s -m        YYYYMMDDhhmm    YYYYMMDDhhmm" , exec_name);
	err_msg("       %s -s        YYYYMMDDhhmmss  YYYYMMDDhhmmss" , exec_name);
	err_msg("");
	err_msg("       Input restrictions:");
	err_msg("");
	err_msg("                    0000 <= YYYY < 9999");
	err_msg("                    00 <= hh < 24, 00 <= mm < 60, 00 <= ss < 60");;
	err_msg("");
	return ;


} /* end of usage */



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
check_args(const char *const date1, const char *const date2 , const _int32_t *const flag, yyyymmdd_t *const date1_ptr, hhmmss_t *const hms1_ptr, yyyymmdd_t *const date2_ptr, hhmmss_t *const hms2_ptr)
{

	if ( is_number(date1) != EC_OK ) {
		err_msg("%s: Invalid date string (%s) given: it is not a number", exec_name, date1);
		return (EC_NAN);
	}

	if ( is_number(date2) != EC_OK ) {
		err_msg("%s: Invalid date string (%s) given: it is not a number", exec_name, date2);
		return (EC_NAN);
	}
	
	/*
	 *
	 * According to flag apply the appropriate check
	 *
	 */
	switch (*flag) {

	case FLAG_DAY:

		if ( check_length(date1, DAY_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date1, DAY_DATE_LENGTH);
			return (EC_DATELEN);
		}
	
	
		if ( check_length(date2, DAY_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date2, DAY_DATE_LENGTH);
			return (EC_DATELEN);
		}

		break;

	case FLAG_HOUR:

		if ( check_length(date1, HOUR_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date1, HOUR_DATE_LENGTH);
			return (EC_DATELEN);
		}


		if ( check_length(date2, HOUR_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date2, HOUR_DATE_LENGTH);
			return (EC_DATELEN);
		}

		break;

	case FLAG_MIN:

		if ( check_length(date1, MIN_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date1, MIN_DATE_LENGTH);
			return (EC_DATELEN);
		}


		if ( check_length(date2, MIN_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date2, MIN_DATE_LENGTH);
			return (EC_DATELEN);
		}

		break;

	case FLAG_SEC:

		if ( check_length(date1, SEC_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date1, SEC_DATE_LENGTH);
			return (EC_DATELEN);
		}


		if ( check_length(date2, SEC_DATE_LENGTH) != EC_OK ) {
			err_msg("%s: Invalid date string (%s) given: it has a wrong length (!= %d)", exec_name, date2, SEC_DATE_LENGTH);
			return (EC_DATELEN);
		}

		break;
	}



	/*
	 *
	 * According to flag set the appropriate structures and check thier correctness
	 *
	 */
	switch (*flag) {

	case FLAG_DAY:
		
		if ( sscanf(date1, DAY_PARSE_STRING, &(date1_ptr->year), &(date1_ptr->month), &(date1_ptr->day)) != DAY_PARSE_ARGS  ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date1);
			return (EC_DATEFMT);
		}
		if ( is_date(date1_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date1);
			return (EC_DATEINV);
		}


		if ( sscanf(date2, DAY_PARSE_STRING, &(date2_ptr->year), &(date2_ptr->month), &(date2_ptr->day)) != DAY_PARSE_ARGS  ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date2);
			return (EC_DATEFMT);
		}
		if ( is_date(date2_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date2);
			return (EC_DATEINV);
		}

		break;

	case FLAG_HOUR:

		if ( sscanf(date1, HOUR_PARSE_STRING, &(date1_ptr->year), &(date1_ptr->month), &(date1_ptr->day), &(hms1_ptr->hour)) != HOUR_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date1);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date1_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date1);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms1_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date1);
		      	return (EC_TIMEINV);	
		}


		if ( sscanf(date2, HOUR_PARSE_STRING, &(date2_ptr->year), &(date2_ptr->month), &(date2_ptr->day), &(hms2_ptr->hour)) != HOUR_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date2);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date2_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date2);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms2_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date2);
		      	return (EC_TIMEINV);	
		}

		break;

	case FLAG_MIN:

		if ( sscanf(date1, MIN_PARSE_STRING, &(date1_ptr->year), &(date1_ptr->month), &(date1_ptr->day), &(hms1_ptr->hour), &(hms1_ptr->min)) != MIN_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date1);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date1_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date1);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms1_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date1);
		      	return (EC_TIMEINV);	
		}


		if ( sscanf(date2, MIN_PARSE_STRING, &(date2_ptr->year), &(date2_ptr->month), &(date2_ptr->day), &(hms2_ptr->hour), &(hms2_ptr->min)) != MIN_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date2);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date2_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date2);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms2_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date2);
		      	return (EC_TIMEINV);	
		}

		break;

	case FLAG_SEC:

		if ( sscanf(date1, SEC_PARSE_STRING, &(date1_ptr->year), &(date1_ptr->month), &(date1_ptr->day), &(hms1_ptr->hour), &(hms1_ptr->min), &(hms1_ptr->sec)) != SEC_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date1);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date1_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date1);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms1_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date1);
		      	return (EC_TIMEINV);	
		}


		if ( sscanf(date2, SEC_PARSE_STRING, &(date2_ptr->year), &(date2_ptr->month), &(date2_ptr->day), &(hms2_ptr->hour), &(hms2_ptr->min), &(hms2_ptr->sec)) != SEC_PARSE_ARGS ) {
			err_msg("%s: Invalid date format (%s)", exec_name, date2);
		       	return (EC_DATEFMT);
		}
		if ( is_date(date2_ptr) != EC_OK ) {
			err_msg("%s: Date incorrect (%s)", exec_name, date2);
		       	return (EC_DATEINV);	
		}
		if ( is_hms(hms2_ptr) != EC_OK ) {
			err_msg("%s: Time incorrect (%s)", exec_name, date2);
		      	return (EC_TIMEINV);	
		}

		break;

	}

	
	return (EC_OK);


} /* end of check_args */




static exit_t
main_function(const char *const date1, const char *const date2, const _int32_t *const flag)
{
	yyyymmdd_t        date1_str = {0,0,0}, date2_str = {0,0,0};
	hhmmss_t          hms1_str = {0,0,0}, hms2_str = {0,0,0};
	_int32_t          diff = 0;
	exit_t            exit_status = 0;




	/*
	 *    Check arguments
	 */
	if (  (exit_status = check_args(date1, date2, flag, &date1_str, &hms1_str, &date2_str, &hms2_str)) != EC_OK ) {
		usage();
		return (exit_status);
	}



	/*
	 *
	 * According to flag call the appropriate function
	 *
	 */
	switch (*flag) {

	case FLAG_DAY:

		if ( (exit_status = dateMinusDate(&date1_str, &date2_str, &diff)) == EC_OK) {	
			(void) printf(INT32FRM, diff);
		} else
			return (exit_status);
		
		break;

	case FLAG_HOUR:

		if ( (exit_status = hour_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, &diff)) == EC_OK) {	
			(void) printf(INT32FRM, diff);
		} else
			return (exit_status);
		
		break;

	case FLAG_MIN:

		if ( (exit_status = min_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, &diff)) == EC_OK) {	
			(void) printf(INT32FRM, diff);
		} else
			return (exit_status);
		
		break;

	case FLAG_SEC:

		if ( (exit_status = sec_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, &diff)) == EC_OK) {	
			(void) printf(INT32FRM, diff);
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
        char            *date1 = NULL;
        char            *date2 = NULL;


        /*
	 *
	 * No getopt error message displayed
	 *
	 */
	opterr = 0;
#if ! defined(__alpha) && !defined(LINUX) && !defined(_AIX43) && !defined(linux)
	if ( setlabel(exec_name) != 0 )
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
   
	date1 = argv[1];
	date2 = argv[2];
	

	
	/*
	 *	Call the main routine
	 */	
	if ( ( exit_status = main_function(date1, date2, &flag)) != EC_OK) {
		return (exit_status);
	} else {
		return (EC_OK);
	}



} /* end of main */
