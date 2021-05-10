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

#ifndef __julian_H__
#define __julian_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#if defined(LINUX) || defined(linux)
#include <stdint.h>
#endif



#include <stdio.h>
#include <errno.h>		/* for definition of errno */
#include <stdarg.h>		/* ANSI C header file */

#include <string.h>


static void	err_doit(int, const char *, va_list);
static void	err_msg(const char *fmt, ...);
static void	err_quit(const char *fmt, ...);


/*******************************
 * Macros
 *******************************/

#define	MAXLINE	4096			/* max line length */

#define leap(y) (((y) % 4 == 0 && (y) % 100 != 0) || (y) % 400 == 0)


#ifdef _LANGUAGE_C_PLUS_PLUS
#define INLINE inline
#else
#define INLINE 
#endif



/*******************************
 * Types
 *******************************/


#if defined(POINTER_64) || defined(_ABI64) || defined(__uxpch__) || defined(__alpha) || defined(__64BIT__)
	typedef long int _int64_t;
#else
	typedef long long int _int64_t;
#endif


#if defined(POINTER_64) || defined(_ABI64) || defined(__uxpch__) || defined(__alpha) || defined(NECSX) || defined(__64BIT__)
	typedef int _int32_t;
#else
	typedef long int _int32_t;
#endif

typedef short int _int16_t;

typedef _int32_t exit_t;

/*
typedef unsigned long int _u_int32_t;
typedef unsigned short int _uint16_t;
*/



typedef struct iso_date_str {
	_int32_t julian;
	_int32_t seconds;
} mydate_t;



typedef struct  date_str {  
        _int32_t     day;
        _int32_t     month;
        _int32_t     year;
} yyyymmdd_t;


typedef struct  hms_str {  
        _int32_t     hour;
        _int32_t     min;
        _int32_t     sec;
} hhmmss_t;




/*******************************
 * Externals
 *******************************/

/* None */



/*******************************
 *  Prototypes
 *******************************/ 

/* 
 *
 * Internal routines: set static
 *
 */


static exit_t   addDays(const yyyymmdd_t *const date_ptr, const _int32_t days, yyyymmdd_t *const new_date_ptr);
static exit_t   addHours(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t hours,  yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr);
static exit_t   addMinutes(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t minutes, yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr);
static exit_t   addSeconds(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t seconds, yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr);

static exit_t   dateMinusDate(const yyyymmdd_t *const date1_ptr, const yyyymmdd_t *const date2_ptr, _int32_t *const days_ptr); 
static exit_t   hour_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const hours_ptr);
static exit_t   min_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const minutes_ptr);
static exit_t   sec_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const seconds_ptr);



static exit_t   is_hms(const hhmmss_t *const hms_ptr);
static exit_t   is_date(const yyyymmdd_t *const date_ptr);



static exit_t _addHours(const mydate_t *const fulldate_ptr, const _int32_t hours,  mydate_t *const new_fulldate_ptr);
static exit_t _addMinutes(const mydate_t *const fulldate_ptr, const _int32_t minutes, mydate_t *const new_fulldate_ptr);
static exit_t _addSeconds(const mydate_t *const fulldate_ptr, const _int32_t seconds, mydate_t *const new_fulldate_ptr);



static exit_t   julianToDate(const _int32_t julian, yyyymmdd_t *const date_ptr);
static exit_t   dateToJulian(const yyyymmdd_t *const date_ptr, _int32_t *const julian);
static exit_t   secondsToHms(const _int32_t seconds, hhmmss_t *const hms_ptr);
static exit_t   hmsToSeconds(const hhmmss_t *const hms_ptr, _int32_t *const seconds);
static exit_t   centuryToDate(const _int32_t century, yyyymmdd_t *date_ptr);
static exit_t   dateToCentury(const yyyymmdd_t *const date_ptr, _int32_t *const century_ptr);
static exit_t   dateToYearday(const yyyymmdd_t *const date_ptr, _int32_t *const yearday_ptr);
static exit_t   yeardayToDate(const _int32_t yearday, const _int32_t year, yyyymmdd_t *date_ptr);





/*******************************
 *   Constants
 *******************************/
 

static const _int16_t month_len[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};




static const _int32_t MJDSHIFT = 0;
static const _int32_t CENTURYSHIFT = 2415021;

/*
static const _int32_t MJDSHIFT = 2400000;
static const _int32_t CENTURYSHIFT = 15021;
*/



static const _int64_t   JULIAN_MIN = 0LL;


static const _int32_t   EC_OK       = 0;
static const _int32_t   EC_FALSE    = -1;

static const _int32_t   EC_WRGOPT   = -2;
static const _int32_t   EC_WRGPAR   = -3;
static const _int32_t   EC_WRGLEN   = -4;

static const _int32_t   EC_DATELEN  = -5;
static const _int32_t   EC_DATEFMT  = -6;
static const _int32_t   EC_DATEINV  = -7;
static const _int32_t   EC_TIMEINV  = -8;
static const _int32_t   EC_NAN      = -9;
static const _int32_t   EC_RANGE    = -10;


static const _int32_t   YEAR_MIN    = 0;
static const _int32_t   YEAR_MAX    = 9999;

static const _int32_t   SEC_MIN     = 60;
static const _int32_t   SEC_HOUR    = 3600;
static const _int32_t   SEC_DAY     = 86400;
static const _int32_t   MIN_HOUR    = 60;
static const _int32_t   MIN_DAY     = 1440;
static const _int32_t   HOUR_DAY    = 24;



/*******************************
 *  Functions
 *******************************/


static exit_t 
dateMinusDate(const yyyymmdd_t *const date1_ptr, const yyyymmdd_t *const date2_ptr, _int32_t *const days_ptr)
{
	_int32_t         julian1 = 0, julian2 = 0;
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date1_ptr, &julian1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = dateToJulian(date2_ptr, &julian2) ) != EC_OK) {
		return (exit_status);
	}

	*days_ptr = (julian1 - julian2);

	return (EC_OK);


} /* dateMinusDate */



static exit_t 
hour_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const hours_ptr)
{
	_int32_t         julian1 = 0, julian2 = 0;
	_int32_t         second1 = 0, second2 = 0;
	_int64_t         hours = 0;
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date1_ptr, &julian1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = dateToJulian(date2_ptr, &julian2) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms1_ptr, &second1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms2_ptr, &second2) ) != EC_OK) {
		return (exit_status);
	}

	hours = (julian1 - julian2) * (_int64_t) HOUR_DAY + (second1 - second2) / SEC_HOUR ;

	if ( hours > LONG_MAX || hours < LONG_MIN) {
#if defined(_ABI64)
		err_msg("hour_dateMinusDate: hours = %ld", hours);
#else
		err_msg("hour_dateMinusDate: hours = %lld", hours);
#endif
		err_msg("Exceeded the allowed range");	
		return (EC_RANGE);
	}

	*hours_ptr = hours;

	return (EC_OK);


} /* hour_dateMinusDate */



static exit_t 
min_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const minutes_ptr)
{
	_int32_t         julian1 = 0, julian2 = 0;
	_int32_t         second1 = 0, second2 = 0;
	_int64_t         minutes = 0;
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date1_ptr, &julian1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = dateToJulian(date2_ptr, &julian2) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms1_ptr, &second1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms2_ptr, &second2) ) != EC_OK) {
		return (exit_status);
	}

	minutes = (julian1 - julian2) * (_int64_t) MIN_DAY + (second1 - second2) / SEC_MIN ;

	if ( minutes > LONG_MAX || minutes < LONG_MIN) {
#if defined(_ABI64)
		err_msg("min_dateMinusDate: minutes = %ld", minutes);
#else
		err_msg("min_dateMinusDate: minutes = %lld", minutes);
#endif
		err_msg("Exceeded the allowed range");

		return (EC_RANGE);
	}

	*minutes_ptr = minutes;

	return (EC_OK);


} /* min_dateMinusDate */



static exit_t 
sec_dateMinusDate(const yyyymmdd_t *const date1_ptr, const hhmmss_t *const hms1_ptr, const yyyymmdd_t *const date2_ptr, const hhmmss_t *const hms2_ptr, _int32_t *const seconds_ptr)
{
	_int32_t         julian1 = 0, julian2 = 0;
	_int32_t         second1 = 0, second2 = 0;
	_int64_t         seconds = 0;
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date1_ptr, &julian1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = dateToJulian(date2_ptr, &julian2) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms1_ptr, &second1) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms2_ptr, &second2) ) != EC_OK) {
		return (exit_status);
	}

	seconds = (julian1 - julian2) * (_int64_t) SEC_DAY + (second1 - second2);

	if ( seconds > LONG_MAX || seconds < LONG_MIN) {
#if defined(_ABI64)
		err_msg("sec_dateMinusDate: seconds = %lld", seconds);
#else
		err_msg("sec_dateMinusDate: seconds = %lld", seconds);
#endif
		err_msg("Exceeded the allowed range");	
		return (EC_RANGE);
	}

	*seconds_ptr = seconds;

	return (EC_OK);


} /* sec_dateMinusDate */



static exit_t 
centuryToDate(const _int32_t century, yyyymmdd_t *date_ptr)
{
	_int32_t         julian = 0;
	exit_t           exit_status = 0;


	julian = century + CENTURYSHIFT - 1;

	if ( ( exit_status = julianToDate(julian, date_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* centuryToDate */



INLINE static exit_t 
dateToCentury(const yyyymmdd_t *const date_ptr, _int32_t *const century_ptr)
{
	_int32_t         century = 0;
	exit_t           exit_status = 0;


	if ( is_date(date_ptr) != EC_OK ) {
#if defined(_ABI64)
		err_msg("Date incorrect (%04d%02d%02d)", date_ptr->year, date_ptr->month, date_ptr->day);
#else
		err_msg("Date incorrect (%04ld%02ld%02ld)", date_ptr->year, date_ptr->month, date_ptr->day);
#endif
		return (EC_DATEINV);
	}

	if ( ( exit_status = dateToJulian(date_ptr, &century) ) != EC_OK) {
		return (exit_status);
	}	

	*century_ptr = century - CENTURYSHIFT + 1;

	return (EC_OK);


} /* dateToCentury */



static exit_t 
dateToYearday(const yyyymmdd_t *const date_ptr, _int32_t *const yearday_ptr)
{
	_int32_t         century = 0, year = 0;
	yyyymmdd_t       year_date = {0,0,0};
	exit_t           exit_status = 0;


	
	if ( is_date(date_ptr) != EC_OK ) {
#if defined(_ABI64)
		err_msg("Date incorrect (%04d%02d%02d)", date_ptr->year, date_ptr->month, date_ptr->day);
#else
		err_msg("Date incorrect (%04ld%02ld%02ld)", date_ptr->year, date_ptr->month, date_ptr->day);
#endif
		return (EC_DATEINV);
	}

	if ( ( exit_status = dateToJulian(date_ptr, &century) ) != EC_OK) {
		return (exit_status);
	}	

	year_date.year = date_ptr->year;
	year_date.month = 1;
	year_date.day = 1;

	if ( ( exit_status = dateToJulian(&year_date, &year) ) != EC_OK) {
		return (exit_status);
	}

	*yearday_ptr = century - year  + 1;

	return (EC_OK);


} /* dateToYearday */



static exit_t 
yeardayToDate(const _int32_t yearday, const _int32_t year, yyyymmdd_t *date_ptr)
{
	_int32_t         shift = 0, julian = 0;
	yyyymmdd_t       year_date = {0,0,0};
	exit_t           exit_status = 0;


	year_date.year = year;
	year_date.month = 1;
	year_date.day = 1;

	if ( is_date(&year_date) != EC_OK ) {
#if defined(_ABI64)
		err_msg("Date incorrect (%04d%02d%02d)", year_date.year, year_date.month, year_date.day);
#else
		err_msg("Date incorrect (%04ld%02ld%02ld)", year_date.year, year_date.month, year_date.day);
#endif
		return (EC_DATEINV);
	}

	if ( ( exit_status = dateToJulian(&year_date, &shift) ) != EC_OK) {
		return (exit_status);
	}

	julian = yearday + shift - 1;

	if ( ( exit_status = julianToDate(julian, date_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* yeardayToDate */



static exit_t
addDays(const yyyymmdd_t *const date_ptr, const _int32_t days, yyyymmdd_t *const new_date_ptr)
{
	_int64_t         julian = 0;
	_int32_t         jul = 0;
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date_ptr, &jul) ) != EC_OK) {
		return (exit_status);
	}

	julian = jul;
	julian += days;

	if ( julian > LONG_MAX || julian < LONG_MIN) {
#if defined(_ABI64)
		err_msg("addDays: julian = %ld", julian);
#else
		err_msg("addDays: julian = %lld", julian);
#endif
		err_msg("Exceeded the allowed range");
		return (EC_RANGE);
	}

	jul = (_int32_t) julian;

	if ( ( exit_status = julianToDate(jul, new_date_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* addDays */




static exit_t 
addHours(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t hours,  yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr) 
{

	mydate_t         fulldate_str = {0,0}, new_fulldate_str = {0,0};
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date_ptr, &fulldate_str.julian) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms_ptr, &fulldate_str.seconds) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = _addHours(&fulldate_str, hours, &new_fulldate_str) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = julianToDate(new_fulldate_str.julian, new_date_ptr) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = secondsToHms(new_fulldate_str.seconds, new_hms_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* addHours */



static exit_t 
addMinutes(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t minutes,  yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr) 
{

	mydate_t         fulldate_str = {0,0}, new_fulldate_str = {0,0};
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date_ptr, &fulldate_str.julian) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms_ptr, &fulldate_str.seconds) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = _addMinutes(&fulldate_str, minutes, &new_fulldate_str) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = julianToDate(new_fulldate_str.julian, new_date_ptr) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = secondsToHms(new_fulldate_str.seconds, new_hms_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* addMinutes */



static exit_t 
addSeconds(const yyyymmdd_t *const date_ptr, const hhmmss_t *const hms_ptr, const _int32_t seconds,  yyyymmdd_t *const new_date_ptr, hhmmss_t *const new_hms_ptr) 
{

	mydate_t         fulldate_str = {0,0}, new_fulldate_str = {0,0};
	exit_t           exit_status = 0;


	if ( ( exit_status = dateToJulian(date_ptr, &fulldate_str.julian) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = hmsToSeconds(hms_ptr, &fulldate_str.seconds) ) != EC_OK) {
		return (exit_status);
	}

	if ( ( exit_status = _addSeconds(&fulldate_str, seconds, &new_fulldate_str) ) != EC_OK) {
		return (exit_status);
	}


	if ( ( exit_status = julianToDate(new_fulldate_str.julian, new_date_ptr) ) != EC_OK) {
		return (exit_status);
	}


	if ( ( exit_status = secondsToHms(new_fulldate_str.seconds, new_hms_ptr) ) != EC_OK) {
		return (exit_status);
	}

	return (EC_OK);


} /* addSeconds */



static exit_t
_addHours(const mydate_t *const fulldate_ptr, const _int32_t hours, mydate_t *const new_fulldate_ptr)
{

	_int32_t         days = 0;
	_int32_t         new_hours = 0;
	_int32_t         new_seconds = 0;


	new_hours = hours;

	days = new_hours / HOUR_DAY;

	new_fulldate_ptr->julian = fulldate_ptr->julian + days;
	new_hours %= HOUR_DAY;

	new_seconds = new_hours * SEC_HOUR;

	new_fulldate_ptr->seconds = fulldate_ptr->seconds + new_seconds;

	if ( new_fulldate_ptr->seconds < 0) {
		new_fulldate_ptr->julian --;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds + SEC_DAY;
	}
	
	if ( new_fulldate_ptr->seconds >= SEC_DAY) {
		new_fulldate_ptr->julian ++;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds - SEC_DAY;
	}

	return (EC_OK);
       

} /* _addHours */




static exit_t
_addMinutes(const mydate_t *const fulldate_ptr, const _int32_t minutes, mydate_t *const new_fulldate_ptr)
{

	_int32_t        days = 0;
	_int32_t        new_minutes = 0;
	_int32_t        new_seconds = 0;


	new_minutes = minutes;

	days = new_minutes / MIN_DAY;

	new_fulldate_ptr->julian = fulldate_ptr->julian + days;
	new_minutes %= MIN_DAY;

	new_seconds = new_minutes * SEC_MIN;

	new_fulldate_ptr->seconds = fulldate_ptr->seconds + new_seconds;

	if ( new_fulldate_ptr->seconds < 0) {
		new_fulldate_ptr->julian --;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds + SEC_DAY;
	}
	

	if ( new_fulldate_ptr->seconds >= SEC_DAY) {
		new_fulldate_ptr->julian ++;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds - SEC_DAY;
	}

	return (EC_OK);
       

} /* _addMinutes */



static exit_t
_addSeconds(const mydate_t *const fulldate_ptr, const _int32_t seconds, mydate_t *const new_fulldate_ptr)
{

	_int32_t         days = 0;
	_int32_t         new_seconds = 0;


	new_seconds = seconds;
	days = new_seconds / SEC_DAY;

	new_fulldate_ptr->julian = fulldate_ptr->julian + (_int32_t) days;
	new_seconds %= SEC_DAY;

	new_fulldate_ptr->seconds = fulldate_ptr->seconds + (_int32_t) new_seconds;

	if ( new_fulldate_ptr->seconds < 0) {
		new_fulldate_ptr->julian --;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds + SEC_DAY;
	} else if ( new_fulldate_ptr->seconds >= SEC_DAY) {
		new_fulldate_ptr->julian ++;
		new_fulldate_ptr->seconds = new_fulldate_ptr->seconds - SEC_DAY;
	}

	return (EC_OK);
       

} /* _addSeconds */





INLINE static exit_t
hmsToSeconds(const hhmmss_t *const hms_ptr, _int32_t *const seconds)
{

	if ( is_hms(hms_ptr) != EC_OK ) {
#if defined(_ABI64)
		err_msg("Time incorrect (%02d%02d%02d)", hms_ptr->hour, hms_ptr->min, hms_ptr->sec);
#else
		err_msg("Time incorrect (%02ld%02ld%02ld)", hms_ptr->hour, hms_ptr->min, hms_ptr->sec);
#endif
		return (EC_TIMEINV);
	}  

	*seconds = SEC_HOUR * hms_ptr->hour + SEC_MIN * hms_ptr->min + hms_ptr->sec;

	return (EC_OK);


} /* hmsToSeconds */



static exit_t
secondsToHms(const _int32_t seconds, hhmmss_t *const hms_ptr)
{
	_int32_t        local_sec = 0;


	if ( seconds < 0 || seconds > SEC_DAY)
		return (EC_FALSE);
		
	local_sec = seconds;

	hms_ptr->hour =  local_sec / SEC_HOUR;
	local_sec %= SEC_HOUR;
	hms_ptr->min  =  local_sec / MIN_HOUR;
	local_sec %= MIN_HOUR;
	hms_ptr->sec = local_sec;

	return (EC_OK);

	
} /* secondsToHms */



static exit_t
julianToDate(const _int32_t  julian, yyyymmdd_t *const date_ptr)
{
	_int64_t        l = 0, n = 0, i = 0, j = 0;
	_int64_t        jdate = 0;
	_int64_t        day = 0;
	_int64_t        month = 0;
	_int64_t        year = 0;


      /*
	 * Modified Julian date
	 */
	jdate = julian + MJDSHIFT;
#if defined(LINUX) || defined(linux)
	if ( (_int32_t) jdate < (_int32_t) JULIAN_MIN ) {
#else
	if ( jdate < JULIAN_MIN ) {
#endif

#if defined(_ABI64)
		err_msg("Julian = %ld", jdate);
		err_msg("Julian less than %ld", JULIAN_MIN);
#else
		err_msg("Julian = %lld", jdate);
		err_msg("Julian less than %lld", JULIAN_MIN);
#endif
		return (EC_FALSE);
	}

        /* 
	   l = julian_day + 68569
	   n = ( 4 * l ) / 146097
	   l = l - ( 146097 * n + 3 ) / 4
	   i = ( 4000 * ( l + 1 ) ) / 1461001     (that's 1,461,001)
	   l = l - ( 1461 * i ) / 4 + 31
	   j = ( 80 * l ) / 2447
	   d = l - ( 2447 * j ) / 80
	   l = j / 11
	   m = j + 2 - ( 12 * l )
	   y = 100 * ( n - 49 ) + i + l      

	*/

	l = jdate + 68569;

	n = ( 4 * l ) / 146097;

	l = l - ( 146097 * n + 3 ) / 4;

	i = ( 4000 * ( l + 1 ) ) / 1461001;  

	l = l - ( 1461 * i ) / 4 + 31;

	j = ( 80 * l ) / 2447;

	day = l - ( 2447 * j ) / 80;

	l = j / 11;

	month = j + 2 - ( 12 * l );

	year = 100 * ( n - 49 ) + i + l;

	if ( year > LONG_MAX || year < LONG_MIN) {
#if defined(_ABI64)
		err_msg("julianToDate: Year = %ld", year);
#else
		err_msg("julianToDate: Year = %lld", year);
#endif
		err_msg("Exceeded the allowed range");
		return (EC_RANGE);
	}

	date_ptr->day = day;
	date_ptr->month = month;
	date_ptr->year = year;

	return (EC_OK);


} /* julianToDate */




static exit_t
dateToJulian(const yyyymmdd_t *const date_ptr, _int32_t *const julian)
{

	_int32_t       m1 = 0, m2 = 0, a = 0, b = 0, c = 0;
/*	_int64_t       jul = 0;*/

	if ( is_date(date_ptr) != EC_OK ) {
#if defined(_ABI64)
		err_msg("Date incorrect (%04d%02d%02d)", date_ptr->year, date_ptr->month, date_ptr->day);
#else
		err_msg("Date incorrect (%04ld%02ld%02ld)", date_ptr->year, date_ptr->month, date_ptr->day);
#endif
		return (EC_DATEINV);
	}  


        /*
	 * Compute the Julian Day number applying the following formula

	 julian_day = ( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +
	              ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -
                      ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +
                        d - 32075
	*/

	m1 = (date_ptr->month - 14)/12;

	a = (1461 * (date_ptr->year + 4800 + m1))/4;

	b = (367 * (date_ptr->month - 2 - (12 * m1)))/12;

	m2 = (date_ptr->year + 4900 + m1)/100;

	c = (3 * (m2))/4;

/*
	jul = a + b - c  + date_ptr->day - 32075 - MJDSHIFT;

	if ( jul > LONG_MAX || jul < LONG_MIN) {
		err_msg("dateToJulian: Julian = %lld", jul);
		err_msg("Exceeded the allowed range");
		return (EC_RANGE);
	}
	*/

	*julian = a + b - c  + date_ptr->day - 32075 - MJDSHIFT;

	return (EC_OK);
     

} /* dateToJulian */



static exit_t
is_hms(const hhmmss_t *const hms_ptr)
{

	if ( hms_ptr->hour < 0 || hms_ptr->hour > HOUR_DAY - 1 ||
	     hms_ptr->min  < 0 || hms_ptr->min  > MIN_HOUR - 1 ||
	     hms_ptr->sec  < 0 || hms_ptr->sec  > SEC_MIN - 1 ) 
		return (EC_TIMEINV);
	else
		return (EC_OK);


} /* is_hms */



static exit_t
is_date(const yyyymmdd_t *const date_ptr)
{

	if ( date_ptr->year < YEAR_MIN || date_ptr->year > YEAR_MAX) {
#if defined(_ABI64)
		err_msg("Year %d out of allowed range", date_ptr->year);
#else

#endif
		return(EC_RANGE);
	}

	if ( date_ptr->month  < 1  || 
	     date_ptr->month  > 12 ||
	     date_ptr->day    < 1  || 
	     date_ptr->day    >  (_int32_t) (date_ptr->month==2?(leap(date_ptr->year)?29:28):month_len[date_ptr->month - 1]) ) {
		return (EC_DATEINV);
	} else {
		return (EC_OK);
        }


} /* is_date */

/* Nonfatal error unrelated to a system call.
 * Print a message and return. */

static void
err_msg(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	err_doit(0, fmt, ap);
	va_end(ap);
	return;
}

/* Print a message and return to caller.
 * Caller specifies "errnoflag". */

static void
err_doit(int errnoflag, const char *fmt, va_list ap)
{
	int		errno_save;
	char	buf[MAXLINE];

	errno_save = errno;		/* value caller might want printed */
	(void) vsprintf(buf, fmt, ap);
	if (errnoflag)
		(void) sprintf(buf+strlen(buf), ": %s", strerror(errno_save));
	(void) strcat(buf, "\n");
	(void) fflush(stdout);		/* in case stdout and stderr are the same */
	(void) fputs(buf, stderr);
	(void) fflush(stderr);		/* SunOS 4.1.* doesn't grok NULL argument */
	return;
}

#endif /* __julian_H__ */
