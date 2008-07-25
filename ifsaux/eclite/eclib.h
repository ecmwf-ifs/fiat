/*******************************
 *  Prototypes
 *******************************/ 


/* ECLIB routines */

void     CD2DATE(const _int32_t *const icd, _int32_t *const iy, _int32_t *const im, _int32_t *const id, _int32_t *const iret);
void     YD2DATE(const _int32_t *const iyd, const _int32_t *const iy, _int32_t *const im, _int32_t *const id, _int32_t *const iret);
_int32_t IDATE2CD(const _int32_t *const iy, const _int32_t *const im, const _int32_t *const id, _int32_t *const iret);
_int32_t IDATE2YD(const _int32_t *const iy, const _int32_t *const im, const _int32_t *const id, _int32_t *const iret);
_int32_t ICD2YMD(const _int32_t *const icd, _int32_t *const iret);
_int32_t IYMD2CD(const _int32_t *const iymd, _int32_t *const iret);


void     DAYDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, _int32_t *const days, _int32_t *const iret);
void     HOURDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, _int32_t *const hours, _int32_t *const iret);
void     MINDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const min1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, const _int32_t *const min2, _int32_t *const minutes, _int32_t *const iret);
void     SECDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const min1, const _int32_t *const sec1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, const _int32_t *const min2, const _int32_t *const sec2, _int32_t *const seconds, _int32_t *const iret);


void     HOURINCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const hours,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const iret);
void     MININCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const min, const _int32_t *const minutes,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const new_min,  _int32_t *const iret);
void     SECINCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const min, const _int32_t *const sec, const _int32_t *const seconds,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const new_min, _int32_t *const new_sec,  _int32_t *const iret);



/*******************************
 *  Functions
 *******************************/


void     
DAYDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, _int32_t *const days, _int32_t *const iret)
{
	yyyymmdd_t        date1_str = {0,0,0}, date2_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

	*days = 0;
        *iret = 0;

	date1_str.year = *year1;
	date1_str.month = *month1;
	date1_str.day = *day1;

       
	date2_str.year = *year2;
	date2_str.month = *month2;
	date2_str.day = *day2;


	if ( (exit_status = dateMinusDate(&date1_str, &date2_str, days)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}


	*iret = EC_OK;   
	return ;


} /* DAYDIFF */




void     
HOURDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, _int32_t *const hours, _int32_t *const iret)
{
	yyyymmdd_t        date1_str = {0,0,0}, date2_str = {0,0,0};
	hhmmss_t          hms1_str = {0,0,0}, hms2_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

	*hours = 0;
        *iret = 0;

	date1_str.year = *year1;
	date1_str.month = *month1;
	date1_str.day = *day1;

	hms1_str.hour = *hour1;

       
	date2_str.year = *year2;
	date2_str.month = *month2;
	date2_str.day = *day2;

	hms2_str.hour = *hour2;


	if ( (exit_status = hour_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, hours)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}


	*iret = EC_OK;   
	return ;


} /* HOURDIFF */




void     
MINDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const min1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, const _int32_t *const min2, _int32_t *const minutes, _int32_t *const iret)
{
	yyyymmdd_t        date1_str = {0,0,0}, date2_str = {0,0,0};
	hhmmss_t          hms1_str = {0,0,0}, hms2_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

	*minutes = 0;
        *iret = 0;

	date1_str.year = *year1;
	date1_str.month = *month1;
	date1_str.day = *day1;

	hms1_str.hour = *hour1;
	hms1_str.min = *min1;

       
	date2_str.year = *year2;
	date2_str.month = *month2;
	date2_str.day = *day2;

	hms2_str.hour = *hour2;
	hms2_str.min = *min2;


	if ( (exit_status = min_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, minutes)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}


	*iret = EC_OK;   
	return ;


} /* MINDIFF */




void     
SECDIFF(const _int32_t *const year1, const _int32_t *const month1, const _int32_t *const day1, const _int32_t *const hour1, const _int32_t *const min1, const _int32_t *const sec1, const _int32_t *const year2, const _int32_t *const month2, const _int32_t *const day2, const _int32_t *const hour2, const _int32_t *const min2, const _int32_t *const sec2, _int32_t *const seconds, _int32_t *const iret)
{
	yyyymmdd_t        date1_str = {0,0,0}, date2_str = {0,0,0};
	hhmmss_t          hms1_str = {0,0,0}, hms2_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

	*seconds = 0;
        *iret = 0;

	date1_str.year = *year1;
	date1_str.month = *month1;
	date1_str.day = *day1;

	hms1_str.hour = *hour1;
	hms1_str.min = *min1;
	hms1_str.sec = *sec1;
	
	date2_str.year = *year2;
	date2_str.month = *month2;
	date2_str.day = *day2;

	hms2_str.hour = *hour2;
	hms2_str.min = *min2;
	hms2_str.sec = *sec2;


	if ( (exit_status = sec_dateMinusDate(&date1_str, &hms1_str, &date2_str, &hms2_str, seconds)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}


	*iret = EC_OK;   
	return ;


} /* SECDIFF */






void     
DAYINCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const days, _int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const iret)
{
	yyyymmdd_t        date_str = {0,0,0}, new_date_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

        *iret = 0;


	date_str.year = *year;
	date_str.month = *month;
	date_str.day = *day;


	if ( (exit_status = addDays(&date_str, *days, &new_date_str)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}



	*new_year = new_date_str.year;
	*new_month = new_date_str.month;
	*new_day = new_date_str.day;


	*iret = EC_OK;   
	return ;


} /* DAYINCR */




void     
HOURINCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const hours,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const iret)
{
	yyyymmdd_t        date_str = {0,0,0}, new_date_str = {0,0,0};
	hhmmss_t          hms_str = {0,0,0}, new_hms_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

        *iret = 0;


	date_str.year = *year;
	date_str.month = *month;
	date_str.day = *day;

	hms_str.hour = *hour;


	if ( (exit_status = addHours(&date_str, &hms_str, *hours, &new_date_str, &new_hms_str)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}


	*new_year = new_date_str.year;
	*new_month = new_date_str.month;
	*new_day = new_date_str.day;

	*new_hour = new_hms_str.hour;


	*iret = EC_OK;   
	return ;


} /* HOURINCR */



void     
MININCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const min, const _int32_t *const minutes,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const new_min,  _int32_t *const iret)
{
	yyyymmdd_t        date_str = {0,0,0}, new_date_str = {0,0,0};
	hhmmss_t          hms_str = {0,0,0}, new_hms_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

        *iret = 0;


	date_str.year = *year;
	date_str.month = *month;
	date_str.day = *day;

	hms_str.hour = *hour;
	hms_str.min = *min;


	if ( (exit_status = addMinutes(&date_str, &hms_str, *minutes, &new_date_str, &new_hms_str)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}

	*new_year = new_date_str.year;
	*new_month = new_date_str.month;
	*new_day = new_date_str.day;

	*new_hour = new_hms_str.hour;
	*new_min = new_hms_str.min;


	*iret = EC_OK;   
	return ;


} /* MININCR */


void     
SECINCR(const _int32_t *const year, const _int32_t *const month, const _int32_t *const day, const _int32_t *const hour, const _int32_t *const min, const _int32_t *const sec, const _int32_t *const seconds,_int32_t *const new_year, _int32_t *const new_month, _int32_t *const new_day, _int32_t *const new_hour, _int32_t *const new_min, _int32_t *const new_sec,  _int32_t *const iret)
{
	yyyymmdd_t        date_str = {0,0,0}, new_date_str = {0,0,0};
	hhmmss_t          hms_str = {0,0,0}, new_hms_str = {0,0,0};
	exit_t            exit_status = 0;


        /*
	 *     Initialise variables
	 */

        *iret = 0;


	date_str.year = *year;
	date_str.month = *month;
	date_str.day = *day;

	hms_str.hour = *hour;
	hms_str.min = *min;
	hms_str.sec = *sec;


	if ( (exit_status = addSeconds(&date_str, &hms_str, *seconds, &new_date_str, &new_hms_str)) != EC_OK) {
		*iret = exit_status;
		return ;	
	}

	*new_year = new_date_str.year;
	*new_month = new_date_str.month;
	*new_day = new_date_str.day;

	*new_hour = new_hms_str.hour;
	*new_min = new_hms_str.min;
	*new_sec = new_hms_str.sec;


	*iret = EC_OK;   
	return ;


} /* SECINCR */




void 
CD2DATE(const _int32_t *const icd, _int32_t *const iy, _int32_t *const im, _int32_t *const id, _int32_t *const iret)
{
	yyyymmdd_t       date_str = {0,0,0};
	exit_t           exit_status = 0;


	*iret = 0;

	if ( ( exit_status = centuryToDate(*icd, &date_str)) != EC_OK) {
		*iret = exit_status;
		return ;
	}	

	*id = date_str.day;
	*im = date_str.month;
	*iy = date_str.year;


	*iret = EC_OK;
	return ;


} /* CD2DATE */


void 
YD2DATE(const _int32_t *const iyd, const _int32_t *const iy, _int32_t *const im, _int32_t *const id, _int32_t *const iret)
{
	yyyymmdd_t       date_str = {0,0,0};
	exit_t           exit_status = 0;


	*iret = 0;

	if ( ( exit_status = yeardayToDate(*iyd, *iy, &date_str)) != EC_OK) {
		*iret = exit_status;
		return ;
	}

	
	*id = date_str.day;
	*im = date_str.month;


	*iret = EC_OK;
	return ;


} /* YD2DATE */



_int32_t 
IDATE2CD(const _int32_t *const iy, const _int32_t *const im, const _int32_t *const id, _int32_t *const iret)
{
	yyyymmdd_t       date_str = {0,0,0};
	_int32_t  century = 0;
	exit_t           exit_status = 0;


	*iret = 0;

	date_str.year = *iy;
	date_str.month = *im;
	date_str.day = *id;

	if ( ( exit_status = dateToCentury( &date_str, &century)) != EC_OK) {
		*iret = exit_status;
		return (0);
	}


	*iret = EC_OK;   
	return (century);


} /* IDATE2CD */


_int32_t 
IDATE2YD(const _int32_t *const iy, const _int32_t *const im, const _int32_t *const id, _int32_t *const iret)
{
	yyyymmdd_t       date_str = {0,0,0};
	_int32_t  yearday = 0;
	exit_t           exit_status = 0;


	*iret = 0;

	date_str.year = *iy;
	date_str.month = *im;
	date_str.day = *id;

	if ( ( exit_status = dateToYearday( &date_str, &yearday)) != EC_OK) {
		*iret = exit_status;
		return (0);
	}

	*iret = EC_OK;   
	return (yearday);


} /* IDATE2YD */


_int32_t     
ICD2YMD(const _int32_t *const icd, _int32_t *const iret)
{

	_int32_t         id, im, iy;
	_int64_t         ymd = 0;
	_int32_t  iymd = 0;

	id = 0;
	im = 0;
	iy = 0;

	*iret = 0;

	CD2DATE(icd, &iy, &im, &id, iret);

	if (  *iret != EC_OK) {
		return (0);
	}


	ymd = iy * 10000 + im * 100 + id;

	if ( ymd > LONG_MAX || ymd < LONG_MIN) {
		err_msg("ICD2YMD: ymd = %lld", ymd);
		err_msg("Exceeded the allowed range");
		*iret = EC_RANGE;
		return (0);
	}

	iymd = (_int32_t) ymd;

	*iret = EC_OK;   
	return (iymd);
	       

} /* ICD2YMD */



_int32_t     
IYMD2CD(const _int32_t *const iymd, _int32_t *const iret)
{
	_int32_t id, im, iy, ymd;
	_int32_t icd = 0;

	*iret = 0;

	ymd = *iymd;

	iy = ymd / 10000;
	ymd %= 10000;

	im = ymd / 100;
	ymd %= 100;

	id = ymd;
	

	icd = IDATE2CD(&iy, &im, &id, iret);

	if (  *iret != EC_OK) {
		return (0);
	}
	
	return (icd);

	       
} /* IYMD2CD */
