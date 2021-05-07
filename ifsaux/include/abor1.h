/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#ifndef _ABOR1_H_
#define _ABOR1_H_

/* abor1.h */

void abor1fl(const char* filename, const int linenum, const char* s);
void abor1(const char* s);

#define ABOR1(txt)   abor1( (txt) ) 
#define ABOR1FL(txt) abor1fl( __FILE__, __LINE__, (txt) ) 

#endif /* _ABOR1_H_ */
