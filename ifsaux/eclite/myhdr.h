/*
 * (C) Copyright 2005- ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/* My own header, to be included *after* all standard system headers */

#ifndef	__myhdr_h
#define	__myhdr_h

#include	<sys/types.h>	/* required for some of our prototypes */
#include	<stdio.h>		/* for convenience */
#include	<stdlib.h>		/* for convenience */
#include	<string.h>		/* for convenience */
#include	<unistd.h>		/* for convenience */

#define	MAXLINE	4096			/* max line length */

#define	FILE_MODE	(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
					/* default file access permissions for new files */
#define	DIR_MODE	(FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)
					/* default permissions for new directories */

typedef	void	Sigfunc(int);	/* for signal handlers */

					/* 4.3BSD Reno <signal.h> doesn't define SIG_ERR */
#if	defined(SIG_IGN) && !defined(SIG_ERR)
#define	SIG_ERR	((Sigfunc *)-1)
#endif

#define	min(a,b)	((a) < (b) ? (a) : (b))
#define	max(a,b)	((a) > (b) ? (a) : (b))

					/* prototypes for our own functions */


Sigfunc	*signal_intr(int, Sigfunc *);  /* {Prog signal_intr_function} */





static void	err_dump(const char *, ...);	
static void	err_msg(const char *, ...);
static void	err_quit(const char *, ...);
static void	err_ret(const char *, ...);
static void	err_sys(const char *, ...);



#endif	/* __myhdr_h */
