#include <stdio.h>
#include <errno.h>		/* for definition of errno */
#include <stdarg.h>		/* ANSI C header file */
#include <signal.h>

/* #include <pfmt.h> */

#include	"myhdr.h"

static void	err_doit(int, const char *, va_list);
static void	err_ret(const char *fmt, ...);
static void	err_sys(const char *fmt, ...);
static void	err_dump(const char *fmt, ...);
static void	err_msg(const char *fmt, ...);
static void	err_quit(const char *fmt, ...);

/*static char	*pname = NULL;*/		/* caller can set this from argv[0] */

/* Nonfatal error related to a system call.
 * Print a message and return. */

static void
err_ret(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	err_doit(1, fmt, ap);
	va_end(ap);
	return;
}

/* Fatal error related to a system call.
 * Print a message and terminate. */

static void
err_sys(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	err_doit(1, fmt, ap);
	va_end(ap);
	exit(1);
}

/* Fatal error related to a system call.
 * Print a message, dump core, and terminate. */

static void
err_dump(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	err_doit(1, fmt, ap);
	va_end(ap);
	/* abort(); */		/* dump core and terminate */
	raise(SIGABRT);         /* Will be caught (better) by Dr.Hook ? */
	exit(1);		/* shouldn't get here */
}

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

/* Fatal error unrelated to a system call.
 * Print a message and terminate. */

static void
err_quit(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	err_doit(0, fmt, ap);
	va_end(ap);
	exit(1);
}

/* Print a message and return to caller.
 * Caller specifies "errnoflag". */

/*
static void
myerr_doit(int errnoflag, const char *fmt, va_list ap)
{
	pfmt(stderr, MM_STD, fmt, ap);

}
*/

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

