/* lib_debug.c - debug function for the expect C library, libexpect.a

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include <stdio.h>
#include <varargs.h>
#include "exp_rename.h"

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int is_debugging = FALSE;
FILE *debugfile = 0;
FILE *logfile = 0;

/* send to log if open and debugging enabled */
/* send to stderr if debugging enabled */
/* use this function for recording unusual things in the log */
/*VARARGS*/
void
debuglog(va_alist)
va_dcl
{
	char *fmt;
	va_list args;

	va_start(args);
	fmt = va_arg(args,char *);
	if (debugfile) vfprintf(debugfile,fmt,args);
	if (is_debugging) {
		vfprintf(stderr,fmt,args);
		if (logfile) vfprintf(logfile,fmt,args);
	}
	va_end(args);
}

