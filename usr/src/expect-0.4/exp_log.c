/* exp_log.c - logging routines */

#include <stdio.h>
#include <varargs.h>
#include "exp_global.h"
#include "exp_rename.h"
#include "exp_log.h"

int loguser = TRUE;		/* if TRUE, expect/spawn may write to stdout */
int logfile_all = FALSE;	/* if TRUE, write log of all interactions */
				/* despite value of loguser. */
FILE *logfile = 0;
FILE *cmdfile = 0;
FILE *debugfile = 0;
int exp_is_debugging = FALSE;

/* Following this are several functions that log the conversation. */
/* Most of them have multiple calls to printf-style functions.  */
/* At first glance, it seems stupid to reformat the same arguments again */
/* but we have no way of telling how long the formatted output will be */
/* and hence cannot allocate a buffer to do so. */
/* Fortunately, in production code, most of the duplicate reformatting */
/* will be skipped, since it is due to handling errors and debugging. */

/* send to log if open */
/* send to stderr if debugging enabled */
/* use this for logging everything but the parent/child conversation */
/* (this turns out to be almost nothing) */
/* uppercase L differentiates if from math function of same name */
#define LOGUSER		(loguser || force_stdout)
/*VARARGS*/
void
Log(va_alist)
va_dcl
{
	int force_stdout;
	char *fmt;
	va_list args;

	va_start(args);
	force_stdout = va_arg(args,int);
	fmt = va_arg(args,char *);
	if (debugfile) vfprintf(debugfile,fmt,args);
	if (logfile_all || (LOGUSER && logfile)) vfprintf(logfile,fmt,args);
	if (LOGUSER) vfprintf(stdout,fmt,args);
	va_end(args);
}

/* just like log but does no formatting */
/* send to log if open */
/* use this function for logging the parent/child conversation */
void
nflog(buf,force_stdout)
char *buf;
int force_stdout;	/* override value of loguser */
{
	int length = strlen(buf);

	if (debugfile) fwrite(buf,1,length,debugfile);
	if (logfile_all || (LOGUSER && logfile)) fwrite(buf,1,length,logfile);
	if (LOGUSER) fwrite(buf,1,length,stdout);
}
#undef LOGUSER

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

/* send to log if open */
/* send to stderr */
/* use this function for error conditions */
/*VARARGS*/
void
errorlog(va_alist)
va_dcl
{
	char *fmt;
	va_list args;

	va_start(args);
	fmt = va_arg(args,char *);
	vfprintf(stderr,fmt,args);
	if (debugfile) vfprintf(debugfile,fmt,args);
	if (logfile) vfprintf(logfile,fmt,args);
	va_end(args);
}

/* just like errorlog but does no formatting */
/* send to log if open */
/* use this function for logging the parent/child conversation */
/*ARGSUSED*/
void
nferrorlog(buf,force_stdout)
char *buf;
int force_stdout;	/* not used, only declared here for compat with */
			/* nflog() */
{
	int length = strlen(buf);
	fwrite(buf,1,length,stderr);
	if (debugfile) fwrite(buf,1,length,debugfile);
	if (logfile) fwrite(buf,1,length,logfile);
}
