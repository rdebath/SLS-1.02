/*	xcdbglog.c -- debug logging module for XC
	This file uses 4-character tabstops
*/

#ifdef DEBUG
#undef DEBUG
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include "xc.h"

extern short captflag;
static FILE *dfp;

void dbglog()
{
	long todnow;

	time(&todnow);
	if (!access("debug.log",0) && (dfp=fopen("debug.log","w"))!=NULLF)
		fprintf(dfp,(char*)asctime(localtime(&todnow)));
	return;
}

Fputc(c,stream)
register c;
FILE *stream;
{
	if (!capture && !captflag && dfp && c != '\r')
		fputc(c,dfp);
	return(fputc(c,stream));
}

Fprintf(stream,control,a,b,c,d,e,f,g)
FILE *stream;
char *control;
long a,b,c,d,e,f,g;
{
	if (!capture && !captflag && dfp)
		fprintf(dfp,control,a,b,c,d,e,f,g),
		fflush(dfp);
	fprintf(stream,control,a,b,c,d,e,f,g);
}
#endif /*DEBUG*/
