/* global.h - global definitions

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

/* common return codes for Expect functions */
#define EXP_EOF		-11
#define EXP_ABEOF	-1	/* abnormal eof */
#define EXP_TIMEOUT	-2
#define EXP_TCLERROR	-3
/*#define EXP_DATA	-4*/
#define EXP_FULLBUFFER	-5
#define EXP_MATCH	-6
#define EXP_NOMATCH	-7
#define EXP_CANTMATCH	EXP_NOMATCH
#define EXP_CANMATCH	-8
#define EXP_DATA_NEW	-9	/* if select says there is new data */
#define EXP_DATA_OLD	-10	/* if we already read data in another cmd */

#define EXP_TIME_INFINITY	-1
#define EXP_SPAWN_ID_BAD	-1

#ifndef TRUE
#define FALSE 0
#define TRUE 1
#endif

extern char *sys_errlist[];
extern int errno;

/* if you get errors from the compiler here, add -DNOSTDLIB to Makefile */
#ifdef NOSTDLIB
	char* malloc();
	char* realloc();
#ifndef _TCLINT
	void exit();
#endif
#else
#include <stdlib.h>
#endif

/* yes, I have a weak mind */
#define streq(x,y)	(0 == strcmp((x),(y)))

#ifdef NO_MEMCPY
#define memcpy(x,y,len) bcopy(y,x,len)
#endif

void flush_streams();

void exp_error();

/* yet more TCL return codes */
/* Tcl does not safely provide a way to define the values of these, so */
/* use ridiculously numbers for safety */
#define TCL_CONTINUE_EXPECT	-101	/* continue expect command itself */
#define TCL_RETURN_TCL		-102	/* converted by interact, interpeter */
					/* from "return -tcl" into TCL_RETURN*/
