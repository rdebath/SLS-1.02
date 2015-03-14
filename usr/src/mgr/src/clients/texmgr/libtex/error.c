/*
 * Copyright 1989 Chris Torek
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Chris Torek or M.I.T.
 * not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.  Chris
 * Torek and M.I.T. make no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * CHRIS TOREK AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS.  IN NO EVENT SHALL CHRIS TOREK OR M.I.T. BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * Original Author:
 * 	Chris Torek
 * 	Dept. of Computer Science
 * 	Univ. of Maryland
 * 	chris@cs.umd.edu
 */ 

/*
 * Print an error message with an optional system error number, and
 * optionally quit.
 */

#include <stdio.h>
#include <errno.h>
#include <varargs.h>

extern int sys_nerr;
extern char *sys_errlist[];

extern char *ProgName;

error(va_alist)
	va_dcl
{
	va_list l;
	int quit, e;
	char *fmt;

	(void) fflush(stdout);	/* sync error messages */
	(void) fprintf(stderr, "%s: ", ProgName);
	va_start(l);
	/* pick up the constant arguments: quit, errno, printf format */
	quit = va_arg(l, int);
	e = va_arg(l, int);
	if (e < 0)
		e = errno;
	fmt = va_arg(l, char *);
	(void) vfprintf(stderr, fmt, l);
	va_end(l);
	if (e) {
		if (e < sys_nerr)
			(void) fprintf(stderr, ": %s", sys_errlist[e]);
		else
			(void) fprintf(stderr, ": Unknown error code %d", e);
	}
	(void) putc('\n', stderr);
	(void) fflush(stderr);	/* just in case */
	if (quit)
		exit(quit);
}

panic(va_alist)
	va_dcl
{
	va_list l;
	char *fmt;

	(void) fflush(stdout);
	(void) fprintf(stderr, "%s: panic: ", ProgName);
	va_start(l);
	/* pick up the constant argument: printf format */
	fmt = va_arg(l, char *);
	(void) vfprintf(stderr, fmt, l);
	va_end(l);
	(void) putc('\n', stderr);
	(void) fflush(stderr);
	abort();
}
