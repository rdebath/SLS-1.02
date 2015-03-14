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

#ifndef lint
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/fio.c,v 1.3 89/02/13 14:31:01 grunwald Exp Locker: grunwald $";
#endif

/*
 * File I/O subroutines for getting bytes, words, 3bytes, and longwords.
 */

#include <stdio.h>
#include "types.h"
#include "fio.h"

static char eofmsg[] = "unexpected EOF";

/* for symmetry: */
#define	fGetByte(fp, r)	((r) = getc(fp))
#define	Sign32(i)	(i)

#define make(name, func, signextend) \
i32 \
name(fp) \
	register FILE *fp; \
{ \
	register i32 n; \
 \
	func(fp, n); \
	if (feof(fp)) \
		error(1, 0, eofmsg); \
	return (signextend(n)); \
}

make(GetByte,  fGetByte,  Sign8)
make(GetWord,  fGetWord,  Sign16)
make(Get3Byte, fGet3Byte, Sign24)
make(GetLong,  fGetLong,  Sign32)
