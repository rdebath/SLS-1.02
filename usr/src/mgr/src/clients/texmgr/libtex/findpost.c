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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/findpost.c,v 1.4 89/02/13 14:31:00 grunwald Exp Locker: grunwald $";
#endif

/*
 * FindPostAmble - Find the postamble of a DVI file.
 *
 * N.B.: This routine assumes that ftell() returns byte offsets,
 * not magic cookies.
 */

#include <stdio.h>
#include "types.h"
#include "dvicodes.h"
#include "fio.h"

/*
 * I am making the assumption that 530 bytes will always be enough
 * to find the end of the DVI file.  12 should suffice, as there
 * should be at most seven DVI_FILLER bytes, preceded by the version
 * number, preceded by the four byte postamble pointer; but at least
 * one VMS TeX must pad to a full `sector'.
 */
/*
 * The above is not correct.  The DVItype program, as authoritative on
 * DVI format, states regarding postambles:
 *
 *	The [last] byte is followed by four or more bytes that are all
 *	equal to the decimal number 223 (i.e., 337 in octal). TeX puts
 *	out four to seven of these trailing bytes, until the total length
 *	of the file is a multiple of four bytes, since this works out
 *	best on machines that pack four bytes per word; but any number
 *	of 223's is allowed, as long as there are at least four of them.
 *
 * Thus assuming "at most seven DVI_FILLER bytes" is wrong.  In fact,
 * PC-TeX seems to put out liberal amounts of DVI_FILLER at the end.
 *
 * The original code was efficient, but had to assume a certain
 * number of bytes.  Since the postamble is only read once anyway,
 * efficiency is not really a consideration.  Plus, like I always
 * say, it's better to get the right answer slowly than the wrong
 * answer fast....
 *
 * Vahe Sarkissian, UCLA Math. Sci., 4/13/88.
 */

#ifdef ORIGINAL_CODE
#ifdef vms
#define POSTSIZE	530	/* make only VMS pay for its errors; */
#else
#define POSTSIZE	16	/* others get to use something reasonable */
#endif

long	ftell();

FindPostAmble(f)
	register FILE *f;
{
	register long offset;
	register char *p;
	register int i;
	register i32 n;
	char postbuf[POSTSIZE];

	/*
	 * Avoid fseek'ing beyond beginning of file; it may
	 * give odd results.
	 */
	fseek(f, 0L, 2);		/* seek to end */
	offset = ftell(f) - POSTSIZE;	/* and compute where to go next */
	if (offset < 0L)		/* but make sure it is positive */
		offset = 0L;
	fseek(f, offset, 0);
	p = postbuf;
	for (i = 0; i < POSTSIZE; i++) {
		*p++ = getc(f);
		if (feof(f)) {
			p--;
			break;
		}
	}

	/*
	 * Now search backwards for the VERSION byte.  The postamble
	 * pointer will be four bytes behind that.
	 */
	while (--i >= 0) {
		if (UnSign8(*--p) == DVI_VERSION)
			goto foundit;
		if (UnSign8(*p) != DVI_FILLER)
			break;
	}
	return (-1);		/* cannot find postamble ptr */

foundit:
	/*
	 * Change offset from the position at the beginning of postbuf
	 * to the position of the VERSION byte, and seek to four bytes
	 * before that.  Then get a long and use its value to seek to
	 * the postamble itself.
	 */
	offset += p - postbuf;
	fseek(f, offset - 4L, 0);
	fGetLong(f, n);
	offset = n;
	fseek(f, offset, 0);
	return (0);		/* success */
}

#else !ORIGINAL_CODE

FindPostAmble(f)
	register FILE *f;
{
	register long offset;
	register i32 n;
	
	offset = -4;	/* At least four bytes of DVI_FILLER must be present. */
	do {
		offset -= 1;
		(void) fseek(f, offset, 2);
		n = fgetbyte(f);
	} while (n == DVI_FILLER);

	if (n != DVI_VERSION)
		return (-1);	/* Bad version of DVI file */
	
	/*
	 * Position file four bytes before DVI_VERSION byte,
	 * and read a long.  Use that long to seek to the
	 * beginning of the postamble itself.
	 */
	offset -= 4;
	(void) fseek(f, offset, 2);
	fGetLong(f, n);
	offset = n;
	(void) fseek(f, offset, 0);
	return (0);		/* success */
}
#endif ORIGINAL_CODE
