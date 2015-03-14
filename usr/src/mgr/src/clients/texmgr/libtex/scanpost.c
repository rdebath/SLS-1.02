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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/scanpost.c,v 1.3 89/02/13 14:31:20 grunwald Exp Locker: grunwald $";
#endif

/*
 * ScanPostAmble - read a DVI postamble.
 */

#include <stdio.h>
#include "types.h"
#include "dvicodes.h"
#include "fio.h"
#include "postamble.h"

ScanPostAmble(f, headerfunc, fontfunc)
	register FILE *f;
	int (*headerfunc)();
	register int (*fontfunc)();
{
	register int n;
	register char *s;
	char name[512];

	if (FindPostAmble(f)) {
	  GripeCannotFindPostamble();
	  return(1);
	}
	if (GetByte(f) != Sign8(DVI_POST)) {
	  GripeMissingOp("POST");
	  return(1);
	}

	/* Read the postamble info stuff. */
	{
		struct PostAmbleInfo pai;
		register struct PostAmbleInfo *p = &pai;

		p->pai_PrevPagePointer = GetLong(f);
		p->pai_Numerator = GetLong(f);
		p->pai_Denominator = GetLong(f);
		p->pai_DVIMag = GetLong(f);
		p->pai_TallestPageHeight = GetLong(f);
		p->pai_WidestPageWidth = GetLong(f);
		p->pai_DVIStackSize = GetWord(f);
		p->pai_NumberOfPages = GetWord(f);

		(*headerfunc)(p);
	}

	/* Now read all the font definitions. */
	{
		struct PostAmbleFont paf;
		register struct PostAmbleFont *p = &paf;

		for (;;) {
			switch (UnSign8(getc(f))) {

			case DVI_FNTDEF1:
				p->paf_DVIFontIndex = UnSign8(getc(f));
				break;

			case DVI_FNTDEF2:
				p->paf_DVIFontIndex = UnSign16(GetWord(f));
				break;

			case DVI_FNTDEF3:
				p->paf_DVIFontIndex = UnSign24(Get3Byte(f));
				break;

			case DVI_FNTDEF4:
				p->paf_DVIFontIndex = GetLong(f);
				break;

			case DVI_POSTPOST:
				return(0);

			default:
				GripeMissingOp("POSTPOST");
				return(1);
				/*NOTREACHED*/
			}
			p->paf_DVIChecksum = GetLong(f);
			p->paf_DVIMag = GetLong(f);
			p->paf_DVIDesignSize = GetLong(f);
			p->paf_n1 = UnSign8(getc(f));
			p->paf_n2 = UnSign8(getc(f));
			p->paf_name = name;	/* never trust people not to
						   clobber it */
			n = p->paf_n1 + p->paf_n2;
			s = name;
			while (--n >= 0)
				*s++ = GetByte(f);
			*s = 0;
			(*fontfunc)(p);
		}
	}
}
