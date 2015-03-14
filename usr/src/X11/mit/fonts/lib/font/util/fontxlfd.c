/*
 * $XConsortium: fontxlfd.c,v 1.4 91/07/20 11:24:00 rws Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include	"fontmisc.h"
#include	"fontstruct.h"
#include	"fontxlfd.h"
#include	<X11/Xos.h>

static char *
GetInt(ptr, val)
    char       *ptr;
    int        *val;
{
    ptr++;
    if (*ptr == '*') {
	*val = -1;
	ptr++;
    } else
	for (*val = 0; *ptr >= '0' && *ptr <= '9';)
	    *val = *val * 10 + *ptr++ - '0';
    if (*ptr == '-')
	return ptr;
    return (char *) 0;
}

Bool
FontParseXLFDName(fname, vals, subst)
    char       *fname;
    FontScalablePtr vals;
    int         subst;
{
    register char *ptr;
    register char *ptr1,
               *ptr2,
               *ptr3,
               *ptr4;
    FontScalableRec tmpvals;
    char        replaceChar = '0';
    char        tmpBuf[1024];
    int         spacingLen;

    if (!(*fname == '-') ||	/* foundry */
	    !(ptr = index(fname + 1, '-')) ||	/* family_name */
	    !(ptr = index(ptr + 1, '-')) ||	/* weight_name */
	    !(ptr = index(ptr + 1, '-')) ||	/* slant */
	    !(ptr = index(ptr + 1, '-')) ||	/* setwidth_name */
	    !(ptr = index(ptr + 1, '-')) ||	/* add_style_name */
	    !(ptr1 = ptr = index(ptr + 1, '-')) ||	/* pixel_size */
	    !(ptr = GetInt(ptr, &tmpvals.pixel)) ||
	    !(ptr = GetInt(ptr, &tmpvals.point)) ||	/* point_size */
	    !(ptr = GetInt(ptr, &tmpvals.x)) ||	/* resolution_x */
	    !(ptr2 = ptr = GetInt(ptr, &tmpvals.y)) ||	/* resolution_y */
	    !(ptr3 = ptr = index(ptr + 1, '-')) ||	/* spacing */
	    !(ptr4 = ptr = GetInt(ptr, &tmpvals.width)) ||	/* average_width */
	    !(ptr = index(ptr + 1, '-')) ||	/* charset_registry */
	    index(ptr + 1, '-'))/* charset_encoding */
	return FALSE;

    spacingLen = ptr3 - ptr2 + 1;
    switch (subst) {
    case FONT_XLFD_REPLACE_NONE:
	*vals = tmpvals;
	break;
    case FONT_XLFD_REPLACE_STAR:
	replaceChar = '*';
    case FONT_XLFD_REPLACE_ZERO:
	ptr = ptr1 + 1;
	*ptr++ = replaceChar;
	*ptr++ = '-';
	*ptr++ = replaceChar;
	*ptr++ = '-';
	*ptr++ = '*';
	*ptr++ = '-';
	*ptr++ = '*';
	bcopy(ptr2, ptr, spacingLen);
	ptr += spacingLen;
	*ptr++ = replaceChar;
	strcpy(ptr, ptr4);
	*vals = tmpvals;
	break;
    case FONT_XLFD_REPLACE_VALUE:
	if (vals->pixel >= 0)
	    tmpvals.pixel = vals->pixel;
	if (vals->point >= 0)
	    tmpvals.point = vals->point;
	if (vals->x >= 0)
	    tmpvals.x = vals->x;
	if (vals->y >= 0)
	    tmpvals.y = vals->y;
	if (vals->width >= 0)
	    tmpvals.width = vals->width;
	sprintf(tmpBuf, "%d-%d-%d-%d%*.*s%d%s",
		tmpvals.pixel, tmpvals.point, tmpvals.x, tmpvals.y,
		spacingLen, spacingLen, ptr2, tmpvals.width, ptr4);
	strcpy(ptr1 + 1, tmpBuf);
	break;
    }
    return TRUE;
}
