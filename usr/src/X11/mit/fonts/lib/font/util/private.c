/*
 * $XConsortium: private.c,v 1.1 91/05/29 15:27:02 keith Exp $
 *
 * Copyright 1991 Massachusetts Institute of Technology
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

#include    "fontmisc.h"
#include    "fontstruct.h"

int _FontPrivateAllocateIndex;

int
AllocateFontPrivateIndex ()
{
    return _FontPrivateAllocateIndex++;
}

int
ResetFontPrivateIndex ()
{
    _FontPrivateAllocateIndex = 0;
}

Bool
_FontSetNewPrivate (pFont, n, ptr)
    FontPtr pFont;
    int	    n;
    pointer ptr;
{
    pointer *new;

    if (n > pFont->maxPrivate)
    {
	new = (pointer *) xrealloc (pFont->devPrivates, (n + 1) * sizeof (pointer));
	if (!new)
	    return FALSE;
	pFont->maxPrivate = n;
	pFont->devPrivates = new;
    }
    pFont->devPrivates[n] = ptr;
    return TRUE;
}
