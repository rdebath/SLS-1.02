/* $Header: /home/x_cvs/mit/fonts/lib/font/fontfile/renderers.c,v 1.4 1992/08/25 08:55:34 dawes Exp $ */
/*
 * $XConsortium: renderers.c,v 1.1 91/05/10 14:46:38 keith Exp $
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

#include    "fontfilest.h"

/*
FontFileRegisterFontFileFunctions ()
{
    BitmapRegisterFontFileFunctions ();
    SpeedoRegisterFontFileFunctions ();
#ifdef TYPE1
    Type1RegisterFontFileFunctions();
#endif
}
*/

static FontRenderersRec	renderers;

Bool
FontFileRegisterRenderer (renderer)
    FontRendererPtr renderer;
{
    int		    i;
    FontRendererPtr *new;

    for (i = 0; i < renderers.number; i++)
	if (!strcmp (renderers.renderers[i]->fileSuffix, renderer->fileSuffix))
	    return TRUE;
    i = renderers.number + 1;
    new = (FontRendererPtr *) xrealloc (renderers.renderers, sizeof *new * i);
    if (!new)
	return FALSE;
    renderer->number = i - 1;
    renderers.renderers = new;
    renderers.renderers[i - 1] = renderer;
    renderers.number = i;
    return TRUE;
}

FontRendererPtr
FontFileMatchRenderer (fileName)
    char    *fileName;
{
    int			i;
    int			fileLen;
    FontRendererPtr	r;
    
    fileLen = strlen (fileName);
    for (i = 0; i < renderers.number; i++)
    {
	r = renderers.renderers[i];
	if (fileLen >= r->fileSuffixLen &&
	    !strcmp (fileName + fileLen - r->fileSuffixLen, r->fileSuffix))
	{
	    return r;
	}
    }
    return 0;
}
