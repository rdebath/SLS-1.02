/* $XConsortium: parse.c,v 1.12 91/07/30 14:10:46 keith Exp $ */
/*
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
 */

/*
 * parse.c
 *
 * parse dvi input
 */

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <ctype.h>
#include "DviP.h"

static int StopSeen = 0;
static ParseDrawFunction(), ParseDeviceControl();
static push_env(), pop_env();

extern char *GetWord(), *GetLine ();

#define HorizontalMove(dw, delta)	((dw)->dvi.state->x += (delta))

#define charWidth(fi,c) (\
    (fi)->per_char ?\
	(fi)->per_char[(c) - (fi)->min_char_or_byte2].width\
    :\
	(fi)->max_bounds.width\
)
    
ParseInput(dw)
    register DviWidget	dw;
{
	int		n, k;
	int		c;
	char		Buffer[BUFSIZ];
	int		NextPage;
	int		prevFont;
	int		otherc;
	unsigned char	tc;

	StopSeen = 0;

	/*
	 * make sure some state exists
	 */

	if (!dw->dvi.state)
	    push_env (dw);
	for (;;) {
		switch (DviGetC(dw, &c)) {
		case '\n':	
			break;
		case ' ':	/* when input is text */
		case 0:		/* occasional noise creeps in */
			break;
		case '{':	/* push down current environment */
			push_env(dw);
			break;
		case '}':
			pop_env(dw);
			break;
		/*
		 * two motion digits plus a character
		 */
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			HorizontalMove(dw, (c-'0')*10 +
					   DviGetC(dw,&otherc)-'0');
			/* fall through */
		case 'c':	/* single ascii character */
			(void) DviGetC(dw,&c);
		    	if (c == ' ')
			    break;
			tc = c;
			PutCharacters (dw, &tc, 1);
			break;
		case 'C':
			GetWord(dw, Buffer, BUFSIZ);
			{
	    	    	    DviCharNameMap	*map;
			    int			i;
			    unsigned char	*ligature;
    	    	    
			    c = -1;
	    	    	    map = QueryFontMap (dw, dw->dvi.state->font_number);
	    	    	    if (map)
			    {
		    	    	c = DviCharIndex (map, Buffer);
				if (c == -1)
				{
				    ligature = DviCharIsLigature (map, Buffer);
				    if (ligature) {
					i = strlen ((char *) ligature);
					PutCharacters (dw, ligature, i);
					break;
   				    }
				}
			    }
			    prevFont = -1;
	    	    	    if (c == -1) {
			    	for (i = 1; map = QueryFontMap (dw, i); i++)
				    if (map->special)
				    	if ((c = DviCharIndex (map, Buffer)) != -1) {
					    prevFont = dw->dvi.state->font_number;
					    dw->dvi.state->font_number = i;
					    break;
				    	}
			    }
			    if (c != -1)
			    {
				tc = c;
				PutCharacters (dw, &tc, 1);
			    }
			    if (prevFont != -1)
				dw->dvi.state->font_number = prevFont;
			}
			break;
		case 'D':	/* draw function */
			GetLine(dw, Buffer, BUFSIZ);
			ParseDrawFunction(dw, Buffer);
			break;
		case 's':	/* ignore fractional sizes */
			n = GetNumber(dw);
			dw->dvi.state->font_size = n;
			dw->dvi.state->line_width = FontSizeInDevice(dw, n / 10.0);
			break;
		case 'f':
			n = GetNumber(dw);
			dw->dvi.state->font_number = n;
			break;
		case 'H':	/* absolute horizontal motion */
			k = GetNumber(dw);
			HorizontalGoto(dw, k);
			break;
		case 'h':	/* relative horizontal motion */
			k = GetNumber(dw);
			HorizontalMove(dw, k);
			break;
		case 'w':	/* word space */
			break;
		case 'V':
			n = GetNumber(dw);
			VerticalGoto(dw, n);
			break;
		case 'v':
			n = GetNumber(dw);
			VerticalMove(dw, n);
			break;
		case 'P':	/* new spread */
			break;
		case 'p':	/* new page */
			(void) GetNumber(dw);
			NextPage = dw->dvi.current_page + 1;
			RememberPagePosition(dw, NextPage);
			FlushCharCache (dw);
			return(NextPage);
		case 'n':	/* end of line */
			GetNumber(dw);
			GetNumber(dw);
			HorizontalGoto(dw, 0);
			break;
		case '#':	/* comment */
			GetLine(dw, NULL, 0);
			break;
		case 'x':	/* device control */
			ParseDeviceControl(dw);
			break;
		case EOF:
			dw->dvi.last_page = dw->dvi.current_page;
			FlushCharCache (dw);
			return dw->dvi.current_page;
		default:
			break;
		}
	}
}

static
push_env(dw)
	DviWidget	dw;
{
	DviState	*new;

	new = (DviState *) XtMalloc (sizeof (*new));
	if (dw->dvi.state)
		*new = *(dw->dvi.state);
	else {
		new->font_size = 10;
		new->font_number = 1;
		new->line_style = 0;
		new->line_width = FontSizeInDevice(dw, 10/10);
		new->x = 0;
		new->y = 0;
	}
	new->next = dw->dvi.state;
	dw->dvi.state = new;
}

static
pop_env(dw)
	DviWidget	dw;
{
	DviState	*old;

	old = dw->dvi.state;
	dw->dvi.state = old->next;
	XtFree ((char *) old);
}

static
InitTypesetter (dw)
	DviWidget	dw;
{
	while (dw->dvi.state)
		pop_env (dw);
	push_env (dw);
	FlushCharCache (dw);
}

SetFont (dw)
    DviWidget	dw;
{
    dw->dvi.cache.font_size = dw->dvi.state->font_size;
    dw->dvi.cache.font_number = dw->dvi.state->font_number;
    dw->dvi.cache.font = QueryFont (dw,
			  dw->dvi.cache.font_number,
			  dw->dvi.cache.font_size);
}

PutCharacters (dw, src, len)
    DviWidget	    dw;
    unsigned char   *src;
    int		    len;
{
    int	    xx, yx;
    int	    fx, fy;
    char    *dst;
    int	    c;

    xx = ToX(dw, dw->dvi.state->x);
    yx = ToX(dw, dw->dvi.state->y);
    fy = FontSizeInPixels (dw, dw->dvi.state->font_size);
    fx = fy * len;
    /*
     * quick and dirty extents calculation:
     */
    if (yx + fy >= dw->dvi.extents.y1 &&
	yx - fy <= dw->dvi.extents.y2 &&
	xx + fx >= dw->dvi.extents.x1 &&
	xx - fx <= dw->dvi.extents.x2)
    {
	register XFontStruct	*font;
	register XTextItem		*text;

	if (!dw->dvi.display_enable)
	    return FALSE;

	if (yx != dw->dvi.cache.y ||
	    dw->dvi.cache.char_index + len > DVI_CHAR_CACHE_SIZE)
	    FlushCharCache (dw);
	/*
	 * load a new font, if the current block is not empty,
	 * step to the next.
	 */
	if (dw->dvi.cache.font_size != dw->dvi.state->font_size ||
	    dw->dvi.cache.font_number != dw->dvi.state->font_number)
	{
	    SetFont (dw);
	    if (dw->dvi.cache.cache[dw->dvi.cache.index].nchars != 0) {
		++dw->dvi.cache.index;
		if (dw->dvi.cache.index >= dw->dvi.cache.max)
		    FlushCharCache (dw);
		dw->dvi.cache.cache[dw->dvi.cache.index].nchars = 0;
	    }
	}
	if (xx != dw->dvi.cache.x) {
	    if (dw->dvi.cache.cache[dw->dvi.cache.index].nchars != 0) {
		++dw->dvi.cache.index;
		if (dw->dvi.cache.index >= dw->dvi.cache.max)
		    FlushCharCache (dw);
		dw->dvi.cache.cache[dw->dvi.cache.index].nchars = 0;
	    }
	}
	text = &dw->dvi.cache.cache[dw->dvi.cache.index];
	font = dw->dvi.cache.font;
	dst = &dw->dvi.cache.char_cache[dw->dvi.cache.char_index];
	if (text->nchars == 0) {
	    text->chars = dst;
	    text->delta = xx - dw->dvi.cache.x;
	    if (font != dw->dvi.font) {
		text->font = font->fid;
		dw->dvi.font = font;
	    } else
		text->font = None;
	    dw->dvi.cache.x += text->delta;
	}
	dw->dvi.cache.char_index += len;
	text->nchars += len;
	while (len--)
	{
	    c = *src++;
	    *dst++ = c;
	    dw->dvi.cache.x += charWidth(font,c);
	}
	return TRUE;
    }
    return FALSE;
}

static
ParseDrawFunction(dw, buf)
	DviWidget	dw;
	char		*buf;
{
    int	n, m, n1, m1;

    SetGCForDraw (dw);
    switch (buf[0]) {
    case 'l':				/* draw a line */
	sscanf(buf+1, "%d %d", &n, &m);
	DrawLine(dw, n, m);
	break;
    case 'c':				/* circle */
	sscanf(buf+1, "%d", &n);
	DrawCircle(dw, n);
	break;
    case 'e':				/* ellipse */
	sscanf(buf+1, "%d %d", &m, &n);
	DrawEllipse(dw, m, n);
	break;
    case 'a':				/* arc */
	sscanf(buf+1, "%d %d %d %d", &n, &m, &n1, &m1);
	DrawArc(dw, n, m, n1, m1);
	break;
    case '~':				/* wiggly line */
	DrawSpline(dw, buf+1,1);
	break;
    case 't':				/* line width */
	sscanf(buf+1, "%d", &n);
	dw->dvi.state->line_width = n;
	break;
    case 's':				/* line style */
	sscanf(buf+1, "%d", &n);
	/* XXX */
	break;
    default:
	/* warning("unknown drawing function %s", buf); */
	break;
    }
} 

static
ParseDeviceControl(dw)				/* Parse the x commands */
	DviWidget	dw;
{
    char str[20], str1[50];
    int c, n;
    extern int LastPage, CurrentPage;

    GetWord (dw, str, 20);
    switch (str[0]) {			/* crude for now */
    case 'T':				/* output device */
	GetWord(dw, str, 20);
	break;
    case 'i':				/* initialize */
	InitTypesetter (dw);
	break;
    case 't':				/* trailer */
	break;
    case 'p':				/* pause -- can restart */
	break;
    case 's':				/* stop */
	StopSeen = 1;
	return;
    case 'r':				/* resolution when prepared */
	SetDeviceResolution (dw, GetNumber (dw));
	break;
    case 'f':				/* font used */
	n = GetNumber(dw);
	GetWord(dw, str, 20);
	GetLine(dw, str1, 50);
	SetFontPosition(dw, n, str, str1);
	break;
    case 'H':				/* char height */
	break;
    case 'S':				/* slant */
	break;
    }
    while (DviGetC(dw,&c) != '\n')		/* skip rest of input line */
	    if (c == EOF)
		    return;
    return;
}
