/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef OW_I18N

static	char	sccsid[] ="@(#) i18n.c 1.3 91/09/14 SMI";

#include "i18n.h"
#include "mem.h"

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xlibos.h>
#include <X11/Xatom.h>

/*
 * When converting the wide char to CTEXT, we need to estimate the
 * space, but there are no right way to do this without actually
 * converting.  "wslen(wchar) * sizeof(wchar_t)" will give us the how
 * many bytes consume by the characters, but this does not include the
 * any control sequences.  I decided use fudge bytes for this control
 * sequnces for now.  This is absolutely bad idea to having a this
 * value, but otherwise we need to convert it twice.  One control
 * sequnce require the 3 bytes, so, following allow to switch the code
 * set 6 times.
 */
#define WCSTOCTS_FUDGE_BYTES	(3 * 6)

wchar_t *
mbstowcsdup(mbs)
register char	*mbs;
{
	register int		n;
	register wchar_t	*wcs;

	if (mbs == NULL)
	    return NULL;

	n = strlen(mbs) + 1;
	wcs = (wchar_t *) MemAlloc(n * sizeof(wchar_t));
	mbstowcs(wcs, mbs, n);
#if DEBUG > 4
fprintf(stderr, "mbstowcsdup: mbs [%s] -> wcs [%ws]\n", mbs, wcs);
#endif

	return wcs;
}


wchar_t *
ctstowcsdup(cts)
register char	*cts;
{
	register int		n;
	register wchar_t	*wcs;

	if (cts == NULL)
	    return NULL;

	n = strlen(cts) + 1;
	wcs = (wchar_t *) MemAlloc(n * sizeof(wchar_t));
	ctstowcs(wcs, cts, n);

#ifdef DEBUG > 4
fprintf(stderr, "ctstowcsdup: cts [%d:", n);
ascii_dump(cts);
fprintf(stderr, "] -> wcs [%d:%ws]\n",
	(n * sizeof(wchar_t)), wcs);
#endif

	return wcs;
}


char *
ctstombsdup(cts)
register char	*cts;
{
	register int		n;
	register wchar_t	*wcs;
	register char		*mbs;

	wcs = ctstowcsdup(cts);
	if (wcs == NULL)
		return NULL;

	n = wslen(wcs) * sizeof(wchar_t) + 1;
	mbs = (char *) MemAlloc(n);
	wcstombs(mbs, wcs, n);

	MemFree (wcs);
	return mbs;
}


char *
wcstoctsdup(wcs)
register wchar_t	*wcs;
{
	register int		n;
	register char		*cts;

	if (wcs == NULL)
		return NULL;

	n = wslen(wcs) * sizeof(wchar_t) + WCSTOCTS_FUDGE_BYTES + 1;
	cts = (char *) MemAlloc(n);
	/*
	 * FIX_ME: We should check the result from wcstotcs, and if
	 * cts does not have a enough space, we should re-allocate the
	 * space and try again.
	 */
	wcstocts(cts, wcs, n);

#ifdef DEBUG > 4
fprintf(stderr, "wcstoctsdup: wcs [%d:%ws] -> cts [%d:",
	wslen(wcs), wcs, n+1);
ascii_dump(cts);
fprintf(stderr, "]\n");
#endif

	return cts;
}

XwcFetchName(dpy, w, name)
Display	*dpy;
Window	w;
wchar_t	**name;
{
	return XwcGetTextWindowProperty(XA_WM_NAME, dpy, w, name);
}


XwcGetIconName(dpy, w, name)
Display	*dpy;
Window	w;
wchar_t	**name;
{
	return XwcGetTextWindowProperty(XA_WM_ICON_NAME, dpy, w, name);
}


XwcGetTextWindowProperty(props, dpy, w, name)
Atom	props;
Display	*dpy;
Window	w;
wchar_t	**name;
{
	register int	len;
	int		ret;
	Atom		actual_type;
	int		actual_format;
	unsigned long	nitems;
	unsigned long	leftover;
	unsigned char	*data = NULL;
	extern Atom	AtomCompoundText;

	ret = 0;
	if (XGetWindowProperty(dpy, w, props, 0L, (long)BUFSIZ, False,
			       AnyPropertyType, &actual_type, &actual_format,
			       &nitems, &leftover, &data) != Success)
	{
		*name = NULL;
	}
	else if ((actual_type == XA_STRING) &&  (actual_format == 8))
	{
		*name = mbstowcsdup((char *)data);
		ret = 1;
	}
	else if ((actual_type == AtomCompoundText) &&  (actual_format == 8))
	{
		*name = ctstowcsdup((char *)data);
		ret = 1;
	}
	else
		*name = NULL;

	if (data)
		Xfree((char *)data);
	return ret;
}


#ifdef DEBUG > 4
ascii_dump(s)
register unsigned char	*s;
{
	while (*s)
	{
		if (*s < ' ')
			fprintf(stderr, "^%c", *s + '@');
		else if (*s > 0x80)
			fprintf(stderr, "(%2x)", *s);
		else
			fputc(*s, stderr);
		s++;
	}
}
#endif DEBUG

#endif OW_I18N
