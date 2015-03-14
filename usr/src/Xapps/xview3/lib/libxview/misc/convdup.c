#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)convdup.c 1.8 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#ifdef OW_I18N
#include <stdio.h>
#include <stdlib.h>
#include <xview/xv_i18n.h>

/*
 * When converting the wide char to CTEXT, we need to estimate the
 * space, but there are no right way to do this without actually
 * converting.  "wslen(wchar) * MB_CUR_MAX" will give us the how many
 * bytes consume by the characters, but this does not include the any
 * control sequences.  I decided use fudge bytes for this control
 * sequnces for now.  This is absolutely bad idea to having a this
 * value, but otherwise we need to convert it twice.  One control
 * sequnce require the 3 bytes, so, following allow to switch the code
 * set 6 times.
 */
#define WCSTOCTS_FUDGE_BYTES	(3 * 6)

extern char	*malloc();

wchar_t *
mbstowcsdup(mbs)
register char	*mbs;
{
	register int		n;
	register wchar_t	*wcs;

	if (mbs == NULL)
	    return NULL;

	n = strlen(mbs) + 1;
	wcs = (wchar_t *) malloc(n * sizeof(wchar_t));
	mbstowcs(wcs, mbs, n);

	return wcs;
}


char *
wcstombsdup(wcs)
register wchar_t	*wcs;
{
	register int	n;
	register char	*mbs;

	if (wcs == NULL)
		return NULL;

	/*
	 * FIX_ME: multiple by sizeof (wchar_t) is not quite correct.
	 * Wide char is wide char and multibyte is multibyte, no
	 * straight relation here.  But there are no portable way to
	 * find out how many byte consume by largest/longest multibyte
	 * character.  So, using wchar_t is accetable guess (may be)?
	 */
	n = (wslen(wcs) * MB_CUR_MAX) + 1;
	mbs = malloc (n);
	wcstombs(mbs, wcs, n);

	return mbs;
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
	wcs = (wchar_t *) malloc(n * sizeof(wchar_t));
	ctstowcs(wcs, cts, n);

	return wcs;
}


char *
wcstoctsdup(wcs)
register wchar_t	*wcs;
{
	register int		n;
	register char		*cts;

	if (wcs == NULL)
		return NULL;

	n = wslen(wcs) * MB_CUR_MAX + WCSTOCTS_FUDGE_BYTES + 1;
	cts = malloc(n);
	/*
	 * FIX_ME: We should check the result from wcstotcs, and if
	 * cts does not have a enough space, we should re-allocate the
	 * space and try again.
	 */
	wcstocts(cts, wcs, n);

	return cts;
}
#endif OW_I18N
