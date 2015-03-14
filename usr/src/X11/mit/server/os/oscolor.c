/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $Header: /home/x_cvs/mit/server/os/oscolor.c,v 1.6 1992/09/17 13:42:06 dawes Exp $ */
/* $XConsortium: oscolor.c,v 1.20 91/06/30 15:58:30 rws Exp $ */
#ifdef NDBM
#ifdef __386BSD__
#include <sys/types.h>
#endif
#ifdef SDBM
#include <sdbm.h>
#else
#include <ndbm.h>
#endif
#else
#if defined(SVR4) || defined(ISC)
#include <rpcsvc/dbm.h>
#else
#include <dbm.h>
#endif
#endif
#include "rgb.h"
#include "os.h"
#include "opaque.h"

/* Note that we are assuming there is only one database for all the screens. */

#ifdef NDBM
DBM *rgb_dbm = (DBM *)NULL;
#else
int rgb_dbm = 0;
#endif

extern void CopyISOLatin1Lowered();

int
OsInitColors()
{
    if (!rgb_dbm)
    {
#ifdef NDBM
	rgb_dbm = dbm_open(rgbPath, 0, 0);
#else
	if (dbminit(rgbPath) == 0)
	    rgb_dbm = 1;
#endif
	if (!rgb_dbm) {
	    ErrorF( "Couldn't open RGB_DB '%s'\n", rgbPath );
	    return FALSE;
	}
    }
    return TRUE;
}

/*ARGSUSED*/
int
OsLookupColor(screen, name, len, pred, pgreen, pblue)
    int		screen;
    char	*name;
    unsigned	len;
    unsigned short	*pred, *pgreen, *pblue;

{
    datum		dbent;
    RGB			rgb;
    char		buf[64];
    char		*lowername;

    if(!rgb_dbm)
	return(0);

    /* we use Xalloc here so that we can compile with cc without alloca
     * when otherwise using gcc */
    if (len < sizeof(buf))
	lowername = buf;
    else if (!(lowername = (char *)Xalloc(len + 1)))
	return(0);
    CopyISOLatin1Lowered ((unsigned char *) lowername, (unsigned char *) name,
			  (int)len);

    dbent.dptr = lowername;
    dbent.dsize = len;
#ifdef NDBM
    dbent = dbm_fetch(rgb_dbm, dbent);
#else
    dbent = fetch (dbent);
#endif

    if (len >= sizeof(buf))
	Xfree(lowername);

    if(dbent.dptr)
    {
	bcopy(dbent.dptr, (char *) &rgb, sizeof (RGB));
	*pred = rgb.red;
	*pgreen = rgb.green;
	*pblue = rgb.blue;
	return (1);
    }
    return(0);
}

