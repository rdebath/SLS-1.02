/*
 * $XConsortium: parsedpy.c,v 1.7 89/12/10 17:00:56 rws Exp $
 *
 * parse_displayname - utility routine for splitting up display name strings
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
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
 * Author:  Jim Fulton, MIT X Consortium
 */

#include <stdio.h>			/* for NULL */
#include <ctype.h>			/* for isascii() and isdigit() */
#include <X11/Xos.h>			/* for index() and string routines */
#include <X11/Xlib.h>			/* for Family contants */
#ifdef hpux
#include <sys/utsname.h>		/* for struct utsname */
#endif
#include <X11/Xauth.h>			/* for FamilyLocal */
#include <X11/Xmu/SysUtil.h>

#ifdef UNIXCONN
#define UNIX_CONNECTION "unix"
#define UNIX_CONNECTION_LENGTH 4
#endif

extern char *malloc();


/*
 * private utility routines
 */

/*static*/ char *copystring (src, len)
    char *src;
    int len;
{
    char *cp;

    if (!src && len != 0) return NULL;
    cp = malloc (len + 1);
    if (cp) {
	if (src) strncpy (cp, src, len);
	cp[len] = '\0';
    }
    return cp;
}


char *get_local_hostname (buf, maxlen)
    char *buf;
    int maxlen;
{
    buf[0] = '\0';
    (void) XmuGetHostname (buf, maxlen);
    return (buf[0] ? buf : NULL);
}

#ifndef UNIXCONN
static char *copyhostname ()
{
    char buf[256];

    return (get_local_hostname (buf, sizeof buf) ? 
	    copystring (buf, strlen (buf)) : NULL);
}
#endif

/*
 * parse_displayname - display a display string up into its component parts
 */
Bool parse_displayname (displayname, familyp, hostp, dpynump, scrnump, restp)
    char *displayname;
    int *familyp;			/* return */
    char **hostp;			/* return */
    int *dpynump, *scrnump;		/* return */
    char **restp;			/* return */
{
    char *ptr;				/* work variables */
    int len;				/* work variable */
    int family = -1;			/* value to be returned */
    char *host = NULL;			/* must free if set and error return */
    int dpynum = -1;			/* value to be returned */
    int scrnum = 0;			/* value to be returned */
    char *rest = NULL;			/* must free if set and error return */
    Bool dnet = False;			/* if true then using DECnet */

					/* check the name */
    if (!displayname || !displayname[0]) return False;

					/* must have at least :number */
    ptr = index (displayname, ':');
    if (!ptr || !ptr[1]) return False;
    if (ptr[1] == ':') {
	if (ptr[2] == '\0') return False;
	dnet = True;
    }


    /*
     * get the host string; if none is given, use the most effiecient path
     */

    len = (ptr - displayname);	/* length of host name */
    if (len == 0) {			/* choose most efficient path */
#ifdef UNIXCONN
	host = copystring (UNIX_CONNECTION, UNIX_CONNECTION_LENGTH);
	family = FamilyLocal;
#else
	if (dnet) {
	    host = copystring ("0", 1);
	    family = FamilyDECnet;
	} else {
	    host = copyhostname ();
	    family = FamilyInternet;
	}
#endif
    } else {
	host = copystring (displayname, len);
	if (dnet) {
	    family = dnet;
	} else {
#ifdef UNIXCONN
	    if (host && strcmp (host, UNIX_CONNECTION) == 0)
	      family = FamilyLocal;
	    else
#endif
	      family = FamilyInternet;
	}
    }

    if (!host) return False;


    /*
     * get the display number; we know that there is something after the
     * colon (or colons) from above.  note that host is now set and must
     * be freed if there is an error.
     */

    if (dnet) ptr++;			/* skip the extra DECnet colon */
    ptr++;				/* move to start of display num */
    {
	register char *cp;

	for (cp = ptr; *cp && isascii(*cp) && isdigit(*cp); cp++) ;
	len = (cp - ptr);
					/* check present and valid follow */
	if (len == 0 || (*cp && *cp != '.')) {
	    free (host);
	    return False;
	}
	
	dpynum = atoi (ptr);		/* it will handle num. as well */
	ptr = cp;
    }

    /*
     * now get screen number if given; ptr may point to nul at this point
     */
    if (ptr[0] == '.') {
	register char *cp;

	ptr++;
	for (cp = ptr; *cp && isascii(*cp) && isdigit(*cp); cp++) ;
	len = (cp - ptr);
	if (len == 0 || (*cp && *cp != '.')) {	/* all prop name */
	    free (host);
	    return False;
	}

	scrnum = atoi (ptr);		/* it will handle num. as well */
	ptr = cp;
    }

    /*
     * and finally, get any additional stuff that might be following the
     * the screen number; ptr must point to a period if there is anything
     */

    if (ptr[0] == '.') {
	ptr++;
	len = strlen (ptr);
	if (len > 0) {
	    rest = copystring (ptr, len);
	    if (!rest) {
		free (host);
		return False;
	    }
	}
    }

    /*
     * and we are done!
     */

    *familyp = family;
    *hostp = host;
    *dpynump = dpynum;
    *scrnump = scrnum;
    *restp = rest;
    return True;
}

	    
