/* $XConsortium: osglue.c,v 1.5 91/10/24 21:46:59 rws Exp $ */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)osglue.c,v 4.6 1991/07/09 14:07:30 lemke Exp $
 *
 */

/*
 * this is miscellaneous OS specific stuff.
 *
 * Catalogue support, alternate servers, and cloneing
 */

#include "osstruct.h"
#include <stdio.h>
#define  XK_LATIN1
#include <X11/keysymdef.h>

Bool        drone_server = FALSE;
extern Bool CloneSelf;
extern int  ListenSock;
extern char *progname;
extern char *configfilename;

static int  num_alts;
static AlternateServerPtr alt_servers = (AlternateServerPtr) 0;

/*
 * XXX
 *
 * Catalogue support is absolutely minimal.  Some guts are here, but
 * we don't actually do anything with them so the only one exported is
 * 'all'.  Be warned that other parts of the server may incorrectly
 * assume the catalogue list is global, and will therefore need fixing.
 *
 */

static char *catalogue_name = "all";

static Bool			/* stolen from R4 Match() */
pattern_match(pat, plen, string)
    char       *pat;
    int         plen;
    char       *string;
{
    register int i,
                l;
    int         j,
                m,
                res;
    register char cp,
                cs;
    int         head,
                tail;

    head = 0;
    tail = plen;

    res = -1;
    for (i = 0; i < head; i++) {
	cp = pat[i];
	if (cp == XK_question) {
	    if (!string[i])
		return res;
	    res = 0;
	} else if (cp != string[i])
	    return res;
    }
    if (head == plen)
	return (string[head] ? res : 1);
    l = head;
    while (++i < tail) {
	/* we just skipped an asterisk */
	j = i;
	m = l;
	while ((cp = pat[i]) != XK_asterisk) {
	    if (!(cs = string[l]))
		return 0;
	    if ((cp != cs) && (cp != XK_question)) {
		m++;
		cp = pat[j];
		if (cp == XK_asterisk) {
		    if (!string[m])
			return 0;
		} else {
		    while ((cs = string[m]) != cp) {
			if (!cs)
			    return 0;
			m++;
		    }
		}
		l = m;
		i = j;
	    }
	    l++;
	    i++;
	}
    }
    m = strlen(&string[l]);
    j = plen - tail;
    if (m < j)
	return 0;
    l = (l + m) - j;
    while (cp = pat[i]) {
	if ((cp != string[l]) && (cp != XK_question))
	    return 0;
	l++;
	i++;
    }
    return 1;
}

int
ListCatalogues(pattern, patlen, maxnames, catalogues, len)
    char       *pattern;
    int         patlen;
    int         maxnames;
    char      **catalogues;
    int        *len;
{
    int         count = 0;
    char       *catlist = NULL;
    int         size = 0;

    if (maxnames) {
	if (pattern_match(pattern, patlen, catalogue_name)) {
	    size = strlen(catalogue_name);
	    catlist = (char *) fsalloc(size + 1);
	    if (!catlist)
		goto bail;
	    *catlist = size;
	    bcopy(catalogue_name, &catlist[1], size);
	    size++;		/* for length */
	    count++;
	}
    }
bail:
    *len = size;
    *catalogues = catlist;
    return count;
}

/*
 * check if catalogue list is valid
 */

int
ValidateCatalogues(num, cats)
    int        *num;
    char       *cats;
{
    char       *c = cats;
    int         i,
                len;

    for (i = 0; i < *num; i++) {
	len = *c++;
	if (strncmp(c, catalogue_name, len)) {
	    *num = i;		/* return bad entry index */
	    return FSBadName;
	}
	c += len;
    }
    return FSSuccess;
}

int
SetAlternateServers(list)
    char       *list;
{
    char       *t,
               *st;
    AlternateServerPtr alts,
                a;
    int         num,
                i;

    t = list;
    num = 1;
    while (*t) {
	if (*t == ',')
	    num++;
	t++;
    }

    a = alts = (AlternateServerPtr) fsalloc(sizeof(AlternateServerRec) * num);
    if (!alts)
	return FSBadAlloc;

    st = t = list;
    a->namelen = 0;
    while (*t) {
	if (*t == ',') {
	    a->name = (char *) fsalloc(a->namelen);
	    if (!a->name) {
		/* XXX  -- leak */
		return FSBadAlloc;
	    }
	    bcopy(st, a->name, a->namelen);
	    a->subset = FALSE;	/* XXX */
	    a++;
	    t++;
	    st = t;
	    a->namelen = 0;
	} else {
	    a->namelen++;
	    t++;
	}
    }
    a->name = (char *) fsalloc(a->namelen);
    if (!a->name) {
	/* XXX  -- leak */
	return FSBadAlloc;
    }
    bcopy(st, a->name, a->namelen);
    a->subset = FALSE;		/* XXX */

    for (i = 0; i < num_alts; i++) {
	fsfree((char *) alt_servers[i].name);
    }
    fsfree((char *) alt_servers);
    num_alts = num;
    alt_servers = alts;
    return FSSuccess;
}

int
ListAlternateServers(svrs)
    AlternateServerPtr *svrs;
{
    *svrs = alt_servers;
    return num_alts;
}

/*
 * here's some fun stuff.  in order to cleanly handle becoming overloaded,
 * this allows us to clone ourselves.  the parent keeps the Listen
 * socket open, and sends it to itself.  the child stops listening,
 * and becomes a drone, hanging out till it loses all its clients.
 */
int
CloneMyself()
{
    int         child;
    char        sockarg[32];
    int         i;
    int         lastfdesc;

    assert(!drone_server);	/* a drone shouldn't hit this */

    if (!CloneSelf)
	return -1;

#if defined(hpux) || defined(SVR4)
    lastfdesc = _NFILE - 1;
#else
    lastfdesc = getdtablesize() - 1;
#endif				/* hpux */

    NoticeF("attempting clone...\n");
    child = fork();
    if (child == -1) {
	/* failed to fork */
	ErrorF("Clone failed to fork()\n");
	return -1;
    }
    /*
     * Note:  they still share the same process group, and killing the parent
     * will take out all the kids as well.  this is considered a feature (at
     * least until i'm convinced otherwise)
     */
    if (child == 0) {
	StopListening();
	NoticeF("Clone: child becoming drone\n");
	drone_server = TRUE;
	return 1;
    } else {			/* parent */
	NoticeF("Clone: parent revitalizing as %s\n", progname);
	CloseErrors();
	/* XXX should we close stdio as well? */
	for (i = 3; i < lastfdesc; i++) {
	    if (i != ListenSock)
		(void) close(i);
	}
	sprintf(sockarg, "%d", ListenSock);
	execlp(progname, progname,
	       "-ls", sockarg,
	       "-cf", configfilename,
	       NULL);
	InitErrors();		/* reopen errors, since we don't want to lose
				 * this */
	Error("Clone failed");
	FatalError("Failed to clone self\n");
    }
    /* NOTREACHED */
}
