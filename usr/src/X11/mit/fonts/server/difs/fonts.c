/* $XConsortium: fonts.c,v 1.13 92/05/28 17:17:15 gildea Exp $ */
/*
 * font control
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * M.I.T. not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND M.I.T. DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES,
 * DIGITAL OR M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

#include        "FS.h"
#include        "FSproto.h"
#include	<stdio.h>
#include	<X11/Xos.h>
#include	"clientstr.h"
#include	"resource.h"
#include	"difsfontst.h"
#include	"fontstruct.h"
#include	"closestr.h"
#include	"globals.h"

extern void (*ReplySwapVector[NUM_PROC_VECTORS]) ();
extern FSID FakeClientID();

static FontPathElementPtr *font_path_elements = (FontPathElementPtr *) 0;
static int  num_fpes = 0;
static FPEFunctions *fpe_functions = (FPEFunctions *) 0;
static int  num_fpe_types = 0;

static int  num_slept_fpes = 0;
static int  size_slept_fpes = 0;
static FontPathElementPtr *slept_fpes = (FontPathElementPtr *) 0;

extern FontPatternCachePtr fontPatternCache;

#define	NUM_IDS_PER_CLIENT	5

int
FontToFSError(err)
    int         err;
{
    switch (err) {
    case Successful:
	return FSSuccess;
    case AllocError:
	return FSBadAlloc;
    case BadFontName:
    case BadFontPath:
	return FSBadName;
    case BadFontFormat:
	return FSBadFormat;
    case BadCharRange:
	return FSBadRange;
    default:
	return err;
    }
}

/* XXX -- these two funcs may want to be broken into macros */
void
UseFPE(fpe)
    FontPathElementPtr fpe;
{
    fpe->refcount++;
}

void
FreeFPE(fpe)
    FontPathElementPtr fpe;
{
    fpe->refcount--;
    if (fpe->refcount == 0) {
	(*fpe_functions[fpe->type].free_fpe) (fpe);
	fsfree(fpe->name);
	fsfree(fpe);
    }
}

/*
 * note that the font wakeup queue is not refcounted.  this is because
 * an fpe needs to be added when its inited, and removed when its finally
 * freed, in order to handle any data that isn't requested, like FS events.
 *
 * since the only thing that should call these routines is the renderer's
 * init_fpe() and free_fpe(), there shouldn't be any problem in using
 * freed data.
 */
void
QueueFontWakeup(fpe)
    FontPathElementPtr fpe;
{
    int         i;
    FontPathElementPtr *new;

    for (i = 0; i < num_slept_fpes; i++) {
	if (slept_fpes[i] == fpe) {

#ifdef DEBUG
	    fprintf(stderr, "re-queueing fpe wakeup\n");
#endif

	    return;
	}
    }
    if (num_slept_fpes == size_slept_fpes) {
	new = (FontPathElementPtr *)
	    fsrealloc(slept_fpes,
		      sizeof(FontPathElementPtr) * (size_slept_fpes + 4));
	if (!new)
	    return;
	slept_fpes = new;
	size_slept_fpes += 4;
    }
    slept_fpes[num_slept_fpes] = fpe;
    num_slept_fpes++;
}

void
RemoveFontWakeup(fpe)
    FontPathElementPtr fpe;
{
    int         i,
                j;

    for (i = 0; i < num_slept_fpes; i++) {
	if (slept_fpes[i] == fpe) {
	    for (j = i; j < num_slept_fpes; j++) {
		slept_fpes[j] = slept_fpes[j + 1];
	    }
	    num_slept_fpes--;
	    return;
	}
    }
}

/* ARGSUSED */
void
FontWakeup(data, count, LastSelectMask)
    pointer     data;
    int         count;
    long       *LastSelectMask;
{
    int         i;
    FontPathElementPtr fpe;

    if (count < 0)
	return;			/* ignore -1 return from select XXX */
    /* wake up any fpe's that may be waiting for information */
    for (i = 0; i < num_slept_fpes; i++) {
	fpe = slept_fpes[i];
	(void) (*fpe_functions[fpe->type].wakeup_fpe) (fpe, LastSelectMask);
    }
}

static Bool
add_id_to_list(ids, fid)
    FontIDListPtr ids;
    Font        fid;
{
    Font       *newlist;

    /*
     * assumes the list is packed tightly
     */
    if (ids->num == ids->size) {
	/* increase size of array */
	newlist = (Font *) fsrealloc(ids->client_list,
			      sizeof(Font) * ids->size + NUM_IDS_PER_CLIENT);
	if (!newlist)
	    return FALSE;
	ids->client_list = newlist;
	ids->size += NUM_IDS_PER_CLIENT;
    }
    ids->client_list[ids->num++] = fid;
    return TRUE;
}

static void
remove_id_from_list(ids, fid)
    FontIDListPtr ids;
    Font        fid;
{
    int         i;

    for (i = 0; i < ids->num; i++) {
	if (ids->client_list[i] == fid) {
	    /* a bcopy() might be better here */
	    while (i < ids->num) {
		ids->client_list[i] = ids->client_list[i + 1];
		i++;
	    }
	    ids->num--;
	    return;
	}
    }
    assert(0);
}

static      FontIDListPtr
make_clients_id_list()
{
    FontIDListPtr ids;
    Font       *fids;

    ids = (FontIDListPtr) fsalloc(sizeof(FontIDListRec));
    fids = (Font *) fsalloc(sizeof(Font) * NUM_IDS_PER_CLIENT);
    if (!ids || !fids) {
	fsfree(ids);
	fsfree(fids);
	return (FontIDListPtr) 0;
    }
    bzero((char *) fids, sizeof(Font) * NUM_IDS_PER_CLIENT);
    ids->client_list = fids;
    ids->size = NUM_IDS_PER_CLIENT;
    ids->num = 0;
    return ids;
}

static Bool
do_open_font(client, c)
    ClientPtr   client;
    OFclosurePtr c;
{
    FontPtr     pfont = NullFont;
    FontPathElementPtr fpe;
    int         err;
    int         i;
    char       *alias,
               *newname;
    int         newlen;
    ClientFontPtr cfp;
    fsOpenBitmapFontReply rep;
    Font        orig;
    FontIDListPtr *idlist,
                ids;

    if (client->clientGone == CLIENT_GONE) {
	if (c->current_fpe < c->num_fpes) {
	    fpe = c->fpe_list[c->current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto dropout;
    }
    while (c->current_fpe < c->num_fpes) {
	fpe = c->fpe_list[c->current_fpe];
	err = (*fpe_functions[fpe->type].open_font)
	    ((pointer) c->client, fpe, c->flags,
	     c->fontname, c->fnamelen, c->format, c->format_mask,
	     c->fontid, &pfont, &alias);

	if (err == FontNameAlias && alias) {
	    newlen = strlen(alias);
	    newname = (char *) fsrealloc(c->fontname, newlen);
	    if (!newname) {
		err = AllocError;
		break;
	    }
	    bcopy(alias, newname, newlen);
	    c->fontname = newname;
	    c->fnamelen = newlen;
	    c->current_fpe = 0;
	    continue;
	}
	if (err == BadFontName) {
	    c->current_fpe++;
	    continue;
	}
	if (err == Suspended) {
	    if (!c->slept) {
		c->slept = TRUE;
		ClientSleep(client, do_open_font, (pointer) c);
	    }
	    return TRUE;
	}
	break;
    }
    if (err != Successful) {
	goto dropout;
    }
    if (!pfont) {
	err = BadFontName;
	goto dropout;
    }
    cfp = (ClientFontPtr) fsalloc(sizeof(ClientFontRec));
    if (!cfp) {
	err = AllocError;
	goto dropout;
    }
    cfp->font = pfont;
    cfp->clientindex = c->client->index;

    if (fontPatternCache)
	CacheFontPattern(fontPatternCache, c->orig_name, c->orig_len, pfont);

    /* either pull out the other id or make the array */
    if (pfont->refcnt != 0) {
	idlist = (FontIDListPtr *) pfont->svrPrivate;
	ids = idlist[c->client->index];
	if (!ids) {
	    ids = make_clients_id_list();
	    if (!ids) {
		err = AllocError;
		fsfree(cfp);
		goto dropout;
	    }
	    idlist[c->client->index] = ids;
	}
	orig = ids->client_list[0];
    } else {
	idlist = (FontIDListPtr *) fsalloc(sizeof(FontIDListPtr) * MAXCLIENTS);
	if (!idlist) {
	    err = AllocError;
	    fsfree(cfp);
	    goto dropout;
	}
	ids = make_clients_id_list();
	if (!ids) {
	    err = AllocError;
	    fsfree(idlist);
	    fsfree(cfp);
	    goto dropout;
	}
	bzero((char *) idlist, (sizeof(FontIDListPtr) * MAXCLIENTS));
	idlist[c->client->index] = ids;
	orig = (Font) 0;
	pfont->svrPrivate = (pointer) idlist;
    }
    if (!AddResource(c->client->index, c->fontid, RT_FONT, (pointer) cfp)) {
	fsfree(cfp);
	fsfree(pfont->svrPrivate);
	pfont->svrPrivate = (pointer) 0;
	err = AllocError;
	goto dropout;
    }
    add_id_to_list(ids, c->fontid);
    /* send the reply */
    rep.type = FS_Reply;
    rep.otherid = orig;
    if (orig)
	rep.otherid_valid = TRUE;
    else
	rep.otherid_valid = FALSE;
    rep.cachable = TRUE;	/* XXX */
    rep.sequenceNumber = client->sequence;
    rep.length = sizeof(fsOpenBitmapFontReply) >> 2;
    WriteReplyToClient(client,
		       sizeof(fsOpenBitmapFontReply), &rep);
    if (pfont->refcnt == 0) {
	pfont->fpe = fpe;
	UseFPE(pfont->fpe);
    }
    pfont->refcnt++;
dropout:
    if (err != Successful) {
	SendErrToClient(c->client, FontToFSError(err), (pointer) &(c->fontid));
    }
    if (c->slept)
	ClientWakeup(c->client);
    for (i = 0; i < c->num_fpes; i++) {
	FreeFPE(c->fpe_list[i]);
    }
    fsfree(c->fpe_list);
    fsfree(c->fontname);
    fsfree(c);
    return TRUE;
}

int
OpenFont(client, fid, format, format_mask, namelen, name)
    ClientPtr   client;
    Font        fid;
    fsBitmapFormat format;
    fsBitmapFormatMask format_mask;
    int         namelen;
    char       *name;
{
    FontPtr     pfont;
    fsOpenBitmapFontReply rep;
    OFclosurePtr c;
    Font        orig;
    FontIDListPtr *idlist,
                ids;
    int         i;

    /* check name cache */
    if (fontPatternCache &&
	  (pfont = FindCachedFontPattern(fontPatternCache, name, namelen))) {
	ClientFontPtr cfp;

	idlist = (FontIDListPtr *) pfont->svrPrivate;
	ids = idlist[client->index];
	if (!ids) {
	    ids = make_clients_id_list();
	    if (!ids) {
		goto lowmem;
	    }
	    idlist[client->index] = ids;
	}
	orig = ids->client_list[0];
	cfp = (ClientFontPtr) fsalloc(sizeof(ClientFontRec));
	if (!cfp) {
    lowmem:
	    SendErrToClient(client, FSBadAlloc, (pointer) 0);
	    return FSBadAlloc;
	}
	cfp->font = pfont;
	cfp->clientindex = client->index;
	if (!AddResource(client->index, fid, RT_FONT, (pointer) cfp)) {
	    goto lowmem;
	}
	if (!add_id_to_list(ids, fid)) {
	    goto lowmem;
	}
	pfont->refcnt++;
	rep.type = FS_Reply;
	rep.otherid = orig;
	rep.otherid_valid = TRUE;
	rep.cachable = TRUE;	/* XXX */
	rep.sequenceNumber = client->sequence;
	rep.length = sizeof(fsOpenBitmapFontReply) >> 2;
	WriteReplyToClient(client,
			   sizeof(fsOpenBitmapFontReply), &rep);
	return FSSuccess;
    }
    c = (OFclosurePtr) fsalloc(sizeof(OFclosureRec));
    if (!c)
	goto lowmem;
    c->fontname = (char *) fsalloc(namelen);
    if (!c->fontname) {
	fsfree(c);
	goto lowmem;
    }
    /*
     * copy the current FPE list, so that if it gets changed by another client
     * while we're blocking, the request still appears atomic
     */
    c->fpe_list = (FontPathElementPtr *)
	fsalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list) {
	fsfree(c->fontname);
	fsfree(c);
	goto lowmem;
    }
    bcopy(name, c->fontname, namelen);
    for (i = 0; i < num_fpes; i++) {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->client = client;
    c->fontid = fid;
    c->current_fpe = 0;
    c->num_fpes = num_fpes;
    c->fnamelen = namelen;
    c->orig_name = name;
    c->orig_len = namelen;
    c->slept = FALSE;
    c->flags = (FontLoadInfo | FontLoadProps);
    c->format = format;
    c->format_mask = format_mask;

    (void) do_open_font(client, c);
    return FSSuccess;
}

static int
close_font(pfont)
    FontPtr     pfont;
{
    FontPathElementPtr fpe;

    assert(pfont);
    if (--pfont->refcnt == 0) {
	if (fontPatternCache)
	    RemoveCachedFontPattern(fontPatternCache, pfont);
	fpe = pfont->fpe;
	fsfree((char *) pfont->svrPrivate);
	(*fpe_functions[fpe->type].close_font) (fpe, pfont);
	FreeFPE(fpe);
    }
    return FSSuccess;
}

int
CloseClientFont(cfp, fid)
    ClientFontPtr cfp;
    FSID        fid;
{
    FontIDListPtr *idlist,
                ids;

    assert(cfp);
    /* clear otherid id */
    idlist = (FontIDListPtr *) cfp->font->svrPrivate;
    ids = idlist[cfp->clientindex];
    remove_id_from_list(ids, fid);
    return close_font(cfp->font);
}

/*
 * search all the knwon FPE prefixes looking for one to match the given
 * FPE name
 */
static int
determine_fpe_type(name)
    char       *name;
{
    int	i;
    for (i = 0; i < num_fpe_types; i++) {
	if ((*fpe_functions[i].name_check) (name))
	    return i;
    }
    return -1;
}

static void
free_font_path(list, n)
    FontPathElementPtr *list;
    int         n;
{
    int         i;

    for (i = 0; i < n; i++) {
	FreeFPE(list[i]);
    }
    fsfree((char *) list);
}

static      FontPathElementPtr
find_existing_fpe(list, num, name, len)
    FontPathElementPtr *list;
    int         num;
    char       *name;
    int         len;
{
    FontPathElementPtr fpe;
    int         i;

    for (i = 0; i < num; i++) {
	fpe = list[i];
	if (fpe->name_length == len && bcmp(name, fpe->name, len) == 0)
	    return fpe;
    }
    return (FontPathElementPtr) 0;
}

/*
 * does the work of setting up the fpe list
 *
 * paths should be a counted string
 */
static int
set_font_path_elements(npaths, paths, bad)
    int         npaths;
    char       *paths;
    int        *bad;
{
    int         i,
                err;
    int		len;
    int		type;
    char       *cp = paths;
    FontPathElementPtr fpe,
               *fplist;

    fplist = (FontPathElementPtr *)
	fsalloc(sizeof(FontPathElementPtr) * npaths);
    if (!fplist) {
	*bad = 0;
	return FSBadAlloc;
    }
    for (i = 0; i < npaths; i++) {
	len = *cp++;
	if (len) {
	    /* if its already in our active list, just reset it */
	    /*
	     * note that this can miss FPE's in limbo -- may be worth catching
	     * them, though it'd muck up refcounting
	     */
	    fpe = find_existing_fpe(font_path_elements, num_fpes, cp, len);
	    if (fpe) {
		err = (*fpe_functions[fpe->type].reset_fpe) (fpe);
		if (err == Successful) {
		    UseFPE(fpe);/* since it'll be decref'd later when freed
				 * from the old list */
		    fplist[i] = fpe;
		    cp += len;
		    continue;
		}
		/* can't do it, so act like its a new one */
	    }
	    type = determine_fpe_type(cp);
	    if (type == -1)
	    {
		err = FSBadName;
		goto bail;
	    }
	    /* must be new -- make it */
	    fpe = (FontPathElementPtr) fsalloc(sizeof(FontPathElementRec));
	    if (!fpe) {
		err = FSBadAlloc;
		goto bail;
	    }
	    fpe->type = type;
	    fpe->name = (char *) fsalloc(len + 1);
	    if (!fpe->name) {
		fsfree(fpe);
		err = FSBadAlloc;
		goto bail;
	    }
	    fpe->refcount = 1;
	    fplist[i] = fpe;

	    strncpy(fpe->name, (char *) cp, len);
	    fpe->name[len] = '\0';
	    cp += len;
	    fpe->name_length = len;
	    err = (*fpe_functions[fpe->type].init_fpe) (fpe);
	    if (err != Successful) {
		fsfree(fpe->name);
		fsfree(fpe);
		err = FontToFSError(err);
		goto bail;
	    }
	}
    }
    free_font_path(font_path_elements, num_fpes);
    font_path_elements = fplist;
    num_fpes = npaths;
    if (fontPatternCache)
	EmptyFontPatternCache(fontPatternCache);
    return FSSuccess;
bail:
    *bad = i;
    while (--i >= 0)
	FreeFPE(fplist[i]);
    fsfree(fplist);
    return err;
}

/*
 * expects comma seperated string
 */
int
SetFontCatalogue(str, badpath)
    char       *str;
    int        *badpath;
{
    int         len,
                npaths;
    char       *paths,
               *end,
               *p;
    int         err;

    len = strlen(str) + 1;
    paths = p = (char *) ALLOCATE_LOCAL(len);
    npaths = 0;

    while (*str) {
	end = index(str, ',');
	if (!end) {
	    end = str + strlen(str);
	}
	*p++ = len = end - str;
	bcopy(str, p, len);
	npaths++;
	str += len;		/* skip entry */
	if (*str == ',')
           str++;		/* skip any comma */
	p += len;
    }

    err = set_font_path_elements(npaths, paths, badpath);

    DEALLOCATE_LOCAL(paths);

    return err;
}

static Bool
do_list_fonts(client, c)
    ClientPtr   client;
    LFclosurePtr c;
{
    int         err = Successful;
    fsListFontsReply reply;
    FontNamesPtr names = NULL;
    FontPathElementPtr fpe;
    int         stringLens,
                i,
                nnames;
    char       *bufptr,
               *bufferStart;

    if (client->clientGone == CLIENT_GONE) {
	if (c->current_fpe < c->num_fpes) {
	    fpe = c->fpe_list[c->current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto bail;
    }
    /* try each fpe in turn, returning if one wants to be blocked */
    while (c->current_fpe < c->num_fpes && c->names->nnames <= c->maxnames) {
	fpe = c->fpe_list[c->current_fpe];

	err = (*fpe_functions[fpe->type].list_fonts)
	    ((pointer) c->client, fpe, c->pattern, c->patlen,
	     c->maxnames - c->names->nnames, c->names);

	if (err == Suspended) {
	    if (!c->slept) {
		c->slept = TRUE;
		ClientSleep(client, do_list_fonts, (pointer) c);
	    }
	    return TRUE;
	}
	if (err != Successful)
	    break;
	c->current_fpe++;
    }

    if (err != Successful) {
	SendErrToClient(client, FontToFSError(err), (pointer) 0);
	goto bail;
    }
    names = c->names;
    nnames = names->nnames;
    client = c->client;
    stringLens = 0;
    for (i = 0; i < nnames; i++)
	stringLens += names->length[i];

    reply.type = FS_Reply;
    reply.length = (sizeof(fsListFontsReply) + stringLens + nnames + 3) >> 2;
    reply.following = 0;
    reply.nFonts = nnames;
    reply.sequenceNumber = client->sequence;

    bufptr = bufferStart = (char *) ALLOCATE_LOCAL(reply.length << 2);

    if (!bufptr && reply.length) {
	SendErrToClient(client, FSBadAlloc, (pointer) 0);
	goto bail;
    }
    /*
     * since WriteToClient long word aligns things, copy to temp buffer and
     * write all at once
     */
    for (i = 0; i < nnames; i++) {
	*bufptr++ = names->length[i];
	bcopy(names->names[i], bufptr, names->length[i]);
	bufptr += names->length[i];
    }
    WriteReplyToClient(client, sizeof(fsListFontsReply), &reply);
    (void) WriteToClient(client, stringLens + nnames, bufferStart);
    DEALLOCATE_LOCAL(bufferStart);
bail:
    if (c->slept)
	ClientWakeup(client);
    for (i = 0; i < c->num_fpes; i++)
	FreeFPE(c->fpe_list[i]);
    fsfree(c->fpe_list);
    FreeFontNames(names);
    fsfree(c->pattern);
    fsfree(c);
    return TRUE;
}

static      LFclosurePtr
make_list_fonts_closure(client, pattern, length, maxnames)
    ClientPtr   client;
    unsigned char *pattern;
    unsigned int length;
    unsigned int maxnames;
{
    LFclosurePtr c;
    int         i;

    c = (LFclosurePtr) fsalloc(sizeof(LFclosureRec));
    if (!c)
	return (LFclosurePtr) 0;
    c->pattern = (char *) fsalloc(length);
    if (!c->pattern) {
	fsfree(c);
	return (LFclosurePtr) 0;
    }
    c->names = MakeFontNamesRecord(maxnames < 100 ? maxnames : 100);
    c->fpe_list = (FontPathElementPtr *)
	fsalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list || !c->names) {
	FreeFontNames(c->names);
	fsfree(c->pattern);
	fsfree(c);
	return (LFclosurePtr) 0;
    }
    bcopy(pattern, c->pattern, length);
    for (i = 0; i < num_fpes; i++) {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->patlen = length;
    c->client = client;
    c->maxnames = maxnames;
    c->current_fpe = 0;
    c->num_fpes = num_fpes;
    c->slept = FALSE;
    return c;
}

int
ListFonts(client, length, pattern, maxnames)
    ClientPtr   client;
    unsigned int length;
    unsigned char *pattern;
    unsigned int maxnames;
{
    LFclosurePtr c;

    c = make_list_fonts_closure(client, pattern, length, maxnames);
    if (!c)
	return FSBadAlloc;

    (void) do_list_fonts(client, c);
    return FSSuccess;
}

static int padlength[4] = {0, 3, 2, 1};
static char padding[3];

do_list_fonts_with_info(client, c)
    ClientPtr   client;
    LFWXIclosurePtr c;
{
    FontPathElementPtr fpe;
    int         err = Successful;
    char       *name;
    int         namelen;
    int         numFonts;
    FontInfoRec fontInfo,
               *pFontInfo;
    fsListFontsWithXInfoReply *reply;
    int         length;
    fsPropInfo *prop_info;
    fsFontHeader hdr;
    int         lenpropdata;
    int         i;

    if (client->clientGone == CLIENT_GONE) {
	if (c->current.current_fpe < c->num_fpes) {
	    fpe = c->fpe_list[c->current.current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto bail;
    }
    while (c->current.current_fpe < c->num_fpes) {
	fpe = c->fpe_list[c->current.current_fpe];
	err = Successful;
	if (!c->current.list_started) {
	    err = (*fpe_functions[fpe->type].start_list_fonts_with_info)
		((pointer) c->client, fpe, c->current.pattern,
		 c->current.patlen, c->current.max_names,
		 &c->current.private);
	    if (err == Suspended) {
		if (!c->slept) {
		    ClientSleep(client, do_list_fonts_with_info,
				(pointer) c);
		    c->slept = TRUE;
		}
		return TRUE;
	    }
	    if (err == Successful)
		c->current.list_started = TRUE;
	}
	if (err == Successful) {
	    name = 0;
	    pFontInfo = &fontInfo;
	    err = (*fpe_functions[fpe->type].list_next_font_with_info)
		((pointer) c->client, fpe, &name, &namelen,
		 &pFontInfo, &numFonts, c->current.private);
	    if (err == Suspended) {
		if (!c->slept) {
		    ClientSleep(client, do_list_fonts_with_info,
				(pointer) c);
		    c->slept = TRUE;
		}
		return TRUE;
	    }
	}
	/*
	 * When we get an alias back, save our state and reset back to the
	 * start of the FPE looking for the specified name.  As soon as a real
	 * font is found for the alias, pop back to the old state
	 */
	if (err == FontNameAlias) {
	    if (!c->haveSaved)
		c->saved = c->current;
	    c->current.pattern = name;
	    c->current.patlen = namelen;
	    c->current.max_names = 1;
	    c->current.current_fpe = 0;
	    c->current.private = 0;
	    c->current.list_started = FALSE;
	    c->haveSaved = TRUE;
	    c->savedNumFonts = numFonts;
	    c->savedName = (char *) pFontInfo;
	}
	/*
	 * At the end of this FPE, step to the next.  If we've finished
	 * processing an alias, pop state back. If we've sent enough font
	 * names, quit.
	 */
	else if (err == BadFontName) {
	    c->current.list_started = FALSE;
	    c->current.current_fpe++;
	    err = Successful;
	    if (c->haveSaved) {
		if (c->current.max_names == 0 ||
			c->current.current_fpe == c->num_fpes) {
		    c->haveSaved = FALSE;
		    c->saved.max_names -= (1 - c->current.max_names);
		    c->current = c->saved;
		}
	    }
	    if (c->current.max_names == 0)
		break;
	} else if (err == Successful) {
/* XXX why is it xFontProp ? */
	    length = sizeof(*reply) + pFontInfo->nprops * sizeof(xFontProp);
	    reply = c->reply;
	    if (c->length < length) {
		reply = (fsListFontsWithXInfoReply *) fsrealloc(c->reply, length);
		if (!reply) {
		    err = AllocError;
		    break;
		}
		c->reply = reply;
		c->length = length;
	    }
	    if (c->haveSaved) {
		numFonts = c->savedNumFonts;
		name = c->savedName;
		namelen = strlen(name);
	    }
	    err = LoadXFontInfo(client, pFontInfo, &hdr, &prop_info);
	    if (err != Successful)
		break;
	    lenpropdata = sizeof(fsPropInfo) +
		prop_info->num_offsets * sizeof(fsPropOffset) +
		prop_info->data_len;

	    reply->type = FS_Reply;
	    reply->length =
		(sizeof(fsListFontsWithXInfoReply) +
		 lenpropdata + namelen + 3) >> 2;
	    reply->sequenceNumber = client->sequence;
	    reply->nameLength = namelen;
	    reply->nReplies = numFonts;
	    reply->header = hdr;
	    WriteReplyToClient(client, sizeof(fsListFontsWithXInfoReply), reply);
	    if (client->swapped)
		SwapPropInfo(prop_info);
	    if (client->major_version > 1)
	    {
		(void)WriteToClientUnpadded(client, lenpropdata, (char *) prop_info);
		(void)WriteToClientUnpadded(client, namelen, name);
		(void)WriteToClientUnpadded(client,
					    padlength[(lenpropdata+namelen)&3],
					    padding);
	    } else {
		(void) WriteToClient(client, namelen, name);
		(void) WriteToClient(client, lenpropdata, (char *) prop_info);
	    }
	    if (pFontInfo == &fontInfo) {
		fsfree(fontInfo.props);
		fsfree(fontInfo.isStringProp);
	    }
	    fsfree(prop_info);

	    --c->current.max_names;
	    if (c->current.max_names < 0)
		abort();
	}
    }

    /*
     * send the final reply
     */
    if (err == Successful) {
	fsGenericReply *final_reply;

	final_reply = (fsGenericReply *)c->reply;
	if (client->major_version > 1)
	    length = sizeof(fsGenericReply);
	else
	    length = sizeof(fsListFontsWithXInfoReply);
	if (c->length < length) {
	    final_reply = (fsGenericReply *) fsrealloc(c->reply, length);
	    if (final_reply) {
		c->reply = (fsListFontsWithXInfoReply *)final_reply;
		c->length = length;
	    } else
		err = AllocError;
	}
	if (err == Successful) {
	    final_reply->type = FS_Reply;
	    final_reply->data1 = 0; /* notes that this is final */
	    final_reply->sequenceNumber = client->sequence;
	    final_reply->length = length >> 2;
	    WriteReplyToClient(client, length, final_reply);
	}
    }
    if (err != Successful)
	SendErrToClient(client, FontToFSError(err), (pointer) 0);
bail:
    if (c->slept)
	ClientWakeup(client);
    for (i = 0; i < c->num_fpes; i++)
	FreeFPE(c->fpe_list[i]);
    fsfree(c->fpe_list);
    fsfree(c->current.pattern);
    fsfree(c);
    return TRUE;
}

int
StartListFontsWithInfo(client, length, pattern, maxNames)
    ClientPtr   client;
    int         length;
    unsigned char *pattern;
    int         maxNames;
{
    int         i;
    LFWXIclosurePtr c;

    if (!(c = (LFWXIclosurePtr) fsalloc(sizeof *c)))
	goto badAlloc;
    if (!(c->current.pattern = (char *) fsalloc(length))) {
	fsfree(c);
	goto badAlloc;
    }
    c->fpe_list = (FontPathElementPtr *)
	fsalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list) {
	fsfree(c->current.pattern);
	fsfree(c);
	goto badAlloc;
    }
    bcopy(pattern, c->current.pattern, length);
    for (i = 0; i < num_fpes; i++) {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->client = client;
    c->num_fpes = num_fpes;
    c->reply = 0;
    c->length = 0;
    c->current.patlen = length;
    c->current.current_fpe = 0;
    c->current.max_names = maxNames;
    c->current.list_started = FALSE;
    c->current.private = 0;
    c->savedNumFonts = 0;
    c->haveSaved = FALSE;
    c->slept = FALSE;
    do_list_fonts_with_info(client, c);
    return TRUE;
badAlloc:
    SendErrToClient(client, FSBadAlloc, (pointer) 0);
    return TRUE;
}

int
RegisterFPEFunctions(name_func, init_func, free_func, reset_func,
	   open_func, close_func, list_func, start_lfwi_func, next_lfwi_func,
		     wakeup_func, client_died)
    Bool        (*name_func) ();
    int         (*init_func) ();
    int         (*free_func) ();
    int         (*reset_func) ();
    int         (*open_func) ();
    int         (*close_func) ();
    int         (*list_func) ();
    int         (*start_lfwi_func) ();
    int         (*next_lfwi_func) ();
    int         (*wakeup_func) ();
    int         (*client_died) ();
{
    FPEFunctions *new;

    /* grow the list */
    new = (FPEFunctions *) fsrealloc(fpe_functions,
				 (num_fpe_types + 1) * sizeof(FPEFunctions));
    if (!new)
	return -1;
    fpe_functions = new;

    fpe_functions[num_fpe_types].name_check = name_func;
    fpe_functions[num_fpe_types].open_font = open_func;
    fpe_functions[num_fpe_types].close_font = close_func;
    fpe_functions[num_fpe_types].wakeup_fpe = wakeup_func;
    fpe_functions[num_fpe_types].list_fonts = list_func;
    fpe_functions[num_fpe_types].start_list_fonts_with_info =
	start_lfwi_func;
    fpe_functions[num_fpe_types].list_next_font_with_info =
	next_lfwi_func;
    fpe_functions[num_fpe_types].init_fpe = init_func;
    fpe_functions[num_fpe_types].free_fpe = free_func;
    fpe_functions[num_fpe_types].reset_fpe = reset_func;

    fpe_functions[num_fpe_types].client_died = client_died;
    return num_fpe_types++;
}

FreeFonts()
{
}

/* convenience functions for FS interface */

FontPtr
find_old_font(id)
    FSID        id;
{
    return (FontPtr) LookupIDByType(SERVER_CLIENT, id, RT_NONE);
}

Font
GetNewFontClientID()
{
    return (Font) FakeClientID(SERVER_CLIENT);
}

int
StoreFontClientFont(pfont, id)
    FontPtr     pfont;
    Font        id;
{
    return AddResource(SERVER_CLIENT, id, RT_NONE, (pointer) pfont);
}

DeleteFontClientID(id)
    Font        id;
{
    FreeResource(SERVER_CLIENT, id, RT_NONE);
}

static int  fs_handlers_installed = 0;
static unsigned int last_server_gen;

init_fs_handlers(fpe, block_handler)
    FontPathElementPtr fpe;
    void         (*block_handler) ();
{
    /* if server has reset, make sure the b&w handlers are reinstalled */
    if (last_server_gen < serverGeneration) {
	last_server_gen = serverGeneration;
	fs_handlers_installed = 0;
    }
    if (fs_handlers_installed == 0) {

#ifdef DEBUG
	fprintf(stderr, "adding FS b & w handlers\n");
#endif

	if (!RegisterBlockAndWakeupHandlers(block_handler,
					    FontWakeup, (pointer) 0))
	    return AllocError;
	fs_handlers_installed++;
    }
    QueueFontWakeup(fpe);
    return Successful;
}

remove_fs_handlers(fpe, block_handler, all)
    FontPathElementPtr fpe;
    void        (*block_handler) ();
    Bool        all;
{
    if (all) {
	/* remove the handlers if no one else is using them */
	if (--fs_handlers_installed == 0) {

#ifdef DEBUG
	    fprintf(stderr, "removing FS b & w handlers\n");
#endif

	    RemoveBlockAndWakeupHandlers(block_handler, FontWakeup,
					 (pointer) 0);
	}
    }
    RemoveFontWakeup(fpe);
}
