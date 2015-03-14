/* $XConsortium: fserve.c,v 1.25 92/07/09 16:08:03 gildea Exp $ */
/*
 * Copyright 1990 Network Computing Devices
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
 *
 * Author:  	Dave Lemke, Network Computing Devices, Inc
 */
/*
 * font server specific font access
 */

#include	<X11/X.h>
#include	<X11/Xos.h>
#include	"FS.h"
#include	"FSproto.h"
#include	"fontmisc.h"
#include	"fontstruct.h"
#include	"fservestr.h"
#include	<errno.h>

#ifdef NCD
#include	<ncd/nvram.h>
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef MIN
#define MIN(a,b)    ((a)<(b)?(a):(b))
#endif

#define NONZEROMETRICS(pci) ((pci)->leftSideBearing || \
			     (pci)->rightSideBearing || \
			     (pci)->ascent || \
			     (pci)->descent || \
			     (pci)->characterWidth)

extern int  errno;


extern FontPtr find_old_font();

extern int  fs_build_range();

static int  fs_read_glyphs();
static int  fs_read_list();
static int  fs_read_list_info();

static int  fs_font_type;
extern unsigned long fs_fd_mask[];

static void fs_block_handler();
static int  fs_wakeup();

static FSFpePtr awaiting_reconnect;

void        _fs_connection_died();
static int  _fs_restart_connection();
static void _fs_try_reconnect();
static int  fs_send_query_info();
static int  fs_send_query_extents();
static int  fs_send_query_bitmaps();
static int  fs_send_close_font();
static int  fs_read_extents();
static int  fs_read_bitmaps();
static void _fs_client_access();

/*
 * Font server access
 *
 * the basic idea for the non-blocking access is to have the function
 * called multiple times until the actual data is returned, instead
 * of ClientBlocked.
 *
 * the first call to the function will cause the request to be sent to
 * the font server, and a block record to be stored in the fpe's list
 * of outstanding requests.  the FS block handler also sticks the
 * proper set of fd's into the select mask.  when data is ready to be
 * read in, the FS wakup handler will be hit.  this will read the
 * data off the wire into the proper block record, and then signal the
 * client that caused the block so that it can restart.  it will then
 * call the access function again, which will realize that the data has
 * arrived and return it.
 */


/* XXX this should probably be a macro once its fully debugged */
/* ARGSUSED */
static void
_fs_add_req_log(conn, opcode)
    FSFpePtr    conn;
    int         opcode;
{

#ifdef DEBUG
    conn->reqbuffer[conn->reqindex++] = opcode;
    if (conn->reqindex == REQUEST_LOG_SIZE)
	conn->reqindex = 0;
#endif

    conn->current_seq++;
}

static Bool
fs_name_check(name)
    char       *name;
{
    return (!strncmp(name, "tcp/", MIN(4, (int) strlen(name))));
}

/*
 * sends the stuff that's meaningful to a newly opened or reset FS
 */
static int
fs_send_init_packets(conn)
    FSFpePtr    conn;
{
    fsSetResolutionReq srreq;
    fsSetCataloguesReq screq;
    fsListCataloguesReq lcreq;
    fsListCataloguesReply lcreply;
    int         num_cats,
                clen,
                len;
    char       *client_cat = (char *) 0,
               *cp,
               *sp,
               *end;
    int         num_res;
    fsResolution *res;
    extern fsResolution *GetClientResolutions();
    int         err = Successful;

#define	CATALOGUE_SEP	'+'

    res = GetClientResolutions(&num_res);
    if (num_res) {
	srreq.reqType = FS_SetResolution;
	srreq.num_resolutions = num_res;
	srreq.length = (sizeof(fsSetResolutionReq) +
			(num_res * sizeof(fsResolution)) + 3) >> 2;

	_fs_add_req_log(conn, FS_SetResolution);
	if (_fs_write(conn, (char *) &srreq, sizeof(fsSetResolutionReq)) == -1)
	{
	    err = BadFontPath;
	    goto fail;
	}
	if (_fs_write_pad(conn, (char *) res, (num_res * sizeof(fsResolution))) == -1)
	{
	    err = BadFontPath;
	    goto fail;
	}
    }
    sp = rindex(conn->servername, '/');

    /* don't get tricked by a non-existant catalogue list */
    if (sp == index(conn->servername, '/')) {
	/*
	 * try original name -- this might be an alternate with no catalogues
	 */
	sp = rindex(conn->requestedname, '/');
	if (sp == index(conn->requestedname, '/'))
		sp = (char *) 0;
    }
    if (sp) {			/* turn cats into counted list */
	sp++;
	/* allocate more than enough room */
	cp = client_cat = (char *) xalloc(strlen(conn->servername));
	if (!cp) {
	    err = BadAlloc;
	    goto fail;
	}
	num_cats = 0;
	while (*sp) {
	    end = index(sp, CATALOGUE_SEP);
	    if (!end)
		end = sp + strlen(sp);
	    *cp++ = len = end - sp;
	    num_cats++;
	    bcopy(sp, cp, len);
	    sp += len;
	    if (*sp == CATALOGUE_SEP)
		sp++;
	    cp += len;
	}
	clen = cp - client_cat;
	/* our list checked out, so send it */
	screq.reqType = FS_SetCatalogues;
	screq.num_catalogues = num_cats;
	screq.length = (sizeof(fsSetCataloguesReq) + clen + 3) >> 2;

	_fs_add_req_log(conn, FS_SetCatalogues);
	if (_fs_write(conn, (char *) &screq, sizeof(fsSetCataloguesReq)) == -1)
	{
	    err = BadFontPath;
	    goto fail;
	}
	if (_fs_write_pad(conn, (char *) client_cat, clen) == -1)
	{
	    err = BadFontPath;
	    goto fail;
	}

	/*
	 * now sync up with the font server, to see if an error was generated
	 * by a bogus catalogue
	 */
	lcreq.reqType = FS_ListCatalogues;
	lcreq.length = (sizeof(fsListCataloguesReq)) >> 2;
	lcreq.maxNames = 0;
	lcreq.nbytes = 0;
	_fs_add_req_log(conn, FS_SetCatalogues);
	if (_fs_write(conn, (char *) &lcreq, sizeof(fsListCataloguesReq)) == -1)
	{
	    err = BadFontPath;
	    goto fail;
	}

	/*
	 * next bit will either by the ListCats reply, or an error followed by
	 * the reply
	 */
	if (_fs_read(conn, (char *) &lcreply, sizeof(fsReplyHeader)) == -1) {
	    err = BadFontPath;
	    goto fail;
	}
	if (lcreply.type == FS_Error &&
		((fsError *) & lcreply)->major_opcode == FS_SetCatalogues) {
	    _fs_eat_rest_of_error(conn, (fsError *) & lcreply);
	    /* get ListCats response */
	    (void) _fs_read(conn, (char *) &lcreply,
			    sizeof(fsListCataloguesReply));
	    err = BadFontPath;
	    goto fail;
	}
	/* must be reply, swallow the rest of it */
	_fs_eat_rest_of_error(conn, (fsError *) & lcreply);
    }
fail:
    xfree(client_cat);
    return err;
}

/* 
 * close font server and remove any state associated with
 * this connection - this includes any client records.
 */

static void
fs_close_conn(conn)
    FSFpePtr	conn;
{
    FSClientPtr	client, nclient;

    (void) close(conn->fs_fd);

    _fs_bit_clear(fs_fd_mask, conn->fs_fd);

    for (client = conn->clients; client; client = nclient) 
    {
	nclient = client->next;
	xfree (client);
    }
    conn->clients = NULL;
}

/*
 * the wakeup handlers have to be set when the FPE is open, and not
 * removed until it is freed, in order to handle unexpected data, like
 * events
 */
/* ARGSUSED */
static int
fs_init_fpe(fpe, format)
    FontPathElementPtr fpe;
    fsBitmapFormat format;
{
    FSFpePtr    conn;
    char       *name;
    int         err;

    /* open font server */
    /* create FS specific fpe info */
    errno = 0;

    name = fpe->name;

    /* hack for old style names */
    if (*name == ':')
	name++;			/* skip ':' */

    conn = _fs_open_server(name);
    if (conn) {
	conn->requestedname = fpe->name; /* stash this for later init use */
	fpe->private = (pointer) conn;
	err = fs_send_init_packets(conn);
	if (err != Successful) {
	    fs_close_conn(conn);
    	    xfree(conn->servername);
    	    xfree(conn->alts);
    	    xfree(conn);
	    return err;
	}
	if (init_fs_handlers(fpe, fs_block_handler) != Successful)
	    return AllocError;
	_fs_set_bit(fs_fd_mask, conn->fs_fd);
	conn->attemptReconnect = TRUE;

#ifdef NCD
	if (configData.ExtendedFontDiags)
	    printf("Connected to font server \"%s\"\n", name);
#endif

	return err;
    }

#ifdef DEBUG
    fprintf(stderr, "failed to connect to FS \"%s\"\n", name);
#endif

#ifdef NCD
    if (configData.ExtendedFontDiags)
	printf("Failed to connect to font server \"%s\"\n", name);
#endif

    return (errno == ENOMEM) ? AllocError : BadFontPath;
}

static int
fs_reset_fpe(fpe)
    FontPathElementPtr fpe;
{
    (void) fs_send_init_packets((FSFpePtr) fpe->private);
    return Successful;
}

/*
 * this shouldn't be called till all refs to the FPE are gone
 */

static int
fs_free_fpe(fpe)
    FontPathElementPtr fpe;
{
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    FSFpePtr    recon,
               *prev;
    prev = &awaiting_reconnect;
    while (*prev) {
	recon = *prev;
	if (conn == recon) {
	    *prev = recon->next_reconnect;
	    break;
	}
	prev = &recon->next_reconnect;
    }

    fs_close_conn(conn);

    remove_fs_handlers(fpe, fs_block_handler,
		       !_fs_any_bit_set(fs_fd_mask) && !awaiting_reconnect);

    xfree(conn->alts);
    xfree(conn->servername);
    xfree(conn);
    fpe->private = (pointer) 0;

#ifdef NCD
    if (configData.ExtendedFontDiags)
	printf("Disconnected from font server \"%s\"\n", fpe->name);
#endif

    return Successful;
}

static      FSBlockDataPtr
fs_new_block_rec(fpe, client, type)
    FontPathElementPtr fpe;
    pointer     client;
    int         type;
{
    FSBlockDataPtr blockrec,
                br;
    FSFpePtr    fsfpe = (FSFpePtr) fpe->private;
    int         size;

    blockrec = (FSBlockDataPtr) xalloc(sizeof(FSBlockDataRec));
    if (!blockrec)
	return (FSBlockDataPtr) 0;
    switch (type) {
    case FS_OPEN_FONT:
	size = sizeof(FSBlockedFontRec);
	break;
    case FS_LOAD_GLYPHS:
	size = sizeof(FSBlockedGlyphRec);
	break;
    case FS_LIST_FONTS:
	size = sizeof(FSBlockedListRec);
	break;
    case FS_LIST_WITH_INFO:
	size = sizeof(FSBlockedListInfoRec);
	break;
    case FS_LOAD_EXTENTS:
	size = sizeof(FSBlockedExtentRec);
	break;
    case FS_LOAD_BITMAPS:
	size = sizeof(FSBlockedBitmapRec);
	break;
    default:
	break;
    }
    blockrec->data = (pointer) xalloc(size);
    if (!blockrec->data) {
	xfree(blockrec);
	return (FSBlockDataPtr) 0;
    }
    blockrec->client = client;
    blockrec->sequence_number = fsfpe->current_seq;
    blockrec->type = type;
    blockrec->depending = 0;
    blockrec->next = (FSBlockDataPtr) 0;

    /* stick it on the end of the list (since its expected last) */
    br = (FSBlockDataPtr) fsfpe->blocked_requests;
    if (!br) {
	fsfpe->blocked_requests = (pointer) blockrec;
    } else {
	while (br->next)
	    br = br->next;
	br->next = blockrec;
    }

    return blockrec;
}

static void
fs_remove_blockrec(conn, blockrec)
    FSFpePtr    conn;
    FSBlockDataPtr blockrec;
{
    FSBlockDataPtr br,
                last;

    last = (FSBlockDataPtr) 0;
    br = (FSBlockDataPtr) conn->blocked_requests;
    while (br) {
	if (br == blockrec) {
	    if (last)
		last->next = br->next;
	    else
		conn->blocked_requests = (pointer) br->next;
	    xfree(br->data);
	    xfree(br);
	    return;
	}
	last = br;
	br = br->next;
    }
}

static void
fs_free_font(bfont)
    FSBlockedFontPtr bfont;
{
    FontPtr     pfont;
    FSFontPtr   fsfont;
    FSFontDataRec *fsd;

    pfont = bfont->pfont;
    fsfont = (FSFontPtr) pfont->fontPrivate;
    fsd = (FSFontDataRec *) pfont->fpePrivate;

    /* xfree better be able to handle NULL */
    xfree(fsfont->encoding);
    xfree(fsfont->bitmaps);
    DeleteFontClientID(fsd->fontid);
    xfree(fsfont);
    xfree(pfont->info.isStringProp);
    xfree(pfont->info.props);
    xfree(pfont);
    xfree(fsd);
    bfont->pfont = (FontPtr) 0;
}

static void
fs_cleanup_font(bfont)
    FSBlockedFontPtr bfont;
{
    FSFontDataRec *fsd;

    if (bfont->pfont)
    {
    	fsd = (FSFontDataRec *) bfont->pfont->fpePrivate;
    
    	/* make sure the FS knows we choked on it */
    	fs_send_close_font(fsd->fpe, bfont->fontid);
    
    	fs_free_font(bfont);
    }
    bfont->errcode = AllocError;
}

static int
fs_read_open_font(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FontPtr     newfont;
    FSBlockedFontPtr bfont = (FSBlockedFontPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsOpenBitmapFontReply rep;
    FSBlockDataPtr blockOrig;
    FSBlockedFontPtr origBfont;

    /* pull out the OpenFont reply */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));

    if (rep.type == FS_Error) {
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	return BadFontName;
    } else {			/* get rest of reply */
	if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	      sizeof(fsOpenBitmapFontReply) - sizeof(fsReplyHeader)) == -1) {
	    /* when re-established, we'll allocate the font again */
	    fs_free_font(bfont);
	    return StillWorking;
	}
    }

    /* make sure the sequence number is correct */
    if (rep.otherid) {
	newfont = find_old_font(rep.otherid);
	if (!newfont) {
	    /* XXX - something nasty happened */
	    return BadFontName;
	}
	(void) fs_send_close_font(fpe, bfont->fontid);

	fs_free_font(bfont);
	bfont->fontid = rep.otherid;
	bfont->pfont = newfont;
	bfont->state = FS_DONE_REPLY;
	/*
	 * look for a blocked request to open the same font
	 */
	for (blockOrig = (FSBlockDataPtr) conn->blocked_requests;
		blockOrig;
		blockOrig = blockOrig->next) {
	    if (blockOrig != blockrec && blockOrig->type == FS_OPEN_FONT) {
		origBfont = (FSBlockedFontPtr) blockOrig->data;
		if (origBfont->fontid == rep.otherid) {
		    blockrec->depending = blockOrig->depending;
		    blockOrig->depending = blockrec;
		    bfont->state = FS_DEPENDING;
		    break;
		}
	    }
	}
	return AccessDone;
    } else {
	bfont->pfont->info.cachable = rep.cachable != 0;
	bfont->state = FS_INFO_REPLY;
	/* ask for the next stage */
	(void) fs_send_query_info(fpe, blockrec);
	return StillWorking;
    }
}

static int
fs_read_query_info(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedFontPtr bfont = (FSBlockedFontPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXInfoReply rep;
    fsPropInfo  pi;
    fsPropOffset *po;
    pointer     pd;
    unsigned long prop_len;

    /* pull out the QueryXInfo reply */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
		 sizeof(fsQueryXInfoReply) - sizeof(fsReplyHeader)) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    /* move the data over */
    (void) fs_convert_header(conn, &rep.header, &bfont->pfont->info);
    if (bfont->pfont->info.terminalFont)
    {
	bfont->format =
	    (bfont->format & ~ (BitmapFormatImageRectMask)) |
	    BitmapFormatImageRectMax;
    }

    if (_fs_read(conn, (char *) &pi, sizeof(fsPropInfo)) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    prop_len = pi.num_offsets * sizeof(fsPropOffset);
    po = (fsPropOffset *) xalloc(prop_len);
    pd = (pointer) xalloc(pi.data_len);
    if (!po || !pd) {
	xfree(pd);
	xfree(po);
	/* clear the wire */
	(void) _fs_drain_bytes(conn, prop_len + pi.data_len);
	/* clean up the font */
	(void) fs_cleanup_font(bfont);
	return AllocError;
    }
    if (_fs_read_pad(conn, (char *) po, prop_len) == -1 ||
	    _fs_read_pad(conn, (char *) pd, pi.data_len) == -1) {
	xfree(pd);
	xfree(po);
	fs_free_font(bfont);
	return StillWorking;
    }
    if (fs_convert_props(&pi, po, pd, &bfont->pfont->info) == -1)
    {
    	xfree(po);
    	xfree(pd);
	(void) fs_cleanup_font(bfont);
	return AllocError;
    }
    xfree(po);
    xfree(pd);

    bfont->state = FS_EXTENT_REPLY;

    fs_send_query_extents(fpe, blockrec);
    return StillWorking;
}

static int
fs_read_extent_info(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedFontPtr bfont = (FSBlockedFontPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXExtents8Reply rep;
    int         i;
    int		numInfos;
    Bool	haveInk = FALSE; /* need separate ink metrics? */
    CharInfoPtr ci,
                pCI;
    FSFontPtr   fsfont = (FSFontPtr) bfont->pfont->fontPrivate;
    fsCharInfo *fsci,
               *fscip;

    /* read the QueryXExtents reply */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	      sizeof(fsQueryXExtents8Reply) - sizeof(fsReplyHeader)) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    /* move the data over */
    /* need separate inkMetrics for fixed font server protocol version */
    numInfos =  rep.num_extents;
    if (bfont->pfont->info.terminalFont && conn->fsMajorVersion > 1)
    {
	numInfos *= 2;
	haveInk = TRUE;
    }
    ci = pCI = (CharInfoPtr) xalloc(sizeof(CharInfoRec) * numInfos);
/* XXX this could be done with an ALLOCATE_LOCAL */
    fsci = (fsCharInfo *) xalloc(sizeof(fsCharInfo) * rep.num_extents);
    if (!pCI || !fsci) {
	xfree(pCI);
	xfree(fsci);
	/* clear the unusable data */
	_fs_drain_bytes(conn, sizeof(fsCharInfo) * rep.num_extents);
	fs_cleanup_font(bfont);
	return AllocError;
    }
    fsfont->encoding = pCI;
    if (haveInk)
	fsfont->inkMetrics = pCI + rep.num_extents;
    else
        fsfont->inkMetrics = pCI;

    if (_fs_read_pad(conn, (char *) fsci,
		     sizeof(fsCharInfo) * rep.num_extents) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    ci = fsfont->inkMetrics;
    for (i = 0, fscip = fsci; i < rep.num_extents; i++, ci++, fscip++) {
	fs_convert_char_info(fscip, ci);
    }

    xfree(fsci);

    /* build bitmap metrics, ImageRectMax style */
    if (haveInk)
    {
	FontInfoRec *fi = &bfont->pfont->info;
	CharInfoPtr ii;

	ci = fsfont->encoding;
	ii = fsfont->inkMetrics;
	for (i = 0; i < rep.num_extents; i++, ci++, ii++)
	{
	    if (NONZEROMETRICS(&ii->metrics))
	    {
		ci->metrics.leftSideBearing = FONT_MIN_LEFT(fi);
		ci->metrics.rightSideBearing = FONT_MAX_RIGHT(fi);
		ci->metrics.ascent = FONT_MAX_ASCENT(fi);
		ci->metrics.descent = FONT_MAX_DESCENT(fi);
		ci->metrics.characterWidth = FONT_MAX_WIDTH(fi);
		ci->metrics.attributes = ii->metrics.attributes;
	    }
	    else
	    {
		ci->metrics = ii->metrics;
	    }
	}
    }
    {
	unsigned int r, c, numCols, firstCol;

	firstCol = bfont->pfont->info.firstCol;
	numCols = bfont->pfont->info.lastCol - firstCol + 1;
	c = bfont->pfont->info.defaultCh;
	fsfont->pDefault = 0;
	if (bfont->pfont->info.lastRow)
	{
	    r = c >> 8;
	    r -= bfont->pfont->info.firstRow;
	    c &= 0xff;
	    c -= firstCol;
	    if (r < bfont->pfont->info.lastRow-bfont->pfont->info.firstRow+1 &&
		c < numCols)
		fsfont->pDefault = &pCI[r * numCols + c];
	}
	else
	{
	    c -= firstCol;
	    if (c < numCols)
		fsfont->pDefault = &pCI[c];
	}
    }
    bfont->state = FS_GLYPHS_REPLY;

    if (bfont->flags & FontLoadBitmaps) {
	fs_send_query_bitmaps(fpe, blockrec);
	return StillWorking;
    }
    return Successful;
}

/*
 * XXX should probably continue to read here if we can, but must be sure
 * its our packet waiting, rather than another interspersed
 */
static int
fs_do_open_font(fpe, blockrec, readheader)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
    Bool        readheader;
{
    FSBlockedFontPtr bfont = (FSBlockedFontPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    int         err;

    switch (bfont->state) {
    case FS_OPEN_REPLY:
	if (readheader) {
	    /* get the next header */
	    if (_fs_read(conn, (char *) &blockrec->header,
			 sizeof(fsReplyHeader)) == -1) {
		fs_free_font(bfont);
		err = StillWorking;
		break;
	    }
	}
	bfont->errcode = fs_read_open_font(fpe, blockrec);
	if (bfont->errcode != StillWorking) {	/* already loaded, or error */
	    /* if font's already loaded, massage error code */
	    switch (bfont->state) {
	    case FS_DONE_REPLY:
		bfont->errcode = Successful;
		break;
	    case FS_DEPENDING:
		bfont->errcode = StillWorking;
		break;
	    }
	    err = bfont->errcode;
	    break;
	}
	/* if more data to read or Sync, fall thru, else return */
	if (!(bfont->flags & FontOpenSync)) {
	    err = bfont->errcode;
	    break;
	} else {
	    if (_fs_read(conn, (char *) &blockrec->header,
			 sizeof(fsReplyHeader)) == -1) {
		fs_free_font(bfont);
		err = StillWorking;
		break;
	    }
	}
	/* fall through */
    case FS_INFO_REPLY:
	bfont->errcode = fs_read_query_info(fpe, blockrec);
	if (bfont->errcode != StillWorking) {
	    err = bfont->errcode;
	    break;
	}
	if (!(bfont->flags & FontOpenSync)) {
	    err = bfont->errcode;
	    break;
	    /* if more data to read, fall thru, else return */
	} else {
	    if (_fs_read(conn, (char *) &blockrec->header,
			 sizeof(fsReplyHeader))) {
		fs_free_font(bfont);
		err = StillWorking;
		break;
	    }
	}
	/* fall through */
    case FS_EXTENT_REPLY:
	bfont->errcode = fs_read_extent_info(fpe, blockrec);
	if (bfont->errcode != StillWorking) {
	    err = bfont->errcode;
	    break;
	}
	if (!(bfont->flags & FontOpenSync)) {
	    err = bfont->errcode;
	    break;
	} else if (bfont->flags & FontLoadBitmaps) {
	    if (_fs_read(conn, (char *) &blockrec->header,
			 sizeof(fsReplyHeader))) {
		fs_free_font(bfont);
		err = StillWorking;
		break;
	    }
	}
	/* fall through */
    case FS_GLYPHS_REPLY:
	if (bfont->flags & FontLoadBitmaps) {
	    bfont->errcode = fs_read_glyphs(fpe, blockrec);
	}
	err = bfont->errcode;
	break;
    case FS_DEPENDING:		/* "cant" happen */
	err = bfont->errcode;
    default:
	err = bfont->errcode;
	break;
    }
    if (err != StillWorking) {
	while (blockrec = blockrec->depending) {
	    bfont = (FSBlockedFontPtr) blockrec->data;
	    bfont->errcode = err;
	}
    }
    return err;
}

/* ARGSUSED */
static void
fs_block_handler(data, wt, LastSelectMask)
    pointer     data;
    struct timeval **wt;
    long       *LastSelectMask;
{
    static struct timeval recon_timeout;
    long        now,
                soonest;
    FSFpePtr    recon;

    _fs_or_bits(LastSelectMask, LastSelectMask, fs_fd_mask);
    if (recon = awaiting_reconnect) {
	now = time((long *) 0);
	soonest = recon->time_to_try;
	while (recon = recon->next_reconnect) {
	    if (recon->time_to_try < soonest)
		soonest = recon->time_to_try;
	}
	if (soonest < now)
	    soonest = now;
	soonest = soonest - now;
	recon_timeout.tv_sec = soonest;
	recon_timeout.tv_usec = 0;
	if (*wt == (struct timeval *) 0) {
	    *wt = &recon_timeout;
	} else if ((*wt)->tv_sec > soonest) {
	    **wt = recon_timeout;
	}
    }
}

static void
fs_handle_unexpected(conn, rep)
    FSFpePtr    conn;
    fsReplyHeader *rep;
{
    if (rep->type == FS_Event && rep->pad == KeepAlive) {
	fsNoopReq   req;

	/* ping it back */
	req.reqType = FS_Noop;
	req.length = sizeof(fsNoopReq) >> 2;
	_fs_add_req_log(conn, FS_Noop);
	_fs_write(conn, (char *) &req, sizeof(fsNoopReq));
    }
    /* this should suck up unexpected replies and events */
    _fs_eat_rest_of_error(conn, (fsError *) rep);
}

static int
fs_wakeup(fpe, LastSelectMask)
    FontPathElementPtr fpe;
    unsigned long *LastSelectMask;
{
    FSBlockDataPtr blockrec,
                br;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    int         err;
    fsReplyHeader rep;

    /* see if there's any data to be read */
    if (_fs_is_bit_set(LastSelectMask, conn->fs_fd)) {

#ifdef NOTDEF			/* bogus - doesn't deal with EOF very well,
				 * now does it ... */
	/*
	 * make sure it isn't spurious - mouse events seem to trigger extra
	 * problems
	 */
	if (_fs_data_ready(conn) <= 0) {
	    return FALSE;
	}
#endif

	/* get the header */
	if (_fs_read(conn, (char *) &rep, sizeof(fsReplyHeader)) == -1)
	    return FALSE;

	/* find the matching block record */

	for (br = (FSBlockDataPtr) conn->blocked_requests; br; br = br->next) {
	    if (br->sequence_number == (rep.sequenceNumber - 1))
		break;
	}
	if (!br) {
	    fs_handle_unexpected(conn, &rep);
	    return FALSE;
	}
	blockrec = br;

	bcopy((char *) &rep, (char *) &blockrec->header, sizeof(fsReplyHeader));

	/* go read it, and if we're done, wake up the appropriate client */
	switch (blockrec->type) {
	case FS_OPEN_FONT:
	    err = fs_do_open_font(fpe, blockrec, FALSE);
	    break;
	case FS_LOAD_GLYPHS:
	    err = fs_read_glyphs(fpe, blockrec);
	    break;
	case FS_LIST_FONTS:
	    err = fs_read_list(fpe, blockrec);
	    break;
	case FS_LIST_WITH_INFO:
	    err = fs_read_list_info(fpe, blockrec);
	    break;
	case FS_LOAD_EXTENTS:
	    err = fs_read_extents(fpe, blockrec);
	    break;
	case FS_LOAD_BITMAPS:
	    err = fs_read_bitmaps(fpe, blockrec);
	    break;
	default:
	    break;
	}

	if (err != StillWorking) {
	    while (blockrec) {
		ClientSignal(blockrec->client);
		blockrec = blockrec->depending;
	    }
	}
	/*
	 * Xx we could loop here and eat any additional replies, but it should
	 * feel more responsive for other clients if we come back later
	 */
    } else if (awaiting_reconnect) {
	_fs_try_reconnect();
    }
    return FALSE;
}

/*
 * Reconnection code
 */

void
_fs_connection_died(conn)
    FSFpePtr    conn;
{
    if (!conn->attemptReconnect)
	return;
    conn->attemptReconnect = FALSE;
    fs_close_conn(conn);
    conn->time_to_try = time((long *) 0) + FS_RECONNECT_WAIT;
    conn->reconnect_delay = FS_RECONNECT_WAIT;
    conn->fs_fd = -1;
    conn->next_reconnect = awaiting_reconnect;
    awaiting_reconnect = conn;
}

static int
_fs_restart_connection(conn)
    FSFpePtr    conn;
{
    FSBlockDataPtr block;

    conn->current_seq = 0;
    _fs_set_bit(fs_fd_mask, conn->fs_fd);
    if (!fs_send_init_packets(conn))
	return FALSE;
    while (block = (FSBlockDataPtr) conn->blocked_requests) {
	ClientSignal(block->client);
	fs_remove_blockrec(conn, block);
    }
    return TRUE;
}

static void
_fs_try_reconnect()
{
    FSFpePtr    conn,
               *prev;
    long        now;

    prev = &awaiting_reconnect;
    now = time((long *) 0);
    while (conn = *prev) {
	if (now - conn->time_to_try > 0) {
	    if (_fs_reopen_server(conn) && _fs_restart_connection(conn)) {
		conn->attemptReconnect = TRUE;
		*prev = conn->next_reconnect;
	    } else {
		if (conn->reconnect_delay < FS_MAX_RECONNECT_WAIT)
		    conn->reconnect_delay *= 2;
		now = time((long *) 0);
		conn->time_to_try = now + conn->reconnect_delay;
	    }
	}
	prev = &conn->next_reconnect;
    }
}

/*
 * sends the actual request out
 */
/* ARGSUSED */
static int
fs_send_open_font(client, fpe, flags, name, namelen, format, fmask, id, ppfont)
    pointer     client;
    FontPathElementPtr fpe;
    Mask        flags;
    char       *name;
    int         namelen;
    fsBitmapFormat format;
    fsBitmapFormatMask fmask;
    XID         id;
    FontPtr    *ppfont;
{
    FontPtr     newfont;
    FSBlockDataPtr blockrec;
    FSBlockedFontPtr blockedfont;
    FSFontDataPtr fsd;
    FSFontPtr   fsfont;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsOpenBitmapFontReq openreq;
    int         err = Suspended;
    XID         newid;
    unsigned char buf[1024];

    if (namelen > sizeof (buf) - 1)
	return BadFontName;
    _fs_client_access (conn, client, (flags & FontOpenSync) != 0);

    newid = GetNewFontClientID();

    /* make the font */
    newfont = (FontPtr) xalloc(sizeof(FontRec));

    /* and the FS data */
    fsd = (FSFontDataPtr) xalloc(sizeof(FSFontDataRec));

    fsfont = (FSFontPtr) xalloc(sizeof(FSFontRec));

    if (!newfont || !fsd || !fsfont) {
lowmem:
	xfree((char *) newfont);
	xfree((char *) fsd);
	xfree((char *) fsfont);
	return AllocError;
    }
    bzero((char *) newfont, sizeof(FontRec));
    bzero((char *) fsfont, sizeof(FSFontRec));
    bzero((char *) fsd, sizeof(FSFontDataRec));

    /* make a new block record, and add it to the end of the list */
    blockrec = fs_new_block_rec(fpe, client, FS_OPEN_FONT);
    if (!blockrec) {
	goto lowmem;
    }
    newfont->refcnt = 0;
    newfont->maxPrivate = -1;
    newfont->devPrivates = (pointer *) 0;
    newfont->format = format;
    newfont->fpe = fpe;
    newfont->fpePrivate = (pointer) fsd;
    newfont->fontPrivate = (pointer) fsfont;
    fs_init_font(newfont);

    fsd->fontid = newid;
    fsd->fpe = fpe;
    fsd->generation = conn->generation;

/* XXX - hack */
    /* for now, always load everything at startup time */
    flags |= FontLoadBitmaps;

    blockedfont = (FSBlockedFontPtr) blockrec->data;
    blockedfont->fontid = newid;
    blockedfont->pfont = newfont;
    blockedfont->state = FS_OPEN_REPLY;
    blockedfont->flags = flags;
    blockedfont->format = format;

    /* save the ID */
    if (!StoreFontClientFont(blockedfont->pfont, blockedfont->fontid)) {
	goto lowmem;
    }
    /* do an FS_OpenFont, FS_QueryXInfo and FS_QueryXExtents */
    buf[0] = (unsigned char) namelen;
    bcopy(name, (char *) &buf[1], namelen);
    namelen++;
    openreq.reqType = FS_OpenBitmapFont;
    openreq.fid = newid;
    openreq.format_hint = format;
    openreq.format_mask = fmask;
    openreq.length = (sizeof(fsOpenBitmapFontReq) + namelen + 3) >> 2;

    _fs_add_req_log(conn, FS_OpenBitmapFont);
    _fs_write(conn, (char *) &openreq, sizeof(fsOpenBitmapFontReq));
    _fs_write_pad(conn, (char *) buf, namelen);

#ifdef NCD
    if (configData.ExtendedFontDiags) {
	bcopy(name, buf, MIN(256, namelen));
	buf[MIN(256, namelen)] = '\0';
	printf("Requesting font \"%s\" from font server \"%s\"\n",
	       buf, fpe->name);
    }
#endif

    if (flags & FontOpenSync) {
	err = fs_do_open_font(fpe, blockrec, TRUE);
	if (blockedfont->errcode == Successful) {
	    *ppfont = blockedfont->pfont;
	} else {
	    fs_cleanup_font(blockedfont);
	}
	fs_remove_blockrec(conn, blockrec);
    }
    return err;
}

static int
fs_send_query_info(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedFontPtr bfont;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXInfoReq inforeq;

    bfont = (FSBlockedFontPtr) blockrec->data;

    inforeq.reqType = FS_QueryXInfo;
    inforeq.id = bfont->fontid;
    inforeq.length = sizeof(fsQueryXInfoReq) >> 2;

    blockrec->sequence_number = conn->current_seq;
    _fs_add_req_log(conn, FS_QueryXInfo);
    _fs_write(conn, (char *) &inforeq, sizeof(fsQueryXInfoReq));

    return Successful;
}

static int
fs_send_query_extents(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedFontPtr bfont;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXExtents8Req extreq;

    bfont = (FSBlockedFontPtr) blockrec->data;

    extreq.reqType = FS_QueryXExtents8;
    extreq.range = fsTrue;
    extreq.fid = bfont->fontid;
    extreq.num_ranges = 0;
    extreq.length = sizeof(fsQueryXExtents8Req) >> 2;

    blockrec->sequence_number = conn->current_seq;
    _fs_add_req_log(conn, FS_QueryXExtents8);
    _fs_write(conn, (char *) &extreq, sizeof(fsQueryXExtents8Req));

    return Successful;
}

static int
fs_send_query_bitmaps(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedFontPtr bfont;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXBitmaps8Req bitreq;


    bfont = (FSBlockedFontPtr) blockrec->data;

    /* send the request */
    bitreq.reqType = FS_QueryXBitmaps8;
    bitreq.fid = bfont->fontid;
    bitreq.format = bfont->format;
    bitreq.range = TRUE;
    bitreq.length = sizeof(fsQueryXBitmaps8Req) >> 2;
    bitreq.num_ranges = 0;

    blockrec->sequence_number = conn->current_seq;
    _fs_add_req_log(conn, FS_QueryXBitmaps8);
    _fs_write(conn, (char *) &bitreq, sizeof(fsQueryXBitmaps8Req));

    return Successful;
}

/* ARGSUSED */
static int
fs_open_font(client, fpe, flags, name, namelen, format, fmask, id, ppfont,
	     alias)
    pointer     client;
    FontPathElementPtr fpe;
    Mask        flags;
    char       *name;
    fsBitmapFormat format;
    fsBitmapFormatMask fmask;
    int         namelen;
    XID         id;
    FontPtr    *ppfont;
    char      **alias;
{
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    FSBlockDataPtr blockrec;
    FSBlockedFontPtr blockedfont;
    int         err;

    *alias = (char *) 0;
    /* XX if we find the blockrec for the font */
    blockrec = (FSBlockDataPtr) conn->blocked_requests;
    while (blockrec != (FSBlockDataPtr) 0) {
	if (blockrec->type == FS_OPEN_FONT &&
		blockrec->client == client) {
	    blockedfont = (FSBlockedFontPtr) blockrec->data;
	    err = blockedfont->errcode;
	    if (err == Successful) {
		*ppfont = blockedfont->pfont;
	    } else {
		fs_cleanup_font(blockedfont);
	    }
	    /* cleanup */
	    fs_remove_blockrec(conn, blockrec);
	    return err;
	}
	blockrec = blockrec->next;
    }
    return fs_send_open_font(client, fpe, flags, name, namelen, format, fmask,
			     id, ppfont);
}

/* ARGSUSED */
static int
fs_send_close_font(fpe, id)
    FontPathElementPtr fpe;
    Font        id;
{
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsCloseReq  req;

    /* tell the font server to close the font */
    req.reqType = FS_CloseFont;
    req.length = sizeof(fsCloseReq) >> 2;
    req.id = id;
    _fs_add_req_log(conn, FS_CloseFont);
    _fs_write(conn, (char *) &req, sizeof(fsCloseReq));

    return Successful;
}

/* ARGSUSED */
static int
fs_close_font(fpe, pfont)
    FontPathElementPtr fpe;
    FontPtr     pfont;
{
    FSFontDataPtr fsd = (FSFontDataPtr) pfont->fpePrivate;
    FSFpePtr    conn = (FSFpePtr) fpe->private;

    /* XXX we may get called after the resource DB has been cleaned out */
    if (find_old_font(fsd->fontid))
	DeleteFontClientID(fsd->fontid);
    if (conn->generation == fsd->generation)
	fs_send_close_font(fpe, fsd->fontid);
    (*pfont->unload_font) (pfont);

    xfree(fsd);

    xfree(pfont->info.isStringProp);
    xfree(pfont->info.props);
    xfree(pfont);

    return Successful;
}

static int
fs_read_glyphs(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedGlyphPtr bglyph = (FSBlockedGlyphPtr) blockrec->data;
    FSBlockedFontPtr bfont = (FSBlockedFontPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    FSFontDataPtr fsd = (FSFontDataPtr) (bglyph->pfont->fpePrivate);
    FSFontPtr   fsdata = (FSFontPtr) bglyph->pfont->fontPrivate;
    fsQueryXBitmaps8Reply rep;
    fsOffset   *ppbits;
    pointer     pbitmaps;
    char	*bits;
    int         glyph_size,
                offset_size,
                i;

    /* get reply header */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (rep.type == FS_Error) {
/* XXX -- translate FS error */
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	return AllocError;
    }
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	      sizeof(fsQueryXBitmaps8Reply) - sizeof(fsReplyHeader)) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    /* allocate space for glyphs */
    offset_size = sizeof(fsOffset) * (rep.num_chars);
    glyph_size = (rep.length << 2) - sizeof(fsQueryXBitmaps8Reply)
	- offset_size;
    ppbits = (fsOffset *) xalloc(offset_size);
    pbitmaps = (pointer) xalloc(glyph_size);
    if (!pbitmaps || !ppbits) {
	xfree(pbitmaps);
	xfree(ppbits);

	/* clear wire */
	(void) _fs_drain_bytes_pad(conn, offset_size);
	(void) _fs_drain_bytes_pad(conn, glyph_size);

	/* XXX may want to do something else for an FS_LOAD_GLYPHS */
	fs_cleanup_font(bfont);
	return AllocError;
    }
    /* read offsets */
    if (_fs_read_pad(conn, (char *) ppbits, offset_size) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    /* adjust them */
    for (i = 0; i < rep.num_chars; i++) {
	if (ppbits[i].length || NONZEROMETRICS(&fsdata->encoding[i].metrics))
	    bits = (char *) pbitmaps + ppbits[i].position;
	else
	    bits = 0;
	fsdata->encoding[i].bits = bits;
	/* copy the pointer into inkMetrics so _fs_get_metrics can use it */
	fsdata->inkMetrics[i].bits = bits;
    }

    /* read glyphs according to the range */
    if (_fs_read_pad(conn, (char *) pbitmaps, glyph_size) == -1) {
	fs_free_font(bfont);
	return StillWorking;
    }
    fsdata->bitmaps = pbitmaps;

    xfree(ppbits);

    fsd->complete = TRUE;
    if (blockrec->type == FS_LOAD_GLYPHS)
	bglyph->done = TRUE;
    else
	bfont->state = FS_DONE_REPLY;
    return Successful;
}


static int
fs_send_load_glyphs(client, pfont, nchars, item_size, data)
    pointer     client;
    FontPtr     pfont;
    unsigned int nchars;
    int         item_size;
    unsigned char *data;
{
    FSBlockDataPtr blockrec;
    FSBlockedGlyphPtr blockedglyph;
    fsRange     range;
    int         res;
    fsQueryXBitmaps8Req req;
    int         err;
    FSFontDataPtr fsd = (FSFontDataPtr) (pfont->fpePrivate);
    FontPathElementPtr fpe = fsd->fpe;
    FSFpePtr    conn = (FSFpePtr) fpe->private;

    /*
     * see if the desired glyphs already exist, and return Successful if they
     * do, otherwise build up character range/character string
     */
    res = fs_build_range(pfont, nchars, item_size, &range, data);
    if (res == AccessDone)
	return Successful;

    /* make a new block record, and add it to the end of the list */
    blockrec = fs_new_block_rec(fpe, client, FS_LOAD_GLYPHS);
    if (!blockrec)
	return AllocError;
    blockedglyph = (FSBlockedGlyphPtr) blockrec->data;
    blockedglyph->pfont = pfont;
    blockedglyph->expected_range = range;
    blockedglyph->done = FALSE;

    /* send the request */
    req.reqType = FS_QueryXBitmaps8;
    req.fid = ((FSFontDataPtr) pfont->fpePrivate)->fontid;
    req.format = pfont->format;
    req.range = TRUE;
    req.length = sizeof(fsQueryXBitmaps8Req) >> 2;
    req.num_ranges = 0;
    _fs_add_req_log(conn, FS_QueryXBitmaps8);
    _fs_write(conn, (char *) &req, sizeof(fsQueryXBitmaps8Req));

/* XXX -- hack -- need to support blocking eventually */
    err = fs_read_glyphs(fpe, blockrec);

    fs_remove_blockrec(conn, blockrec);

    return err;
}

int
fs_load_glyphs(client, pfont, nchars, item_size, data)
    pointer     client;
    FontPtr     pfont;
    unsigned int nchars;
    int         item_size;
    unsigned char *data;
{

#ifdef notyet
    FSBlockDataPtr blockrec;
    FSBlockedGlyphPtr blockedglyph;

    /* see if the result is already there */
    /* XXX - this is pretty lame -- need a better solution */
    blockrec = (FSBlockDataPtr) conn->blocked_requests;
    while (blockrec) {
	if (blockrec->type == FS_LOAD_GLYPHS && blockrec->client == c->client) {
	    blockedglyph = (FSBlockedListPtr) blockrec->data;
	    if (blockedglyph->pfont == pfont && blockedglyph->done) {
		fs_remove_blockrec(conn, blockrec);
		return Successful;
	    }
	}
	blockrec = blockrec->next;
    }
#endif

    /* didn't find waiting record, so send a new one */
    return fs_send_load_glyphs(client, pfont, nchars, item_size, data);
}


/*
 * FS chaining extent loader
 */

static int
fs_read_extents(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedExtentPtr bextent = (FSBlockedExtentPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXExtents8Reply rep;
    fsCharInfo *extents;
    int         size;

    /* get reply header */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (rep.type == FS_Error) {
/* XXX -- translate FS error */
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	return AllocError;
    }
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	      sizeof(fsQueryXExtents8Reply) - sizeof(fsReplyHeader)) == -1) {
	/* XXX what to free, what to free */
	return StillWorking;
    }
    /* allocate space for glyphs */
    bextent->nextents = rep.num_extents;
    size = rep.num_extents * sizeof(fsCharInfo);
    extents = (fsCharInfo *) xalloc(size);
    if (!extents) {
	_fs_drain_bytes(conn, size);
	return AllocError;
    }
    /* read extents */
    if (_fs_read_pad(conn, (char *) extents, size) == -1) {
	/* XXX what to free? */
	return StillWorking;
    }
    bextent->extents = extents;

    bextent->done = TRUE;
    return Successful;
}


/*
 * almost identical to the above, but meant for FS chaining
 */

static int
fs_read_bitmaps(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedBitmapPtr bbitmap = (FSBlockedBitmapPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsQueryXBitmaps8Reply rep;
    fsOffset   *ppbits;
    pointer     pbitmaps;
    int         glyph_size,
                offset_size;

    /* read reply header */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (rep.type == FS_Error) {
/* XXX -- translate FS error */
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	return AllocError;
    }
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	      sizeof(fsQueryXBitmaps8Reply) - sizeof(fsReplyHeader)) == -1) {
	/* XXX what to free? */
	return StillWorking;
    }
    /* allocate space for glyphs */
    bbitmap->nglyphs = rep.num_chars;
    offset_size = sizeof(fsOffset) * rep.num_chars;
    glyph_size = (rep.length << 2) - sizeof(fsQueryXBitmaps8Reply)
	- offset_size;
    bbitmap->size = glyph_size;
    ppbits = (fsOffset *) xalloc(offset_size);
    pbitmaps = (pointer) xalloc(glyph_size);
    if (!pbitmaps || !ppbits) {
	xfree(pbitmaps);
	xfree(ppbits);
	_fs_drain_bytes_pad(conn, offset_size + glyph_size);
	return AllocError;
    }
    /* read offsets */
    if (_fs_read_pad(conn, (char *) ppbits, offset_size) == -1) {
	/* XXX what to free? */
	return StillWorking;
    }
    bbitmap->offsets = ppbits;

    /* read glyphs according to the range */
    if (_fs_read_pad(conn, (char *) pbitmaps, glyph_size) == -1) {
	/* XXX what to free? */
	return StillWorking;
    }
    bbitmap->gdata = pbitmaps;

    bbitmap->done = TRUE;
    return Successful;
}

static int
fs_read_list(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedListPtr blist = (FSBlockedListPtr) blockrec->data;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsListFontsReply rep;
    char       *data,
               *dp;
    int         length,
                i;

    blist->done = TRUE;

    /* read reply header */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (rep.type == FS_Error) {
/* XXX -- translate FS error */
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	return AllocError;
    }
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
		 sizeof(fsListFontsReply) - sizeof(fsReplyHeader)) == -1) {
	/* nothing to free (i think) */
	return StillWorking;
    }
    length = (rep.length << 2) - sizeof(fsListFontsReply);
    data = (char *) xalloc(length);
    if (!data) {
	_fs_drain_bytes_pad(conn, length);
	return AllocError;
    }
    /* read the list */
    if (_fs_read_pad(conn, data, length) == -1) {
	/* nothing to free (i think) */
	return StillWorking;
    }
    /* copy data into FontPathRecord */
    dp = data;
    for (i = 0; i < rep.nFonts; i++) {
	length = *dp++;
	if (AddFontNamesName(blist->names, dp, length) != Successful) {
	    blist->errcode = AllocError;
	    break;
	}
	dp += length;
    }

    xfree(data);
    return Successful;
}

static int
fs_send_list_fonts(client, fpe, pattern, patlen, maxnames, newnames)
    pointer     client;
    FontPathElementPtr fpe;
    char       *pattern;
    int         patlen;
    int         maxnames;
    FontNamesPtr newnames;
{
    FSBlockDataPtr blockrec;
    FSBlockedListPtr blockedlist;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsListFontsReq req;

    _fs_client_access (conn, client, FALSE);

    /* make a new block record, and add it to the end of the list */
    blockrec = fs_new_block_rec(fpe, client, FS_LIST_FONTS);
    if (!blockrec)
	return AllocError;
    blockedlist = (FSBlockedListPtr) blockrec->data;
    blockedlist->patlen = patlen;
    blockedlist->errcode = Successful;
    blockedlist->names = newnames;
    blockedlist->done = FALSE;

    /* send the request */
    req.reqType = FS_ListFonts;
    req.maxNames = maxnames;
    req.nbytes = patlen;
    req.length = (sizeof(fsListFontsReq) + patlen + 3) >> 2;
    _fs_add_req_log(conn, FS_ListFonts);
    _fs_write(conn, (char *) &req, sizeof(fsListFontsReq));
    _fs_write_pad(conn, (char *) pattern, patlen);

#ifdef NCD
    if (configData.ExtendedFontDiags) {
	char        buf[256];

	bcopy(pattern, buf, MIN(256, patlen));
	buf[MIN(256, patlen)] = '\0';
	printf("Listing fonts on pattern \"%s\" from font server \"%s\"\n",
	       buf, fpe->name);
    }
#endif

    return Suspended;
}

static int
fs_list_fonts(client, fpe, pattern, patlen, maxnames, newnames)
    pointer     client;
    FontPathElementPtr fpe;
    char       *pattern;
    int         patlen;
    int         maxnames;
    FontNamesPtr newnames;
{
    FSBlockDataPtr blockrec;
    FSBlockedListPtr blockedlist;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    int         err;

    /* see if the result is already there */
    blockrec = (FSBlockDataPtr) conn->blocked_requests;
    while (blockrec) {
	if (blockrec->type == FS_LIST_FONTS && blockrec->client == client) {
	    blockedlist = (FSBlockedListPtr) blockrec->data;
	    if (blockedlist->patlen == patlen && blockedlist->done) {
		err = blockedlist->errcode;
		fs_remove_blockrec(conn, blockrec);
		return err;
	    }
	}
	blockrec = blockrec->next;
    }

    /* didn't find waiting record, so send a new one */
    return fs_send_list_fonts(client, fpe, pattern, patlen, maxnames, newnames);
}

static int  padlength[4] = {0, 3, 2, 1};

static int
fs_read_list_info(fpe, blockrec)
    FontPathElementPtr fpe;
    FSBlockDataPtr blockrec;
{
    FSBlockedListInfoPtr binfo = (FSBlockedListInfoPtr) blockrec->data;
    fsListFontsWithXInfoReply rep;
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    fsPropInfo  pi;
    fsPropOffset *po;
    char       *name;
    pointer     pd;
    int		err;

    /* clean up anything from the last trip */
    if (binfo->name)
    {
	xfree(binfo->name);
	binfo->name = NULL;
    }
    if (binfo->pfi) {
	xfree(binfo->pfi->isStringProp);
	xfree(binfo->pfi->props);
	xfree(binfo->pfi);
	binfo->pfi = NULL;
    }
    /* get reply header */
    bcopy((char *) &blockrec->header, (char *) &rep, sizeof(fsReplyHeader));
    if (rep.type == FS_Error) {
/* XXX -- translate FS error */
	_fs_eat_rest_of_error(conn, (fsError *) & rep);
	binfo->errcode = AllocError;
	return AllocError;
    }
    if (conn->fsMajorVersion > 1)
	if (rep.nameLength == 0)
	    goto done;
    /* old protocol sent a full-length reply even for the last one */
    if (_fs_read(conn, (char *) &rep + sizeof(fsReplyHeader),
	  sizeof(fsListFontsWithXInfoReply) - sizeof(fsReplyHeader)) == -1) {
	goto done;
    }
    if (rep.nameLength == 0)
	goto done;

    /* read the data */
    name = (char *) xalloc(rep.nameLength);
    binfo->pfi = (FontInfoPtr) xalloc(sizeof(FontInfoRec));
    if (!name || !binfo->pfi) {
	xfree(name);
	xfree(binfo->pfi);
	binfo->pfi = NULL;
	_fs_drain_bytes(conn,
			rep.length - (sizeof(fsListFontsWithXInfoReply) -
				      sizeof(fsReplyHeader)));
	binfo->errcode = AllocError;
	return AllocError;
    }
    if (conn->fsMajorVersion == 1)
	if (_fs_read_pad(conn, name, rep.nameLength) == -1)
	    goto done;
    if (_fs_read_pad(conn, (char *) &pi, sizeof(fsPropInfo)) == -1)
	    goto done;

    po = (fsPropOffset *) xalloc(sizeof(fsPropOffset) * pi.num_offsets);
    pd = (pointer) xalloc(pi.data_len);
    if (!po || !pd) {
	xfree(name);
	xfree(po);
	xfree(pd);
	xfree (binfo->pfi);
	binfo->pfi = NULL;
	binfo->errcode = AllocError;
	return AllocError;
    }
    err = _fs_read_pad(conn, (char *) po,
		       (pi.num_offsets * sizeof(fsPropOffset)));
    if (err != -1)
    {
	if (conn->fsMajorVersion > 1)
	    err = _fs_read(conn, (char *) pd, pi.data_len);
	else
	    err = _fs_read_pad(conn, (char *) pd, pi.data_len);
    }
    if (err != -1  &&  conn->fsMajorVersion != 1)
    {
	err = _fs_read(conn, name, rep.nameLength);
	if (err != -1)
	    err = _fs_drain_bytes(conn, padlength[(pi.data_len+rep.nameLength)&3]);
    }

    if (err == -1) {
	xfree(name);
	xfree(po);
	xfree(pd);
	xfree (binfo->pfi);
	binfo->pfi = NULL;
	goto done;
    }

    if (fs_convert_lfwi_reply(conn, binfo->pfi, &rep, &pi, po, pd) != Successful)
    {
	xfree(name);
	xfree(po);
	xfree(pd);
	xfree (binfo->pfi);
	binfo->pfi = NULL;
	goto done;
    }
    xfree(po);
    xfree(pd);
    binfo->name = name;
    binfo->namelen = rep.nameLength;
    binfo->remaining = rep.nReplies;

    binfo->status = FS_LFWI_REPLY;
    binfo->errcode = Suspended;
    /* disable this font server until we've processed this response */
    _fs_bit_clear(fs_fd_mask, conn->fs_fd);

    return Successful;

done:
    binfo->status = FS_LFWI_FINISHED;
    binfo->errcode = BadFontName;
    binfo->name = (char *) 0;
    return Successful;
}

/* ARGSUSED */
static int
fs_start_list_with_info(client, fpe, pattern, len, maxnames, pdata)
    pointer     client;
    FontPathElementPtr fpe;
    char       *pattern;
    int         len;
    int         maxnames;
    pointer    *pdata;
{
    FSBlockDataPtr blockrec;
    FSBlockedListInfoPtr blockedinfo;
    fsListFontsWithXInfoReq req;
    FSFpePtr    conn = (FSFpePtr) fpe->private;

    _fs_client_access (conn, client, FALSE);

    /* make a new block record, and add it to the end of the list */
    blockrec = fs_new_block_rec(fpe, client, FS_LIST_WITH_INFO);
    if (!blockrec)
	return AllocError;
    blockedinfo = (FSBlockedListInfoPtr) blockrec->data;
    bzero((char *) blockedinfo, sizeof(FSBlockedListInfoRec));
    blockedinfo->status = FS_LFWI_WAITING;
    blockedinfo->errcode = Suspended;

    /* send the request */
    req.reqType = FS_ListFontsWithXInfo;
    req.maxNames = maxnames;
    req.nbytes = len;
    req.length = (sizeof(fsListFontsWithXInfoReq) + len + 3) >> 2;
    _fs_add_req_log(conn, FS_ListFontsWithXInfo);
    (void) _fs_write(conn, (char *) &req, sizeof(fsListFontsWithXInfoReq));
    (void) _fs_write_pad(conn, pattern, len);

#ifdef NCD
    if (configData.ExtendedFontDiags) {
	char        buf[256];

	bcopy(pattern, buf, MIN(256, len));
	buf[MIN(256, len)] = '\0';
	printf("Listing fonts with info on pattern \"%s\" from font server \"%s\"\n",
	       buf, fpe->name);
    }
#endif

    return Successful;
}

/* ARGSUSED */
static int
fs_next_list_with_info(client, fpe, namep, namelenp, pFontInfo, numFonts,
		       private)
    pointer     client;
    FontPathElementPtr fpe;
    char      **namep;
    int        *namelenp;
    FontInfoPtr *pFontInfo;
    int        *numFonts;
    pointer     private;
{
    FSBlockDataPtr blockrec;
    FSBlockedListInfoPtr blockedinfo;
    FSFpePtr    conn = (FSFpePtr) fpe->private;

    /* see if the result is already there */
    blockrec = (FSBlockDataPtr) conn->blocked_requests;
    while (blockrec) {
	if (blockrec->type == FS_LIST_WITH_INFO &&
		blockrec->client == client) {
	    blockedinfo = (FSBlockedListInfoPtr) blockrec->data;
	    break;
	}
	blockrec = blockrec->next;
    }

    if (!blockrec)
	return Successful;

    if (blockedinfo->status == FS_LFWI_WAITING)
	return Suspended;

    *namep = blockedinfo->name;
    *namelenp = blockedinfo->namelen;
    *pFontInfo = blockedinfo->pfi;
    *numFonts = blockedinfo->remaining;
    _fs_set_bit(fs_fd_mask, conn->fs_fd);
    if (blockedinfo->status == FS_LFWI_FINISHED) {
	int         err = blockedinfo->errcode;

	fs_remove_blockrec(conn, blockrec);
	return err;
    }
    if (blockedinfo->status == FS_LFWI_REPLY) {
	blockedinfo->status = FS_LFWI_WAITING;
	return Successful;
    } else {
	return blockedinfo->errcode;
    }
}

/*
 * Called when client exits
 */

fs_client_died(client, fpe)
    pointer     client;
    FontPathElementPtr fpe;
{
    FSFpePtr    conn = (FSFpePtr) fpe->private;
    FSBlockDataPtr blockrec,
                depending;
    FSClientPtr	*prev, cur;
    fsFreeACReq	freeac;

    for (prev = &conn->clients; cur = *prev; prev = &cur->next)
    {
	if (cur->client == client) {
	    freeac.reqType = FS_FreeAC;
	    freeac.id = cur->acid;
	    freeac.length = sizeof (fsFreeACReq) >> 2;
	    _fs_add_req_log(conn, FS_FreeAC);
	    _fs_write (conn, (char *) &freeac, sizeof (fsFreeACReq));
	    *prev = cur->next;
	    xfree (cur);
	    break;
	}
    }
    /* see if the result is already there */
    blockrec = (FSBlockDataPtr) conn->blocked_requests;
    while (blockrec) {
	if (blockrec->client == client)
	    break;
	blockrec = blockrec->next;
    }
    if (!blockrec)
	return;
    if (blockrec->type == FS_LIST_WITH_INFO)
    {
	FSBlockedListInfoPtr binfo;
	binfo = (FSBlockedListInfoPtr) blockrec->data;
	if (binfo->status == FS_LFWI_REPLY)
	    _fs_set_bit(fs_fd_mask, conn->fs_fd);
    	if (binfo->name)
	{
	    xfree(binfo->name);
	    binfo->name = NULL;
	}
    	if (binfo->pfi) 
	{
	    xfree(binfo->pfi->isStringProp);
	    xfree(binfo->pfi->props);
	    xfree(binfo->pfi);
	    binfo->pfi = NULL;
    	}
    }
    /* replace the client pointers in this block rec with the chained one */
    if (depending = blockrec->depending) {
	blockrec->client = depending->client;
	blockrec->depending = depending->depending;
	blockrec = depending;
    }
    fs_remove_blockrec(conn, blockrec);
}

static void
_fs_client_access (conn, client, sync)
    FSFpePtr	conn;
    pointer	client;
    Bool	sync;
{
    FSClientPtr	*prev,	    cur;
    fsCreateACReq	    crac;
    fsSetAuthorizationReq   setac;
    fsReplyHeader	    rep;

    for (prev = &conn->clients; cur = *prev; prev = &cur->next)
    {
	if (cur->client == client)
	{
	    if (prev != &conn->clients)
	    {
		*prev = cur->next;
		cur->next = conn->clients;
		conn->clients = cur;
	    }
	    break;
	}
    }
    if (!cur)
    {
	cur = (FSClientPtr) xalloc (sizeof (FSClientRec));
	if (!cur)
	    return;
	cur->client = client;
	cur->next = conn->clients;
	conn->clients = cur;
	cur->acid = GetNewFontClientID ();
	crac.reqType = FS_CreateAC;
	crac.num_auths = 0;
	crac.length = sizeof (fsCreateACReq) >> 2;
	crac.acid = cur->acid;
	_fs_add_req_log(conn, FS_CreateAC);
	_fs_write(conn, (char *) &crac, sizeof (fsCreateACReq));
	/* if we're synchronous, open_font will be confused by
	 * the reply; eat it and continue
	 */
	if (sync)
	{
	    if (_fs_read(conn, (char *) &rep, sizeof (fsReplyHeader)) == -1)
		return;
	    fs_handle_unexpected(conn, &rep);
	}
	/* ignore reply; we don't even care about it */
    }
    if (conn->curacid != cur->acid)
    {
    	setac.reqType = FS_SetAuthorization;
    	setac.length = sizeof (fsSetAuthorizationReq) >> 2;
    	setac.id = cur->acid;
    	_fs_add_req_log(conn, FS_SetAuthorization);
    	_fs_write(conn, (char *) &setac, sizeof (fsSetAuthorizationReq));
	conn->curacid = cur->acid;
    }
}

/*
 * called at server init time
 */

void
fs_register_fpe_functions()
{
    fs_font_type = RegisterFPEFunctions(fs_name_check,
					fs_init_fpe,
					fs_free_fpe,
					fs_reset_fpe,
					fs_open_font,
					fs_close_font,
					fs_list_fonts,
					fs_start_list_with_info,
					fs_next_list_with_info,
					fs_wakeup,
					fs_client_died);
}
