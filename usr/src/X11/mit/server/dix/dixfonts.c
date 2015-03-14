/* $Header: /home/x_cvs/mit/server/dix/dixfonts.c,v 1.4 1992/05/23 06:09:57 dawes Exp $ */
/************************************************************************
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

************************************************************************/

/* $XConsortium: dixfonts.c,v 1.32 91/08/23 13:57:34 keith Exp $ */

#define NEED_REPLIES
#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "scrnintstr.h"
#include "resource.h"
#include "dixstruct.h"
#include "cursorstr.h"
#include "misc.h"
#include "opaque.h"
#include "dixfontstr.h"
#include "osstruct.h"
#include "closestr.h"

#ifdef DEBUG
#include	<stdio.h>
#endif

#define QUERYCHARINFO(pci, pr)  *(pr) = (pci)->metrics

static Mask FontFormat = 
#if IMAGE_BYTE_ORDER == LSBFirst
    BitmapFormatByteOrderLSB |
#else
    BitmapFormatByteOrderMSB |
#endif

#if BITMAP_BIT_ORDER == LSBFirst
    BitmapFormatBitOrderLSB |
#else
    BitmapFormatBitOrderMSB |
#endif

    BitmapFormatImageRectMin |

#if GLYPHPADBYTES == 1
    BitmapFormatScanlinePad8 |
#endif

#if GLYPHPADBYTES == 2
    BitmapFormatScanlinePad16 |
#endif

#if GLYPHPADBYTES == 4
    BitmapFormatScanlinePad32 |
#endif

#if GLYPHPADBYTES == 8
    BitmapFormatScanlinePad64 |
#endif

    BitmapFormatScanlineUnit8;

static int  FinishListFontsWithInfo();

extern pointer fosNaturalParams;
extern FontPtr defaultFont;

extern void (*ReplySwapVector[256]) ();

static FontPathElementPtr *font_path_elements = (FontPathElementPtr *) 0;
static int  num_fpes = 0;
static FPEFunctions *fpe_functions = (FPEFunctions *) 0;
static int  num_fpe_types = 0;

static unsigned char *font_path_string;

static int  num_slept_fpes = 0;
static int  size_slept_fpes = 0;
static FontPathElementPtr *slept_fpes = (FontPathElementPtr *) 0;
static FontPatternCachePtr patternCache;

int
FontToXError(err)
    int         err;
{
    switch (err) {
    case Successful:
	return Success;
    case AllocError:
	return BadAlloc;
    case BadFontName:
    case BadFontPath:
	return BadName;
    case BadFontFormat:	/* is there something better? */
    case BadCharRange:
	return BadValue;
    default:
	return err;
    }
}


/*
 * adding RT_FONT prevents conflict with default cursor font
 */
Bool
SetDefaultFont(defaultfontname)
    char       *defaultfontname;
{
    int         err;
    FontPtr     pf;
    XID         fid;

    fid = FakeClientID(0);
    err = OpenFont(serverClient, fid, FontLoadAll | FontOpenSync,
		   (unsigned) strlen(defaultfontname), defaultfontname);
    if (err != Success)
	return FALSE;
    pf = (FontPtr) LookupIDByType(fid, RT_FONT);
    if (pf == (FontPtr) NULL)
	return FALSE;
    defaultFont = pf;
    return TRUE;
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
	    xrealloc(slept_fpes,
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
int
FontWakeup(data, count, LastSelectMask)
    pointer     data;
    int         count;
    long       *LastSelectMask;
{
    int         i;
    FontPathElementPtr fpe;

    if (count < 0)
	return Success;		/* ignore -1 return from select XXX */
    /* wake up any fpe's that may be waiting for information */
    for (i = 0; i < num_slept_fpes; i++) {
	fpe = slept_fpes[i];
	(void) (*fpe_functions[fpe->type].wakeup_fpe) (fpe, LastSelectMask);
    }

    return Success;
}

static Bool
doOpenFont(client, c)
    ClientPtr   client;
    OFclosurePtr c;
{
    FontPtr     pfont = NullFont;
    FontPathElementPtr fpe;
    ScreenPtr   pScr;
    int         err = Successful;
    int         i;
    char       *alias,
               *newname;
    int         newlen;

    if (client->clientGone)
    {
	if (c->current_fpe < c->num_fpes)
	{
	    fpe = c->fpe_list[c->current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto bail;
    }
    while (c->current_fpe < c->num_fpes) {
	fpe = c->fpe_list[c->current_fpe];
	err = (*fpe_functions[fpe->type].open_font)
	    ((pointer) client, fpe, c->flags,
	     c->fontname, c->fnamelen, FontFormat,
	     BitmapFormatMaskByte |
	     BitmapFormatMaskBit |
	     BitmapFormatMaskImageRectangle |
	     BitmapFormatMaskScanLinePad |
	     BitmapFormatMaskScanLineUnit,
	     c->fontid, &pfont, &alias);

	if (err == FontNameAlias && alias) {
	    newlen = strlen(alias);
	    newname = (char *) xrealloc(c->fontname, newlen);
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
		ClientSleep(client, doOpenFont, (pointer) c);
	    }
	    return TRUE;
	}
	break;
    }

    if (err != Successful)
	goto bail;
    if (!pfont) {
	err = BadFontName;
	goto bail;
    }
    pfont->fpe = fpe;
    pfont->refcnt++;
    if (pfont->refcnt == 1) {
	UseFPE(pfont->fpe);
	for (i = 0; i < screenInfo.numScreens; i++) {
	    pScr = screenInfo.screens[i];
	    if (pScr->RealizeFont)
	    {
		if (!(*pScr->RealizeFont) (pScr, pfont))
		{
		    CloseFont (pfont, (Font) 0);
		    err = AllocError;
		    goto bail;
		}
	    }
	}
    }
    if (!AddResource(c->fontid, RT_FONT, (pointer) pfont)) {
	err = AllocError;
	goto bail;
    }
    if (patternCache && pfont->info.cachable)
	CacheFontPattern(patternCache, c->origFontName, c->origFontNameLen,
			 pfont);
bail:
    if (err != Successful && c->client != serverClient) {
	SendErrorToClient(c->client, X_OpenFont, 0,
			  c->fontid, FontToXError(err));
    }
    if (c->slept)
	ClientWakeup(c->client);
    for (i = 0; i < c->num_fpes; i++) {
	FreeFPE(c->fpe_list[i], FALSE);
    }
    xfree(c->fpe_list);
    xfree(c->fontname);
    xfree(c);
    return TRUE;
}

int
OpenFont(client, fid, flags, lenfname, pfontname)
    ClientPtr   client;
    XID         fid;
    Mask        flags;
    unsigned    lenfname;
    char       *pfontname;
{
    OFclosurePtr c;
    int         i;
    FontPtr     cached;

    if (patternCache)
    {
	cached = FindCachedFontPattern(patternCache, pfontname, lenfname);
	if (cached)
	{
	    if (!AddResource(fid, RT_FONT, (pointer) cached))
		return BadAlloc;
	    cached->refcnt++;
	    return Success;
	}
    }
    c = (OFclosurePtr) xalloc(sizeof(OFclosureRec));
    if (!c)
	return BadAlloc;
    c->fontname = (char *) xalloc(lenfname);
    c->origFontName = pfontname;
    c->origFontNameLen = lenfname;
    if (!c->fontname) {
	xfree(c);
	return BadAlloc;
    }
    /*
     * copy the current FPE list, so that if it gets changed by another client
     * while we're blocking, the request still appears atomic
     */
    c->fpe_list = (FontPathElementPtr *)
	xalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list) {
	xfree(c->fontname);
	xfree(c);
	return BadAlloc;
    }
    bcopy(pfontname, c->fontname, lenfname);
    for (i = 0; i < num_fpes; i++) {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->client = client;
    c->fontid = fid;
    c->current_fpe = 0;
    c->num_fpes = num_fpes;
    c->fnamelen = lenfname;
    c->slept = FALSE;
    c->flags = flags;

    (void) doOpenFont(client, c);
    return Success;
}

/*
 * Decrement font's ref count, and free storage if ref count equals zero
 */
/*ARGSUSED*/
int
CloseFont(pfont, fid)
    FontPtr     pfont;
    Font        fid;
{
    int         nscr;
    ScreenPtr   pscr;
    FontPathElementPtr fpe;

    if (pfont == NullFont)
	return (Success);
    if (--pfont->refcnt == 0) {
	if (patternCache && pfont->info.cachable)
	    RemoveCachedFontPattern (patternCache, pfont);
	/*
	 * since the last reference is gone, ask each screen to free any
	 * storage it may have allocated locally for it.
	 */
	for (nscr = 0; nscr < screenInfo.numScreens; nscr++) {
	    pscr = screenInfo.screens[nscr];
	    if (pscr->UnrealizeFont)
		(*pscr->UnrealizeFont) (pscr, pfont);
	}
	if (pfont == defaultFont)
	    defaultFont = NULL;
	fpe = pfont->fpe;
	(*fpe_functions[fpe->type].close_font) (fpe, pfont);
	FreeFPE(fpe, FALSE);
    }
    return (Success);
}


/***====================================================================***/

 /*
  * \ Sets up pReply as the correct QueryFontReply for pFont with the first
  * nProtoCCIStructs char infos. \
  */

/* 5/23/89 (ef) -- XXX! Does this already exist somewhere? */
static xCharInfo xciNoSuchChar = {0, 0, 0, 0, 0, 0};

void
QueryFont(pFont, pReply, nProtoCCIStructs)
    FontPtr     pFont;
    xQueryFontReply *pReply;	/* caller must allocate this storage */
    int         nProtoCCIStructs;
{
    FontPropPtr pFP;
    int         r,
                c,
                i;
    xFontProp  *prFP;
    xCharInfo  *prCI;
    xCharInfo  *charInfos[256];
    char        chars[512];
    int         nrows,
                ncols;
    int         ninfos;
    int         count;

    /* pr->length set in dispatch */
    pReply->minCharOrByte2 = pFont->info.firstCol;
    pReply->defaultChar = pFont->info.defaultCh;
    pReply->maxCharOrByte2 = pFont->info.lastCol;
    pReply->drawDirection = pFont->info.drawDirection;
    pReply->allCharsExist = pFont->info.allExist;
    pReply->minByte1 = pFont->info.firstRow;
    pReply->maxByte1 = pFont->info.lastRow;
    pReply->fontAscent = pFont->info.fontAscent;
    pReply->fontDescent = pFont->info.fontDescent;

    pReply->minBounds = pFont->info.ink_minbounds;
    pReply->maxBounds = pFont->info.ink_maxbounds;

    pReply->nFontProps = pFont->info.nprops;
    pReply->nCharInfos = nProtoCCIStructs;

    for (i = 0, pFP = pFont->info.props, prFP = (xFontProp *) (&pReply[1]);
	    i < pFont->info.nprops;
	    i++, pFP++, prFP++) {
	prFP->name = pFP->name;
	prFP->value = pFP->value;
    }

    ninfos = 0;
    ncols = pFont->info.lastCol - pFont->info.firstCol + 1;
    prCI = (xCharInfo *) (prFP);
    for (r = pFont->info.firstRow;
	    ninfos < nProtoCCIStructs && r <= pFont->info.lastRow;
	    r++) {
	i = 0;
	for (c = pFont->info.firstCol; c <= pFont->info.lastCol; c++) {
	    chars[i++] = r;
	    chars[i++] = c;
	}
	(*pFont->get_metrics) (pFont, ncols, chars, TwoD16Bit,
			       &count, charInfos);
	i = 0;
	for (i = 0; i < count && ninfos < nProtoCCIStructs; i++) {
	    if (charInfos[i])
		*prCI = *charInfos[i];
	    else
		*prCI = xciNoSuchChar;
	    prCI++;
	    ninfos++;
	}
    }
    return;
}

static Bool
doListFonts(client, c)
    ClientPtr   client;
    LFclosurePtr c;
{
    int         err;
    FontPathElementPtr fpe;
    xListFontsReply reply;
    FontNamesPtr names;
    int         stringLens,
                i,
                nnames;
    char       *bufptr,
               *bufferStart;
    int         count;

    if (client->clientGone)
    {
	if (c->current_fpe < c->num_fpes)
	{
	    fpe = c->fpe_list[c->current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto bail;
    }
    /* try each fpe in turn, returning if one wants to be blocked */

    while (c->current_fpe < c->num_fpes && c->names->nnames <= c->max_names)
    {

	fpe = c->fpe_list[c->current_fpe];

	err = (*fpe_functions[fpe->type].list_fonts)
	    (c->client, fpe, c->pattern, c->patlen,
	     c->max_names - c->names->nnames, c->names);

	if (err == Suspended) {
	    if (!c->slept) {
		c->slept = TRUE;
		ClientSleep(client, doListFonts, c);
	    }
	    return TRUE;
	}
	if (err != Successful)
	    break;
	c->current_fpe++;
    }
    if (err != Successful) {
	SendErrorToClient(client, X_ListFonts, 0, 0, FontToXError(err));
	goto bail;
    }
    names = c->names;
    nnames = names->nnames;
    client = c->client;
    stringLens = 0;
    for (i = 0; i < nnames; i++)
	stringLens += names->length[i];

    reply.type = X_Reply;
    reply.length = (stringLens + nnames + 3) >> 2;
    reply.nFonts = nnames;
    reply.sequenceNumber = client->sequence;

    bufptr = bufferStart = (char *) ALLOCATE_LOCAL(reply.length << 2);

    if (!bufptr && reply.length) {
	SendErrorToClient(client, X_ListFonts, 0, 0, BadAlloc);
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
    client->pSwapReplyFunc = ReplySwapVector[X_ListFonts];
    WriteSwappedDataToClient(client, sizeof(xListFontsReply), &reply);
    (void) WriteToClient(client, stringLens + nnames, bufferStart);
    DEALLOCATE_LOCAL(bufferStart);
bail:
    if (c->slept)
	ClientWakeup(client);
    for (i = 0; i < c->num_fpes; i++)
	FreeFPE(c->fpe_list[i], FALSE);
    xfree(c->fpe_list);
    FreeFontNames (c->names);
    xfree(c->pattern);
    xfree(c);
    return TRUE;
}

static      LFclosurePtr
MakeListFontsClosure(client, pattern, length, max_names)
    ClientPtr   client;
    unsigned char *pattern;
    unsigned int length;
    unsigned int max_names;
{
    LFclosurePtr c;
    int         i;
    FontNamesPtr all_names;

    c = (LFclosurePtr) xalloc(sizeof(LFclosureRec));
    if (!c)
	return 0;
    c->pattern = (char *) xalloc(length);
    if (!c->pattern) {
	xfree(c);
	return 0;
    }
    c->names = MakeFontNamesRecord(max_names < 100 ? max_names : 100);
    if (!c->names) {
	xfree(c->pattern);
	xfree(c);
	return 0;
    }
    c->fpe_list = (FontPathElementPtr *)
	xalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list)
    {
	FreeFontNames(c->names);
	xfree(c->pattern);
	xfree(c);
	return 0;
    }
    bcopy(pattern, c->pattern, length);
    for (i = 0; i < num_fpes; i++) {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->patlen = length;
    c->client = client;
    c->max_names = max_names;
    c->current_fpe = 0;
    c->num_fpes = num_fpes;
    c->slept = FALSE;
    return c;
}

int
ListFonts(client, pattern, length, max_names)
    ClientPtr   client;
    unsigned char *pattern;
    unsigned int length;
    unsigned int max_names;
{
    LFclosurePtr c;

    c = MakeListFontsClosure(client, pattern, length, max_names);
    if (!c)
	return BadAlloc;

    (void) doListFonts(client, c);
    return Success;
}

doListFontsWithInfo(client, c)
    ClientPtr   client;
    LFWIclosurePtr c;
{
    FontPathElementPtr fpe;
    int         err = Successful;
    char       *name;
    int         namelen;
    int         numFonts;
    FontInfoRec fontInfo,
               *pFontInfo;
    xListFontsWithInfoReply *reply;
    int         length;
    xFontProp  *pFP;
    int         i;
    xListFontsWithInfoReply finalReply;

    if (client->clientGone)
    {
	if (c->current.current_fpe < c->num_fpes)
 	{
	    fpe = c->fpe_list[c->current.current_fpe];
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
	}
	err = Successful;
	goto bail;
    }
    client->pSwapReplyFunc = ReplySwapVector[X_ListFontsWithInfo];
    while (c->current.current_fpe < c->num_fpes)
    {
	fpe = c->fpe_list[c->current.current_fpe];
	err = Successful;
	if (!c->current.list_started)
 	{
	    err = (*fpe_functions[fpe->type].start_list_fonts_with_info)
		(client, fpe, c->current.pattern, c->current.patlen,
		 c->current.max_names, &c->current.private);
	    if (err == Suspended)
 	    {
		if (!c->slept)
 		{
		    ClientSleep(client, doListFontsWithInfo, c);
		    c->slept = TRUE;
		}
		return TRUE;
	    }
	    if (err == Successful)
		c->current.list_started = TRUE;
	}
	if (err == Successful)
 	{
	    name = 0;
	    pFontInfo = &fontInfo;
	    err = (*fpe_functions[fpe->type].list_next_font_with_info)
		(client, fpe, &name, &namelen, &pFontInfo,
		 &numFonts, c->current.private);
	    if (err == Suspended)
 	    {
		if (!c->slept)
 		{
		    ClientSleep(client, doListFontsWithInfo, c);
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
	if (err == FontNameAlias)
 	{
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
	else if (err == BadFontName)
 	{
	    c->current.list_started = FALSE;
	    c->current.current_fpe++;
	    err = Successful;
	    if (c->haveSaved)
 	    {
		if (c->current.max_names == 0 ||
			c->current.current_fpe == c->num_fpes)
 		{
		    c->haveSaved = FALSE;
		    c->saved.max_names -= (1 - c->current.max_names);
		    c->current = c->saved;
		}
	    }
	    if (c->current.max_names == 0)
		break;
	}
 	else if (err == Successful)
 	{
	    length = sizeof(*reply) + pFontInfo->nprops * sizeof(xFontProp);
	    reply = c->reply;
	    if (c->length < length)
 	    {
		reply = (xListFontsWithInfoReply *) xrealloc(c->reply, length);
		if (!reply)
 		{
		    err = AllocError;
		    break;
		}
		c->reply = reply;
		c->length = length;
	    }
	    if (c->haveSaved)
 	    {
		numFonts = c->savedNumFonts;
		name = c->savedName;
		namelen = strlen(name);
	    }
	    reply->type = X_Reply;
	    reply->length = (sizeof *reply - sizeof(xGenericReply) +
			     pFontInfo->nprops * sizeof(xFontProp) +
			     namelen + 3) >> 2;
	    reply->sequenceNumber = client->sequence;
	    reply->nameLength = namelen;
	    reply->minBounds = pFontInfo->ink_minbounds;
	    reply->maxBounds = pFontInfo->ink_maxbounds;
	    reply->minCharOrByte2 = pFontInfo->firstCol;
	    reply->maxCharOrByte2 = pFontInfo->lastCol;
	    reply->defaultChar = pFontInfo->defaultCh;
	    reply->nFontProps = pFontInfo->nprops;
	    reply->drawDirection = pFontInfo->drawDirection;
	    reply->minByte1 = pFontInfo->firstRow;
	    reply->maxByte1 = pFontInfo->lastRow;
	    reply->allCharsExist = pFontInfo->allExist;
	    reply->fontAscent = pFontInfo->fontAscent;
	    reply->fontDescent = pFontInfo->fontDescent;
	    reply->nReplies = numFonts;
	    pFP = (xFontProp *) (reply + 1);
	    for (i = 0; i < pFontInfo->nprops; i++)
 	    {
		pFP->name = pFontInfo->props[i].name;
		pFP->value = pFontInfo->props[i].value;
		pFP++;
	    }
	    WriteSwappedDataToClient(client, length, reply);
	    (void) WriteToClient(client, namelen, name);
	    if (pFontInfo == &fontInfo)
 	    {
		xfree(fontInfo.props);
		xfree(fontInfo.isStringProp);
	    }
	    --c->current.max_names;
	    if (c->current.max_names < 0)
		break;
	}
    }
    length = sizeof(xListFontsWithInfoReply);
    bzero((char *) &finalReply, sizeof(xListFontsWithInfoReply));
    finalReply.type = X_Reply;
    finalReply.sequenceNumber = client->sequence;
    finalReply.length = (sizeof(xListFontsWithInfoReply)
		     - sizeof(xGenericReply)) >> 2;
    WriteSwappedDataToClient(client, length, &finalReply);
bail:
    if (c->slept)
	ClientWakeup(client);
    for (i = 0; i < c->num_fpes; i++)
	FreeFPE(c->fpe_list[i], FALSE);
    xfree(c->fpe_list);
    xfree(c->current.pattern);
    xfree(c);
    return TRUE;
}

int
StartListFontsWithInfo(client, length, pattern, max_names)
    ClientPtr   client;
    int         length;
    char       *pattern;
    int         max_names;
{
    int		    err;
    int		    i;
    LFWIclosurePtr  c;

    if (!(c = (LFWIclosurePtr) xalloc(sizeof *c)))
	goto badAlloc;
    if (!(c->current.pattern = (char *) xalloc(length)))
    {
	xfree(c);
	goto badAlloc;
    }
    c->fpe_list = (FontPathElementPtr *)
	xalloc(sizeof(FontPathElementPtr) * num_fpes);
    if (!c->fpe_list)
    {
	xfree(c->current.pattern);
	xfree(c);
	goto badAlloc;
    }
    bcopy(pattern, c->current.pattern, length);
    for (i = 0; i < num_fpes; i++)
    {
	c->fpe_list[i] = font_path_elements[i];
	UseFPE(c->fpe_list[i]);
    }
    c->client = client;
    c->num_fpes = num_fpes;
    c->reply = 0;
    c->length = 0;
    c->current.patlen = length;
    c->current.current_fpe = 0;
    c->current.max_names = max_names;
    c->current.list_started = FALSE;
    c->current.private = 0;
    c->savedNumFonts = 0;
    c->haveSaved = FALSE;
    c->slept = FALSE;
    doListFontsWithInfo(client, c);
    return Success;
badAlloc:
    return BadAlloc;
}

/* does the necessary magic to figure out the fpe type */
static int
DetermineFPEType(pathname)
    char       *pathname;
{
    int         i;

    for (i = 0; i < num_fpe_types; i++) {
	if ((*fpe_functions[i].name_check) (pathname))
	    return i;
    }
    return -1;
}


static void
FreeFontPath(list, n, force)
    FontPathElementPtr	*list;
    Bool		force;
    int         n;
{
    int         i;

    for (i = 0; i < n; i++) {
	FreeFPE(list[i], force);
    }
    xfree((char *) list);
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


static int
SetFontPathElements(npaths, paths, bad)
    int         npaths;
    unsigned char *paths;
    int        *bad;
{
    int         i,
                err;
    int         valid_paths = 0;
    int         badpath = 0;
    Bool        builtinflag = FALSE;
    unsigned int len;
    unsigned char *cp = paths;
    FontPathElementPtr fpe,
               *fplist;

    fplist = (FontPathElementPtr *)
	xalloc(sizeof(FontPathElementPtr) * npaths);
    if (!fplist) {
	*bad = 0;
	return BadAlloc;
    }
    for (i = 0; i < npaths; i++) {
	len = (unsigned int) (*cp++);

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
		    fplist[valid_paths++] = fpe;
		    cp += len;
		    continue;
		}
		/* if error or can't do it, act like its a new one */
	    }
	    fpe = (FontPathElementPtr) xalloc(sizeof(FontPathElementRec));
	    if (!fpe) {
		err = BadAlloc;
		goto bail;
	    }
	    fpe->name = (char *) xalloc(len + 1);
	    if (!fpe->name) {
		xfree(fpe);
		err = BadAlloc;
		goto bail;
	    }
	    fpe->refcount = 1;

	    strncpy(fpe->name, (char *) cp, (int) len);
	    cp += len;
	    fpe->name[len] = '\0';
	    fpe->name_length = len;
	    fpe->type = DetermineFPEType(fpe->name);
	    if (fpe->type == -1) {
		xfree(fpe->name);
		xfree(fpe);
		err = BadValue;
		goto bail;
	    }
	    err = (*fpe_functions[fpe->type].init_fpe) (fpe, FontFormat);
	    if (err != Successful) {
		xfree(fpe->name);
		xfree(fpe);
		err = BadValue;
		goto bail;
	    }
	    fplist[valid_paths++] = fpe;
	}
    }

    FreeFontPath(font_path_elements, num_fpes, FALSE);
    font_path_elements = fplist;
    if (patternCache)
	EmptyFontPatternCache(patternCache);
    num_fpes = valid_paths;

    return Success;
bail:
    *bad = i;
    while (--i >= 0)
	FreeFPE(fplist[i], FALSE);
    xfree(fplist);
    return err;
}

/* XXX -- do we need to pass error down to each renderer? */
int
SetFontPath(client, npaths, paths, error)
    ClientPtr   client;
    int         npaths;
    unsigned char *paths;
    int        *error;
{
    int         len,
                err = Success;

    if (npaths == 0) {
	if (SetDefaultFontPath(defaultFontPath) != Success)
	    return BadName;
    } else {
	err = SetFontPathElements(npaths, paths, error);
    }
    return err;
}

SetDefaultFontPath(path)
    char       *path;
{
    unsigned char *cp,
               *pp,
               *nump,
               *newpath;
    int         num = 1,
                len,
                err,
                size = 0,
                bad;

    /* get enough for string, plus values -- use up commas */
    len = strlen(path) + 1;
    nump = cp = newpath = (unsigned char *) ALLOCATE_LOCAL(len);
    if (!newpath)
	return BadAlloc;
    pp = (unsigned char *) path;
    cp++;
    while (*pp) {
	if (*pp == ',') {
	    *nump = (unsigned char) size;
	    nump = cp++;
	    pp++;
	    num++;
	    size = 0;
	} else {
	    *cp++ = *pp++;
	    size++;
	}
    }
    *nump = (unsigned char) size;

    err = SetFontPathElements(num, newpath, &bad);

    DEALLOCATE_LOCAL(newpath);

    return err;
}

unsigned char *
GetFontPath(count, length)
    int			*count;
    int			*length;
{
    int			i;
    unsigned char       *c;
    int			len;
    FontPathElementPtr	fpe;

    len = 0;
    for (i = 0; i < num_fpes; i++) {
	fpe = font_path_elements[i];
	len += fpe->name_length + 1;
    }
    font_path_string = (unsigned char *) xrealloc(font_path_string, len);
    if (!font_path_string)
	return NULL;

    c = font_path_string;
    *length = 0;
    for (i = 0; i < num_fpes; i++) {
	fpe = font_path_elements[i];
	*c = fpe->name_length;
	*length += *c++;
	bcopy(fpe->name, c, fpe->name_length);
	c += fpe->name_length;
    }
    *count = num_fpes;
    return font_path_string;
}

LoadGlyphs(client, pfont, nchars, item_size, data)
    ClientPtr   client;
    FontPtr     pfont;
    unsigned    nchars;
    int         item_size;
    unsigned char *data;
{

#ifdef NOTDEF
/* under construction */
    /* either returns Success, ClientBlocked, or some nasty error */
    return (*fpe_functions[pfont->type].load_glyphs)
	(client, pfont, nchars, item_size, data);
#endif
}

/* XXX -- these two funcs may want to be broken into macros */
void
UseFPE(fpe)
    FontPathElementPtr fpe;
{
    fpe->refcount++;
}

void
FreeFPE (fpe, force)
    FontPathElementPtr	fpe;
    Bool		force;
{
    fpe->refcount--;
    if (force || fpe->refcount == 0) {
        /*
         * This is a hack to work around a problem that occurs when using
         * the Type1 renderer.  The problem is that a free'd element of
         * font_path_elements is getting referenced during server shutdown
         * (and server reset?).  This occurs when there is a path containing
         * Type 1 fonts in font_path_elements, and a path has been added
         * using "xset +fp".  The real solution to the problem would be to
         * find where refcount is getting out of sync.
         *
         * The real problem is that the resouce list of a client started
         * before the "xset" somehow gets the new path -- so when the client
         * is shutdown it free's the fontpath when it shouldn't.  I haven't
         * been able to track down where this is happening -- and don't know
         * for sure if it's connected to the Type 1 renderer.
         * DHD Feb 1992
         */

        if (!force) {
            int i;
            for (i=0; i<num_fpes; i++) {
                /*
                 * If fpe is part of the current font_path_elements only free it
                 * at the end (when force is set).  This should prevent part of
                 * of the current font_path_elements being prematurely free'd.
                 */
              if (fpe==font_path_elements[i]) {
                    fpe->refcount++;
                    return;
                }
            }
        }

	(*fpe_functions[fpe->type].free_fpe) (fpe);
	xfree(fpe->name);
	xfree(fpe);
    }
}

DeleteClientFontStuff(client)
    ClientPtr	client;
{
    int			i;
    FontPathElementPtr	fpe;

    for (i = 0; i < num_fpes; i++)
    {
	fpe = font_path_elements[i];
	if (fpe_functions[fpe->type].client_died)
	    (*fpe_functions[fpe->type].client_died) ((pointer) client, fpe);
    }
}

InitFonts ()
{
    patternCache = MakeFontPatternCache();
    FontFileRegisterFpeFunctions();
    fs_register_fpe_functions();
}

GetDefaultPointSize ()
{
    return 120;
}

struct resolution {
    CARD16 x_resolution B16;
    CARD16 y_resolution B16;
    CARD16 point_size B16;
};

struct resolution *
GetClientResolutions (num)
    int        *num;
{
    static struct resolution res;
    int         mm,
                pix;
    ScreenPtr   pScreen;

    pScreen = screenInfo.screens[0];
    res.x_resolution = (pScreen->width * 25.4) / pScreen->mmWidth;
    /*
     * XXX - we'll want this as long as bitmap instances are prevalent so that
     * we can match them from scalable fonts
     */
    if (res.x_resolution < 88)
	res.x_resolution = 75;
    else
	res.x_resolution = 100;
    res.y_resolution = (pScreen->height * 25.4) / pScreen->mmHeight;
    if (res.y_resolution < 88)
	res.y_resolution = 75;
    else
	res.y_resolution = 100;
    res.point_size = 120;
    *num = 1;
    return &res;
}

/*
 * returns the type index of the new fpe
 *
 * should be called (only once!) by each type of fpe when initialized
 */

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
    int		(*client_died) ();
{
    FPEFunctions *new;

    /* grow the list */
    new = (FPEFunctions *) xrealloc(fpe_functions,
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
    if (patternCache) {
	FreeFontPatternCache(patternCache);
	patternCache = 0;
    }
    FreeFontPath(font_path_elements, num_fpes, TRUE);
    font_path_elements = 0;
    num_fpes = 0;
    xfree(fpe_functions);
    num_fpe_types = 0;
    fpe_functions = (FPEFunctions *) 0;
}

/* convenience functions for FS interface */

FontPtr
find_old_font(id)
    XID         id;
{
    return (FontPtr) LookupIDByType(id, RT_NONE);
}

Font
GetNewFontClientID()
{
    return FakeClientID(0);
}

int
StoreFontClientFont(pfont, id)
    FontPtr     pfont;
    Font        id;
{
    return AddResource(id, RT_NONE, (pointer) pfont);
}

DeleteFontClientID(id)
    Font        id;
{
    FreeResource(id, RT_NONE);
}


static int  fs_handlers_installed = 0;
static unsigned int last_server_gen;

init_fs_handlers(fpe, block_handler)
    FontPathElementPtr fpe;
    int         (*block_handler) ();
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
    int         (*block_handler) ();
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

#ifdef DEBUG
#define GLWIDTHBYTESPADDED(bits,nbytes) \
	((nbytes) == 1 ? (((bits)+7)>>3)        /* pad to 1 byte */ \
	:(nbytes) == 2 ? ((((bits)+15)>>3)&~1)  /* pad to 2 bytes */ \
	:(nbytes) == 4 ? ((((bits)+31)>>3)&~3)  /* pad to 4 bytes */ \
	:(nbytes) == 8 ? ((((bits)+63)>>3)&~7)  /* pad to 8 bytes */ \
	: 0)

#define GLYPH_SIZE(ch, nbytes)          \
	GLWIDTHBYTESPADDED((ch)->metrics.rightSideBearing - \
			(ch)->metrics.leftSideBearing, (nbytes))
dump_char_ascii(cip)
    CharInfoPtr cip;
{
    int         r,
                l;
    int         bpr;
    int         byte;
    static unsigned maskTab[] = {
	(1 << 7), (1 << 6), (1 << 5), (1 << 4),
	(1 << 3), (1 << 2), (1 << 1), (1 << 0),
    };

    bpr = GLYPH_SIZE(cip, 4);
    for (r = 0; r < (cip->metrics.ascent + cip->metrics.descent); r++) {
	pointer     row = (pointer) cip->bits + r * bpr;

	byte = 0;
	for (l = 0; l <= (cip->metrics.rightSideBearing -
			  cip->metrics.leftSideBearing); l++) {
	    if (maskTab[l & 7] & row[l >> 3])
		putchar('X');
	    else
		putchar('.');
	}
	putchar('\n');
    }
}

#endif
