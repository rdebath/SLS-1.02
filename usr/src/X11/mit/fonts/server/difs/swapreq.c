/* $XConsortium: swapreq.c,v 1.5 92/05/28 16:43:04 gildea Exp $ */
/*
 * swapped requests
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

#include	"misc.h"
#include	"FSproto.h"
#include	"clientstr.h"
#include	"globals.h"

extern int  (*ProcVector[NUM_PROC_VECTORS]) ();

void
SwapLongs(list, count)
    long       *list;
    unsigned long count;
{
    int         n;

    while (count >= 8) {
	swapl(list + 0, n);
	swapl(list + 1, n);
	swapl(list + 2, n);
	swapl(list + 3, n);
	swapl(list + 4, n);
	swapl(list + 5, n);
	swapl(list + 6, n);
	swapl(list + 7, n);
	list += 8;
	count -= 8;
    }
    if (count != 0) {
	do {
	    swapl(list, n);
	    list++;
	} while (--count != 0);
    }
}

/* Byte swap a list of shorts */

void
SwapShorts(list, count)
    register short *list;
    register unsigned long count;
{
    register int n;

    while (count >= 16) {
	swaps(list + 0, n);
	swaps(list + 1, n);
	swaps(list + 2, n);
	swaps(list + 3, n);
	swaps(list + 4, n);
	swaps(list + 5, n);
	swaps(list + 6, n);
	swaps(list + 7, n);
	swaps(list + 8, n);
	swaps(list + 9, n);
	swaps(list + 10, n);
	swaps(list + 11, n);
	swaps(list + 12, n);
	swaps(list + 13, n);
	swaps(list + 14, n);
	swaps(list + 15, n);
	list += 16;
	count -= 16;
    }
    if (count != 0) {
	do {
	    swaps(list, n);
	    list++;
	} while (--count != 0);
    }
}

/*
 * used for all requests that have nothing but 'length' swapped
 */
int
SProcSimpleRequest(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsReq);
    swaps(&stuff->length, n);
    return ((*ProcVector[stuff->reqType]) (client));
}

/*
 * used for all requests that have nothing but 'length' & a resource id swapped
 */
int
SProcResourceRequest(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsResourceReq);
    swaps(&stuff->length, n);
    swapl(&stuff->id, n);
    return ((*ProcVector[stuff->reqType]) (client));
}

static void
swap_auth(data, num)
    pointer     data;
    int         num;
{
    pointer     p;
    unsigned char t;
    CARD16      namelen,
                datalen;
    int         i;

    p = data;
    for (i = 0; i < num; i++) {
	namelen = *(CARD16 *) p;
	t = p[0];
	p[0] = p[1];
	p[1] = t;
	p += 2;
	datalen = *(CARD16 *) p;
	t = p[0];
	p[0] = p[1];
	p[1] = t;
	p += 2;
	p += (namelen + 3) & ~3;
	p += (datalen + 3) & ~3;
    }
}

int
SProcCreateAC(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsCreateACReq);
    swaps(&stuff->length, n);
    swapl(&stuff->acid, n);
    swap_auth((pointer) &stuff[1], stuff->num_auths);
    return ((*ProcVector[stuff->reqType]) (client));
}

int
SProcSetResolution(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsSetResolutionReq);
    swaps(&stuff->length, n);
    swaps(&stuff->num_resolutions, n);
    SwapShorts((short *) &stuff[1], stuff->num_resolutions);

    return ((*ProcVector[stuff->reqType]) (client));
}


int
SProcQueryExtension(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsQueryExtensionReq);
    swaps(&stuff->length, n);
    swaps(&stuff->nbytes, n);
    return ((*ProcVector[FS_QueryExtension]) (client));
}

int
SProcListCatalogues(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsListCataloguesReq);
    swaps(&stuff->length, n);
    swapl(&stuff->maxNames, n);
    swaps(&stuff->nbytes, n);
    return ((*ProcVector[FS_ListCatalogues]) (client));
}

int
SProcListFonts(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsListFontsReq);
    swaps(&stuff->length, n);
    swapl(&stuff->maxNames, n);
    swaps(&stuff->nbytes, n);
    return ((*ProcVector[FS_ListFonts]) (client));
}

int
SProcListFontsWithXInfo(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsListFontsWithXInfoReq);
    swaps(&stuff->length, n);
    swapl(&stuff->maxNames, n);
    swaps(&stuff->nbytes, n);
    return ((*ProcVector[FS_ListFontsWithXInfo]) (client));
}

int
SProcOpenBitmapFont(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsOpenBitmapFontReq);
    swaps(&stuff->length, n);
    swapl(&stuff->fid, n);
    swapl(&stuff->format_hint, n);
    swapl(&stuff->format_mask, n);
    return ((*ProcVector[FS_OpenBitmapFont]) (client));
}

int
SProcQueryXExtents(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsQueryXExtents8Req); /* 8 and 16 are the same here */
    swaps(&stuff->length, n);
    swapl(&stuff->fid, n);
    swapl(&stuff->num_ranges, n);

    return ((*ProcVector[stuff->reqType]) (client));
}

int
SProcQueryXBitmaps(client)
    ClientPtr   client;
{
    char        n;

    REQUEST(fsQueryXBitmaps8Req); /* 8 and 16 are the same here */
    swaps(&stuff->length, n);
    swapl(&stuff->fid, n);
    swapl(&stuff->format, n);
    swapl(&stuff->num_ranges, n);

    return ((*ProcVector[stuff->reqType]) (client));
}

SwapConnClientPrefix(pCCP)
    fsConnClientPrefix *pCCP;
{
    char        n;

    swaps(&pCCP->major_version, n);
    swaps(&pCCP->minor_version, n);
    swaps(&pCCP->auth_len, n);
    swap_auth((pointer) &pCCP[1], pCCP->num_auths);
}
