/* $XConsortium: swaprep.c,v 1.7 92/05/28 16:43:00 gildea Exp $ */
/*
 * font server reply swapping
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

#include	"FSproto.h"
#include	"clientstr.h"
#include	"misc.h"

static void SwapConnSetupAccept();

void
Swap32Write(client, size, pbuf)
    ClientPtr   client;
    int         size;
    long       *pbuf;
{
    int         n,
                i;

    size >>= 2;
    for (i = 0; i < size; i++) {
	swapl(&pbuf[i], n);
    }
    (void) WriteToClient(client, size << 2, (char *) pbuf);
}

void
Swap16Write(client, size, pbuf)
    ClientPtr   client;
    int         size;
    short      *pbuf;
{
    int         n,
                i;

    size >>= 1;
    for (i = 0; i < size; i++) {
	swaps(&pbuf[i], n);
    }
    (void) WriteToClient(client, size << 1, (char *) pbuf);
}

CopySwap32Write(client, size, pbuf)
    ClientPtr   client;
    int         size;
    long       *pbuf;
{
    int         bufsize = size;
    long       *pbufT;
    long       *from,
               *to,
               *fromLast,
               *toLast;
    long        tmpbuf[1];

    while (!(pbufT = (long *) ALLOCATE_LOCAL(bufsize))) {
	bufsize >>= 1;
	if (bufsize == 4) {
	    pbufT = tmpbuf;
	    break;
	}
    }
    /* convert lengths from # of bytes to # of longs */
    size >>= 2;
    bufsize >>= 2;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int         nbytes;

	to = pbufT;
	toLast = to + min(bufsize, fromLast - from);
	nbytes = (toLast - to) << 2;
	while (to < toLast) {
	    /*
	     * can't write "cpswapl(*from++, *to++)" because cpswapl is a
	     * macro that evaulates its args more than once
	     */
	    cpswapl(*from, *to);
	    from++;
	    to++;
	}
	(void) WriteToClient(client, nbytes, (char *) pbufT);
    }

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL((char *) pbufT);
}

void
CopySwap16Write(client, size, pbuf)
    ClientPtr   client;
    int         size;
    short      *pbuf;
{
    int         bufsize = size;
    short      *pbufT;
    register short *from,
               *to,
               *fromLast,
               *toLast;
    short       tmpbuf[2];

    /* Allocate as big a buffer as we can... */
    while (!(pbufT = (short *) ALLOCATE_LOCAL(bufsize))) {
	bufsize >>= 1;
	if (bufsize == 4) {
	    pbufT = tmpbuf;
	    break;
	}
    }

    /* convert lengths from # of bytes to # of shorts */
    size >>= 1;
    bufsize >>= 1;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int         nbytes;

	to = pbufT;
	toLast = to + min(bufsize, fromLast - from);
	nbytes = (toLast - to) << 1;
	while (to < toLast) {
	    /*
	     * can't write "cpswaps(*from++, *to++)" because cpswaps is a
	     * macro that evaulates its args more than once
	     */
	    cpswaps(*from, *to);
	    from++;
	    to++;
	}
	(void) WriteToClient(client, nbytes, (char *) pbufT);
    }

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL((char *) pbufT);
}

void
SGenericReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsGenericReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SListExtensionsReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsListExtensionsReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SQueryExtensionReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsQueryExtensionReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swaps(&pRep->major_version, n);
    swaps(&pRep->minor_version, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SListCataloguesReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsListCataloguesReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->num_replies, n);
    swapl(&pRep->num_catalogues, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SCreateACReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsCreateACReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swaps(&pRep->status, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SGetEventMaskReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsGetEventMaskReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->event_mask, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SGetResolutionReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsGetResolutionReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SListFontsReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsListFontsReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->following, n);
    swapl(&pRep->nFonts, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SListFontsWithXInfoReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsListFontsWithXInfoReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->nReplies, n);
    SwapFontHeader(&pRep->header);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SOpenBitmapFontReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsOpenBitmapFontReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->otherid, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SQueryXInfoReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsQueryXInfoReply *pRep;
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    SwapFontHeader(&pRep->header);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SQueryXExtentsReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsQueryXExtents8Reply *pRep; /* QueryXExtents16Reply is the same */
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->num_extents, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SQueryXBitmapsReply(client, size, pRep)
    ClientPtr   client;
    int         size;
    fsQueryXBitmaps8Reply *pRep; /* QueryXBitmaps16Reply is the same */
{
    int         n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->replies_hint, n);
    swapl(&pRep->num_chars, n);
    swapl(&pRep->nbytes, n);
    (void) WriteToClient(client, size, (char *) pRep);
}

void
SErrorEvent(error, perror)
    fsError    *error,
               *perror;
{
    int         n;

    *perror = *error;
    swaps(&perror->sequenceNumber, n);
    swapl(&perror->length, n);
    swapl(&perror->timestamp, n);
}

void
WriteSConnectionInfo(client, size, pInfo)
    ClientPtr   client;
    unsigned long size;
    char       *pInfo;
{
    char       *pInfoT,
               *pInfoTBase;
    fsConnSetupAccept *pConnSetup = (fsConnSetupAccept *) pInfo;
    int         i;

    pInfoT = pInfoTBase = (char *) ALLOCATE_LOCAL(size);
    if (!pInfoT) {
	client->noClientException = -2;
	return;
    }
    SwapConnSetupAccept(pConnSetup, (fsConnSetupAccept *) pInfoT);
    pInfoT += sizeof(fsConnSetup);
    pInfo += sizeof(fsConnSetup);

    i = (pConnSetup->vendor_len + 3) & ~3;
    bcopy(pInfo, pInfoT, i);

    (void) WriteToClient(client, (int) size, (char *) pInfoTBase);
    DEALLOCATE_LOCAL(pInfoTBase);
}

static void
SwapConnSetupAccept(pConnSetup, pConnSetupT)
    fsConnSetupAccept *pConnSetup,
               *pConnSetupT;
{
    cpswapl(pConnSetup->length, pConnSetupT->length);
    cpswaps(pConnSetup->max_request_len, pConnSetupT->max_request_len);
    cpswaps(pConnSetup->vendor_len, pConnSetupT->vendor_len);
    cpswapl(pConnSetup->release_number, pConnSetupT->release_number);
}

void
WriteSConnSetup(client, pcsp)
    ClientPtr   client;
    fsConnSetup *pcsp;
{
    fsConnSetup cspT;

    cpswaps(pcsp->status, cspT.status);
    cpswaps(pcsp->major_version, cspT.major_version);
    cpswaps(pcsp->minor_version, cspT.minor_version);
    cspT.num_alternates = pcsp->num_alternates;
    cspT.auth_index = pcsp->auth_index;
    cpswaps(pcsp->alternate_len, cspT.alternate_len);
    cpswaps(pcsp->auth_len, cspT.auth_len);
    (void) WriteToClient(client, sizeof(cspT), (char *) &cspT);
}

void
SwapCharInfo(ci)
    fsCharInfo *ci;
{
    SwapShorts((short *) ci, sizeof(fsCharInfo) / sizeof(CARD16));
}

void
SwapFontHeader(hdr)
    fsFontHeader *hdr;
{
    int         n;

    SwapCharInfo(&hdr->min_bounds);
    SwapCharInfo(&hdr->max_bounds);
    swaps(&hdr->font_ascent, n);
    swaps(&hdr->font_descent, n);
    swapl(&hdr->flags, n);
}

void
SwapPropOffset(po)
    fsPropOffset *po;
{
    int         n;

    swapl(&po->name.position, n);
    swapl(&po->name.length, n);
    swapl(&po->value.position, n);
    swapl(&po->value.length, n);
}

void
SwapPropInfo(pi)
    fsPropInfo *pi;
{
    int         i,
                n;
    fsPropOffset *po;

    po = (fsPropOffset *) ((pointer) pi + sizeof(fsPropInfo));
    for (i = 0; i < pi->num_offsets; i++, po++)
	SwapPropOffset(po);

    swapl(&pi->num_offsets, n);
    swapl(&pi->data_len, n);
}

void
SwapExtents(extents, num)
    fsCharInfo *extents;
    int         num;
{
    int         i;

    for (i = 0; i < num; i++) {
	SwapCharInfo(&extents[i]);
    }
}
