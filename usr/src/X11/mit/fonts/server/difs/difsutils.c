/* $XConsortium: difsutils.c,v 1.6 91/07/18 22:35:28 keith Exp $ */
/*
 * misc utility routines
 */
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
 * $NCDId: @(#)difsutils.c,v 4.6 1991/07/02 16:58:06 lemke Exp $
 *
 */

#define	XK_LATIN1
#include	<stdio.h>
#include	<ctype.h>
#include	"misc.h"
#include	"globals.h"
#include	"clientstr.h"
#include	"accstr.h"
#include	"fontstruct.h"
#include	<X11/keysymdef.h>

extern ClientPtr currentClient;
static FontResolutionPtr default_resolutions;
static int  num_resolutions;
static int  default_point_size;

AuthContextPtr
GetClientAuthorization()
{
    return currentClient->auth;
}

void
SetDefaultPointSize(ps)
    int         ps;
{
    int         i;

    default_point_size = ps;
    for (i = 0; i < num_resolutions; i++)
	default_resolutions[i].point_size = ps;
}

int
SetDefaultResolutions(str)
    char       *str;
{
    int         num,
                numr = 0,
                n;
    char       *s;
    FontResolutionPtr new,
                nr;
    int         state;

    s = str;
    while (*s) {		/* count commas */
	if (*s == ',')
	    numr++;
	s++;
    }

    if ((numr % 2) != 1) {	/* must be multiple of 2 + 1 */
	return FSBadResolution;
    }
    numr = (numr + 1) / 2;
    nr = new = (FontResolutionPtr) fsalloc(sizeof(FontResolutionRec)
					   * numr);
    if (!new)
	return FSBadAlloc;
    s = str;
    num = 0;
    state = 0;
    while (*s) {
	if (*s == ',') {
	    if (state == 0) {
		nr->x_resolution = num;
		state++;
	    } else {
		state = 0;
		nr->y_resolution = num;
		nr->point_size = default_point_size;
		nr++;
	    }
	    num = 0;
	    s++;
	    continue;
	}
	if (!isdigit(*s)) {
	    fsfree((char *) new);
	    return FSBadResolution;
	}
	n = *s - '0';
	num = num * 10 + n;
	s++;
    }

    /* do the last one */
    assert(state == 1);
    nr->y_resolution = num;
    nr->point_size = default_point_size;

    if (default_resolutions) {
	fsfree((char *) default_resolutions);
    }
    default_resolutions = new;
    num_resolutions = numr;
    return FSSuccess;
}

FontResolutionPtr
GetClientResolutions(num)
    int        *num;
{
    /* return the client's if it has them, otherwise the default values */
    if (currentClient->num_resolutions) {
	*num = currentClient->num_resolutions;
	return (FontResolutionPtr) currentClient->resolutions;
    } else {
	*num = num_resolutions;
	return default_resolutions;
    }
}

int
GetDefaultPointSize()
{
    FontResolutionPtr res;
    int         num;

    res = GetClientResolutions(&num);
    if (res)
	return res->point_size;
    else
	return 120;
}

void
CopyISOLatin1Lowered(dest, source, length)
    register unsigned char *dest,
               *source;
    int         length;
{
    register int i;

    for (i = 0; i < length; i++, source++, dest++) {
	if ((*source >= XK_A) && (*source <= XK_Z))
	    *dest = *source + (XK_a - XK_A);
	else if ((*source >= XK_Agrave) && (*source <= XK_Odiaeresis))
	    *dest = *source + (XK_agrave - XK_Agrave);
	else if ((*source >= XK_Ooblique) && (*source <= XK_Thorn))
	    *dest = *source + (XK_oslash - XK_Ooblique);
	else
	    *dest = *source;
    }
    *dest = '\0';
}

int
strncmpnocase(first, second, n)
    char       *first,
               *second;
    int         n;
{
    register unsigned char *ap,
               *bp;

    for (ap = (unsigned char *) first,
	    bp = (unsigned char *) second;
    /* SUPPRESS 112 */
	    n > 0 && *ap && *bp; n--, ap++, bp++) {
	register unsigned char a,
	            b;

	/* SUPPRESS 112 */
	if ((a = *ap) != (b = *bp)) {
	    /* try lowercasing and try again */

	    if ((a >= XK_A) && (a <= XK_Z))
		a += (XK_a - XK_A);
	    else if ((a >= XK_Agrave) && (a <= XK_Odiaeresis))
		a += (XK_agrave - XK_Agrave);
	    else if ((a >= XK_Ooblique) && (a <= XK_Thorn))
		a += (XK_oslash - XK_Ooblique);

	    if ((b >= XK_A) && (b <= XK_Z))
		b += (XK_a - XK_A);
	    else if ((b >= XK_Agrave) && (b <= XK_Odiaeresis))
		b += (XK_agrave - XK_Agrave);
	    else if ((b >= XK_Ooblique) && (b <= XK_Thorn))
		b += (XK_oslash - XK_Ooblique);

	    if (a != b)
		break;
	}
    }
    /* SUPPRESS 112 */
    return (n ? (((int) *ap) - ((int) *bp)) : 0);
}

void
NoopDDA()
{
}

/* host list manipulation */
int
AddHost(list, addr)
    HostList   *list;
    HostAddress *addr;
{
    HostAddress *new;

    new = (HostAddress *) fsalloc(sizeof(HostAddress));
    if (!new)
	return FSBadAlloc;
    new->address = (pointer) fsalloc(addr->addr_len);
    if (!new->address) {
	fsfree((char *) addr);
	return FSBadAlloc;
    }
    new->type = addr->type;
    new->addr_len = addr->addr_len;
    bcopy((char *) addr->address, (char *) new->address, new->addr_len);

    new->next = *list;
    *list = new;
    return FSSuccess;
}

int
RemoveHost(list, addr)
    HostList   *list;
    HostAddress *addr;
{
    HostAddress *t,
               *last;

    last = (HostAddress *) 0;
    t = *list;
    while (t) {
	if (t->type == addr->type &&
		t->addr_len == addr->addr_len &&
		bcmp((char *) t->address, (char *) addr->address,
		     min(t->addr_len, addr->addr_len)) == 0) {
	    if (last) {
		last->next = t->next;
	    } else {
		*list = t->next;
	    }
	    fsfree((char *) t->address);
	    fsfree((char *) t);
	    return FSSuccess;
	}
	last = t;
	t = t->next;
    }
    return FSBadName;		/* bad host name */
}

Bool
ValidHost(list, addr)
    HostList    list;
    HostAddress *addr;
{
    HostAddress *t;

    t = list;
    while (t) {
	if (t->type == addr->type &&
		t->addr_len == addr->addr_len &&
		bcmp((char *) t->address, (char *) addr->address,
		     min(t->addr_len, addr->addr_len)) == 0) {
	    return TRUE;
	}
    }
    return FALSE;
}

/* block & wakeup handlers */

typedef struct _BlockHandler {
    void        (*BlockHandler) ();
    void        (*WakeupHandler) ();
    pointer     blockData;
    Bool        deleted;
}           BlockHandlerRec, *BlockHandlerPtr;

static BlockHandlerPtr handlers;
static int  numHandlers;
static int  sizeHandlers;
static Bool inHandler;
static Bool handlerDeleted;

/* called from the OS layer */
BlockHandler(pTimeout, pReadmask)
    pointer     pTimeout;	/* DIX doesn't want to know how OS represents
				 * time */
    pointer     pReadmask;	/* nor how it represents the set of
				 * descriptors */
{
    register int i,
                j;

    ++inHandler;
    for (i = 0; i < numHandlers; i++)
	(*handlers[i].BlockHandler) (handlers[i].blockData,
				     pTimeout, pReadmask);
    if (handlerDeleted) {
	for (i = 0; i < numHandlers;)
	    if (handlers[i].deleted) {
		for (j = i; j < numHandlers - 1; j++)
		    handlers[j] = handlers[j + 1];
		numHandlers--;
	    } else
		i++;
    }
    --inHandler;
}


WakeupHandler(result, pReadmask)
    unsigned long result;	/* 32 bits of undefined result from the wait */
    pointer     pReadmask;	/* the resulting descriptor mask */
{
    register int i,
                j;

    ++inHandler;
    for (i = numHandlers - 1; i >= 0; i--)
	(*handlers[i].WakeupHandler) (handlers[i].blockData,
				      result, pReadmask);
    if (handlerDeleted) {
	for (i = 0; i < numHandlers;)
	    if (handlers[i].deleted) {
		for (j = i; j < numHandlers - 1; j++)
		    handlers[j] = handlers[j + 1];
		numHandlers--;
	    } else
		i++;
    }
    --inHandler;
}

/* Reentrant with BlockHandler and WakeupHandler, except wakeup won't
 * get called until next time
 */

Bool
RegisterBlockAndWakeupHandlers(blockHandler, wakeupHandler, blockData)
    void        (*blockHandler) ();
    void        (*wakeupHandler) ();
    pointer     blockData;
{
    BlockHandlerPtr new;

    if (numHandlers >= sizeHandlers) {
	new = (BlockHandlerPtr) fsrealloc(handlers, (numHandlers + 1) *
					  sizeof(BlockHandlerRec));
	if (!new)
	    return FALSE;
	handlers = new;
	sizeHandlers = numHandlers + 1;
    }
    handlers[numHandlers].BlockHandler = blockHandler;
    handlers[numHandlers].WakeupHandler = wakeupHandler;
    handlers[numHandlers].blockData = blockData;
    numHandlers = numHandlers + 1;
    return TRUE;
}

void
RemoveBlockAndWakeupHandlers(blockHandler, wakeupHandler, blockData)
    void        (*blockHandler) ();
    void        (*wakeupHandler) ();
    pointer     blockData;
{
    int         i;

    for (i = 0; i < numHandlers; i++)
	if (handlers[i].BlockHandler == blockHandler &&
		handlers[i].WakeupHandler == wakeupHandler &&
		handlers[i].blockData == blockData) {
	    if (inHandler) {
		handlerDeleted = TRUE;
		handlers[i].deleted = TRUE;
	    } else {
		for (; i < numHandlers - 1; i++)
		    handlers[i] = handlers[i + 1];
		numHandlers--;
	    }
	    break;
	}
}

InitBlockAndWakeupHandlers()
{
    fsfree(handlers);
    handlers = (BlockHandlerPtr) 0;
    numHandlers = 0;
    sizeHandlers = 0;
}

/*
 * A general work queue.  Perform some task before the server
 * sleeps for input.
 */

WorkQueuePtr workQueue;
static WorkQueuePtr *workQueueLast = &workQueue;

/* ARGSUSED */
void
ProcessWorkQueue()
{
    WorkQueuePtr q,
                n,
                p;

    p = NULL;
    /*
     * Scan the work queue once, calling each function.  Those which return
     * TRUE are removed from the queue, otherwise they will be called again.
     * This must be reentrant with QueueWorkProc, hence the crufty usage of
     * variables.
     */
    for (q = workQueue; q; q = n) {
	if ((*q->function) (q->client, q->closure)) {
	    /* remove q from the list */
	    n = q->next;	/* don't fetch until after func called */
	    if (p)
		p->next = n;
	    else
		workQueue = n;
	    fsfree(q);
	} else {
	    n = q->next;	/* don't fetch until after func called */
	    p = q;
	}
    }
    if (p)
	workQueueLast = &p->next;
    else {
	workQueueLast = &workQueue;
    }
}

Bool
QueueWorkProc(function, client, data)
    Bool        (*function) ();
    ClientPtr   client;
    pointer     data;
{
    WorkQueuePtr q;

    q = (WorkQueuePtr) fsalloc(sizeof *q);
    if (!q)
	return FALSE;
    q->function = function;
    q->client = client;
    q->closure = data;
    q->next = NULL;
    *workQueueLast = q;
    workQueueLast = &q->next;
    return TRUE;
}

/*
 * Manage a queue of sleeping clients, awakening them
 * when requested, by using the OS functions IgnoreClient
 * and AttendClient.  Note that this *ignores* the troubles
 * with request data interleaving itself with events, but
 * we'll leave that until a later time.
 */

typedef struct _SleepQueue {
    struct _SleepQueue *next;
    ClientPtr   client;
    Bool        (*function) ();
    pointer     closure;
}           SleepQueueRec, *SleepQueuePtr;

static SleepQueuePtr sleepQueue = NULL;

Bool
ClientSleep(client, function, data)
    ClientPtr   client;
    Bool        (*function) ();
    pointer     data;
{
    SleepQueuePtr q;

    q = (SleepQueuePtr) fsalloc(sizeof *q);
    if (!q)
	return FALSE;

    IgnoreClient(client);
    q->next = sleepQueue;
    q->client = client;
    q->function = function;
    q->closure = data;
    sleepQueue = q;
    return TRUE;
}

Bool
ClientSignal(client)
    ClientPtr   client;
{
    SleepQueuePtr q;

    for (q = sleepQueue; q; q = q->next)
	if (q->client == client) {
	    return QueueWorkProc(q->function, q->client, q->closure);
	}
    return FALSE;
}

ClientWakeup(client)
    ClientPtr   client;
{
    SleepQueuePtr q,
               *prev;

    prev = &sleepQueue;
    while ((q = *prev) != (SleepQueuePtr) 0) {
	if (q->client == client) {
	    *prev = q->next;
	    fsfree(q);
	    if (client->clientGone != CLIENT_GONE)
		AttendClient(client);
	    break;
	}
	prev = &q->next;
    }
}

Bool
ClientIsAsleep(client)
    ClientPtr   client;
{
    SleepQueuePtr q;

    for (q = sleepQueue; q; q = q->next)
	if (q->client == client)
	    return TRUE;
    return FALSE;
}


unsigned char _reverse_byte[0x100] = {
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
    0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
    0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
    0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
    0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
    0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
    0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
    0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
    0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
    0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
    0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
    0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
    0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
    0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
    0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
    0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
    0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
    0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
    0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
    0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
    0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
    0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

void
BitOrderInvert(buf, nbytes)
    pointer     buf;
    long        nbytes;
{
    unsigned char *rev = _reverse_byte;

    while (--nbytes >= 0) {
	*buf = rev[*buf];
	buf++;
    }
}

void
TwoByteSwap(buf, nbytes)
    pointer     buf;
    long        nbytes;
{
    unsigned char c;

    while (nbytes > 0) {
	c = *buf;
	*buf = *(buf + 1);
	*(buf + 1) = c;
	nbytes -= 2;
	buf += 2;
    }
}

void
FourByteSwap(buf, nbytes)
    pointer     buf;
    long        nbytes;
{
    unsigned char c;

    while (nbytes > 0) {
	c = *buf;
	*buf = *(buf + 3);
	*(buf + 3) = c;
	c = *(buf + 1);
	*(buf + 1) = *(buf + 2);
	*(buf + 2) = c;
	nbytes -= 4;
	buf += 4;
    }
}

/*
 *	Repad a bitmap
 */

int
RepadBitmap(pSrc, pDst, srcPad, dstPad, width, height)
    char       *pSrc,
               *pDst;
    unsigned    srcPad,
                dstPad;
    int         width,
                height;
{
    int         srcWidthBytes,
                dstWidthBytes;
    int         row,
                col;
    char       *pTmpSrc,
               *pTmpDst;

    switch (srcPad) {
    case 1:
	srcWidthBytes = (width + 7) >> 3;
	break;
    case 2:
	srcWidthBytes = ((width + 15) >> 4) << 1;
	break;
    case 4:
	srcWidthBytes = ((width + 31) >> 5) << 2;
	break;
    case 8:
	srcWidthBytes = ((width + 63) >> 6) << 3;
	break;
    default:
	return 0;
    }
    switch (dstPad) {
    case 1:
	dstWidthBytes = (width + 7) >> 3;
	break;
    case 2:
	dstWidthBytes = ((width + 15) >> 4) << 1;
	break;
    case 4:
	dstWidthBytes = ((width + 31) >> 5) << 2;
	break;
    case 8:
	dstWidthBytes = ((width + 63) >> 6) << 3;
	break;
    default:
	return 0;
    }

    width = srcWidthBytes;
    if (width > dstWidthBytes)
	width = dstWidthBytes;
    pTmpSrc = pSrc;
    pTmpDst = pDst;
    for (row = 0; row < height; row++) {
	for (col = 0; col < width; col++)
	    *pTmpDst++ = *pTmpSrc++;
	while (col < dstWidthBytes) {
	    *pTmpDst++ = '\0';
	    col++;
	}
	pTmpSrc += srcWidthBytes - width;
    }
    return dstWidthBytes * height;
}

unsigned long *
Xalloc(m)
    unsigned long m;
{
    return fsalloc(m);
}

unsigned long *
Xrealloc(n, m)
    unsigned long *n,
                m;
{
    return fsrealloc(n, m);
}

void
Xfree(n)
    unsigned long *n;
{
    fsfree(n);
}
