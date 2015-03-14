/* $Header: /home/x_cvs/mit/server/os/io.c,v 1.4 1992/05/23 06:12:23 dawes Exp $ */
/***********************************************************
Copyright 1987, 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
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
/* $XConsortium: io.c,v 1.68 91/03/29 09:34:07 rws Exp $ */
/*****************************************************************
 * i/o functions
 *
 *   WriteToClient, ReadRequestFromClient
 *   InsertFakeRequest, ResetCurrentRequest
 *
 *****************************************************************/

#include <stdio.h>
#include "Xos.h"
#include "Xmd.h"
#include <errno.h>
#include <sys/param.h>
#include <sys/uio.h>
#include "X.h"
#include "Xproto.h"
#include "os.h"
#include "osdep.h"
#include "opaque.h"
#include "dixstruct.h"
#include "misc.h"

/* check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN
 */
#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define ETEST(err) (err == EAGAIN || err == EWOULDBLOCK)
#else
#ifdef EAGAIN
#define ETEST(err) (err == EAGAIN)
#else
#define ETEST(err) (err == EWOULDBLOCK)
#endif
#endif

extern void MarkClientException();
extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long OutputPending[];
extern int ConnectionTranslation[];
extern Bool NewOutputPending;
extern Bool AnyClientsWriteBlocked;
static Bool CriticalOutputPending;
static int timesThisConnection = 0;
static ConnectionInputPtr FreeInputs = (ConnectionInputPtr)NULL;
static ConnectionOutputPtr FreeOutputs = (ConnectionOutputPtr)NULL;
static OsCommPtr AvailableInput = (OsCommPtr)NULL;

static ConnectionInputPtr AllocateInputBuffer();
static ConnectionOutputPtr AllocateOutputBuffer();

extern int errno;

#define request_length(req, cli) ((cli->swapped ? \
	lswaps((req)->length) : (req)->length) << 2)
#define MAX_TIMES_PER         10

/*****************************************************************
 * ReadRequestFromClient
 *    Returns one request in client->requestBuffer.  Return status is:
 *
 *    > 0  if  successful, specifies length in bytes of the request
 *    = 0  if  entire request is not yet available
 *    < 0  if  client should be terminated
 *
 *    The request returned must be contiguous so that it can be
 *    cast in the dispatcher to the correct request type.  Because requests
 *    are variable length, ReadRequestFromClient() must look at the first 4
 *    bytes of a request to determine the length (the request length is
 *    always the 3rd and 4th bytes of the request).  
 *
 *    Note: in order to make the server scheduler (WaitForSomething())
 *    "fair", the ClientsWithInput mask is used.  This mask tells which
 *    clients have FULL requests left in their buffers.  Clients with
 *    partial requests require a read.  Basically, client buffers
 *    are drained before select() is called again.  But, we can't keep
 *    reading from a client that is sending buckets of data (or has
 *    a partial request) because others clients need to be scheduled.
 *****************************************************************/

#define YieldControl()				\
        { isItTimeToYield = TRUE;		\
	  timesThisConnection = 0; }
#define YieldControlNoInput()			\
        { YieldControl();			\
	  BITCLEAR(ClientsWithInput, fd); }
#define YieldControlDeath()			\
        { timesThisConnection = 0; }

int
ReadRequestFromClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register int gotnow, needed;
    int result;
    register xReq *request;

    if (AvailableInput)
    {
	if (AvailableInput != oc)
	{
	    register ConnectionInputPtr aci = AvailableInput->input;
	    if (aci->size > BUFWATERMARK)
	    {
		xfree(aci->buffer);
		xfree(aci);
	    }
	    else
	    {
		aci->next = FreeInputs;
		FreeInputs = aci;
	    }
	    AvailableInput->input = (ConnectionInputPtr)NULL;
	}
	AvailableInput = (OsCommPtr)NULL;
    }
    if (!oci)
    {
	if (oci = FreeInputs)
	{
	    FreeInputs = oci->next;
	}
	else if (!(oci = AllocateInputBuffer()))
	{
	    YieldControlDeath();
	    return -1;
	}
	oc->input = oci;
    }
    oci->bufptr += oci->lenLastReq;

    request = (xReq *)oci->bufptr;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if ((gotnow < sizeof(xReq)) ||
	(gotnow < (needed = request_length(request, client))))
    {
	oci->lenLastReq = 0;
	if ((gotnow < sizeof(xReq)) || (needed == 0))
	   needed = sizeof(xReq);
	else if (needed > MAXBUFSIZE)
	{
	    YieldControlDeath();
	    return -1;
	}
	if ((gotnow == 0) ||
	    ((oci->bufptr - oci->buffer + needed) > oci->size))
	{
	    if ((gotnow > 0) && (oci->bufptr != oci->buffer))
		bcopy(oci->bufptr, oci->buffer, gotnow);
	    if (needed > oci->size)
	    {
		char *ibuf;

		ibuf = (char *)xrealloc(oci->buffer, needed);
		if (!ibuf)
		{
		    YieldControlDeath();
		    return -1;
		}
		oci->size = needed;
		oci->buffer = ibuf;
	    }
	    oci->bufptr = oci->buffer;
	    oci->bufcnt = gotnow;
	}
	result = read(fd, oci->buffer + oci->bufcnt, 
		      oci->size - oci->bufcnt); 
	if (result <= 0)
	{
#if !defined(SYSV386) || !defined(SVR4)
	    if ((result < 0) && ETEST(errno))
	    {
		YieldControlNoInput();
		return 0;
	    }
#endif
	    YieldControlDeath();
	    return -1;
	}
	oci->bufcnt += result;
	gotnow += result;
	/* free up some space after huge requests */
	if ((oci->size > BUFWATERMARK) &&
	    (oci->bufcnt < BUFSIZE) && (needed < BUFSIZE))
	{
	    char *ibuf;

	    ibuf = (char *)xrealloc(oci->buffer, BUFSIZE);
	    if (ibuf)
	    {
		oci->size = BUFSIZE;
		oci->buffer = ibuf;
		oci->bufptr = ibuf + oci->bufcnt - gotnow;
	    }
	}
	request = (xReq *)oci->bufptr;
	if ((gotnow < sizeof(xReq)) ||
	    (gotnow < (needed = request_length(request, client))))
	{
	    YieldControlNoInput();
	    return 0;
	}
    }
    if (needed == 0)
	needed = sizeof(xReq);
    oci->lenLastReq = needed;

    /*
     *  Check to see if client has at least one whole request in the
     *  buffer.  If there is only a partial request, treat like buffer
     *  is empty so that select() will be called again and other clients
     *  can get into the queue.   
     */

    if (gotnow >= needed + sizeof(xReq)) 
    {
	request = (xReq *)(oci->bufptr + needed);
        if (gotnow >= needed + request_length(request, client))
	    BITSET(ClientsWithInput, fd);
        else
	    YieldControlNoInput();
    }
    else
    {
	if (gotnow == needed)
	    AvailableInput = oc;
	YieldControlNoInput();
    }
    if (++timesThisConnection >= MAX_TIMES_PER)
	YieldControl();

    client->requestBuffer = (pointer)oci->bufptr;
    return needed;
}

/*****************************************************************
 * InsertFakeRequest
 *    Splice a consed up (possibly partial) request in as the next request.
 *
 **********************/

Bool
InsertFakeRequest(client, data, count)
    ClientPtr client;
    char *data;
    int count;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register xReq *request;
    register int gotnow, moveup;

    if (AvailableInput)
    {
	if (AvailableInput != oc)
	{
	    register ConnectionInputPtr aci = AvailableInput->input;
	    if (aci->size > BUFWATERMARK)
	    {
		xfree(aci->buffer);
		xfree(aci);
	    }
	    else
	    {
		aci->next = FreeInputs;
		FreeInputs = aci;
	    }
	    AvailableInput->input = (ConnectionInputPtr)NULL;
	}
	AvailableInput = (OsCommPtr)NULL;
    }
    if (!oci)
    {
	if (oci = FreeInputs)
	    FreeInputs = oci->next;
	else if (!(oci = AllocateInputBuffer()))
	    return FALSE;
	oc->input = oci;
    }
    oci->bufptr += oci->lenLastReq;
    oci->lenLastReq = 0;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if ((gotnow + count) > oci->size)
    {
	char *ibuf;

	ibuf = (char *)xrealloc(oci->buffer, gotnow + count);
	if (!ibuf)
	    return(FALSE);
	oci->size = gotnow + count;
	oci->buffer = ibuf;
	oci->bufptr = ibuf + oci->bufcnt - gotnow;
    }
    moveup = count - (oci->bufptr - oci->buffer);
    if (moveup > 0)
    {
	if (gotnow > 0)
	    bcopy(oci->bufptr, oci->bufptr + moveup, gotnow);
	oci->bufptr += moveup;
	oci->bufcnt += moveup;
    }
    bcopy(data, oci->bufptr - count, count);
    oci->bufptr -= count;
    request = (xReq *)oci->bufptr;
    gotnow += count;
    if ((gotnow >= sizeof(xReq)) &&
	(gotnow >= request_length(request, client)))
	BITSET(ClientsWithInput, fd);
    else
	YieldControlNoInput();
    return(TRUE);
}

/*****************************************************************
 * ResetRequestFromClient
 *    Reset to reexecute the current request, and yield.
 *
 **********************/

ResetCurrentRequest(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register xReq *request;
    int gotnow;

    if (AvailableInput == oc)
	AvailableInput = (OsCommPtr)NULL;
    oci->lenLastReq = 0;
    request = (xReq *)oci->bufptr;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if ((gotnow >= sizeof(xReq)) &&
	(gotnow >= request_length(request, client)))
    {
	BITSET(ClientsWithInput, fd);
	YieldControl();
    }
    else
	YieldControlNoInput();
}

    /* lookup table for adding padding bytes to data that is read from
    	or written to the X socket.  */
static int padlength[4] = {0, 3, 2, 1};

 /********************
 * FlushClient()
 *    If the client isn't keeping up with us, then we try to continue
 *    buffering the data and set the apropriate bit in ClientsWritable
 *    (which is used by WaitFor in the select).  If the connection yields
 *    a permanent error, or we can't allocate any more space, we then
 *    close the connection.
 *
 **********************/

int
FlushClient(who, oc, extraBuf, extraCount)
    ClientPtr who;
    OsCommPtr oc;
    char *extraBuf;
    int extraCount; /* do not modify... returned below */
{
    register ConnectionOutputPtr oco = oc->output;
    int connection = oc->fd;
    struct iovec iov[3];
    char padBuffer[3];
    long written;
    long padsize;
    long notWritten;
    long todo;

    if (!oco)
	return 0;
    written = 0;
    padsize = padlength[extraCount & 3];
    notWritten = oco->count + extraCount + padsize;
    todo = notWritten;
    while (notWritten) {
	long before = written;	/* amount of whole thing written */
	long remain = todo;	/* amount to try this time, <= notWritten */
	int i = 0;
	long len;

	/* You could be very general here and have "in" and "out" iovecs
	 * and write a loop without using a macro, but what the heck.  This
	 * translates to:
	 *
	 *     how much of this piece is new?
	 *     if more new then we are trying this time, clamp
	 *     if nothing new
	 *         then bump down amount already written, for next piece
	 *         else put new stuff in iovec, will need all of next piece
	 *
	 * Note that todo had better be at least 1 or else we'll end up
	 * writing 0 iovecs.
	 */
#define InsertIOV(pointer, length) \
	len = (length) - before; \
	if (len > remain) \
	    len = remain; \
	if (len <= 0) { \
	    before = (-len); \
	} else { \
	    iov[i].iov_len = len; \
	    iov[i].iov_base = (pointer) + before; \
	    i++; \
	    remain -= len; \
	    before = 0; \
	}

	InsertIOV ((char *)oco->buf, oco->count)
	InsertIOV (extraBuf, extraCount)
	InsertIOV (padBuffer, padsize)

	errno = 0;
	if ((len = writev(connection, iov, i)) >= 0)
	{
	    written += len;
	    notWritten -= len;
	    todo = notWritten;
	}
	else if (ETEST(errno)
#ifdef SUNSYSV /* check for another brain-damaged OS bug */
		 || (errno == 0)
#endif
#ifdef EMSGSIZE /* check for another brain-damaged OS bug */
		 || ((errno == EMSGSIZE) && (todo == 1))
#endif
#ifdef SERVER_LOCALCONN /* STREAMS ... */
		 || ((errno == ERANGE) && (todo == 1))
#endif
		)
	{
	    /* If we've arrived here, then the client is stuffed to the gills
	       and not ready to accept more.  Make a note of it and buffer
	       the rest. */
	    BITSET(ClientsWriteBlocked, connection);
	    AnyClientsWriteBlocked = TRUE;

	    if (written < oco->count)
	    {
		if (written > 0)
		{
		    oco->count -= written;
		    bcopy((char *)oco->buf + written,
			  (char *)oco->buf,
			  oco->count);
		    written = 0;
		}
	    }
	    else
	    {
		written -= oco->count;
		oco->count = 0;
	    }

	    if (notWritten > oco->size)
	    {
		unsigned char *obuf;

		obuf = (unsigned char *)xrealloc(oco->buf,
						 notWritten + BUFSIZE);
		if (!obuf)
		{
		    close(connection);
		    MarkClientException(who);
		    oco->count = 0;
		    return(-1);
		}
		oco->size = notWritten + BUFSIZE;
		oco->buf = obuf;
	    }

	    /* If the amount written extended into the padBuffer, then the
	       difference "extraCount - written" may be less than 0 */
	    if ((len = extraCount - written) > 0)
		bcopy (extraBuf + written,
		       (char *)oco->buf + oco->count,
		       len);

	    oco->count = notWritten; /* this will include the pad */
	    /* return only the amount explicitly requested */
	    return extraCount;
	}
#ifdef EMSGSIZE /* check for another brain-damaged OS bug */
	else if (errno == EMSGSIZE)
	{
	    todo >>= 1;
	}
#endif
#ifdef SERVER_LOCALCONN /* STREAMS ... */
	else if (errno == ERANGE)
	{
	    todo >>= 1;
	}
#endif
	else
	{
	    close(connection);
	    MarkClientException(who);
	    oco->count = 0;
	    return(-1);
	}
    }

    /* everything was flushed out */
    oco->count = 0;
    /* check to see if this client was write blocked */
    if (AnyClientsWriteBlocked)
    {
	BITCLEAR(ClientsWriteBlocked, oc->fd);
 	if (! ANYSET(ClientsWriteBlocked))
	    AnyClientsWriteBlocked = FALSE;
    }
    if (oco->size > BUFWATERMARK)
    {
	xfree(oco->buf);
	xfree(oco);
    }
    else
    {
	oco->next = FreeOutputs;
	FreeOutputs = oco;
    }
    oc->output = (ConnectionOutputPtr)NULL;
    return extraCount; /* return only the amount explicitly requested */
}

 /********************
 * FlushAllOutput()
 *    Flush all clients with output.  However, if some client still
 *    has input in the queue (more requests), then don't flush.  This
 *    will prevent the output queue from being flushed every time around
 *    the round robin queue.  Now, some say that it SHOULD be flushed
 *    every time around, but...
 *
 **********************/

void
FlushAllOutput()
{
    register int index, base, mask;
    OsCommPtr oc;
    register ClientPtr client;

    if (! NewOutputPending)
	return;

    /*
     * It may be that some client still has critical output pending,
     * but he is not yet ready to receive it anyway, so we will
     * simply wait for the select to tell us when he's ready to receive.
     */
    CriticalOutputPending = FALSE;
    NewOutputPending = FALSE;

    for (base = 0; base < mskcnt; base++)
    {
	mask = OutputPending[ base ];
	OutputPending[ base ] = 0;
	while (mask)
	{
	    index = ffs(mask) - 1;
	    mask &= ~lowbit(mask);
	    if ((index = ConnectionTranslation[(base << 5) + index]) == 0)
		continue;
	    client = clients[index];
	    if (client->clientGone)
		continue;
	    oc = (OsCommPtr)client->osPrivate;
	    if (GETBIT(ClientsWithInput, oc->fd))
	    {
		BITSET(OutputPending, oc->fd); /* set the bit again */
		NewOutputPending = TRUE;
	    }
	    else
		(void)FlushClient(client, oc, (char *)NULL, 0);
	}
    }

}

void
FlushIfCriticalOutputPending()
{
    if (CriticalOutputPending)
	FlushAllOutput();
}

void
SetCriticalOutputPending()
{
    CriticalOutputPending = TRUE;
}

/*****************
 * WriteToClient
 *    Copies buf into ClientPtr.buf if it fits (with padding), else
 *    flushes ClientPtr.buf and buf to client.  As of this writing,
 *    every use of WriteToClient is cast to void, and the result
 *    is ignored.  Potentially, this could be used by requests
 *    that are sending several chunks of data and want to break
 *    out of a loop on error.  Thus, we will leave the type of
 *    this routine as int.
 *****************/

int
WriteToClient (who, count, buf)
    ClientPtr who;
    char *buf;
    int count;
{
    OsCommPtr oc = (OsCommPtr)who->osPrivate;
    register ConnectionOutputPtr oco = oc->output;
    int padBytes;

    if (!count)
	return(0);

    if (!oco)
    {
	if (oco = FreeOutputs)
	{
	    FreeOutputs = oco->next;
	}
	else if (!(oco = AllocateOutputBuffer()))
	{
	    close(oc->fd);
	    MarkClientException(who);
	    return -1;
	}
	oc->output = oco;
    }

    padBytes =  padlength[count & 3];

    if (oco->count + count + padBytes > oco->size)
    {
	BITCLEAR(OutputPending, oc->fd);
	CriticalOutputPending = FALSE;
	NewOutputPending = FALSE;
	return FlushClient(who, oc, buf, count);
    }

    NewOutputPending = TRUE;
    BITSET(OutputPending, oc->fd);
    bcopy(buf, (char *)oco->buf + oco->count, count);
    oco->count += count + padBytes;
    
    return(count);
}

static ConnectionInputPtr
AllocateInputBuffer()
{
    register ConnectionInputPtr oci;

    oci = (ConnectionInputPtr)xalloc(sizeof(ConnectionInput));
    if (!oci)
	return (ConnectionInputPtr)NULL;
    oci->buffer = (char *)xalloc(BUFSIZE);
    if (!oci->buffer)
    {
	xfree(oci);
	return (ConnectionInputPtr)NULL;
    }
    oci->size = BUFSIZE;
    oci->bufptr = oci->buffer;
    oci->bufcnt = 0;
    oci->lenLastReq = 0;
    return oci;
}

static ConnectionOutputPtr
AllocateOutputBuffer()
{
    register ConnectionOutputPtr oco;

    oco = (ConnectionOutputPtr)xalloc(sizeof(ConnectionOutput));
    if (!oco)
	return (ConnectionOutputPtr)NULL;
    oco->buf = (unsigned char *) xalloc(BUFSIZE);
    if (!oco->buf)
    {
	xfree(oco);
	return (ConnectionOutputPtr)NULL;
    }
    oco->size = BUFSIZE;
    oco->count = 0;
    return oco;
}

void
FreeOsBuffers(oc)
    OsCommPtr oc;
{
    register ConnectionInputPtr oci;
    register ConnectionOutputPtr oco;

    if (AvailableInput == oc)
	AvailableInput = (OsCommPtr)NULL;
    if (oci = oc->input)
    {
	if (FreeInputs)
	{
	    xfree(oci->buffer);
	    xfree(oci);
	}
	else
	{
	    FreeInputs = oci;
	    oci->next = (ConnectionInputPtr)NULL;
	    oci->bufptr = oci->buffer;
	    oci->bufcnt = 0;
	    oci->lenLastReq = 0;
	}
    }
    if (oco = oc->output)
    {
	if (FreeOutputs)
	{
	    xfree(oco->buf);
	    xfree(oco);
	}
	else
	{
	    FreeOutputs = oco;
	    oco->next = (ConnectionOutputPtr)NULL;
	    oco->count = 0;
	}
    }
}

void
ResetOsBuffers()
{
    register ConnectionInputPtr oci;
    register ConnectionOutputPtr oco;

    while (oci = FreeInputs)
    {
	FreeInputs = oci->next;
	xfree(oci->buffer);
	xfree(oci);
    }
    while (oco = FreeOutputs)
    {
	FreeOutputs = oco->next;
	xfree(oco->buf);
	xfree(oco);
    }
}
