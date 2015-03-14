/* $XConsortium: client.h,v 1.3 91/05/13 16:48:09 gildea Exp $ */
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
 * @(#)client.h	4.2	91/05/03
 *
 */

#ifndef	_CLIENT_H_
#define	_CLIENT_H_

typedef struct _Client *ClientPtr;

extern ClientPtr *clients;
extern ClientPtr serverClient;

#define	NullClient	((ClientPtr) NULL)

#define	SERVER_CLIENT	0
#define	MINCLIENT	1

#define	CLIENT_ALIVE		0
#define	CLIENT_GONE		1
#define	CLIENT_AGED		2
#define	CLIENT_TIMED_OUT	4

extern int  currentMaxClients;

#define	REQUEST(type)	\
	type *stuff = (type *)client->requestBuffer

#define	REQUEST_FIXED_SIZE(req, n)					\
	if (((sizeof(req) >> 2) > stuff->length) ||			\
		(((sizeof(req) + (n) + 3) >> 2) != stuff->length)) {	\
	    SendErrToClient(client, FSBadLength, (pointer)&stuff->length); \
	    return (FSBadLength);	\
	}

#define	REQUEST_SIZE_MATCH(req)				\
	if ((sizeof(req) >> 2) != stuff->length) {	\
	    SendErrToClient(client, FSBadLength, (pointer)&stuff->length); \
	    return (FSBadLength);	\
	}

#define	REQUEST_AT_LEAST_SIZE(req)					\
	if ((sizeof(req) >> 2) > stuff->length) {			\
	    SendErrToClient(client, FSBadLength, (pointer)&stuff->length); \
	    return (FSBadLength);	\
	}

#define	WriteReplyToClient(client, size, reply)			\
	if ((client)->swapped)						\
	    (*ReplySwapVector[((fsReq *)(client)->requestBuffer)->reqType]) \
		(client, (int)(size), reply);				\
	else	(void)WriteToClient(client, (int)(size), (char *)(reply));

#define	WriteSwappedDataToClient(client, size, pbuf)		\
	if ((client)->swapped)						\
	    (*(client)->pSwapReplyFunc)(client, (int)(size), pbuf);	\
	else (void) WriteToClient(client, (int)(size), (char *)(pbuf));


extern void SendErrToClient();

extern void	SwapFontHeader();
extern void	SwapExtents();
extern void	SwapPropInfo();
extern void	SwapPropOffset();
extern void	SwapCharInfo();
extern void	WriteSConnSetup();
extern void	WriteSConnectionInfo();
extern void	SErrorEvent();

typedef struct _WorkQueue       *WorkQueuePtr;
extern void	ProcessWorkQueue();

#endif				/* _CLIENT_H_ */
