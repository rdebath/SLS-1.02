/* $XConsortium: resource.h,v 1.2 91/05/13 16:49:21 gildea Exp $ */
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
 * @(#)resource.h	4.1	91/05/02
 *
 */
#ifndef _RESOURCE_H_
#define _RESOURCE_H_

#include	"misc.h"

typedef unsigned long RESTYPE;

#define	RC_VANILLA	((RESTYPE)0)
#define	RC_CACHED	((RESTYPE)1<<31)
#define	RC_LASTPREDEF	RC_CACHED
#define	RC_ANY		(~(RESTYPE)0)

#define	RT_FONT		((RESTYPE)1)
#define	RT_AUTHCONT	((RESTYPE)2)
#define	RT_LASTPREDEF	RT_AUTHCONT
#define	RT_NONE		((RESTYPE)0)

#define	CLIENTOFFSET		22
#define	RESOURCE_ID_MASK	0x3FFFFF
#define	CLIENT_BITS(id)		((id) & 0x1fc00000)
#define	CLIENT_ID(id)		((int)(CLIENT_BITS(id) >> CLIENTOFFSET))
#define	SERVER_BIT		0x20000000

#define	INVALID			(0)

#define	BAD_RESOURCE		0xe0000000

#ifdef NOTYET
extern RESTYPE CreateNewResourceType();
extern RESTYPE CreateNewResourceClass();
extern FSID FakeClientID();
extern pointer LookupIDByClass();
extern void	FreeResourceByType();
extern Bool	ChangeResourceValue();
#endif /* NOTYET */

extern Bool AddResource();
extern void FreeResource();
extern void FreeClientResources();
extern pointer LookupIDByType();

#endif				/* _RESOURCE_H_ */
