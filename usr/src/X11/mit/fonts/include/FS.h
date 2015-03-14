/* $XConsortium: FS.h,v 1.5 92/05/12 18:07:12 gildea Exp $ */
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
 */
#ifndef _FS_H_
#define	_FS_H_

#include <fsmasks.h>

#define	FS_PROTOCOL		2
#define	FS_PROTOCOL_MINOR	0

typedef unsigned long FSID;

#ifndef X_PROTOCOL
/* protocol familes */
#define FamilyInternet          0
#define FamilyDECnet            1
#define FamilyChaos             2

typedef unsigned long Mask;

typedef FSID	Font;
typedef FSID	AccContext;

typedef unsigned int    FSDrawDirection;
#endif

#ifndef None
#define	None		0L
#endif

#define	LeftToRightDrawDirection	0
#define	RightToLeftDrawDirection	1

/* font info flags */
#define	FontInfoAllCharsExist		(1L << 0)
#define	FontInfoInkInside		(1L << 1)
#define	FontInfoHorizontalOverlap	(1L << 2)

/* auth status flags */
#define	AuthSuccess	0
#define	AuthContinue	1
#define	AuthBusy	2
#define	AuthDenied	3

/* property types */
#define	PropTypeString		0
#define	PropTypeUnsigned	1
#define	PropTypeSigned		2

#ifndef LSBFirst
/* byte order */
#define LSBFirst                0
#define MSBFirst                1
#endif

/* event masks */
#define	CatalogueChangeNotifyMask	(1L << 0)
#define	FontChangeNotifyMask		(1L << 1)

/* errors */
#define	FSSuccess		-1
#define	FSBadRequest		0
#define	FSBadFormat		1
#define	FSBadFont		2
#define	FSBadRange		3
#define	FSBadEventMask		4
#define	FSBadAccessContext	5
#define	FSBadIDChoice		6
#define	FSBadName		7
#define	FSBadResolution		8
#define	FSBadAlloc		9
#define	FSBadLength		10
#define	FSBadImplementation	11

#define	FirstExtensionError	128
#define	LastExtensionError	255

/* events */
#define	KeepAlive		0
#define	CatalogueChangeNotify	1
#define	FontChangeNotify	2
#define FSLASTEvent		3

#endif				/* _FS_H_ */
