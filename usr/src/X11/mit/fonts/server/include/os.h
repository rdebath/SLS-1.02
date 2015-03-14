/* $XConsortium: os.h,v 1.4 91/07/16 20:21:49 keith Exp $ */
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
 * $NCDId: @(#)os.h,v 4.2 1991/05/10 07:59:16 lemke Exp $
 *
 */

#ifndef	_OS_H_
#define	_OS_H_

#include	"FSproto.h"
#include	"misc.h"

/* XXX -- this needs to be in the config stuff */

#ifdef sun
#define	INCLUDE_ALLOCA_H
#endif

#ifdef INCLUDE_ALLOCA_H
#include	<alloca.h>
#else
extern char *alloca();

#endif

#define	MAX_REQUEST_SIZE	16384

extern unsigned long *FSalloc();
extern unsigned long *FSrealloc();
extern void FSfree();

#define	fsalloc(size)		FSalloc((unsigned long)size)
#define	fsrealloc(ptr, size)	FSrealloc((pointer)ptr, (unsigned long)size)
#define	fsfree(ptr)		FSfree((pointer)ptr)

#if defined(vax) || defined(sun) || defined(apollo) || defined(stellar)
#define	ALLOCATE_LOCAL(size)		alloca((int)size)
#define	DEALLOCATE_LOCAL(ptr)
#endif

#ifndef ALLOCATE_LOCAL
#define	ALLOCATE_LOCAL(size)	FSalloc((unsigned long)size)
#define	DEALLOCATE_LOCAL(ptr)	FSfree((pointer)ptr)
#endif				/* ALLOCATE_LOCAL */

int         ReadRequest();

Bool        CloseDownConnection();
void        CreateSockets();
void        FlushAllOuput();
long        GetTimeInMIllis();
void        Error();
void        InitErrors();
void        CloseErrors();
void        NoticeF();
void        ErrorF();
void        FatalError();
void        SetConfigValues();

typedef pointer FID;
typedef struct _FontPathRec *FontPathPtr;

FontPathPtr expand_font_name_pattern();

typedef struct _alt_server *AlternateServerPtr;
typedef struct _auth *AuthPtr;

extern int  ListCatalogues();
extern int  ListAlternateServers();

#endif				/* _OS_H_ */
