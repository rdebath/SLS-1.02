/* $XConsortium: misc.h,v 1.3 91/05/13 16:48:58 gildea Exp $ */
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
 * @(#)misc.h	4.1	91/05/02
 *
 */

#ifndef _MISC_H_
#define _MISC_H_

#include	<X11/Xosdefs.h>
#include	<X11/Xfuncs.h>

#include	"assert.h"	/* so its everywhere */

#ifndef NULL

#ifndef X_NOT_STDC_ENV
#include	<stddef.h>
#else
#define	NULL	0
#endif

#endif

#define	MAXCLIENTS	128

typedef unsigned char *pointer;
typedef int Bool;
typedef unsigned long Atom;

#define	MILLI_PER_SECOND	(1000)
#define	MILLI_PER_MINUTE	(1000 * 60)

#ifndef TRUE
#define	TRUE 1
#define	FALSE 0
#endif

#define	min(a, b)	(((a) < (b)) ? (a) : (b))
#define	max(a, b)	(((a) > (b)) ? (a) : (b))
#define	abs(a)		((a) > 0 ? (a) : -(a))

#include	"os.h"

#define	lowbit(x)	((x) & (~(x) + 1))

/* byte swapping helpers */

/* byte swap a long literal */
#define lswapl(x) ((((x) & 0xff) << 24) |\
		   (((x) & 0xff00) << 8) |\
		   (((x) & 0xff0000) >> 8) |\
		   (((x) >> 24) & 0xff))

/* byte swap a short literal */
#define lswaps(x) ((((x) & 0xff) << 8) | (((x) >> 8) & 0xff))

/* some macros to help swap requests, replies, and events */

#define LengthRestB(stuff) \
    (((unsigned long)stuff->length << 2) - sizeof(*stuff))

#define LengthRestS(stuff) \
    (((unsigned long)stuff->length << 1) - (sizeof(*stuff) >> 1))

#define LengthRestL(stuff) \
    ((unsigned long)stuff->length - (sizeof(*stuff) >> 2))

#define SwapRestS(stuff) \
    SwapShorts((short *)(stuff + 1), LengthRestS(stuff))

#define SwapRestL(stuff) \
    SwapLongs((long *)(stuff + 1), LengthRestL(stuff))

/* byte swap a long */
#define swapl(x, n) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[3];\
		 ((char *) (x))[3] = n;\
		 n = ((char *) (x))[1];\
		 ((char *) (x))[1] = ((char *) (x))[2];\
		 ((char *) (x))[2] = n;

/* byte swap a short */
#define swaps(x, n) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[1];\
		 ((char *) (x))[1] = n

/* copy long from src to dst byteswapping on the way */
#define cpswapl(src, dst) \
                 ((char *)&(dst))[0] = ((char *) &(src))[3];\
                 ((char *)&(dst))[1] = ((char *) &(src))[2];\
                 ((char *)&(dst))[2] = ((char *) &(src))[1];\
                 ((char *)&(dst))[3] = ((char *) &(src))[0];

/* copy short from src to dst byteswapping on the way */
#define cpswaps(src, dst)\
		 ((char *) &(dst))[0] = ((char *) &(src))[1];\
		 ((char *) &(dst))[1] = ((char *) &(src))[0];

extern void SwapLongs();
extern void SwapShorts();

extern void CopyIsoLatin1Lowered();
extern void NoopDDA();
extern char *NameForAtom();
extern void BitOrderInvert();
extern void TwoByteInvert();
extern void FourByteInvert();

extern long GetTimeInMillis();

#endif				/* _MISC_H_ */
