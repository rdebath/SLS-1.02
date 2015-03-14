/* $Header: /home/x_cvs/mit/fonts/lib/fs/FSlibos.h,v 1.2 1992/06/18 11:22:55 dawes Exp $ */
/* $XConsortium: FSlibos.h,v 1.11 91/07/23 18:59:49 rws Exp $ */

/* @(#)FSlibos.h	4.1	91/05/02
 * Copyright 1990 Network Computing Devices;
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

/*
 * FSlib networking & os include file
 */

#include <X11/Xfuncs.h>
#include <X11/Xosdefs.h>

/* Sorry, we do not really support streams yet */
#ifdef STREAMSCONN
#undef STREAMSCONN
#endif

#ifdef STREAMSCONN
#ifdef SYSV
/*
 * UNIX System V Release 3.2
 */
#include <sys/stropts.h>
#define BytesReadable(fd,ptr) (_FSBytesReadable ((fd), (ptr)))
#define MALLOC_0_RETURNS_NULL
#include <sys/ioctl.h>
#endif /* SYSV */
#ifdef SVR4
/*
 * TLI (Streams-based) networking
 */
#define BytesReadable(fd,ptr) (_FSBytesReadable ((fd), (ptr)))
#include <sys/uio.h>		/* define struct iovec */

#endif /* SVR4 */
#else /* not STREAMSCONN */
/*
 * 4.2BSD-based systems
 */
#if defined(TCPCONN) || defined(UNIXCONN) || defined(DNETCONN)
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h>
#endif
#include <sys/uio.h>		/* needed for FSlibInt.c */
#ifdef SVR4
#include <sys/filio.h>
#endif

#if defined(SYSV386) && defined(SYSV)
#if defined(TCPCONN) || defined(STREAMSCONN)
#include <net/errno.h>
#endif
#include <sys/stropts.h>
#define BytesReadable(fd,ptr) ioctl((fd), I_NREAD, (ptr))
#else
#define BytesReadable(fd, ptr) ioctl ((fd), FIONREAD, (ptr))
#endif
#endif /* STREAMSCONN else */

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef OPEN_MAX
#ifdef SVR4
#define OPEN_MAX 256
#else
#include <sys/param.h>
#ifndef OPEN_MAX
#ifdef NOFILE
#define OPEN_MAX NOFILE
#else
#define OPEN_MAX NOFILES_MAX
#endif
#endif
#endif
#endif

/* Utek leaves kernel macros around in include files (bleah) */

#ifdef dirty
#undef dirty
#endif

#ifdef CRAY
#define WORD64
#endif

#if OPEN_MAX > 256
#undef OPEN_MAX
#define OPEN_MAX 256
#endif

#define MSKCNT ((OPEN_MAX + 31) / 32)

#if (MSKCNT==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif

#if (MSKCNT>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#if (MSKCNT==1)
#define COPYBITS(src, dst) dst[0] = src[0]
#define CLEARBITS(buf) buf[0] = 0
#define MASKANDSETBITS(dst, b1, b2) dst[0] = (b1[0] & b2[0])
#define ORBITS(dst, b1, b2) dst[0] = (b1[0] | b2[0])
#define UNSETBITS(dst, b1) (dst[0] &= ~b1[0])
#define _FSANYSET(src) (src[0])
#endif

#if (MSKCNT==2)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; }
#define _FSANYSET(src) (src[0] || src[1])
#endif

#if (MSKCNT==3)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; \
			     dst[2] = src[2]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; buf[2] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]);\
		      dst[2] = (b1[2] & b2[2]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]);\
		      dst[2] = (b1[2] | b2[2]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]; }
#define _FSANYSET(src) (src[0] || src[1] || src[2])
#endif

#if (MSKCNT==4)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]; \
			   dst[2] = src[2]; dst[3] = src[3]
#define CLEARBITS(buf) buf[0] = 0; buf[1] = 0; buf[2] = 0; buf[3] = 0
#define MASKANDSETBITS(dst, b1, b2)  \
                      dst[0] = (b1[0] & b2[0]);\
                      dst[1] = (b1[1] & b2[1]);\
                      dst[2] = (b1[2] & b2[2]);\
                      dst[3] = (b1[3] & b2[3])
#define ORBITS(dst, b1, b2)  \
                      dst[0] = (b1[0] | b2[0]);\
                      dst[1] = (b1[1] | b2[1]);\
                      dst[2] = (b1[2] | b2[2]);\
                      dst[3] = (b1[3] | b2[3])
#define UNSETBITS(dst, b1) \
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]; \
                      dst[3] &= ~b1[3]
#define _FSANYSET(src) (src[0] || src[1] || src[2] || src[3])
#endif

#if (MSKCNT>4)
#define COPYBITS(src, dst) bcopy((caddr_t) src, (caddr_t) dst,\
				 MSKCNT*sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t) buf, MSKCNT*sizeof(long))
#define MASKANDSETBITS(dst, b1, b2)  \
		      { int cri;			\
			for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] = (b1[cri] & b2[cri]) }
#define ORBITS(dst, b1, b2)  \
		      { int cri;			\
		      for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] = (b1[cri] | b2[cri]) }
#define UNSETBITS(dst, b1) \
		      { int cri;			\
		      for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] &= ~b1[cri];  }
#if (MSKCNT==8)
#define _FSANYSET(src) (src[0] || src[1] || src[2] || src[3] || \
			src[4] || src[5] || src[6] || src[7])
#endif
/*
 * If MSKCNT>4 and not 8, then _FSANYSET is a routine defined in FSlibInt.c.
 *
 * #define _FSANYSET(src) (src[0] || src[1] || src[2] || src[3] || src[4] ...)
 */
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#include <string.h>
#else
char *malloc(), *realloc(), *calloc();
void exit();
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
#endif /* macII */

/*
 * The following definitions can be used for locking requests in multi-threaded
 * address spaces.
 */
#define LockDisplay(dis)
#define LockMutex(mutex)
#define UnlockMutex(mutex)
#define UnlockDisplay(dis)
#define FSfree(ptr) free((ptr))


/*
 * Note that some machines do not return a valid pointer for malloc(0), in
 * which case we provide an alternate under the control of the
 * define MALLOC_0_RETURNS_NULL.  This is necessary because some
 * FSlib code expects malloc(0) to return a valid pointer to storage.
 */

#ifdef MALLOC_0_RETURNS_NULL
#define FSmalloc(size) malloc(((size) > 0 ? (size) : 1))
#define FSrealloc(ptr, size) realloc((ptr), ((size) > 0 ? (size) : 1))
#define FScalloc(nelem, elsize) calloc(((nelem) > 0 ? (nelem) : 1), (elsize))

#else

#define FSmalloc(size) malloc((size))
#define FSrealloc(ptr, size) realloc((ptr), (size))
#define FScalloc(nelem, elsize) calloc((nelem), (elsize))
#endif

/*
 *	ReadvFromServer and WritevToSever use struct iovec, normally found
 *	in Berkeley systems in <sys/uio.h>.  See the readv(2) and writev(2)
 *	manual pages for details.
 *
 *	struct iovec {
 *		caddr_t iov_base;
 *		int iov_len;
 *	};
 */

#ifdef USG

#if defined(USG) && !defined(CRAY) && !defined(umips) && !defined(MOTOROLA)
struct iovec {
    caddr_t     iov_base;
    int         iov_len;
};

#ifndef __TIMEVAL__
#define __TIMEVAL__
struct timeval {		/* BSD has in <sys/time.h> */
    long        tv_sec;
    long        tv_usec;
};

#endif				/* __TIMEVAL__ */

#endif				/* not CRAY or umips */

#endif				/* USG */


#ifdef STREAMSCONN
#include "FSstreams.h"

#if (!defined(EWOULDBLOCK)) && defined(EAGAIN)
#define EWOULDBLOCK EAGAIN
#endif

extern char _FSsTypeOfStream[];
extern FSstream _FSsStream[];

#define ReadFromServer(svr, data, size) \
	(*_FSsStream[_FSsTypeOfStream[svr]].ReadFromStream)((svr), (data), (size), \
						     BUFFERING)
#define WriteToServer(svr, bufind, size) \
	(*_FSsStream[_FSsTypeOfStream[svr]].WriteToStream)((svr), (bufind), (size))

#else				/* else not STREAMSCONN */

/*
 * bsd can read from sockets directly
 */
#define ReadFromServer(svr, data, size) read((svr), (data), (size))
#define WriteToServer(svr, bufind, size) write((svr), (bufind), (size))
#endif				/* STREAMSCONN */


#ifndef USL_COMPAT
#if !defined(USG) || defined(MOTOROLA)
#if !(defined(SYSV) && defined(SYSV386))
#define _FSReadV readv
#endif
#define _FSWriteV writev
#endif
#endif /* !USL_COMPAT */

#define ReadvFromServer(svr, iov, iovcnt) _FSReadV((svr), (iov), (iovcnt))
#define WritevToServer(svr, iov, iovcnt) _FSWriteV((svr), (iov), (iovcnt))

#define SearchString(string, char) index((string), (char))
