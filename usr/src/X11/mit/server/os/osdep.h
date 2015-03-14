/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
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
/* $XConsortium: osdep.h,v 1.27 91/07/23 19:04:22 rws Exp $ */

#define BOTIMEOUT 200 /* in milliseconds */
#define BUFSIZE 4096
#define BUFWATERMARK 8192
#define MAXBUFSIZE (1 << 18)

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
#define OPEN_MAX 128
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

#if OPEN_MAX <= 128
#define MAXSOCKS (OPEN_MAX - 1)
#else
#define MAXSOCKS 128
#endif

#ifndef NULL
#define NULL 0
#endif

#define mskcnt ((MAXSOCKS + 31) / 32)	/* size of bit array */

#if (mskcnt==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif
#if (mskcnt>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#if (mskcnt==1)
#define COPYBITS(src, dst) dst[0] = src[0]
#define CLEARBITS(buf) buf[0] = 0
#define MASKANDSETBITS(dst, b1, b2) dst[0] = (b1[0] & b2[0])
#define ORBITS(dst, b1, b2) dst[0] = (b1[0] | b2[0])
#define UNSETBITS(dst, b1) (dst[0] &= ~b1[0])
#define ANYSET(src) (src[0])
#endif
#if (mskcnt==2)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]
#define CLEARBITS(buf) buf[0] = 0; buf[1] = 0
#define MASKANDSETBITS(dst, b1, b2)  \
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1])
#define ORBITS(dst, b1, b2)  \
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1])
#define UNSETBITS(dst, b1) \
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]
#define ANYSET(src) (src[0] || src[1])
#endif
#if (mskcnt==3)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]; dst[2] = src[2];
#define CLEARBITS(buf) buf[0] = 0; buf[1] = 0; buf[2] = 0
#define MASKANDSETBITS(dst, b1, b2)  \
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]);\
		      dst[2] = (b1[2] & b2[2])
#define ORBITS(dst, b1, b2)  \
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]);\
		      dst[2] = (b1[2] | b2[2])
#define UNSETBITS(dst, b1) \
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]
#define ANYSET(src) (src[0] || src[1] || src[2])
#endif
#if (mskcnt==4)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]; dst[2] = src[2];\
		      dst[3] = src[3]
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
#define ANYSET(src) (src[0] || src[1] || src[2] || src[3])
#endif

#if (mskcnt>4)
#define COPYBITS(src, dst) bcopy((caddr_t) src, (caddr_t) dst,\
				 mskcnt*sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t) buf, mskcnt*sizeof(long))
#define MASKANDSETBITS(dst, b1, b2)  \
		      { int cri;			\
			for (cri=mskcnt; --cri>=0; )	\
		          dst[cri] = (b1[cri] & b2[cri]); }
#define ORBITS(dst, b1, b2)  \
		      { int cri;			\
		      for (cri=mskcnt; --cri>=0; )	\
		          dst[cri] = (b1[cri] | b2[cri]); }
#define UNSETBITS(dst, b1) \
		      { int cri;			\
		      for (cri=mskcnt; --cri>=0; )	\
		          dst[cri] &= ~b1[cri];  }
#if (mskcnt==8)
#define ANYSET(src) (src[0] || src[1] || src[2] || src[3] || \
		     src[4] || src[5] || src[6] || src[7])
#endif
/*
 * If mskcnt>4 and not 8, then ANYSET is a routine defined in WaitFor.c.
 *
 * #define ANYSET(src) (src[0] || src[1] || src[2] || src[3] || src[4] ...)
 */
#endif

typedef struct _connectionInput {
    struct _connectionInput *next;
    char *buffer;               /* contains current client input */
    char *bufptr;               /* pointer to current start of data */
    int  bufcnt;                /* count of bytes in buffer */
    int lenLastReq;
    int size;
} ConnectionInput, *ConnectionInputPtr;

typedef struct _connectionOutput {
    struct _connectionOutput *next;
    int size;
    unsigned char *buf;
    int count;
} ConnectionOutput, *ConnectionOutputPtr;

typedef struct _osComm {
    int fd;
    ConnectionInputPtr input;
    ConnectionOutputPtr output;
    XID	auth_id;		/* authorization id */
    long conn_time;		/* timestamp if not established, else 0  */
} OsCommRec, *OsCommPtr;
