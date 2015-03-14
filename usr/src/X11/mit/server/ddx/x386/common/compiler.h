/* $Header: /home/x_cvs/mit/server/ddx/x386/common/compiler.h,v 1.8 1992/09/16 14:55:18 dawes Exp $ */
/*
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * $Header: /proj/X11/mit/server/ddx/x386/RCS/compiler.h,v 1.2 1991/06/27 00:01:11 root Exp $
 */


#ifndef _COMPILER_H
#define _COMPILER_H

#ifdef __GNUC__

#ifndef FAKEIT
#ifdef GCCUSESGAS

/*
 * If gcc uses gas rather than the native assembler, the syntax of these
 * inlines has to be different.		DHD
 */

static __inline__ void
outb(port, val)
short port;
char val;
{
   __asm__ __volatile__("outb %0,%1" : :"a" (val), "d" (port));
}


static __inline__ void
outw(port, val)
short port;
short val;
{
   __asm__ __volatile__("outw %0,%1" : :"a" (val), "d" (port));
}

static __inline__ unsigned int
inb(port)
short port;
{
   unsigned char ret;
   __asm__ __volatile__("inb %1,%0" :
       "=a" (ret) :
       "d" (port));
   return ret;
}

static __inline__ unsigned int
inw(port)
short port;
{
   unsigned short ret;
   __asm__ __volatile__("inw %1,%0" :
       "=a" (ret) :
       "d" (port));
   return ret;
}

#else	/* GCCUSESGAS */

static __inline__ void
outb(port, val)
     short port;
     char val;
{
  __asm__ __volatile__("out%B0 (%1)" : :"a" (val), "d" (port));
}

static __inline__ void
outw(port, val)
     short port;
     short val;
{
  __asm__ __volatile__("out%W0 (%1)" : :"a" (val), "d" (port));
}

static __inline__ unsigned int
inb(port)
     short port;
{
  unsigned int ret;
  __asm__ __volatile__("in%B0 (%1)" :
		   "=a" (ret) :
		   "d" (port));
  return ret;
}

static __inline__ unsigned int
inw(port)
     short port;
{
  unsigned int ret;
  __asm__ __volatile__("in%W0 (%1)" :
		   "=a" (ret) :
		   "d" (port));
  return ret;
}

#endif /* GCCUSESGAS */

#ifdef linux

#define intr_disable()
#define intr_enable()

#else /* !linux */

static __inline__ void
intr_disable()
{
  __asm__ __volatile__("cli");
}

static __inline__ void
intr_enable()
{
  __asm__ __volatile__("sti");
}

#endif /* else !linux */

#else /* FAKEIT */

static __inline__ void
outb(port, val)
     short port;
     char val;
{
}

static __inline__ void
outw(port, val)
     short port;
     short val;
{
}

static __inline__ unsigned int
inb(port)
     short port;
{
  return 0;
}

static __inline__ unsigned int
inw(port)
     short port;
{
  return 0;
}

static __inline__ void
intr_disable()
{
}

static __inline__ void
intr_enable()
{
}

#endif /* FAKEIT */

#else /* __GNUC__ */
# if defined(__STDC__) && (__STDC__ == 1)
#  define asm __asm
# endif
#include <sys/inline.h>
#define intr_disable() asm("cli")
#define intr_enable()  asm("sti")
#endif

#ifndef __STDC__
#define signed /**/
#define const /**/
#endif

#endif /* _COMPILER_H */
