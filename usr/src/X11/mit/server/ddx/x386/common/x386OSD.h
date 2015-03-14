/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/common/x386OSD.h,v 1.12 1992/09/26 17:41:22 dawes Exp $
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
 * $Header: /proj/X11/mit/server/ddx/x386/RCS/x386OSD.h,v 1.1 1991/06/02 22:36:17 root Exp $
 */

#ifndef _X386OSD_H
#define _X386OSD_H

#include <X11/Xos.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/ioctl.h>
#ifndef linux
#undef NULL
#endif /* !linux */
#include <sys/param.h>
#include <signal.h>
#ifndef MACH386
#ifndef __386BSD__
#include <termio.h>
#else
#include <termios.h>
#define termio termios
#endif /* __386BSD__ */
#endif /* MACH386 */
#include <errno.h>
extern int errno;

#if !defined(__386BSD__) && !defined(linux) && !defined(MACH386)
#ifdef _NEED_SYSI86
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
#include <sys/tss.h>
#include <sys/sysi86.h>
#include <sys/v86.h>
#endif
#endif

#if defined(ATT) && !defined(i386)
#define i386 /* not defined in ANSI C mode */
#endif

#if defined(SYSV) || defined(SVR4) || defined(linux)
# define HAS_USL_VTS
#endif

#ifdef HAS_USL_VTS
#ifdef linux
#include <sys/mman.h>
#include <sys/kd.h>
#include <sys/vt.h>
#define LDGMAP GIO_SCRNMAP
#define LDSMAP PIO_SCRNMAP
#define LDNMAP LDSMAP
#else
#include <sys/emap.h>

#ifndef SCO
# include    <sys/at_ansi.h>
# include    <sys/kd.h>
# include    <sys/vt.h>
#else /* SCO */
# include    <sys/vtkd.h>
# include    <sys/console.h>
# include    <sys/keyboard.h>
# define LED_CAP  0x01
# define LED_NUM  0x02
# define LED_SCR  0x04
#endif /* SCO */
#endif /* linux */
#endif /* HAS_USL_VTS */

/* 
 * Special hack for isc 2.2 posix compatible include files
 */
#if !defined(O_NDELAY) && defined(O_NONBLOCK)
# define	O_NDELAY	O_NONBLOCK
#endif

#ifndef VT_ACKACQ
# define VT_ACKACQ 2  /* bed-time for bonzo ... */
#endif

#if defined(ATT) || defined(SVR4)
# define XQUEUE
# include <sys/xque.h>
#endif

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN 32
#endif

#ifndef linux

/* Do this for SVR4 too to avoid using libucb.a */
#if defined(SYSV) || defined(SVR4)
#define usleep(usec) syscall(3112, usec / 1000)
#endif

#ifdef __386BSD__
#define CONSOLE_X_MODE_ON _IO('t',121)
#define CONSOLE_X_MODE_OFF _IO('t',122)
#endif /* __386BSD__ */

#ifdef MACH386
#ifndef __STDC__
#define __STDC__ 1
#include <i386at/kd.h>
#include <i386at/kd_queue.h>
#undef __STDC__
#else
#include <i386at/kd.h>
#include <i386at/kd_queue.h>
#endif /* __STDC__ */
#include <sys/file.h>
#define SEEK_SET L_SET
#endif /* MACH386 */

#endif

#endif /* _X386OSD_H */
