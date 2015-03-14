/* $Header: /home/x_cvs/mit/lib/Xdmcp/Flush.c,v 1.2 1992/06/18 11:23:59 dawes Exp $ */
/*
 * $XConsortium: Flush.c,v 1.4 91/07/16 20:33:52 gildea Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include <X11/Xos.h>
#include <X11/X.h>
#include <X11/Xmd.h>
#include <X11/Xdmcp.h>

#if defined(TCPCONN) || defined(UNIXCONN) || defined(DNETCONN)
#define HASSOCKETS
#endif

#ifdef STREAMSCONN
#include <tiuser.h>
#else
#ifdef HASSOCKETS
#include <sys/socket.h>
#endif
#endif

int
XdmcpFlush (fd, buffer, to, tolen)
    int		    fd;
    XdmcpBufferPtr  buffer;
    XdmcpNetaddr    to;
    int		    tolen;
{
    int result;

#ifdef STREAMSCONN
    struct t_unitdata dataunit;

    dataunit.addr.buf = to;
    dataunit.addr.len = tolen;
    dataunit.opt.len = 0;	/* default options */
    dataunit.udata.buf = (char *)buffer->data;
    dataunit.udata.len = buffer->pointer;
    result = t_sndudata(fd, &dataunit);
    if (result < 0)
	return FALSE;
#else
#ifdef HASSOCKETS
    result = sendto (fd, buffer->data, buffer->pointer, 0,
		     (struct sockaddr *)to, tolen);
    if (result != buffer->pointer)
	return FALSE;
#else
    return FALSE;
#endif /* HASSOCKETS */
#endif /* STREAMSCONN */
    return TRUE;
}
