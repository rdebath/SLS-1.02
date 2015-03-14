/*
 * $XConsortium: Unwrap.c,v 1.8 91/09/10 14:34:13 keith Exp $
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

#ifdef HASXDMAUTH

/*
 * The following function exists only to demonstrate the
 * desired functional interface for this routine.  You will
 * need to add the appropriate algorithm if you wish to
 * use XDM-AUTHENTICATION-1/XDM-AUTHORIZATION-1.
 *
 * The interface for this routine is quite simple.  All three
 * arguments are arrays of 8 unsigned characters, the first two
 * are 64 bits of useful data, the last is 56 bits of useful
 * data packed into 8 bytes, using the low 7 bits of each
 * byte, filling the high bit with odd parity.
 *
 * Examine the XDMCP specification for the correct algorithm
 */

#include "Wrap.h"

void
XdmcpUnwrap (input, wrapper, output, bytes)
    unsigned char	*input, *output;
    unsigned char	*wrapper;
    int			bytes;
{
    int			i, j, k;
    unsigned char	tmp[8];
    unsigned char	blocks[2][8];
    unsigned char	expand_wrapper[8];
    auth_wrapper_schedule	schedule;

    _XdmcpWrapperToOddParity (wrapper, expand_wrapper);
    _XdmcpAuthSetup (expand_wrapper, schedule);

    k = 0;
    for (j = 0; j < bytes; j += 8)
    {
	if (bytes - j < 8)
	    return; /* bad input length */
	for (i = 0; i < 8; i++)
	    blocks[k][i] = input[j + i];
	_XdmcpAuthDoIt ((unsigned char *) (input + j), (unsigned char *) tmp, schedule, 0);
	/* block chaining */
	k = (k == 0) ? 1 : 0;
	for (i = 0; i < 8; i++)
	{
	    if (j == 0)
		output[j + i] = tmp[i];
	    else
		output[j + i] = tmp[i] ^ blocks[k][i];
	}
    }
}

#endif /* HASXDMAUTH */
