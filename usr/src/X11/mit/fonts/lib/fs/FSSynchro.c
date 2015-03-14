/* $XConsortium: FSSynchro.c,v 1.2 91/05/13 15:11:56 gildea Exp $ */

/* @(#)FSSynchro.c	4.1	91/05/02
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

#include	"FSlibint.h"

int
_FSSyncFunction(svr)
    FSServer     *svr;
{
    FSSync(svr, 0);
}

int
(* FSSynchronize(svr, onoff)) ()
    FSServer     *svr;
    int         onoff;
{
    int         (*temp) ();

    temp = svr->synchandler;
    if (onoff)
	svr->synchandler = _FSSyncFunction;
    else
	svr->synchandler = NULL;
    return temp;
}

int
(* FSSetAfterFunction(svr, func)) ()
    FSServer     *svr;
    int         (*func) ();
{
    int         (*temp) ();

    temp = svr->synchandler;
    svr->synchandler = func;
    return temp;
}
