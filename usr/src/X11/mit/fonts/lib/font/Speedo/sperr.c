/* $XConsortium: sperr.c,v 1.3 91/07/15 18:15:26 keith Exp $ */
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
 */

#include	"spint.h"

/* VARARGS */
void
SpeedoErr(str, a1)
    char       *str;
    char       *a1;
{
    ErrorF("Speedo: %s %d\n", str, a1);
}


/*
 * Called by Speedo character generator to report an error.
 *
 *  Since character data not available is one of those errors
 *  that happens many times, don't report it to user
 */
void
sp_report_error(n)
    fix15       n;
{
    switch (n) {
    case 1:
	SpeedoErr("Insufficient font data loaded\n");
	break;
    case 3:
	SpeedoErr("Transformation matrix out of range\n");
	break;
    case 4:
	SpeedoErr("Font format error\n");
	break;
    case 5:
	SpeedoErr("Requested specs not compatible with output module\n");
	break;
    case 7:
	SpeedoErr("Intelligent transformation requested but not supported\n");
	break;
    case 8:
	SpeedoErr("Unsupported output mode requested\n");
	break;
    case 9:
	SpeedoErr("Extended font loaded but only compact fonts supported\n");
	break;
    case 10:
	SpeedoErr("Font specs not set prior to use of font\n");
	break;
    case 12:
	break;
    case 13:
	SpeedoErr("Track kerning data not available()\n");
	break;
    case 14:
	SpeedoErr("Pair kerning data not available()\n");
	break;
    default:
	SpeedoErr("report_error(%d)\n", n);
	break;
    }
}
