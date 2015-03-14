/* $XConsortium: globals.h,v 1.3 91/05/13 16:48:50 gildea Exp $ */
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
 * @(#)globals.h	4.2	91/05/03
 *
 */

#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#include	"FSproto.h"	/* for fsResolution */

extern long TimeOutValue;
extern long ReapClientTime;

extern int  currentMaxClients;
extern long MaxClients;
extern int  serverGeneration;

extern char isItTimeToYield;
extern char dispatchException;

extern int  argcGlobal;
extern char **argvGlobal;

/* bit values for dispatchException */
#define	DE_RESET	0x1
#define	DE_TERMINATE	0x2
#define	DE_RECONFIG	0x4
#define	DE_FLUSH	0x8

/* size of vector tables */
#define	NUM_PROC_VECTORS	25
#define	NUM_EVENT_VECTORS	8
#endif				/* _GLOBALS_H_ */
