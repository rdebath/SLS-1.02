/* $XConsortium: extentst.h,v 1.2 91/05/13 16:48:45 gildea Exp $ */
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
 * @(#)extentst.h	4.1	91/05/02
 *
 */

#ifndef _EXTENTST_H_
#define _EXTENTST_H_

typedef struct _ExtensionEntry {
    int         index;
    void        (*CloseDown) ();
    char       *name;
    int         base;
    int         eventBase;
    int         eventLast;
    int         errorBase;
    int         errorLast;
    int         num_aliases;
    char      **aliases;
    pointer     extPrivate;
    unsigned short (*MinorOpcode) ();
}           ExtensionEntry;

extern void (*EventSwapVector[]) ();

typedef void (*ExtensionLookupProc) ();

typedef struct _ProcEntry {
    char       *name;
    ExtensionLookupProc proc;
}           ProcEntryRec, *ProcEntryPtr;

extern void InitExtensions();
extern int  ProcQueryExtension();
extern int  ProcListExtensions();
extern ExtensionEntry *AddExtension();
extern Bool AddExtensionAlias();
extern ExtensionLookupProc LookupProc();
extern Bool RegisterProc();
extern unsigned short MinorOpcodeOfRequest();
extern unsigned short StandardMinorOpcode();

#endif				/* _EXTENTST_H_ */
