/* $XConsortium: extnsionst.h,v 1.9 89/08/31 18:41:12 rws Exp $ */
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
#ifndef EXTENSIONSTRUCT_H
#define EXTENSIONSTRUCT_H 
#include "extension.h"
typedef struct _ExtensionEntry {
    int index;
    void (* CloseDown)();	/* called at server shutdown */
    char *name;               /* extension name */
    int base;                 /* base request number */
    int eventBase;            
    int eventLast;
    int errorBase;
    int errorLast;
    int num_aliases;
    char **aliases;
    pointer extPrivate;
    unsigned short (* MinorOpcode)();	/* called for errors */
} ExtensionEntry;

extern void (* EventSwapVector[128]) ();

typedef void (* ExtensionLookupProc)();

typedef struct _ProcEntry {
    char *name;
    ExtensionLookupProc proc;
} ProcEntryRec, *ProcEntryPtr;

typedef struct _ScreenProcEntry {
    int num;
    ProcEntryPtr procList;
} ScreenProcEntry;

#define    SetGCVector(pGC, VectorElement, NewRoutineAddress, Atom)    \
    pGC->VectorElement = NewRoutineAddress;

#define    GetGCValue(pGC, GCElement)    (pGC->GCElement)

extern void InitExtensions();
extern int ProcQueryExtension();
extern int ProcListExtensions();
extern ExtensionEntry *AddExtension();
extern Bool AddExtensionAlias();
extern ExtensionLookupProc LookupProc();
extern Bool RegisterProc();
extern Bool RegisterScreenProc();
extern unsigned short MinorOpcodeOfRequest();
extern unsigned short StandardMinorOpcode();

#endif /* EXTENSIONSTRUCT_H */
