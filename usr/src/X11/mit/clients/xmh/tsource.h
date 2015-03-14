/* $XConsortium: tsource.h,v 1.2 91/07/23 18:38:31 converse Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
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

#ifndef _tsource_h
#define _tsource_h

#include <X11/Xaw/TextSrc.h>

/* xmh TextSrc widget resources:
 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 toc                 Toc                Pointer         NULL
*/

#define XtCToc "Toc"
#define XtNtoc "toc"

/* Class record constants */

extern WidgetClass tocSourceWidgetClass;

typedef struct _TocSourceClassRec *TocSourceWidgetClass;
typedef struct _TocSourceRec      *TocSourceWidget;

extern void TSourceInvalid();

#endif /* _XawTextSrc_h */
/* DON'T ADD STUFF AFTER THIS #endif */
