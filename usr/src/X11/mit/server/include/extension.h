/* $XConsortium: extension.h,v 1.6 89/07/16 14:37:47 rws Exp $ */
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
#ifndef EXTENSION_H
#define EXTENSION_H 

#define GetGCAndDrawableAndValidate(gcID, pGC, drawID, pDraw, client)\
    if ((client->lastDrawableID != drawID) || (client->lastGCID != gcID))\
    {\
        if (client->lastDrawableID != drawID)\
    	    pDraw = (DrawablePtr)LookupIDByClass(drawID, RC_DRAWABLE);\
        else\
	    pDraw = client->lastDrawable;\
        if (client->lastGCID != gcID)\
	    pGC = (GC *)LookupIDByType(gcID, RT_GC);\
        else\
            pGC = client->lastGC;\
	if (pDraw && pGC)\
	{\
	    if ((pDraw->type == UNDRAWABLE_WINDOW) ||\
		(pGC->depth != pDraw->depth) ||\
		(pGC->pScreen != pDraw->pScreen))\
		return (BadMatch);\
	    client->lastDrawable = pDraw;\
	    client->lastDrawableID = drawID;\
            client->lastGC = pGC;\
            client->lastGCID = gcID;\
	}\
    }\
    else\
    {\
        pGC = client->lastGC;\
        pDraw = client->lastDrawable;\
    }\
    if (!pDraw)\
    {\
        client->errorValue = drawID; \
	return (BadDrawable);\
    }\
    if (!pGC)\
    {\
        client->errorValue = gcID;\
        return (BadGC);\
    }\
    if (pGC->serialNumber != pDraw->serialNumber)\
	ValidateGC(pDraw, pGC);
#endif /* EXTENSION_H */
