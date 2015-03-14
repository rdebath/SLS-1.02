/* $XConsortium: validate.h,v 5.2 90/03/16 17:17:01 keith Exp $ */

/*
Copyright 1989 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.  M.I.T. makes no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.
*/

#ifndef VALIDATE_H
#define VALIDATE_H

#include "miscstruct.h"
#include "regionstr.h"

typedef enum { VTOther, VTStack, VTMove, VTUnmap, VTMap } VTKind;

typedef union _Validate {
    struct BeforeValidate {
	DDXPointRec	oldAbsCorner;	/* old window position */
	RegionPtr	borderVisible;	/* visible region of border, */
					/* non-null when size changes */
	Bool		resized;	/* unclipped winSize has changed - */
					/* don't call SaveDoomedAreas */
    } before;
    struct AfterValidate {
	RegionRec	exposed;	/* exposed regions, absolute pos */
	RegionRec	borderExposed;
    } after;
} ValidateRec, *ValidatePtr;

#define UnmapValData ((ValidatePtr)1)

#endif /* VALIDATE_H */
