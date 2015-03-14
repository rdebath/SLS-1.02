/***********************************************************************
Copyright 1991 by Apple Computer, Inc, Cupertino, California
			All Rights Reserved

Permission to use, copy, modify, and distribute this software
for any purpose and without fee is hereby granted, provided
that the above copyright notice appear in all copies.

APPLE MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS,
OR IMPLIED, WITH RESPECT TO THIS SOFTWARE, ITS QUALITY,
PERFORMANCE, MERCHANABILITY, OR FITNESS FOR A PARTICULAR
PURPOSE. AS A RESULT, THIS SOFTWARE IS PROVIDED "AS IS,"
AND YOU THE USER ARE ASSUMING THE ENTIRE RISK AS TO ITS
QUALITY AND PERFORMANCE. IN NO EVENT WILL APPLE BE LIABLE 
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES RESULTING FROM ANY DEFECT IN THE SOFTWARE.

THE WARRANTY AND REMEDIES SET FORTH ABOVE ARE EXCLUSIVE
AND IN LIEU OF ALL OTHERS, ORAL OR WRITTEN, EXPRESS OR
IMPLIED.

***********************************************************************/
#ifndef _MACFONT_H_
#define _MACFONT_H_

#include <types.h>
#include <memory.h>
#include <quickdraw.h>
#include <toolutils.h>
#include <osutils.h>

#define FontRec MacFontRec
#include <fonts.h>
#undef FontRec

typedef struct _MacBitmapFontRec {
	GrafPtr pgp;
	FMetricRec *pfm;
	int avgWidth;
	int xHeight;
	int quadWidth;
	int weight;
	Boolean forceMono;
	Boolean privateEncoding;
} MacBitmapFontRec, *MacBitmapFontRecPtr;

#endif                          /* _MACFONT_H_ */

