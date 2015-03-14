/***********************************************************
Copyright 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
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

/* $XConsortium: mispans.h,v 5.0 89/11/05 13:52:39 rws Exp $ */

typedef struct {
    int         count;		/* number of spans		    */
    DDXPointPtr points;		/* pointer to list of start points  */
    int         *widths;	/* pointer to list of widths	    */
} Spans;

typedef struct {
    int		size;		/* Total number of *Spans allocated	*/
    int		count;		/* Number of *Spans actually in group   */
    Spans       *group;		/* List of Spans			*/
    int		ymin, ymax;	/* Min, max y values encountered	*/
} SpanGroup;

/* Initialize SpanGroup.  MUST BE DONE before use. */
extern void miInitSpanGroup(/* spanGroup */);

/* Add a Spans to a SpanGroup.  The spans MUST BE in y-sorted order */
extern void miAppendSpans(/* spanGroup, spans */);

/* Paint a span group, possibly with some overlap */
extern void miFillSpanGroup(/* spanGroup */);

/* Paint a span group, insuring that each pixel is painted at most once */
extern void miFillUniqueSpanGroup(/* spanGroup */);

/* Free up data in a span group.  MUST BE DONE or you'll suffer memory leaks */
extern void miFreeSpanGroup(/* spanGroup */);
