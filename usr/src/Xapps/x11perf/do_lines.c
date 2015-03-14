/*****************************************************************************
Copyright 1988, 1989 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************************/

#include "x11perf.h"

static XPoint   *points;
static GC       pgc;

int InitLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int size;
    int half;		/* Half of width if wide line		        */
    int i;
    int     rows;       /* Number of rows filled in current column      */
    int x, y;		/* Next point					*/
    int xdir, ydir;	/* Which direction x, y are going		*/
    int bigxdir;	
    int x1, y1;		/* offsets to compute next point from current	*/
    int phase;		/* how far into 0..4*(size_1) we are		*/
    float phasef;       /* how far we are in real numbers		*/
    float phaseinc;     /* how much to increment phasef at each segment */
    int size4;		/* 4 * size					*/

    pgc = xp->fggc;

    size = p->special;
    size4 = 4 * (size+1);
    half = (size + 19) / 20;

    points = (XPoint *)malloc((p->objects+1) * sizeof(XPoint));

    /* All this x, x1, x1inc, etc. stuff is to create a pattern that
	(1) scans down the screen vertically

	(2) rotates the endpoints through all possible orientations

	(3) bounces off bottom and top of window as needed

	(4) moves left or right at each bounce to make things less boring
    */

    x     = half;
    y     = half;
    xdir  = 1;
    ydir  = 1;
    bigxdir = 1;
    phasef = 0.0;
    phaseinc = ((float)size4) / ((float)p->objects);
    if (phaseinc < 1.0) phaseinc = 1.0;
    rows = 0;

    points[0].x = x;
    points[0].y = y;

    for (i = 1; i != (p->objects+1); i++) {    
	phase = phasef;
	switch (phase / (size+1)) {
	case 0:
	    x1 = size;			
	    y1 = phase;
	    break;

	case 1:
	    x1 = size - phase % (size+1);
	    y1 = size;
	    break;

	case 2:
	    x1 = phase % (size+1);
	    y1 = size;
	    break;

	case 3:
	    x1 = size;
	    y1 = size - phase % (size+1);
	    break;
	} /* end switch */

	/* Move down or up the screen */
	y += (ydir * y1);

	/* If off either top or bottom, backtrack to previous position and go
	   the other way instead.  Also move in bigxdir if not already. */
	rows++;
	if (y < half || y >= (HEIGHT-half) || rows > MAXROWS) {
	    rows = 0;
	    if (bigxdir > 0) {
		if (x + size < WIDTH - half) {
		    xdir = 1;
		} else {
		    bigxdir = -1;
		}
	    } else {
		if (x - size > half) {
		    xdir = -1;
		} else {
		    bigxdir = 1;
		}
	    }
	    ydir = -ydir;
	    y += (2 * ydir * y1);
	    /* If still off top or bottom, we can't do the line we want.
	       This will happen infrequently if the window is not twice the
	       length of the line.  So instead, let's draw a line that puts
	       the line after this approximately through the center of the
	       window.   Since it is very unlikely that both x and y will
	       need such adjustment, line length (in pixels) should not
	       change...we just can't get the slope we want for this line. */
	    if (y < half) {
		y = (HEIGHT - y1)/2;
		ydir = 1;
	    } else if (y > (HEIGHT - half)) {
		y = (HEIGHT + y1)/2;
		ydir = -1;
	    }
	}

	/* Move x left or right by x1 */
	x += (xdir * x1);
	xdir = -xdir;
	/* Again, if we are off the bottom then we can't really draw the line
	   we want.  */
	if (x < half) {
	    x = (WIDTH - x1)/2;
	    xdir = 1;
	} else if (x > (WIDTH - half)) {
	    x = (WIDTH + x1)/2;
	    xdir = -1;
	}
	points[i].x = x;
	points[i].y = y;

	/* Increment phasef */
	phasef += phaseinc;
	if (phasef >= size4) phasef -= size4;

    }
    return reps;
}
 
int InitWideLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int size;

    (void)InitLines(xp, p, reps);

    size = p->special;
    XSetLineAttributes(xp->d, xp->bggc, (int) ((size + 9) / 10),
	LineSolid, CapRound, JoinRound);
    XSetLineAttributes(xp->d, xp->fggc, (int) ((size + 9) / 10),
	LineSolid, CapRound, JoinRound);

    return reps;
}
 
int InitDashedLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    char dashes[2];

    (void)InitLines(xp, p, reps);

    /* Modify GCs to draw dashed */
    XSetLineAttributes(xp->d, xp->bggc, 0, LineOnOffDash, CapButt, JoinMiter);
    XSetLineAttributes(xp->d, xp->fggc, 0, LineOnOffDash, CapButt, JoinMiter);
    dashes[0] = 3;   dashes[1] = 2;
    XSetDashes(xp->d, xp->fggc, 0, dashes, 2);
    XSetDashes(xp->d, xp->bggc, 0, dashes, 2);
    return reps;
}

int InitWideDashedLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int		size;
    XGCValues   gcv;
    char	dashes[2];

    (void)InitWideLines(xp, p, reps);
    size = p->special;
    size = (size + 9) / 10;

    /* Modify GCs to draw dashed */
    dashes[0] = 2*size;   dashes[1] = 2*size;
    gcv.line_style = LineOnOffDash;
    XChangeGC(xp->d, xp->fggc, GCLineStyle, &gcv);
    XChangeGC(xp->d, xp->bggc, GCLineStyle, &gcv);
    XSetDashes(xp->d, xp->fggc, 0, dashes, 2);
    XSetDashes(xp->d, xp->bggc, 0, dashes, 2);
    return reps;
}

int InitDoubleDashedLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    char dashes[2];

    (void)InitLines(xp, p, reps);

    /* Modify GCs to draw dashed */
    XSetLineAttributes(xp->d, xp->bggc, 0, LineDoubleDash, CapButt, JoinMiter);
    XSetLineAttributes(xp->d, xp->fggc, 0, LineDoubleDash, CapButt, JoinMiter);
    dashes[0] = 3;   dashes[1] = 2;
    XSetDashes(xp->d, xp->fggc, 0, dashes, 2);
    XSetDashes(xp->d, xp->bggc, 0, dashes, 2);
    return reps;
}

int InitWideDoubleDashedLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int		size;
    XGCValues   gcv;
    char	dashes[2];

    (void)InitWideLines(xp, p, reps);
    size = p->special;
    size = (size + 9) / 10;

    /* Modify GCs to draw dashed */
    dashes[0] = 2*size;   dashes[1] = 2*size;
    gcv.line_style = LineDoubleDash;
    XChangeGC(xp->d, xp->fggc, GCLineStyle, &gcv);
    XChangeGC(xp->d, xp->bggc, GCLineStyle, &gcv);
    XSetDashes(xp->d, xp->fggc, 0, dashes, 2);
    XSetDashes(xp->d, xp->bggc, 0, dashes, 2);
    return reps;
}

void DoLines(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int i;

    for (i = 0; i != reps; i++)
    {
        XDrawLines(xp->d, xp->w, pgc, points, p->objects+1, CoordModeOrigin);
        if (pgc == xp->bggc)
            pgc = xp->fggc;
        else
            pgc = xp->bggc;
    }
}

void EndLines(xp, p)
    XParms  xp;
    Parms   p;
{
    free(points);
}

