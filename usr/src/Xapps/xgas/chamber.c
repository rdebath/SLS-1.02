/*
 * chamber.c -- Larry Medwin -- Dec. 18, 1989
 *   xgas: Copyright 1990 Larry Medwin: @(#)chamber.c	1.5 2/9/90
 *   Larry Medwin -- Dec. 18, 1989, April 14, 1991
 */

#include "xgas.h"
static void initWall();
static void getLabDimensions();

/*
 * Create Graphics Contexts
 * Initialize client data structure
 * Initialize allPos array of XRectangles
 */
void labInit(w, data)
    Widget w;
    LabData *data;
{
    int i, k;
    Display *dpy = XtDisplay(w);
    int scr = DefaultScreen(dpy);
    unsigned long valuemask;
    XGCValues values;

    /* Create GC for walls */
    data->WallGC = XCreateGC( dpy, DefaultRootWindow(dpy), NULL, NULL);
    XSetForeground( dpy, data->WallGC, data->foreground);

    /* Create GC for molecules */
    valuemask = GCFunction | GCPlaneMask | GCForeground | GCBackground;
    values.function = GXxor;
    values.plane_mask = values.foreground =
	data->background^data->foreground;
    values.background = data->background;
    data->MoleculeGC = XCreateGC( dpy, DefaultRootWindow(dpy),
    	valuemask, &values);

    /* Initialize the client data structure */
    data->lab = w;
    data->nmolecules = 0;
    data->timestep = 0;
    data->timer = 0;
    data->time = 0;
    
    for (i=0; i<2; i++) data->chamber[i].temperature = (float)INITTEMP;

    /* Initialize the allPos array of molecule positions */
    for( i=0; i<data->maxMolecules; i++) {
	for( k=0; k<2; k++) {
	    data->allPos[2*i + k].width = (unsigned short) MOLECULE_SIZE;
	    data->allPos[2*i + k].height = (unsigned short) MOLECULE_SIZE;
        }
    }

}

/*
 * LAB RESIZE -- handle resize events.  Makeing this a callback was the
 *               sole reason for creating the gas widget.
 */
void labResize(w, data, call_data) /* ARGSUSED */
    Widget w;
    LabData *data;
    caddr_t call_data;
{
    Coord p[8];

    /*
     * Have to clear out the array of molecules, since some of them
     * may be outside the walls or in the other box.
     */
    data->nmolecules = 0;

    /* Get dimensions of window in cm */
    getLabDimensions( w, data);

    /* Init wall endpoints */
    p[0].x = 0.0;		p[0].y = 0.0;
    p[1].x = 0.0;		p[1].y = data->heightMM;
    p[2].x = data->widthMM/2.0;	p[2].y = 0.0;
    p[3].x = data->widthMM/2.0;	p[3].y = data->heightMM;
    p[4].x = data->widthMM;	p[4].y = 0.0;
    p[5].x = data->widthMM;	p[5].y = data->heightMM;
    p[6].x = data->widthMM/2.0;	p[6].y = 0.4 * data->heightMM;
    p[7].x = data->widthMM/2.0;	p[7].y = 0.6 * data->heightMM;

    initWall( &data->chamber[0].walls[0], p[6], p[7], RIGHT);
    initWall( &data->chamber[0].walls[1], p[2], p[6], RIGHT);
    initWall( &data->chamber[0].walls[2], p[0], p[2], TOP);
    initWall( &data->chamber[0].walls[3], p[0], p[1], LEFT);
    initWall( &data->chamber[0].walls[4], p[1], p[3], BOTTOM);
    initWall( &data->chamber[0].walls[5], p[7], p[3], RIGHT);

    initWall( &data->chamber[1].walls[0], p[6], p[7], LEFT);
    initWall( &data->chamber[1].walls[1], p[2], p[6], LEFT);
    initWall( &data->chamber[1].walls[2], p[2], p[4], TOP);
    initWall( &data->chamber[1].walls[3], p[4], p[5], RIGHT);
    initWall( &data->chamber[1].walls[4], p[3], p[5], BOTTOM);
    initWall( &data->chamber[1].walls[5], p[7], p[3], LEFT);
}

/* INIT WALL */
static void initWall( wall, end0, end1, type)
    Wall *wall;
    Coord end0, end1;
    int type;
{
    wall->end[0].x = end0.x;
    wall->end[0].y = end0.y;
    wall->end[1].x = end1.x;
    wall->end[1].y = end1.y;
    wall->type = type;
}

/*
 * WHICH CORNER
 *    return corner type or 0 if not a corner
 */
int whichCorner( x, y, box, data )
    int x, y, box;
    LabData *data;
{
int xmid = data->chamber[0].walls[0].end[0].x;
int xend = data->widthMM;
int ybot = data->heightMM;

    /* Check for each box: */
    if (box == 0) {

	/* Check all four corner locations */
	if (y == 0 && x == 0) return NW;
	if (y == 0 && x == xmid) return NE;
	if (y == ybot && x == 0) return SW;
	if (y == ybot && x == xmid) return SE;

	/* Guess it's not a corner */
	return 0;
    }
    else { /* box = 1 */
	/* Check all four corner locations */
	if (y == 0 && x == xmid) return NW;
	if (y == 0 && x == xend) return NE;
	if (y == ybot && x == xmid) return SW;
	if (y == ybot && x == xend) return SE;

	/* Not a corner */
	return 0;
    }
}

/* GET LAB DIMENSIONS */
static void getLabDimensions( w, data)
    Widget w;
    LabData *data;
{
    Arg wargs[2];
    int wPix, hPix;
    float wMM, hMM;

    /* Get dimensions of lab Widget */
    XtSetArg( wargs[0], XtNwidth, &data->width);
    XtSetArg( wargs[1], XtNheight, &data->height);
    XtGetValues( w, wargs, 2);

    /* Decrement height and width so that we can draw on these borders */
    data->width --;
    data->height --;

    /* Get scale factor in pixels/cm */
    wPix = DisplayWidth( XtDisplay(w), DefaultScreen(XtDisplay(w)));
    wMM = DisplayWidthMM( XtDisplay(w), DefaultScreen(XtDisplay(w)));
    hPix = DisplayHeight( XtDisplay(w), DefaultScreen(XtDisplay(w)));
    hMM = DisplayHeightMM( XtDisplay(w), DefaultScreen(XtDisplay(w)));

    data->scale.x = (int) ( wPix / wMM);
    data->scale.y = (int) ( hPix / hMM);

    /* Get dimensions of Chamber in mm */
    data->widthMM = (float)data->width / (float)data->scale.x;
    data->heightMM = (float)data->height / (float)data->scale.y;
}

/*
 * LAB EXPOSE -- handle expose events
 *
 * drawing molecules with XOR
 *   This event handler must guarantee that molecules are drawn
 *   exactly once in their current positions, so that they will
 *   be properly erased by the next XOR.
 */
void labExpose( w, data, event)	/* ARGSUSED */
    Widget w;
    LabData *data;
    XEvent *event;
{
    int i, j;

    /*
     * Clear the window first
     *   to make sure that molecules are drawn an even # of times
     *   on an exposed window
     */
    XClearWindow( XtDisplay(w), XtWindow(w));

    /* Redraw walls */
    for( i=0; i<2; i++) {

	/* Draw walls of Chamber[i] */
	for( j=1; j<NWALLS; j++) {
	    XDrawLine( XtDisplay(w), XtWindow(w), data->WallGC,
	        (int) (data->chamber[i].walls[j].end[0].x * data->scale.x),
	        (int) (data->chamber[i].walls[j].end[0].y * data->scale.y),
	        (int) (data->chamber[i].walls[j].end[1].x * data->scale.x),
	        (int) (data->chamber[i].walls[j].end[1].y * data->scale.y));
        }
    }

    /* Redraw molecules */
    for( i=0; i<data->nmolecules; i++ ) {
	XFillRectangle( XtDisplay(w), XtWindow(w), data->MoleculeGC,
	    (int) data->allPos[2*i + data->timestep % 2 ].x,
	    (int) data->allPos[2*i + data->timestep % 2 ].y,
	    (int) MOLECULE_SIZE, (int) MOLECULE_SIZE);
    }
}
