/*
 * timestep.c
 *   xgas: Copyright 1990 Larry Medwin: @(#)timestep.c	1.1 2/9/90
 */

#include "xgas.h"

/*
 * DO TIMESTEP
 *
 * do the calculations required for one timestep and
 *    replace this routine on the event handler "callback list."
 */
void doTimestep( data, id)	/* ARGSUSED */
    LabData *data;
    XtIntervalId *id;
{
    oneTimestep( data->lab, data);
    
    /* Put back callback (see Young's clock example) */
    data->timer =
	XtAppAddTimeOut(XtWidgetToApplicationContext(data->lab),
			(unsigned long)data->delay, 
			(XtTimerCallbackProc)doTimestep, (XtPointer)data);
}

/*
 *  ONE TIMESTEP
 *    used as callback for "step" pushbutton and called by doTimestep()
 */
void oneTimestep( w, data)	/* ARGSUSED */
    Widget w;
    LabData *data;
{
    int i, k;
    Arg wargs[1];
    char str[20];

    /* MOVE MOLECULES */
    for( i=0; i<data->nmolecules; i++) {

	/* Compute next position */
	dynamics( &data->molecules[i], data);
    }

    /*
     *  DRAW MOLECULES with XOR:
     *    Draw new molecule positions and erase old positions
     *        at the same time.
     *    Draw 2*nmolecules rectangles:
     *      allPos[i][timestep%2] are the current positions
     *        (save them)
     *      allPos[i][(timestep+1)%2] are the old positions
     *        (draw the new rectangles here)
     */

    /* Which half of allPos array gets the new positions? */
    k = (data->timestep + 1) % 2;

    /* Copy molecules to allPos array */
    for( i=0; i<data->nmolecules; i++) {
	data->allPos[ 2*i + k].x
	    = (short) data->molecules[i].pos.x * data->scale.x;
	data->allPos[ 2*i + k].y
	    = (short) data->molecules[i].pos.y * data->scale.y;
    }

    /* Now draw all 2*nmolecule rectangles */
    XFillRectangles( XtDisplay(data->lab), XtWindow(data->lab),
	data->MoleculeGC, (XRectangle *) data->allPos,
	2 * data->nmolecules);

    /* Advance time */
    data->timestep++;
    data->time = data->timestep * data->timestepSize;

    /* Make new time string */
    sprintf( str, "%6.3f msec", 1.0e-3 * data->time);

    /* Tell the clock widget */
    XtSetArg( wargs[0], XtNlabel, str);
    XtSetValues( data->clock, wargs, 1);
}

/* RUN and PAUSE CALLBACKS */
void run_callback(w, data, call_data) /* ARGSUSED */
     Widget     w;
     LabData    *data;
     caddr_t    call_data;
{
     if(! data->timer)
	data->timer =
	   XtAppAddTimeOut(XtWidgetToApplicationContext(w),
			   (unsigned long)data->delay,
			   (XtTimerCallbackProc)doTimestep,(XtPointer)data);
}

void pause_callback(w, data, call_data)	/* ARGSUSED */
     Widget     w;
     LabData    *data;
     caddr_t    call_data;
{
     if(data->timer) {
	XtRemoveTimeOut( data->timer);
	data->timer = 0;
     }
}
