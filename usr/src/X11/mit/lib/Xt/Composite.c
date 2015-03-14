/* $XConsortium: Composite.c,v 1.18 91/04/04 20:52:53 converse Exp $ */

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

#define COMPOSITE
#include "IntrinsicI.h"
#include "StringDefs.h"

static XtResource resources[] = {
    {XtNchildren, XtCReadOnly, XtRWidgetList, sizeof(WidgetList),
     XtOffsetOf(CompositeRec, composite.children), XtRImmediate, NULL},
    {XtNnumChildren, XtCReadOnly, XtRCardinal, sizeof(Cardinal),
     XtOffsetOf(CompositeRec, composite.num_children), XtRImmediate, 0},
    {XtNinsertPosition, XtCInsertPosition, XtRFunction, sizeof(XtOrderProc),
     XtOffsetOf(CompositeRec, composite.insert_position), XtRImmediate, NULL},
};

static void CompositeClassPartInitialize();
static void CompositeInitialize();
static void CompositeInsertChild();
static void CompositeDeleteChild();
static void CompositeDestroy();

externaldef(compositeclassrec) CompositeClassRec compositeClassRec = {
  { /******* CorePart *******/
    /* superclass	    */	&widgetClassRec,
    /* class_name	    */	"Composite",
    /* widget_size	    */	sizeof(CompositeRec),
    /* class_initialize     */  NULL,
    /* class_part_initialize*/	CompositeClassPartInitialize,
    /* class_inited	    */	FALSE,
    /* initialize	    */	CompositeInitialize,
    /* initialize_hook      */	NULL,		
    /* realize		    */	XtInheritRealize,
    /* actions		    */	NULL,
    /* num_actions	    */	0,
    /* resources	    */	resources,
    /* num_resources	    */	XtNumber(resources),
    /* xrm_class	    */	NULLQUARK,
    /* compress_motion      */	FALSE,
    /* compress_exposure    */	TRUE,
    /* compress_enterleave  */  FALSE,
    /* visible_interest     */	FALSE,
    /* destroy		    */	CompositeDestroy,
    /* resize		    */	NULL,
    /* expose		    */	NULL,
    /* set_values	    */	NULL,
    /* set_values_hook      */	NULL,			
    /* set_values_almost    */	XtInheritSetValuesAlmost,  
    /* get_values_hook      */	NULL,			
    /* accept_focus	    */	NULL,
    /* version		    */	XtVersion,
    /* callback_offsets     */  NULL,
    /* tm_table		    */  NULL,
    /* query_geometry	    */  NULL,
    /* display_accelerator  */	NULL,
    /* extension	    */  NULL
  },
  { /**** CompositePart *****/
    /* geometry_handler     */  NULL,
    /* change_managed       */  NULL,
    /* insert_child	    */  CompositeInsertChild,
    /* delete_child	    */  CompositeDeleteChild,
    /* extension	    */  NULL
    }
};

externaldef(compositewidgetclass) WidgetClass compositeWidgetClass = (WidgetClass) &compositeClassRec;

static void CompositeClassPartInitialize(widgetClass)
	WidgetClass widgetClass;
{
    register CompositePartPtr wcPtr;
    register CompositePartPtr superPtr;

    wcPtr = (CompositePartPtr)
	&(((CompositeWidgetClass)widgetClass)->composite_class);

    if (widgetClass != compositeWidgetClass)
	/* don't compute possible bogus pointer */
	superPtr = (CompositePartPtr)&(((CompositeWidgetClass)widgetClass
			->core_class.superclass)->composite_class);
#ifdef lint
    else
	superPtr = NULL;
#endif

    /* We don't need to check for null super since we'll get to composite
       eventually, and it had better define them!  */

    if (wcPtr->geometry_manager == XtInheritGeometryManager) {
	wcPtr->geometry_manager =
		superPtr->geometry_manager;
    }

    if (wcPtr->change_managed == XtInheritChangeManaged) {
	wcPtr->change_managed =
		superPtr->change_managed;
    }

    if (wcPtr->insert_child == XtInheritInsertChild) {
	wcPtr->insert_child = superPtr->insert_child;
    }

    if (wcPtr->delete_child == XtInheritDeleteChild) {
	wcPtr->delete_child = superPtr->delete_child;
    }

}

static void CompositeDestroy(w)
    CompositeWidget	w;
{
    XtFree((char *) w->composite.children);
}

static void CompositeInsertChild(w)
    Widget	w;
{
    register Cardinal	     position;
    register Cardinal        i;
    register CompositeWidget cw;
    register WidgetList      children;

    cw = (CompositeWidget) w->core.parent;
    children = cw->composite.children;

    if (cw->composite.insert_position != NULL)
	position = (*(cw->composite.insert_position))(w);
    else
	position = cw->composite.num_children;

    if (cw->composite.num_children == cw->composite.num_slots) {
	/* Allocate more space */
	cw->composite.num_slots +=  (cw->composite.num_slots / 2) + 2;
	cw->composite.children = children = 
	    (WidgetList) XtRealloc((XtPointer) children,
	    (unsigned) (cw->composite.num_slots) * sizeof(Widget));
    }
    /* Ripple children up one space from "position" */
    for (i = cw->composite.num_children; i > position; i--) {
	children[i] = children[i-1];
    }
    children[position] = w;
    cw->composite.num_children++;
}

static void CompositeDeleteChild(w)
    Widget	w;
{
    register Cardinal	     position;
    register Cardinal	     i;
    register CompositeWidget cw;

    cw = (CompositeWidget) w->core.parent;

    for (position = 0; position < cw->composite.num_children; position++) {
        if (cw->composite.children[position] == w) {
	    break;
	}
    }
    if (position == cw->composite.num_children) return;

    /* Ripple children down one space from "position" */
    cw->composite.num_children--;
    for (i = position; i < cw->composite.num_children; i++) {
        cw->composite.children[i] = cw->composite.children[i+1];
    }
}

/* ARGSUSED */
static void CompositeInitialize(requested_widget, new_widget, args, num_args)
    Widget   new_widget, requested_widget;
    ArgList args;
    Cardinal *num_args;
{
    register CompositeWidget cw;

    cw = (CompositeWidget) new_widget;
    cw->composite.num_children = 0;
    cw->composite.children = NULL;
    cw->composite.num_slots = 0;
}
