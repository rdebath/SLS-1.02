/* $XConsortium: SetValues.c,v 1.15 92/05/22 09:50:27 rws Exp $ */

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

#include "IntrinsicI.h"
#include "StringDefs.h"

/*
 *	XtSetValues(), XtSetSubvalues()
 */


extern void _XtCopyFromArg();
extern XrmResourceList* _XtCreateIndirectionTable();

static void SetValues(base, res, num_resources, args, num_args)
  char*			base;		/* Base address to write values to   */
  XrmResourceList*	res;		/* The current resource values.      */
  register Cardinal	num_resources;	/* number of items in resources      */
  ArgList 		args;		/* The resource values to set        */
  Cardinal		num_args;	/* number of items in arg list       */
{
    register ArgList		arg;
    register int 	        i;
    register XrmName		argName;
    register XrmResourceList*   xrmres;

    /* Resource lists are assumed to be in compiled form already via the
       initial XtGetResources, XtGetSubresources calls */

    for (arg = args ; num_args != 0; num_args--, arg++) {
	argName = StringToName(arg->name);
	for (xrmres = res, i = 0; i < num_resources; i++, xrmres++) {
	    if (argName == (*xrmres)->xrm_name) {
		_XtCopyFromArg(arg->value,
		    base - (*xrmres)->xrm_offset - 1,
		    (*xrmres)->xrm_size);
		break;
	    }
	}
    }
} /* SetValues */

static Boolean CallSetValues (class, current, request, new, args, num_args)
    WidgetClass class;
    Widget      current, request, new;
    ArgList     args;
    Cardinal    num_args;
{
    Boolean redisplay = FALSE;

    if (class->core_class.superclass != NULL)
        redisplay = CallSetValues(
	  class->core_class.superclass, current, request, new, args, num_args);
    if (class->core_class.set_values != NULL)
        redisplay |= (*class->core_class.
		      set_values) (current, request, new, args, &num_args);
    if (class->core_class.set_values_hook != NULL)
	redisplay |=
	    (*class->core_class.set_values_hook) (new, args, &num_args);
    return (redisplay);
}

static Boolean
CallConstraintSetValues (class, current, request, new, args, num_args)
    ConstraintWidgetClass class;
    Widget      current, request, new;
    ArgList     args;
    Cardinal    num_args;
{
    Boolean redisplay = FALSE;

    if ((WidgetClass)class != constraintWidgetClass) {
	if (class == NULL)
	    XtAppErrorMsg(XtWidgetToApplicationContext(current),
		    "invalidClass","constraintSetValue",XtCXtToolkitError,
                 "Subclass of Constraint required in CallConstraintSetValues",
                  (String *)NULL, (Cardinal *)NULL);
	redisplay = CallConstraintSetValues(
	    (ConstraintWidgetClass) (class->core_class.superclass),
	    current, request, new, args, num_args);
    }
    if (class->constraint_class.set_values != NULL)
        redisplay |= (*class->constraint_class.
		      set_values) (current, request, new, args, &num_args);
    return (redisplay);
}

void XtSetSubvalues(base, resources, num_resources, args, num_args)
  XtPointer             base;           /* Base address to write values to   */
  register XtResourceList resources;    /* The current resource values.      */
  register Cardinal     num_resources;  /* number of items in resources      */
  ArgList               args;           /* The resource values to set        */
  Cardinal              num_args;       /* number of items in arg list       */
{
      register XrmResourceList*   xrmres;
      xrmres = _XtCreateIndirectionTable (resources, num_resources);
      SetValues((char*)base,xrmres,num_resources, args, num_args);
      XtFree((char *)xrmres);
}


void XtSetValues(w, args, num_args)
    register Widget   w;
	     ArgList  args;
	     Cardinal num_args;
{
    register Widget oldw, reqw;
    /* need to use strictest alignment rules possible in next two decls. */
    double	    oldwCache[100], reqwCache[100];
    double	    oldcCache[20], reqcCache[20];
    Cardinal	    widgetSize, constraintSize;
    Boolean	    redisplay, cleared_rect_obj = False;
    XtGeometryResult result;
    XtWidgetGeometry geoReq, geoReply;
    WidgetClass     wc = XtClass(w);
    ConstraintWidgetClass cwc;
    Boolean	    hasConstraints;

    if ((args == NULL) && (num_args != 0)) {
        XtAppErrorMsg(XtWidgetToApplicationContext(w),
		"invalidArgCount","xtSetValues",XtCXtToolkitError,
                "Argument count > 0 on NULL argument list in XtSetValues",
                 (String *)NULL, (Cardinal *)NULL);
    }

    /* Allocate and copy current widget into old widget */

    widgetSize = wc->core_class.widget_size;
    oldw = (Widget) XtStackAlloc(widgetSize, oldwCache);
    reqw = (Widget) XtStackAlloc (widgetSize, reqwCache);
    bcopy((char *) w, (char *) oldw, (int) widgetSize);

    /* Set resource values */

    SetValues((char*)w, (XrmResourceList *) wc->core_class.resources,
	wc->core_class.num_resources, args, num_args);

    bcopy ((char *) w, (char *) reqw, (int) widgetSize);

    /* assert: !XtIsShell(w) => (XtParent(w) != NULL) */
    hasConstraints = (!XtIsShell(w) && XtIsConstraint(XtParent(w)));

    /* Some widget sets apparently do ugly things by freeing the
     * constraints on some children, thus the extra test here */
    if (hasConstraints) {
	cwc = (ConstraintWidgetClass) XtClass(w->core.parent);
	if (w->core.constraints)
	    constraintSize = cwc->constraint_class.constraint_size;
	else constraintSize = 0;
    } else constraintSize = 0;
	
    if (constraintSize) {
	/* Allocate and copy current constraints into oldw */
	oldw->core.constraints = XtStackAlloc(constraintSize, oldcCache);
	reqw->core.constraints = XtStackAlloc(constraintSize, reqcCache);
	bcopy((char *) w->core.constraints, 
		(char *) oldw->core.constraints, (int) constraintSize);

	/* Set constraint values */
	SetValues((char*)w->core.constraints,
	    (XrmResourceList *)(cwc->constraint_class.resources),
	    cwc->constraint_class.num_resources, args, num_args);
	bcopy((char *) w->core.constraints,
	      (char *) reqw->core.constraints, (int) constraintSize);
    }

    /* Inform widget of changes, then inform parent of changes */
    redisplay = CallSetValues (wc, oldw, reqw, w, args, num_args);
    if (hasConstraints) {
	redisplay |= CallConstraintSetValues(cwc, oldw, reqw, w, args, num_args);
    }

    if (XtIsRectObj(w)) {
	/* Now perform geometry request if needed */
	geoReq.request_mode = 0;
	if (oldw->core.x	!= w->core.x) {
	    geoReq.x		= w->core.x;
	    w->core.x		= oldw->core.x;
	    geoReq.request_mode |= CWX;
	}
	if (oldw->core.y	!= w->core.y) {
	    geoReq.y		= w->core.y;
	    w->core.y		= oldw->core.y;
	    geoReq.request_mode |= CWY;
	}
	if (oldw->core.width	!= w->core.width) {
	    geoReq.width	= w->core.width;
	    w->core.width	= oldw->core.width;
	    geoReq.request_mode |= CWWidth;
	}
	if (oldw->core.height	!= w->core.height) {
	    geoReq.height	= w->core.height;
	    w->core.height	= oldw->core.height;
	    geoReq.request_mode |= CWHeight;
	}
	if (oldw->core.border_width != w->core.border_width) {
	    geoReq.border_width	    = w->core.border_width;
	    w->core.border_width    = oldw->core.border_width;
	    geoReq.request_mode	    |= CWBorderWidth;
	}
    
	if (geoReq.request_mode != 0) {
	    /* Pass on any requests for unchanged geometry values */
	    if (geoReq.request_mode !=
		(CWX | CWY | CWWidth | CWHeight | CWBorderWidth)) {
		for ( ; num_args != 0; num_args--, args++) {
		    if (! (geoReq.request_mode & CWX) &&
			strcmp(XtNx, args->name) == 0) {
			geoReq.x = w->core.x;
			geoReq.request_mode |= CWX;
		    } else if (! (geoReq.request_mode & CWY) &&
			       strcmp(XtNy, args->name) == 0) {
			geoReq.y = w->core.y;
			geoReq.request_mode |= CWY;
		    } else if (! (geoReq.request_mode & CWWidth) &&
			       strcmp(XtNwidth, args->name) == 0) {
			geoReq.width = w->core.width;
			geoReq.request_mode |= CWWidth;
		    } else if (! (geoReq.request_mode & CWHeight) &&
			       strcmp(XtNheight, args->name) == 0) {
			geoReq.height = w->core.height;
			geoReq.request_mode |= CWHeight;
		    } else if (! (geoReq.request_mode & CWBorderWidth) &&
			       strcmp(XtNborderWidth, args->name) == 0) {
			geoReq.border_width = w->core.border_width;
			geoReq.request_mode |= CWBorderWidth;
		    }
		}
	    }
	    do {
		result = _XtMakeGeometryRequest(w, &geoReq, &geoReply, 
						&cleared_rect_obj);
		if (result == XtGeometryYes || result == XtGeometryDone)
		    break;

		/* An Almost or No reply.  Call widget and let it munge
		   request, reply */
		if (wc->core_class.set_values_almost == NULL) {
		    XtAppWarningMsg(XtWidgetToApplicationContext(w),
			    "invalidProcedure","set_values_almost",
			  XtCXtToolkitError,
			  "set_values_almost procedure shouldn't be NULL",
			  (String *)NULL, (Cardinal *)NULL);
		    break;
		}
		if (result == XtGeometryNo) geoReply.request_mode = 0;
		(*(wc->core_class.set_values_almost))
		    (oldw, w, &geoReq, &geoReply);
	    } while (geoReq.request_mode != 0);
	    /* call resize proc if we changed size and parent
	     * didn't already invoke resize */
	    if ((w->core.width != oldw->core.width ||
		 w->core.height != oldw->core.height)
		&& result != XtGeometryDone
		&& wc->core_class.resize != (XtWidgetProc) NULL) {
		(*(wc->core_class.resize))(w);
	    }
	}
	/* Redisplay if needed.  No point in clearing if the window is
	 * about to disappear, as the Expose event will just go straight
	 * to the bit bucket. */
        if (XtIsWidget(w)) {
            /* widgets can distinguish between redisplay and resize, since
             the server will cause an expose on resize */
            if (redisplay && XtIsRealized(w) && !w->core.being_destroyed)
                XClearArea (XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
        } else { /*non-window object */
	  if (redisplay && ! cleared_rect_obj ) {
	      Widget pw = _XtWindowedAncestor(w);
	      if (XtIsRealized(pw) && !pw->core.being_destroyed) {
		  RectObj r = (RectObj)w;
		  int bw2 = r->rectangle.border_width << 1;
		  XClearArea (XtDisplay (pw), XtWindow (pw),
		      r->rectangle.x, r->rectangle.y,
		      r->rectangle.width + bw2,r->rectangle.height + bw2,TRUE);
	      }
	  }
        }
    }


    /* Free dynamic storage */
    if (constraintSize) {
        XtStackFree(oldw->core.constraints, oldcCache);
        XtStackFree(reqw->core.constraints, reqcCache);
    }
    XtStackFree((XtPointer)oldw, oldwCache);
    XtStackFree((XtPointer)reqw, reqwCache);

} /* XtSetValues */
