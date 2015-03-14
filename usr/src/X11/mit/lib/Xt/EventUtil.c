/* $XConsortium: EventUtil.c,v 1.9 91/06/13 18:15:27 converse Exp $ */

/********************************************************

Copyright (c) 1988 by Hewlett-Packard Company
Copyright (c) 1987, 1988, 1989 by Digital Equipment Corporation, Maynard, 
              Massachusetts, and the Massachusetts Institute of Technology, 
              Cambridge, Massachusetts

Permission to use, copy, modify, and distribute this software 
and its documentation for any purpose and without fee is hereby 
granted, provided that the above copyright notice appear in all 
copies and that both that copyright notice and this permission 
notice appear in supporting documentation, and that the names of 
Hewlett-Packard, Digital or  M.I.T.  not be used in advertising or 
publicity pertaining to distribution of the software without specific, 
written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

#include "IntrinsicI.h"
#include "PassivGraI.h"
#include "StringDefs.h"

static XContext 	perWidgetInputContext = 0;

void _XtFreePerWidgetInput(w, pwi)
    Widget 		w;
    XtPerWidgetInput	pwi;
{
    XDeleteContext(XtDisplay(w), 
		   (Window)w,
		   perWidgetInputContext);
    
    XtFree((char *)pwi);
}

/*
 * This routine gets the passive list associated with the widget
 * from the context manager.
 */
#if NeedFunctionPrototypes
XtPerWidgetInput _XtGetPerWidgetInput(
    Widget	widget,
    _XtBoolean	create
    )
#else
XtPerWidgetInput _XtGetPerWidgetInput(widget, create)
    Widget	widget;
    Boolean	create;
#endif
{
    XtPerWidgetInput	pwi = NULL;
    Display		*dpy = widget->core.screen->display;
    
    if (! perWidgetInputContext)
      perWidgetInputContext = XUniqueContext();
    
    if (XFindContext(dpy, 
		     (Window)widget, 
		     perWidgetInputContext, 
		     (XPointer *)&pwi) &&
	create) 
      {
	  pwi = (XtPerWidgetInput) 
	    XtMalloc((unsigned) sizeof(XtPerWidgetInputRec));
	  
	  pwi->focusKid = NULL;
	  pwi->queryEventDescendant = NULL;
	  pwi->focalPoint = XtUnrelated;
	  pwi->keyList =
	    pwi->ptrList = NULL;

	  pwi->haveFocus =
	      pwi->map_handler_added =
		  pwi->realize_handler_added = 
		      pwi->active_handler_added = FALSE;
	  
	  XtAddCallback(widget, XtNdestroyCallback, 
			_XtDestroyServerGrabs, (XtPointer)pwi);

	  (void) XSaveContext(dpy, 
			      (Window)widget, 
			      perWidgetInputContext, 
			      (char *) pwi);
      }
    return pwi;
}


void _XtFillAncestorList(listPtr, maxElemsPtr, numElemsPtr, start, breakWidget)
    Widget	**listPtr;
    int		*maxElemsPtr, *numElemsPtr;
    Widget	start, breakWidget;
{
#define CACHESIZE 16
    Cardinal	i;
    Widget	w;
    Widget	*trace = *listPtr;

    /* First time in, allocate the ancestor list */
    if (trace == NULL) 
      {
	  trace = (Widget *) XtMalloc(CACHESIZE * sizeof(Widget));
	  *maxElemsPtr = CACHESIZE;
      }	
    /* First fill in the ancestor list */

    trace[0] = start;

    for (i = 1, w = XtParent(start); 
	 w != NULL && !XtIsShell(trace[i-1]) && trace[i-1] != breakWidget; 
	 w = XtParent(w), i++) {
	if (i == *maxElemsPtr) {
	    /* This should rarely happen, but if it does it'll probably
	       happen again, so grow the ancestor list */
	    *maxElemsPtr += CACHESIZE;
	    trace = (Widget *) XtRealloc((char*)trace,
					 sizeof(Widget) * (*maxElemsPtr));
	}
	trace[i] = w;
    }
    *listPtr = trace;
    *numElemsPtr = i;
#undef CACHESIZE
}  


Widget _XtFindRemapWidget(event, widget, mask, pdi)
    XEvent	*event;
    Widget	widget;
    EventMask	mask;
    XtPerDisplayInput pdi;
{
    Widget		dspWidget = widget;
    
    if (!pdi->traceDepth || !(widget == pdi->trace[0]))
      {
	  _XtFillAncestorList(&pdi->trace, &pdi->traceMax, 
			      &pdi->traceDepth, widget, NULL);
	  pdi->focusWidget = NULL; /* invalidate the focus
				      cache */
      }
    if (mask & (KeyPressMask | KeyReleaseMask))
	  dspWidget = _XtProcessKeyboardEvent((XKeyEvent*)event, widget, pdi);
    else if (mask &(ButtonPressMask | ButtonReleaseMask))
	  dspWidget = _XtProcessPointerEvent((XButtonEvent*)event, widget,pdi);

    return dspWidget;
}

void _XtUngrabBadGrabs(event, widget, mask, pdi)
    XEvent	*event;
    Widget	widget;
    EventMask	mask;
    XtPerDisplayInput pdi;
{
    XKeyEvent	* ke = (XKeyEvent *) event;

    if (mask & (KeyPressMask | KeyReleaseMask))
      {
	  if (IsServerGrab(pdi->keyboard.grabType) &&
	      !_XtOnGrabList(pdi->keyboard.grab.widget,
			     pdi->grabList))
	    XtUngrabKeyboard(widget, ke->time);

      }
    else
      {
	  if (IsServerGrab(pdi->pointer.grabType) &&
	      !_XtOnGrabList(pdi->pointer.grab.widget,
			     pdi->grabList))
	    XtUngrabPointer(widget, ke->time);
      }
}
