/* $XConsortium: Pointer.c,v 1.4 91/08/21 16:39:18 converse Exp $ */

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


#define AllButtonsMask (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)

Widget _XtProcessPointerEvent(event, widget, pdi)
    XButtonEvent  	*event;
    Widget 		widget;
    XtPerDisplayInput 	pdi;
{    
    XtDevice		device = &pdi->pointer;
    XtServerGrabPtr	newGrab = NULL, devGrab = &device->grab;
    Widget		dspWidget = NULL;
    Boolean		deactivateGrab = FALSE;

    switch (event->type)
      {
	case ButtonPress:
	  {
	      if (!IsServerGrab(device->grabType))
		{
		    Cardinal		i;

		    for (i = pdi->traceDepth; 
			 i > 0 && !newGrab; 
			 i--)
		      newGrab = _XtCheckServerGrabsOnWidget((XEvent*)event, 
							    pdi->trace[i-1],
							    POINTER);
		}
	      if (newGrab)
		{
		    /* Activate the grab */
		    device->grab = *newGrab;
		    device->grabType = XtPassiveServerGrab;
		}
	  }
	  break;
	  
	case ButtonRelease:
	  {
	      if ((device->grabType == XtPassiveServerGrab) && 
		  !(event->state & ~(Button1Mask << (event->button - 1)) &
		    AllButtonsMask))
		deactivateGrab = TRUE;
	  }
	  break;
      }
    
    if (IsServerGrab(device->grabType) && !(devGrab)->ownerEvents)
      dspWidget = (devGrab)->widget;
    else
      dspWidget = widget;
    
    if (deactivateGrab)
      device->grabType = XtNoServerGrab;

    return dspWidget;
}
