/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 *
 * $XConsortium: events.h,v 1.14 91/05/10 17:53:58 dave Exp $
 *
 * twm event handler include file
 *
 * 17-Nov-87 Thomas E. LaStrange		File created
 *
 ***********************************************************************/

#ifndef _EVENTS_
#define _EVENTS_

typedef void (*event_proc)();

extern void InitEvents();
extern Bool StashEventTime();
extern Time lastTimestamp;
extern void SimulateMapRequest();
extern void AutoRaiseWindow();
#define LastTimestamp() lastTimestamp
extern Bool DispatchEvent();
extern Bool DispatchEvent2();
extern void HandleEvents();
extern void HandleExpose();
extern void HandleDestroyNotify();
extern void HandleMapRequest();
extern void HandleMapNotify();
extern void HandleUnmapNotify();
extern void HandleMotionNotify();
extern void HandleButtonRelease();
extern void HandleButtonPress();
extern void HandleEnterNotify();
extern void HandleLeaveNotify();
extern void HandleConfigureRequest();
extern void HandleClientMessage();
extern void HandlePropertyNotify();
extern void HandleKeyPress();
extern void HandleColormapNotify();
extern void HandleVisibilityNotify();
extern void HandleUnknown();

extern event_proc EventHandler[];
extern Window DragWindow;
extern int origDragX;
extern int origDragY;
extern int DragX;
extern int DragY;
extern int DragWidth;
extern int DragHeight;
extern int CurrentDragX;
extern int CurrentDragY;

extern int ButtonPressed;
extern int Cancel;

extern XEvent Event;

#endif /* _EVENTS_ */
