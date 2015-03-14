/* 
 * tkGrab.c --
 *
 *	This file provides procedures that implement grabs for Tk.
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/wish/RCS/tkGrab.c,v 1.29 92/12/12 16:17:28 ouster Exp $ SPRITE (Berkeley)";
#endif

#include "tkConfig.h"
#include "tkInt.h"

/*
 * This module is ridiculously complicated... sorry about that.  The
 * problem is that the grabs provided by the X server aren't very
 * useful, so this module tries to create a more useful form of grab
 * using the X server's rudimentary facilities.  This requires us to
 * suppress some of the events coming from the X server and generate
 * many additional events.  Managing the events is hard because of the
 * asynchronous way that the event queue is processed.  The current
 * solution uses an extra "grab event queue" for each display, where
 * we put the event that we synthesize here.  Events in this queue
 * are processed before any events in the normal X queue.  The grab
 * event queue is needed (rather than just pushing events back on the
 * X queue) so that we can keep a clear boundary between the events
 * we've synthesized and those coming from the server, so that we can
 * add new events after all the other synthesized events but before
 * those from the server.
 */

/*
 * One of the following structures is kept for each event in the
 * grab queue maintained by this module.
 */

struct TkGrabEvent {
    XEvent event;			/* Event to process. */
    struct TkGrabEvent *nextPtr;	/* Next event in list, or NULL for
					 * end of list. */
};

/*
 * Bit definitions for grabFlags field of TkDisplay structures:
 *
 * GRAB_GLOBAL			1 means this is a global grab (we grabbed via
 *				the server so all applications are locked out).
 *				0 means this is a local grab that affects
 *				only this application.
 * GRAB_TRIGGER_QUEUED		1 means that there are events in the grab
 *				queue for this display and a fake event has
 *				been pushed to cause the events on the grab
 *				queue to be processed before any events on
 *				the regular X event queue.
 * GRAB_TEMP_GLOBAL		1 means we've temporarily grabbed via the
 *				server because a button is down and we want
 *				to make sure that we get the button-up
 *				event.  The grab will be released when the
 *				last mouse button goes up.
 */

#define GRAB_GLOBAL		1
#define GRAB_TRIGGER_QUEUED	2
#define GRAB_TEMP_GLOBAL	4

/*
 * The following magic value is stored in the "send_event" field of
 * EnterNotify and LeaveNotify events that are generated in this
 * file.  This allows us to separate "real" events coming from the
 * server from those that we generated.
 */

#define GENERATED_EVENT_MAGIC ((Bool) 0x147321ac)

/*
 * The following magic value stored in the "send_event" field of
 * an event identifies it as a special dummy event to trigger a
 * change in the grabWinPtr field of a display.  The "window"
 * field of the event actually contains a (TkWindow *) value to
 * put in grabWinPtr.
 */

#define GRAB_WINDOW_EVENT_MAGIC ((Bool) 0x347321ab)

/*
 * Mask that selects any of the state bits corresponding to buttons,
 * plus masks that select individual buttons' bits:
 */

#define ALL_BUTTONS \
	(Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask)
static unsigned int buttonStates[] = {
    Button1Mask, Button2Mask, Button3Mask, Button4Mask, Button5Mask
};

/*
 * Forward declarations for procedures declared later in this file:
 */

static void		ChangeEventWindow _ANSI_ARGS_((XEvent *eventPtr,
			    TkWindow *winPtr));
static void		EatGrabEvents _ANSI_ARGS_((TkDisplay *dispPtr,
			    unsigned int serial));
static TkWindow *	FindCommonAncestor _ANSI_ARGS_((TkWindow *winPtr1,
			    TkWindow *winPtr2, int *countPtr1,
			    int *countPtr2));
static void		MovePointer _ANSI_ARGS_((XEvent *eventPtr,
			    TkWindow *sourcePtr, TkWindow *destPtr,
			    int leaveEvents, int EnterEvents));
static void		MovePointer2 _ANSI_ARGS_((TkWindow *sourcePtr,
			    TkWindow *destPtr, int mode, int leaveEvents,
			    int EnterEvents));
static void		PushTriggerEvent _ANSI_ARGS_((TkDisplay *dispPtr));
static void		QueueEvent _ANSI_ARGS_((TkDisplay *dispPtr,
			    XEvent *eventPtr));
static void		QueueGrabWindowChange _ANSI_ARGS_((TkDisplay *dispPtr,
			    TkWindow *grabWinPtr));

/*
 *----------------------------------------------------------------------
 *
 * Tk_GrabCmd --
 *
 *	This procedure is invoked to process the "grab" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tk_GrabCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int globalGrab;
    Tk_Window tkwin;
    TkDisplay *dispPtr;
    int length;
    char c;

    if (argc < 2) {
	badArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?-global? window\" or \"", argv[0],
		" option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if (c == '.') {
	if (argc != 2) {
	    goto badArgs;
	}
	tkwin = Tk_NameToWindow(interp, argv[1], (Tk_Window) clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	return Tk_Grab(interp, tkwin, 0);
    } else if ((c == '-') && (strncmp(argv[1], "-global", length) == 0)
	    && (length >= 2)) {
	if (argc != 3) {
	    goto badArgs;
	}
	tkwin = Tk_NameToWindow(interp, argv[2], (Tk_Window) clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	return Tk_Grab(interp, tkwin, 1);
    } else if ((c == 'c') && (strncmp(argv[1], "current", length) == 0)) {
	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " current ?window?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (argc == 3) {
	    tkwin = Tk_NameToWindow(interp, argv[2], (Tk_Window) clientData);
	    if (tkwin == NULL) {
		return TCL_ERROR;
	    }
	    dispPtr = ((TkWindow *) tkwin)->dispPtr;
	    if (dispPtr->eventualGrabWinPtr != NULL) {
		interp->result = dispPtr->eventualGrabWinPtr->pathName;
	    }
	} else {
	    for (dispPtr = tkDisplayList; dispPtr != NULL;
		    dispPtr = dispPtr->nextPtr) {
		if (dispPtr->eventualGrabWinPtr != NULL) {
		    Tcl_AppendElement(interp,
			    dispPtr->eventualGrabWinPtr->pathName, 0);
		}
	    }
	}
	return TCL_OK;
    } else if ((c == 'r') && (strncmp(argv[1], "release", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " release window\"", (char *) NULL);
	    return TCL_ERROR;
	}
	tkwin = Tk_NameToWindow(interp, argv[2], (Tk_Window) clientData);
	if (tkwin == NULL) {
	    Tcl_ResetResult(interp);
	} else {
	    Tk_Ungrab(tkwin);
	}
    } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)
	    && (length >= 2)) {
	if (argc > 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " set ?-global? window\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (argc == 3) {
	    globalGrab = 0;
	    tkwin = Tk_NameToWindow(interp, argv[2], (Tk_Window) clientData);
	} else {
	    globalGrab = 1;
	    length = strlen(argv[2]);
	    if ((strncmp(argv[2], "-global", length) != 0) || (length < 2)) {
		Tcl_AppendResult(interp, "bad argument \"", argv[2],
			"\": must be \"", argv[0], " set ?-global? window\"",
			(char *) NULL);
		return TCL_ERROR;
	    }
	    tkwin = Tk_NameToWindow(interp, argv[3], (Tk_Window) clientData);
	}
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	return Tk_Grab(interp, tkwin, globalGrab);
    } else if ((c == 's') && (strncmp(argv[1], "status", length) == 0)
	    && (length >= 2)) {
	TkWindow *winPtr;

	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " status window\"", (char *) NULL);
	    return TCL_ERROR;
	}
	winPtr = (TkWindow *) Tk_NameToWindow(interp, argv[2],
		(Tk_Window) clientData);
	if (winPtr == NULL) {
	    return TCL_ERROR;
	}
	dispPtr = winPtr->dispPtr;
	if (dispPtr->eventualGrabWinPtr != winPtr) {
	    interp->result = "none";
	} else if (dispPtr->grabFlags & GRAB_GLOBAL) {
	    interp->result = "global";
	} else {
	    interp->result = "local";
	}
    } else {
	Tcl_AppendResult(interp, "unknown or ambiguous option \"", argv[1],
		"\": must be current, release, set, or status",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_Grab --
 *
 *	Grabs the pointer and keyboard, so that mouse-related events are
 *	only reported relative to a given window and its descendants.
 *
 * Results:
 *	A standard Tcl result is returned.  TCL_OK is the normal return
 *	value;  if the grab could not be set then TCL_ERROR is returned
 *	and interp->result will hold an error message.
 *
 * Side effects:
 *	Once this call completes successfully, no window outside the
 *	tree rooted at tkwin will receive pointer- or keyboard-related
 *	events until the next call to Tk_Ungrab.  If a previous grab was
 *	in effect within this application, then it is replaced with a new
 *	one.
 *
 *----------------------------------------------------------------------
 */

int
Tk_Grab(interp, tkwin, grabGlobal)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Window tkwin;			/* Window on whose behalf the pointer
					 * is to be grabbed. */
    int grabGlobal;			/* Non-zero means issue a grab to the
					 * server so that no other application
					 * gets mouse or keyboard events.
					 * Zero means the grab only applies
					 * within this application. */
{
    int grabResult;
    TkWindow *winPtr = (TkWindow *) tkwin;
    TkDisplay *dispPtr = winPtr->dispPtr;
    TkWindow *winPtr2;
    unsigned int serial;

    if (dispPtr->eventualGrabWinPtr != NULL) {
	if ((dispPtr->eventualGrabWinPtr == winPtr)
		&& (grabGlobal == ((dispPtr->grabFlags & GRAB_GLOBAL) != 0))) {
	    return TCL_OK;
	}
	if (dispPtr->eventualGrabWinPtr->mainPtr != winPtr->mainPtr) {
	    alreadyGrabbed:
	    interp->result = "grab failed: another application has grab";
	    return TCL_ERROR;
	}
	Tk_Ungrab(tkwin);
    }

    Tk_MakeWindowExist(tkwin);
    if (!grabGlobal) {
	Window dummy1, dummy2;
	int dummy3, dummy4, dummy5, dummy6;
	unsigned int state;

	/*
	 * Local grab.  However, if any mouse buttons are down, turn
	 * it into a global grab temporarily, until the last button
	 * goes up.  This does two things: (a) it makes sure that we
	 * see the button-up event;  and (b) it allows us to track mouse
	 * motion among all of the windows of this application.
	 */

	dispPtr->grabFlags &= (GRAB_GLOBAL|GRAB_TEMP_GLOBAL);
	XQueryPointer(dispPtr->display, winPtr->window, &dummy1,
		&dummy2, &dummy3, &dummy4, &dummy5, &dummy6, &state);
	if ((state & ALL_BUTTONS) != 0) {
	    dispPtr->grabFlags |= GRAB_TEMP_GLOBAL;
	    goto setGlobalGrab;
	}
    } else {
	dispPtr->grabFlags |= GRAB_GLOBAL;
	setGlobalGrab:
	serial = NextRequest(dispPtr->display);
	grabResult = XGrabPointer(dispPtr->display, winPtr->window,
		True, ButtonPressMask|ButtonReleaseMask|ButtonMotionMask,
		GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
	if (grabResult != 0) {
	    grabError:
	    if (grabResult == GrabNotViewable) {
		interp->result = "grab failed: window not viewable";
	    } else if (grabResult == AlreadyGrabbed) {
		goto alreadyGrabbed;
	    } else if (grabResult == GrabFrozen) {
		interp->result = "grab failed: keyboard or pointer frozen";
	    } else if (grabResult == GrabInvalidTime) {
		interp->result = "grab failed: invalid time";
	    } else {
		char msg[100];
	
		sprintf(msg, "grab failed for unknown reason (code %d)",
			grabResult);
		Tcl_AppendResult(interp, msg, (char *) NULL);
	    }
	    return TCL_ERROR;
	}

	/*
	 * Eat up any grab-related events generated by the server for the
	 * grab.  There are several reasons for doing this:
	 *
	 * 1. We have to synthesize the events for local grabs anyway, since
	 *    the server doesn't participate in them.
	 * 2. The server doesn't always generate the right events for global
	 *    grabs (e.g. it generates events even if the current window is
	 *    in the grab tree, which we don't want).
	 * 3. We want all the grab-related events to be processed immediately
	 *    (before other events that are already queued); events coming
	 *    from the server will be in the wrong place, but events we
	 *    synthesize here will go to the front of the queue.
	 */

	EatGrabEvents(dispPtr, serial);

	grabResult = XGrabKeyboard(dispPtr->display, Tk_WindowId(tkwin),
		False, GrabModeAsync, GrabModeAsync, CurrentTime);
	if (grabResult != 0) {
	    XUngrabPointer(dispPtr->display, CurrentTime);
	    goto grabError;
	}
    }

    /*
     * Synthesize leave events to move the pointer from its current window
     * up to the lowest ancestor that it has in common with the grab window.
     * However, only do this if the pointer is outside the grab window's
     * subtree but inside the grab window's application.
     */

    if ((dispPtr->serverWinPtr != NULL)
	    && (dispPtr->serverWinPtr->mainPtr == winPtr->mainPtr)) {
	for (winPtr2 = dispPtr->serverWinPtr; ; winPtr2 = winPtr2->parentPtr) {
	    if (winPtr2 == winPtr) {
		break;
	    }
	    if (winPtr2 == NULL) {
		MovePointer2(dispPtr->serverWinPtr, winPtr, NotifyGrab, 1, 0);
		break;
	    }
	}
    }
    QueueGrabWindowChange(dispPtr, winPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_Ungrab --
 *
 *	Releases a grab on the mouse pointer and keyboard, if there
 *	is one set on the specified window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Pointer and keyboard events will start being delivered to other
 *	windows again.
 *
 *----------------------------------------------------------------------
 */

void
Tk_Ungrab(tkwin)
    Tk_Window tkwin;			/* Window that identifies display
					 * for grab to be released. */
{
    TkDisplay *dispPtr = ((TkWindow *) tkwin)->dispPtr;
    TkWindow *grabWinPtr, *winPtr;
    unsigned int serial;

    grabWinPtr = (TkWindow *) tkwin;
    dispPtr = grabWinPtr->dispPtr;
    if (grabWinPtr != dispPtr->eventualGrabWinPtr) {
	return;
    }
    QueueGrabWindowChange(dispPtr, (TkWindow *) NULL);
    dispPtr->buttonWinPtr = NULL;
    if (dispPtr->grabFlags & (GRAB_GLOBAL|GRAB_TEMP_GLOBAL)) {
	dispPtr->grabFlags &= ~(GRAB_GLOBAL|GRAB_TEMP_GLOBAL);
	serial = NextRequest(dispPtr->display);
	XUngrabPointer(dispPtr->display, CurrentTime);
	XUngrabKeyboard(dispPtr->display, CurrentTime);
	EatGrabEvents(dispPtr, serial);
    }

    /*
     * Generate events to move the pointer back to the window where it
     * really is.  Some notes:
     * 1. As with grabs, only do this if the "real" window is not a
     *    descendant of the grab window, since in this case the pointer
     *    is already where it's supposed to be.
     * 2. If the "real" window is in some other application then don't
     *    generate any events at all, since everything's already been
     *    reported correctly.
     * 3. Only generate enter events.  Don't generate leave events,
     *    because we never told the lower-level windows that they
     *    had the pointer in the first place.
     */

    for (winPtr = dispPtr->serverWinPtr; ; winPtr = winPtr->parentPtr) {
	if (winPtr == grabWinPtr) {
	    break;
	}
	if (winPtr == NULL) {
	    if ((dispPtr->serverWinPtr == NULL) ||
		    (dispPtr->serverWinPtr->mainPtr == grabWinPtr->mainPtr)) {
		MovePointer2(grabWinPtr, dispPtr->serverWinPtr,
			NotifyUngrab, 0, 1);
	    }
	    break;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TkPointerEvent --
 *
 *	This procedure is called for each pointer-related event, before
 *	the event has been processed.  It does various things to make
 *	grabs work correctly.
 *
 * Results:
 *	If the return value is 1 it means the event should be processed
 *	(event handlers should be invoked).  If the return value is 0
 *	it means the event should be ignored in order to make grabs
 *	work correctly.  In some cases this procedure modifies the event.
 *
 * Side effects:
 *	Grab state information may be updated.  New events may also be
 *	pushed back onto the event queue to replace or augment the
 *	one passed in here.
 *
 *----------------------------------------------------------------------
 */

int
TkPointerEvent(eventPtr, winPtr)
    register XEvent *eventPtr;		/* Pointer to the event. */
    TkWindow *winPtr;			/* Tk's information for window
					 * where event was reported. */
{
    register TkWindow *winPtr2;
    TkDisplay *dispPtr = winPtr->dispPtr;
    unsigned int serial;
    int outsideGrabTree = 0;
    int ancestorOfGrab = 0;
    int appGrabbed = 0;			/* Non-zero means event is being
					 * reported to an application that is
					 * affected by the grab. */

    /*
     * If a grab is in effect, see if the event is being reported to
     * a window in the grab tree or to an ancestor of the grab window
     * within its same top-level window.  Also see if the event is
     * being reported to an application that is affected by the grab.
     */

    if (dispPtr->grabWinPtr != NULL) {
	if ((winPtr->mainPtr == dispPtr->grabWinPtr->mainPtr)
		|| (dispPtr->grabFlags & GRAB_GLOBAL)) {
	    appGrabbed = 1;
	}
	for (winPtr2 = winPtr; winPtr2 != dispPtr->grabWinPtr;
		winPtr2 = winPtr2->parentPtr) {
	    if (winPtr2 == NULL) {
		outsideGrabTree = 1;
		for (winPtr2 = dispPtr->grabWinPtr; ;
			winPtr2 = winPtr2->parentPtr) {
		    if (winPtr2 == winPtr) {
			ancestorOfGrab = 1;
		    }
		    if (winPtr2->flags & TK_TOP_LEVEL) {
			break;
		    }
		}
		break;
	    }
	}
    }

    if ((eventPtr->type == EnterNotify) || (eventPtr->type == LeaveNotify)) {
	/*
	 * Keep track of what window the mouse is *really* over.
	 * Any events that we generate have a special send_event value,
	 * which is detected below and used to ignore the event for
	 * purposes of setting serverWinPtr.
	 */

	if (eventPtr->xcrossing.send_event != GENERATED_EVENT_MAGIC) {
	    if ((eventPtr->type == LeaveNotify) &&
		    (winPtr->flags & TK_TOP_LEVEL)) {
		dispPtr->serverWinPtr = NULL;
	    } else {
		dispPtr->serverWinPtr = winPtr;
	    }
	}

	/*
	 * When a grab is active, X continues to report enter and leave
	 * events for windows outside the tree of the grab window:
	 * 1. Detect these events and ignore them except for
	 *    windows above the grab window.
	 * 2. Allow Enter and Leave events to pass through the
	 *    windows above the grab window, but never let them
	 *    end up with the pointer *in* one of those windows.
	 */

	if (dispPtr->grabWinPtr != NULL) {
	    if (outsideGrabTree && appGrabbed) {
		if (!ancestorOfGrab) {
		    return 0;
		}
		switch (eventPtr->xcrossing.detail) {
		    case NotifyInferior:
			return 0;
		    case NotifyAncestor:
			eventPtr->xcrossing.detail = NotifyVirtual;
			break;
		    case NotifyNonlinear:
			eventPtr->xcrossing.detail = NotifyNonlinearVirtual;
			break;
		}
	    }

	    /*
	     * Make buttons have the same grab-like behavior inside a grab
	     * as they do outside a grab:  do this by ignoring enter and
	     * leave events except for the window in which the button was
	     * pressed.
	     */

	    if ((dispPtr->buttonWinPtr != NULL)
		    && (winPtr != dispPtr->buttonWinPtr)) {
		return 0;
	    }
	}
	return 1;
    }
    if (!appGrabbed) {
	return 1;
    }

    if (eventPtr->type == MotionNotify) {
	/*
	 * When grabs are active, X reports motion events relative to the
	 * window under the pointer.  Instead, it should report the events
	 * relative to the window the button went down in, if there is a
	 * button down.  Otherwise, if the pointer window is outside the
	 * subtree of the grab window, the events should be reported
	 * relative to the grab window.  Otherwise, the event should be
	 * reported to the pointer window.
	 */

	winPtr2 = winPtr;
	if (dispPtr->buttonWinPtr != NULL) {
	    winPtr2 = dispPtr->buttonWinPtr;
	} else if (outsideGrabTree || (dispPtr->serverWinPtr == NULL)) {
	    winPtr2 = dispPtr->grabWinPtr;
	}
	if (winPtr2 != winPtr) {
	    XEvent newEvent;

	    newEvent = *eventPtr;
	    ChangeEventWindow(&newEvent, winPtr2);
	    XPutBackEvent(winPtr2->display, &newEvent);
	    return 0;
	}
	return 1;
    }

    /*
     * Process ButtonPress and ButtonRelease events:
     * 1. Keep track of whether a button is down and what window it
     *    went down in.
     * 2. If the first button goes down outside the grab tree, pretend
     *    it went down in the grab window.  Note: it's important to
     *    redirect events to the grab window like this in order to make
     *    things like menus work, where button presses outside the
     *    grabbed menu need to be seen.  An application can always
     *    ignore the events if they occur outside its window.
     * 3. If a button press or release occurs outside the window where
     *    the first button was pressed, retarget the event so it's reported
     *    to the window where the first button was pressed.
     * 4. If the last button is released in a window different than where
     *    the first button was pressed, generate Enter/Leave events to
     *    move the mouse from the button window to its current window.
     * 5. If the grab is set at a time when a button is already down, or
     *    if the window where the button was pressed was deleted, then
     *    dispPtr->buttonWinPtr will stay NULL.  Just forget about the
     *    auto-grab for the button press;  events will go to whatever
     *    window contains the pointer.  If this window isn't in the grab
     *    tree then redirect events to the grab window.
     * 6. When a button is pressed during a local grab, the X server sets
     *    a grab of its own, since it doesn't even know about our local
     *    grab.  This causes enter and leave events no longer to be
     *    generated in the same way as for global grabs.  To eliminate this
     *    problem, set a temporary global grab when the first button goes
     *    down and release it when the last button comes up.
     */

    if ((eventPtr->type == ButtonPress) || (eventPtr->type == ButtonRelease)) {
	winPtr2 = dispPtr->buttonWinPtr;
	if (winPtr2 == NULL) {
	    if (outsideGrabTree) {
		winPtr2 = dispPtr->grabWinPtr;			/* Note 5. */
	    } else {
		winPtr2 = winPtr;				/* Note 5. */
	    }
	}
	if (eventPtr->type == ButtonPress) {
	    if ((eventPtr->xbutton.state & ALL_BUTTONS) == 0) {
		if (outsideGrabTree) {
		    XEvent newEvent;

		    newEvent = *eventPtr;
		    ChangeEventWindow(&newEvent, dispPtr->grabWinPtr);
		    XPutBackEvent(dispPtr->display, &newEvent);
		    return 0;					/* Note 2. */
		}
		if (!(dispPtr->grabFlags & GRAB_GLOBAL)) {	/* Note 6. */
		    serial = NextRequest(dispPtr->display);
		    if (XGrabPointer(dispPtr->display,
			    dispPtr->grabWinPtr->window, True,
			    ButtonPressMask|ButtonReleaseMask|ButtonMotionMask,
			    GrabModeAsync, GrabModeAsync, None, None,
			    CurrentTime) == 0) {
			EatGrabEvents(dispPtr, serial);
			if (XGrabKeyboard(dispPtr->display, winPtr->window,
				False, GrabModeAsync, GrabModeAsync,
				CurrentTime) == 0) {
			    dispPtr->grabFlags |= GRAB_TEMP_GLOBAL;
			} else {
			    XUngrabPointer(dispPtr->display, CurrentTime);
			}
		    }
		}
		dispPtr->buttonWinPtr = winPtr;
		return 1;
	    }
	} else {
	    if ((eventPtr->xbutton.state & ALL_BUTTONS)
		    == buttonStates[eventPtr->xbutton.button - Button1]) {
		if ((dispPtr->buttonWinPtr != winPtr)
			&& (dispPtr->buttonWinPtr != NULL)) {
		    XEvent newEvent;				/* Note 4. */
		    newEvent = *eventPtr;
		    newEvent.xcrossing.send_event = GENERATED_EVENT_MAGIC;
		    newEvent.xcrossing.mode = NotifyUngrab;
		    newEvent.xcrossing.focus = False;
		    newEvent.xcrossing.state =
			    eventPtr->xbutton.state & ~ALL_BUTTONS;
		    MovePointer(&newEvent, dispPtr->buttonWinPtr,
			    dispPtr->serverWinPtr, 1, 1);
		}
		dispPtr->buttonWinPtr = NULL;
		if (dispPtr->grabFlags & GRAB_TEMP_GLOBAL) {	/* Note 6. */
		    dispPtr->grabFlags &= ~GRAB_TEMP_GLOBAL;
		    serial = NextRequest(dispPtr->display);
		    XUngrabPointer(dispPtr->display, CurrentTime);
		    XUngrabKeyboard(dispPtr->display, CurrentTime);
		    EatGrabEvents(dispPtr, serial);
		}
	    }
	}
	if (winPtr2 != winPtr) {
	    XEvent newEvent;

	    newEvent = *eventPtr;
	    ChangeEventWindow(&newEvent, winPtr2);
	    XPutBackEvent(dispPtr->display, &newEvent);
	    return 0;						/* Note 3. */
	}
    }

    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * ChangeEventWindow --
 *
 *	Given an event and a new window to which the event should be
 *	retargeted, modify fields of the event so that the event is
 *	properly retargeted to the new window.
 *
 * Results:
 *	The following fields of eventPtr are modified:  window,
 *	subwindow, x, y, same_screen.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
ChangeEventWindow(eventPtr, winPtr)
    register XEvent *eventPtr;	/* Event to retarget.  Must have
				 * type ButtonPress, ButtonRelease, KeyPress,
				 * KeyRelease, MotionNotify, EnterNotify,
				 * or LeaveNotify. */
    TkWindow *winPtr;		/* New target window for event. */
{
    int x, y, sameScreen, bd;
    register TkWindow *childPtr;

    eventPtr->xmotion.window = Tk_WindowId(winPtr);
    if (eventPtr->xmotion.root ==
	    RootWindow(winPtr->display, winPtr->screenNum)) {
	Tk_GetRootCoords((Tk_Window) winPtr, &x, &y);
	eventPtr->xmotion.x = eventPtr->xmotion.x_root - x;
	eventPtr->xmotion.y = eventPtr->xmotion.y_root - y;
	eventPtr->xmotion.subwindow = None;
	for (childPtr = winPtr->childList; childPtr != NULL;
		childPtr = childPtr->nextPtr) {
	    if (childPtr->flags & TK_TOP_LEVEL) {
		continue;
	    }
	    x = eventPtr->xmotion.x - childPtr->changes.x;
	    y = eventPtr->xmotion.y - childPtr->changes.y;
	    bd = childPtr->changes.border_width;
	    if ((x >= -bd) && (y >= -bd)
		    && (x < (childPtr->changes.width + bd))
		    && (y < (childPtr->changes.width + bd))) {
		eventPtr->xmotion.subwindow = childPtr->window;
	    }
	}
	sameScreen = 1;
    } else {
	eventPtr->xmotion.x = 0;
	eventPtr->xmotion.y = 0;
	eventPtr->xmotion.subwindow = None;
	sameScreen = 0;
    }
    if (eventPtr->type == MotionNotify) {
	eventPtr->xmotion.same_screen = sameScreen;
    } else {
	eventPtr->xbutton.same_screen = sameScreen;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * MovePointer --
 *
 *	This procedure synthesizes EnterNotify and LeaveNotify events
 *	to correctly transfer the pointer from one window to another.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Synthesized events may be pushed back onto the event queue.
 *	The event pointed to by eventPtr is modified.
 *
 *----------------------------------------------------------------------
 */

static void
MovePointer(eventPtr, sourcePtr, destPtr, leaveEvents, enterEvents)
    XEvent *eventPtr;		/* A template X event.  Must have all fields
				 * properly set for EnterNotify and LeaveNotify
				 * events except window, subwindow, x, y,
				 * detail, and same_screen.  (x_root and y_root
				 * must be valid, even though x and y needn't
				 * be valid). */
    TkWindow *sourcePtr;	/* Window currently containing pointer (NULL
				 * means it's not one managed by this
				 * process). */
    TkWindow *destPtr;		/* Window that is to end up containing the
				 * pointer (NULL means it's not one managed
				 * by this process). */
    int leaveEvents;		/* Non-zero means generate leave events for the
				 * windows being left.  Zero means don't
				 * generate leave events. */
    int enterEvents;		/* Non-zero means generate enter events for the
				 * windows being entered.  Zero means don't
				 * generate enter events. */
{
    register TkWindow *winPtr;
    int upLevels, downLevels, i, j;

    /*
     * There are four possible cases to deal with:
     *
     * 1. SourcePtr and destPtr are the same.  There's nothing to do in
     *    this case.
     * 2. SourcePtr is an ancestor of destPtr in the same top-level
     *    window.  Must generate events down the window tree from source
     *    to dest.
     * 3. DestPtr is an ancestor of sourcePtr in the same top-level
     *    window.  Must generate events up the window tree from sourcePtr
     *    to destPtr.
     * 4. All other cases.  Must first generate events up the window tree
     *    from sourcePtr to its top-level, then down from destPtr's
     *    top-level to destPtr. This form is called "non-linear."
     *
     * The call to FindCommonAncestor separates these four cases and decides
     * how many levels up and down events have to be generated for.
     */

    if (sourcePtr == destPtr) {
	return;
    }
    FindCommonAncestor(sourcePtr, destPtr, &upLevels, &downLevels);

    /*
     * Generate enter/leave events and add them to the grab event queue.
     */


#define QUEUE(w, t, d)					\
    if (w->window != None) {				\
	eventPtr->type = t;				\
	eventPtr->xcrossing.detail = d;			\
	ChangeEventWindow(eventPtr, w);			\
	QueueEvent(w->dispPtr, eventPtr);		\
    }

    if (downLevels == 0) {
    
	/*
	 * SourcePtr is an inferior of destPtr.
	 */

	if (leaveEvents) {
	    QUEUE(sourcePtr, LeaveNotify, NotifyAncestor);
	    for (winPtr = sourcePtr->parentPtr, i = upLevels-1; i > 0;
		    winPtr = winPtr->parentPtr, i--) {
		QUEUE(winPtr, LeaveNotify, NotifyVirtual);
	    }
	}
	if ((enterEvents) && (destPtr != NULL)) {
	    QUEUE(destPtr, EnterNotify, NotifyInferior);
	}
    } else if (upLevels == 0) {

	/*
	 * DestPtr is an inferior of sourcePtr.
	 */

	if ((leaveEvents) && (sourcePtr != NULL)) {
	    QUEUE(sourcePtr, LeaveNotify, NotifyInferior);
	}
	if (enterEvents) {
	    for (i = downLevels-1; i > 0; i--) {
		for (winPtr = destPtr->parentPtr, j = 1; j < i;
			winPtr = winPtr->parentPtr, j++) {
		}
		QUEUE(winPtr, EnterNotify, NotifyVirtual);
	    }
	    if (destPtr != NULL) {
		QUEUE(destPtr, EnterNotify, NotifyAncestor);
	    }
	}
    } else {

	/*
	 * Non-linear:  neither window is an inferior of the other.
	 */

	if (leaveEvents) {
	    QUEUE(sourcePtr, LeaveNotify, NotifyNonlinear);
	    for (winPtr = sourcePtr->parentPtr, i = upLevels-1; i > 0;
		    winPtr = winPtr->parentPtr, i--) {
		QUEUE(winPtr, LeaveNotify, NotifyNonlinearVirtual);
	    }
	}
	if (enterEvents) {
	    for (i = downLevels-1; i > 0; i--) {
		for (winPtr = destPtr->parentPtr, j = 1; j < i;
			winPtr = winPtr->parentPtr, j++) {
		}
		QUEUE(winPtr, EnterNotify, NotifyNonlinearVirtual);
	    }
	    if (destPtr != NULL) {
		QUEUE(destPtr, EnterNotify, NotifyNonlinear);
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * QueueEvent --
 *
 *	This procedure adds an enter or leave event to the end of the
 *	grab queue for dispPtr.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A copy of *eventPtr is added to the grab queue for dispPtr.
 *	It will be processed after all other events on the grab queue
 *	but before any "real" X events (those not coming from the grab
 *	queue).
 *
 *----------------------------------------------------------------------
 */

static void
QueueEvent(dispPtr, eventPtr)
    TkDisplay *dispPtr;		/* Display for which the event is to be
				 * queued. */
    XEvent *eventPtr;		/* Template for event;  must have all fields
				 * filled in but those that are provided by
				 * ChangeEventWindow. */
{
    TkGrabEvent *grabEventPtr;

    grabEventPtr = (TkGrabEvent *) ckalloc(sizeof(TkGrabEvent));
    grabEventPtr->event = *eventPtr;
    grabEventPtr->nextPtr = NULL;
    if (dispPtr->firstGrabEventPtr == NULL) {
	dispPtr->firstGrabEventPtr = grabEventPtr;
    } else {
	dispPtr->lastGrabEventPtr->nextPtr = grabEventPtr;
    }
    dispPtr->lastGrabEventPtr = grabEventPtr;
    if (!(dispPtr->grabFlags & GRAB_TRIGGER_QUEUED)) {
	PushTriggerEvent(dispPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PushTriggerEvent --
 *
 *	This procedure creates a special "trigger" event and pushes it
 *	onto the front of the event queue for winPtr's display.
 *	When Tk_HandleEvent sees this event it will call back to this
 *	module so that we can feed events from the grab queue onto the
 *	front of the event queue.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Causes TkGrabTriggerProc to be called later on when the event
 *	is actually handled.
 *
 *----------------------------------------------------------------------
 */

static void
PushTriggerEvent(dispPtr)
    TkDisplay *dispPtr;			/* Display on whose display a trigger
					 * event is to be pushed back. */
{
    XEvent event;

    event.xany.type = -1;
    event.xany.serial = 0;
    event.xany.send_event = True;
    event.xany.display = (Display *) dispPtr;
    event.xany.window = None;
    XPutBackEvent(dispPtr->display, &event);
    dispPtr->grabFlags |= GRAB_TRIGGER_QUEUED;
}

/*
 *----------------------------------------------------------------------
 *
 * TkGrabTriggerProc --
 *
 *	This procedure is invoked when a trigger event is encountered
 *	by Tk_HandleEvent. 
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	See code below.
 *
 *----------------------------------------------------------------------
 */

void
TkGrabTriggerProc(eventPtr)
    XEvent *eventPtr;		/* Pointer to the trigger event. */
{
    TkDisplay *dispPtr = (TkDisplay *) eventPtr->xany.display;
    TkGrabEvent *grabEventPtr;

    /*
     * Remove the first event from the grab queue, if there is one.
     * If there are additional events left on the queue, then push
     * back a new trigger event so that this procedure will get called
     * again to process them.
     */

    dispPtr->grabFlags &= ~GRAB_TRIGGER_QUEUED;
    grabEventPtr = dispPtr->firstGrabEventPtr;
    if (grabEventPtr == NULL) {
	return;
    }
    dispPtr->firstGrabEventPtr = grabEventPtr->nextPtr;
    if (dispPtr->firstGrabEventPtr == NULL) {
	dispPtr->lastGrabEventPtr = NULL;
    } else {
	PushTriggerEvent(dispPtr);
    }

    /*
     * If this is a special event to change grabWinPtr, do that;  otherwise
     * call Tk_HandleEvent recursively to process the grab event. In either
     * case, free up the event when done.
     */

    if (grabEventPtr->event.xany.send_event == GRAB_WINDOW_EVENT_MAGIC) {
	if (XFindContext(grabEventPtr->event.xany.display,
		grabEventPtr->event.xany.window,
		tkWindowContext, (caddr_t *) &dispPtr->grabWinPtr) != 0) {
	    dispPtr->grabWinPtr = NULL;
	}
    } else {
	Tk_HandleEvent(&grabEventPtr->event);
    }
    ckfree((char *) grabEventPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * MovePointer2 --
 *
 *	This procedure synthesizes  EnterNotify and LeaveNotify events
 *	to correctly transfer the pointer from one window to another.
 *	It is different from MovePointer in that no template X event
 *	needs to be supplied;  this procedure generates the template
 *	event and calls MovePointer.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Synthesized events may be pushed back onto the event queue.
 *
 *----------------------------------------------------------------------
 */

static void
MovePointer2(sourcePtr, destPtr, mode, leaveEvents, enterEvents)
    TkWindow *sourcePtr;	/* Window currently containing pointer (NULL
				 * means it's not one managed by this
				 * process). */
    TkWindow *destPtr;		/* Window that is to end up containing the
				 * pointer (NULL means it's not one managed
				 * by this process). */
    int mode;			/* Mode for enter/leave events, such as
				 * NotifyNormal or NotifyUngrab. */
    int leaveEvents;		/* Non-zero means generate leave events for the
				 * windows being left.  Zero means don't
				 * generate leave events. */
    int enterEvents;		/* Non-zero means generate enter events for the
				 * windows being entered.  Zero means don't
				 * generate enter events. */
{
    XEvent event;
    Window dummy1, dummy2;
    int dummy3, dummy4;
    TkWindow *winPtr;

    winPtr = sourcePtr;
    if ((winPtr == NULL) || (winPtr->window == None)) {
	winPtr = destPtr;
	if ((winPtr == NULL) || (winPtr->window == None)) {
	    return;
	}
    }

    event.xcrossing.serial = LastKnownRequestProcessed(winPtr->display);
    event.xcrossing.send_event = GENERATED_EVENT_MAGIC;
    event.xcrossing.display = winPtr->display;
    event.xcrossing.root = RootWindow(winPtr->display, winPtr->screenNum);
    event.xcrossing.time = TkCurrentTime(winPtr->dispPtr);
    XQueryPointer(winPtr->display, winPtr->window, &dummy1, &dummy2,
	    &event.xcrossing.x_root, &event.xcrossing.y_root,
	    &dummy3, &dummy4, &event.xcrossing.state);
    event.xcrossing.mode = mode;
    event.xcrossing.focus = False;
    MovePointer(&event, sourcePtr, destPtr, leaveEvents, enterEvents);
}

/*
 *----------------------------------------------------------------------
 *
 * TkGrabDeadWindow --
 *
 *	This procedure is invoked whenever a window is deleted, so that
 *	grab-related cleanup can be performed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Various cleanups happen, such as generating events to move the
 *	pointer back to its "natural" window as if an ungrab had been
 *	done.  See the code.
 *
 *----------------------------------------------------------------------
 */

void
TkGrabDeadWindow(winPtr)
    register TkWindow *winPtr;		/* Window that is in the process
					 * of being deleted. */
{
    TkDisplay *dispPtr = winPtr->dispPtr;

    if (dispPtr->eventualGrabWinPtr == winPtr) {
	/*
	 * Grab window was deleted.  Release the grab.
	 */

	Tk_Ungrab((Tk_Window) dispPtr->eventualGrabWinPtr);
    } else if (dispPtr->buttonWinPtr == winPtr) {
	/*
	 * The window in which a button was pressed was deleted.  Move the
	 * pointer back to the window where it really is.
	 */

	if (dispPtr->buttonWinPtr != dispPtr->serverWinPtr) {
	    MovePointer2(dispPtr->buttonWinPtr, dispPtr->serverWinPtr, 
		    NotifyUngrab, 1, 1);
	}
	dispPtr->buttonWinPtr = NULL;
    }
    if (dispPtr->serverWinPtr == winPtr) {
	dispPtr->serverWinPtr = NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * EatGrabEvents --
 *
 *	This procedure is called to eliminate any Enter or Leave
 *	events in the event queue for a display that have mode
 *	NotifyGrab or NotifyUngrab and have a serial number no
 *	less than a given value.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	DispPtr's display gets sync-ed, and some of the events get
 *	removed from its queue.  Unaffected events are initially
 *	removed from the queue but they are eventually put back again
 *	in the right order.
 *
 *----------------------------------------------------------------------
 */

static void
EatGrabEvents(dispPtr, serial)
    TkDisplay *dispPtr;		/* Display from which to consume events. */
    unsigned int serial;	/* Only discard events that have a serial
				 * number at least this great. */
{
    int numEvents, i, diff;
    XEvent *events, *eventPtr;

    XSync(dispPtr->display, False);
    numEvents = QLength(dispPtr->display);
    if (numEvents == 0) {
	return;
    }
    events = (XEvent *) ckalloc((unsigned) (numEvents * sizeof(XEvent)));
    for (i = 0; i < numEvents; i++) {
	XNextEvent(dispPtr->display, &events[i]);
    }
    for (i = numEvents-1, eventPtr = &events[i]; i >= 0; i--, eventPtr--) {
	/*
	 * The diff caculation is trickier than it may seem.  Don't forget
	 * that serial numbers can wrap around, so can't compare the two
	 * serial numbers directly.
	 */

	diff = eventPtr->xany.serial - serial;
	if (((eventPtr->type != EnterNotify)
		&& (eventPtr->type != LeaveNotify))
		|| (eventPtr->xcrossing.mode == NotifyNormal)
		|| (diff < 0)) {
	    XPutBackEvent(dispPtr->display, eventPtr);
	}
    }
    ckfree((char *) events);
}

/*
 *----------------------------------------------------------------------
 *
 * QueueGrabWindowChange --
 *
 *	This procedure queues a special event in the grab event queue
 *	for dispPtr, which will cause the "grabWinPtr" field for the
 *	display to get modified when the event is processed.  This
 *	procedure is needed to make sure that the grab window changes
 *	at the proper time relative to grab-related enter and leave
 *	events that are also in the queue.  In particular, this approach
 *	works even when multiple grabs and ungrabs happen back-to-back.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	DispPtr->grabWinPtr will be modified later (by TkGrabTriggerProc)
 *	when the event is removed from the grab event queue.
 *
 *----------------------------------------------------------------------
 */

static void
QueueGrabWindowChange(dispPtr, grabWinPtr)
    TkDisplay *dispPtr;		/* Display on which to change the grab
				 * window. */
    TkWindow *grabWinPtr;	/* Window that is to become the new grab
				 * window (may be NULL). */
{
    XEvent event;

    event.xany.display = dispPtr->display;
    event.xany.send_event = GRAB_WINDOW_EVENT_MAGIC;
    event.xany.window = (grabWinPtr == NULL) ? None : grabWinPtr->window;
    QueueEvent(dispPtr, &event);
    dispPtr->eventualGrabWinPtr = grabWinPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * FindCommonAncestor --
 *
 *	Given two windows, this procedure finds their least common
 *	ancestor and also computes how many levels up this ancestor
 *	is from each of the original windows.
 *
 * Results:
 *	If the windows are in different applications or top-level
 *	windows, then NULL is returned and *countPtr1 and *countPtr2
 *	are set to the depths of the two windows in their respective
 *	top-level windows (1 means the window is a top-level, 2 means
 *	its parent is a top-level, and so on).  Otherwise, the return
 *	value is a pointer to the common ancestor and the counts are
 *	set to the distance of winPtr1 and winPtr2 from this ancestor
 *	(1 means they're children, 2 means grand-children, etc.).
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static TkWindow *
FindCommonAncestor(winPtr1, winPtr2, countPtr1, countPtr2)
    TkWindow *winPtr1;		/* First window.   May be NULL. */
    TkWindow *winPtr2;		/* Second window.  May be NULL. */
    int *countPtr1;		/* Store nesting level of winPtr1 within
				 * common ancestor here. */
    int *countPtr2;		/* Store nesting level of winPtr2 within
				 * common ancestor here. */
{
    register TkWindow *winPtr;
    TkWindow *ancestorPtr;
    int count1, count2, i;

    /*
     * Mark winPtr1 and all of its ancestors with a special flag bit.
     */

    if (winPtr1 != NULL) {
	for (winPtr = winPtr1; ; winPtr = winPtr->parentPtr) {
	    winPtr->flags |= TK_GRAB_FLAG;
	    if (winPtr->flags & TK_TOP_LEVEL) {
		break;
	    }
	}
    }

    /*
     * Search upwards from winPtr2 until an ancestor of winPtr1 is
     * found or a top-level window is reached.
     */

    winPtr = winPtr2;
    count2 = 0;
    ancestorPtr = NULL;
    if (winPtr2 != NULL) {
	for (; ; count2++, winPtr = winPtr->parentPtr) {
	    if (winPtr->flags & TK_GRAB_FLAG) {
		ancestorPtr = winPtr;
		break;
	    }
	    if (winPtr->flags & TK_TOP_LEVEL)  {
		count2++;
		break;
	    }
	}
    }

    /*
     * Search upwards from winPtr1 again, clearing the flag bits and
     * remembering how many levels up we had to go.
     */

    if (winPtr1 == NULL) {
	count1 = 0;
    } else {
	count1 = -1;
	for (i = 0, winPtr = winPtr1; ; i++, winPtr = winPtr->parentPtr) {
	    winPtr->flags &= ~TK_GRAB_FLAG;
	    if (winPtr == ancestorPtr) {
		count1 = i;
	    }
	    if (winPtr->flags & TK_TOP_LEVEL) {
		if (count1 == -1) {
		    count1 = i+1;
		}
		break;
	    }
	}
    }

    *countPtr1 = count1;
    *countPtr2 = count2;
    return ancestorPtr;
}
