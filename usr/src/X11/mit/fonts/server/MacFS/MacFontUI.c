/***********************************************************************
Copyright 1991 by Apple Computer, Inc, Cupertino, California
			All Rights Reserved

Permission to use, copy, modify, and distribute this software
for any purpose and without fee is hereby granted, provided
that the above copyright notice appear in all copies.

APPLE MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS,
OR IMPLIED, WITH RESPECT TO THIS SOFTWARE, ITS QUALITY,
PERFORMANCE, MERCHANABILITY, OR FITNESS FOR A PARTICULAR
PURPOSE. AS A RESULT, THIS SOFTWARE IS PROVIDED "AS IS,"
AND YOU THE USER ARE ASSUMING THE ENTIRE RISK AS TO ITS
QUALITY AND PERFORMANCE. IN NO EVENT WILL APPLE BE LIABLE 
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES RESULTING FROM ANY DEFECT IN THE SOFTWARE.

THE WARRANTY AND REMEDIES SET FORTH ABOVE ARE EXCLUSIVE
AND IN LIEU OF ALL OTHERS, ORAL OR WRITTEN, EXPRESS OR
IMPLIED.

***********************************************************************/

/* Segmentation strategy:

   This program consists of three segments. Main contains most of the code,
   including the MPW libraries, and the main program. Initialize contains
   code that is only used once, during startup, and can be unloaded after the
   program starts. %A5Init is automatically created by the Linker to initialize
   globals for the MPW libraries and is unloaded right away. */


/* SetPort strategy:

   Toolbox routines do not change the current port. In spite of this, in this
   program we use a strategy of calling SetPort whenever we want to draw or
   make calls which depend on the current port. This makes us less vulnerable
   to bugs in other software which might alter the current port (such as the
   bug (feature?) in many desk accessories which change the port on OpenDeskAcc).
   Hopefully, this also makes the routines from this program more self-contained,
   since they don't depend on the current port setting. */


/* Clipboard strategy:

   This program does not maintain a private scrap. Whenever a cut, copy, or paste
   occurs, we import/export from the public scrap to TextEdit's scrap right away,
   using the TEToScrap and TEFromScrap routines. If we did use a private scrap,
   the import/export would be in the activate/deactivate event and suspend/resume
   event routines. */

#define AUX

/* A/UX is case sensitive, so use correct case for include file names */
#include <values.h>
#include <types.h>
#include <quickdraw.h>
#include <fonts.h>
#include <events.h>
#include <controls.h>
#include <windows.h>
#include <menus.h>
#include <textedit.h>
#include <dialogs.h>
#include <desk.h>
#include <scrap.h>
#include <toolutils.h>
#include <memory.h>
#include <segload.h>
#include <files.h>
#include <osutils.h>
#include <osevents.h>
#include <diskinit.h>
#include <packages.h>
#include "MacFontUI.h"		/* bring in all the #defines for MacFontUI */
#include <traps.h>

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/termio.h>
#include <compat.h>
#include <errno.h>

#include "FS.h"
#include "FSproto.h"
#include "servermd.h"

/* A/UX C understands neither #pragma, nor segments, so define pragma to
 * something harmless. */
#ifdef AUX
#define pragma undef
#endif

/* A/UX Hybrid apps need to exit through the libc routine. */
#define ExitToShell() exit(0)


/* A DocumentRecord contains the WindowRecord for one of our document windows,
   as well as the TEHandle for the text we are editing. Other document fields
   can be added to this record as needed. For a similar example, see how the
   Window Manager and Dialog Manager add fields after the GrafPort. */
typedef struct {
	WindowRecord	docWindow;
	TEHandle		docTE;
	ControlHandle	docVScroll;
	ControlHandle	docHScroll;
	ProcPtr			docClik;
} DocumentRecord, *DocumentPeek;



/* The "g" prefix is used to emphasize that a variable is global. */

/* GMac is used to hold the result of a SysEnvirons call. This makes
   it convenient for any routine to check the environment. It is
   global information, anyway. */
SysEnvRec	gMac;				/* set up by Initialize */

/* GHasWaitNextEvent is set at startup, and tells whether the WaitNextEvent
   trap is available. If it is false, we know that we must call GetNextEvent. */
Boolean		gHasWaitNextEvent;	/* set up by Initialize */

/* GInBackground is maintained by our OSEvent handling routines. Any part of
   the program can check it to find out if it is currently in the background. */
Boolean		gInBackground;		/* maintained by Initialize and DoEvent */

/* GNumDocuments is used to keep track of how many open documents there are
   at any time. It is maintained by the routines that open and close documents. */
short		gNumDocuments;		/* maintained by Initialize, DoNew, and DoCloseWindow */


void AlertUser( );
void EventLoop( );
void DoEvent( );
void AdjustCursor( );
void GetGlobalMouse( );
void DoGrowWindow( );
void DoZoomWindow( );
void ResizeWindow( );
void GetLocalUpdateRgn( );
void DoUpdate( );
void DoDeactivate( );
void DoActivate( );
void DoContentClick( );
void DoKeyDown( );
unsigned long GetSleep( );
void CommonAction( );
void VActionProc( );
void HActionProc( );
void DoIdle( );
void DrawWindow( );
void AdjustMenus( );
void DoMenuCommand( );
void DoNew( );
Boolean DoCloseWindow( );
void Terminate( );
void Initialize( );
void BigBadError( ); 
void GetTERect( );
void AdjustViewRect( );
void AdjustTE( );
void AdjustHV( );
void AdjustScrollValues( );
void AdjustScrollSizes( );
void AdjustScrollbars( );
void PascalClikLoop();
ProcPtr GetOldClikLoop();
Boolean IsAppWindow( );
Boolean IsDAWindow( );
Boolean TrapAvailable( );


/* Define HiWrd and LoWrd macros for efficiency. */
#define HiWrd(aLong)	(((aLong) >> 16) & 0xFFFF)
#define LoWrd(aLong)	((aLong) & 0xFFFF)

/* Define TopLeft and BotRight macros for convenience. Notice the implicit
   dependency on the ordering of fields within a Rect */
#define TopLeft(aRect)	(* (Point *) &(aRect).top)
#define BotRight(aRect)	(* (Point *) &(aRect).bottom)


/* A reference to our assembly language routine that gets attached to the clikLoop
field of our TE record. */

#ifdef AUX
extern void AsmClikLoop();
#else
extern pascal void AsmClikLoop();
#endif


#pragma segment Main

static RgnHandle        cursorRgn;
int initDone = 0;

extern unsigned int sleep();
static void MacFontBanner();
static void PreflightOutlineFonts();
extern short CurApRefNum;

extern long LastReapTime, GetTimeInMillis();

OsInit()
{
    LastReapTime = GetTimeInMillis();
    MacFontRegisterFontFileFunctions ();
}

void
InitMacWorld()
{

    if (initDone) return; /* XXX serverGeneration > 0 is neater? */

    set42sig();
    setcompat(getcompat() | COMPAT_BSD);

    if (CurApRefNum == -1) {
        (void) fprintf(stderr,"Could not open application resource fork\n");
        (void) fprintf(stderr,"Is \"%MacFS\" missing?\n");
        (void) sleep((unsigned int) 2);
        exit(1);
    }

    /* XXX gestalt call. check for Bass availability, warning alert if no */
    if (!TrapAvailable(0x54, ToolTrap)) {
        (void) fprintf(stderr,"TrueType Init not found\n");
        exit(2);
    }

    /*  If you have stack requirements that differ from the default,
        then you could use SetApplLimit to increase StackSpace at
        this point, before calling MaxApplZone. */
    MaxApplZone();                  /* expand the heap so code segments load at the top */

    Initialize();                   /* initialize the program */
    PreflightOutlineFonts();        
    cursorRgn = NewRgn();

    initDone = 1;

	MacFontBanner();
}

#ifdef STANDALONE
main()
{
	/* 1.01 - call to ForceEnvirons removed */
	
	/*	If you have stack requirements that differ from the default,
		then you could use SetApplLimit to increase StackSpace at 
		this point, before calling MaxApplZone. */
	MaxApplZone();					/* expand the heap so code segments load at the top */

	Initialize();					/* initialize the program */

	EventLoop();					/* call the main event loop */
}
#endif


/* Get events forever, and handle them by calling DoEvent.
   Also call AdjustCursor each time through the loop. */

#pragma segment Main
void EventLoop()
{
	RgnHandle	cursorRgn;
	Boolean		gotEvent;
	EventRecord	event;
	Point		mouse;

	cursorRgn = NewRgn();/* we'll pass WNE an empty region the 1st time thru */
	do {
		/* use WNE if it is available */
		if ( gHasWaitNextEvent ) {
			GetGlobalMouse(&mouse);
			AdjustCursor(mouse, cursorRgn);
			gotEvent = WaitNextEvent(everyEvent, &event, GetSleep(), cursorRgn);
		}
		else {
			SystemTask();
			gotEvent = GetNextEvent(everyEvent, &event);
		}
		if ( gotEvent ) {
			/* make sure we have the right cursor before handling the event */
			AdjustCursor(event.where, cursorRgn);
			DoEvent(&event);
		}
		else
			DoIdle();/* perform idle tasks when it's not our event */
		/*	If you are using modeless dialogs that have editText items,
			you will want to call IsDialogEvent to give the caret a chance
			to blink, even if WNE/GNE returned FALSE. However, check FrontWindow
			for a non-NIL value before calling IsDialogEvent. */
	} while ( true );	/* loop forever; we quit via ExitToShell */
} /*EventLoop*/


/* Do the right thing for an event. Determine what kind of event it is, and call
 the appropriate routines. */

#pragma segment Main
void DoEvent(event)
	EventRecord	*event;
{
	short		part, err;
	WindowPtr	window;
	char		key;
	Point		aPoint;

	switch ( event->what ) {
		case nullEvent:
			/* we idle for null/mouse moved events ands for events which aren't
				ours (see EventLoop) */
			DoIdle();
			break;
		case mouseDown:
			part = FindWindow(event->where, &window);
			switch ( part ) {
				case inMenuBar:             /* process a mouse menu command (if any) */
					AdjustMenus();	/* bring 'em up-to-date */
					DoMenuCommand(MenuSelect(event->where));
					break;
				case inSysWindow:           /* let the system handle the mouseDown */
					SystemClick(event, window);
					break;
				case inContent:
					if ( window != FrontWindow() ) {
						SelectWindow(window);
						/*DoEvent(event);*/	/* use this line for "do first click" */
					} else
						DoContentClick(window, event);
					break;
				case inDrag:                /* pass screenBits.bounds to get all gDevices */
					DragWindow(window, event->where, &qd.screenBits.bounds);
					break;
				case inGoAway:
					if ( TrackGoAway(window, event->where) )
						DoCloseWindow(window); /* we don't care if the user cancelled */
					break;
				case inGrow:
					DoGrowWindow(window, event);
					break;
				case inZoomIn:
				case inZoomOut:
				if ( TrackBox(window, event->where, part) )
						DoZoomWindow(window, part);
					break;
			}
			break;
		case keyDown:
		case autoKey:                       /* check for menukey equivalents */
			key = event->message & charCodeMask;
			if ( event->modifiers & cmdKey ) {	/* Command key down */
				if ( event->what == keyDown ) {
					AdjustMenus();			/* enable/disable/check menu items properly */
					DoMenuCommand(MenuKey(key));
				}
			} else
				DoKeyDown(event);
			break;
		case activateEvt:
			DoActivate((WindowPtr) event->message, (event->modifiers & activeFlag) != 0);
			break;
		case updateEvt:
			DoUpdate((WindowPtr) event->message);
			break;
		/*	1.01 - It is not a bad idea to at least call DIBadMount in response
			to a diskEvt, so that the user can format a floppy. */
		case diskEvt:
			if ( HiWord(event->message) != noErr ) {
				SetPt(&aPoint, kDILeft, kDITop);
				err = DIBadMount(aPoint, event->message);
			}
			break;
		case kOSEvent:
		/*	1.02 - must BitAND with 0x0FF to get only low byte */
			switch ((event->message >> 24) & 0x0FF) {		/* high byte of message */
				case kMouseMovedMessage:
					DoIdle();					/* mouse-moved is also an idle event */
					break;
				case kSuspendResumeMessage:		/* suspend/resume is also an activate/deactivate */
					gInBackground = (event->message & kResumeMask) == 0;
					DoActivate(FrontWindow(), !gInBackground);
					break;
			}
			break;
	}
} /*DoEvent*/


/*	Change the cursor's shape, depending on its position. This also calculates the region
	where the current cursor resides (for WaitNextEvent). When the mouse moves outside of
	this region, an event is generated. If there is more to the event than just
	"the mouse moved", we get called before the event is processed to make sure
	the cursor is the right one. In any (ahem) event, this is called again before we
	fall back into WNE. */

#pragma segment Main
void AdjustCursor(mouse,region)
	Point		mouse;
	RgnHandle	region;
{
	WindowPtr	window;
	RgnHandle	arrowRgn;
	RgnHandle	iBeamRgn;
	Rect		iBeamRect;

	window = FrontWindow();	/* we only adjust the cursor when we are in front */
	if ( (! gInBackground) && (! IsDAWindow(window)) ) {
		/* calculate regions for different cursor shapes */
		arrowRgn = NewRgn();
		iBeamRgn = NewRgn();

		/* start arrowRgn wide open */
		SetRectRgn(arrowRgn, kExtremeNeg, kExtremeNeg, kExtremePos, kExtremePos);

		/* calculate iBeamRgn */
		if ( IsAppWindow(window) ) {
			iBeamRect = (*((DocumentPeek) window)->docTE)->viewRect;
			SetPort(window);	/* make a global version of the viewRect */
			LocalToGlobal(&TopLeft(iBeamRect));
			LocalToGlobal(&BotRight(iBeamRect));
			RectRgn(iBeamRgn, &iBeamRect);
			/* we temporarily change the port's origin to "globalfy" the visRgn */
			SetOrigin(-window->portBits.bounds.left, -window->portBits.bounds.top);
			SectRgn(iBeamRgn, window->visRgn, iBeamRgn);
			SetOrigin(0, 0);
		}

		/* subtract other regions from arrowRgn */
		DiffRgn(arrowRgn, iBeamRgn, arrowRgn);

		/* change the cursor and the region parameter */
		if ( PtInRgn(mouse, iBeamRgn) ) {
			SetCursor(*GetCursor(iBeamCursor));
			CopyRgn(iBeamRgn, region);
		} else {
			SetCursor(&qd.arrow);
			CopyRgn(arrowRgn, region);
		}

		DisposeRgn(arrowRgn);
		DisposeRgn(iBeamRgn);
	}
} /*AdjustCursor*/


/*	Get the global coordinates of the mouse. When you call OSEventAvail
	it will return either a pending event or a null event. In either case,
	the where field of the event record will contain the current position
	of the mouse in global coordinates and the modifiers field will reflect
	the current state of the modifiers. Another way to get the global
	coordinates is to call GetMouse and LocalToGlobal, but that requires
	being sure that thePort is set to a valid port. */

#pragma segment Main
void GetGlobalMouse(mouse)
	Point	*mouse;
{
	EventRecord	event;
	
	OSEventAvail(kNoEvents, &event);	/* we aren't interested in any events */
	*mouse = event.where;				/* just the mouse position */
} /*GetGlobalMouse*/


/*	Called when a mouseDown occurs in the grow box of an active window. In
	order to eliminate any 'flicker', we want to invalidate only what is
	necessary. Since ResizeWindow invalidates the whole portRect, we save
	the old TE viewRect, intersect it with the new TE viewRect, and
	remove the result from the update region. However, we must make sure
	that any old update region that might have been around gets put back. */

#pragma segment Main
void DoGrowWindow(window,event)
	WindowPtr	window;
	EventRecord	*event;
{
	long		growResult;
	Rect		tempRect;
	RgnHandle	tempRgn;
	DocumentPeek doc;
	
	tempRect = qd.screenBits.bounds;					/* set up limiting values */
	tempRect.left = kMinDocDim;
	tempRect.top = kMinDocDim;
	growResult = GrowWindow(window, event->where, &tempRect);
	/* see if it really changed size */
	if ( growResult != 0 ) {
		doc = (DocumentPeek) window;
		tempRect = (*doc->docTE)->viewRect;				/* save old text box */
		tempRgn = NewRgn();
		GetLocalUpdateRgn(window, tempRgn);				/* get localized update region */
		SizeWindow(window, LoWrd(growResult), HiWrd(growResult), true);
		ResizeWindow(window);
		/* calculate & validate the region that hasn't changed so it won't get redrawn */
		SectRect(&tempRect, &(*doc->docTE)->viewRect, &tempRect);
		ValidRect(&tempRect);							/* take it out of update */
		InvalRgn(tempRgn);								/* put back any prior update */
		DisposeRgn(tempRgn);
	}
} /* DoGrowWindow */


/* 	Called when a mouseClick occurs in the zoom box of an active window.
	Everything has to get re-drawn here, so we don't mind that
	ResizeWindow invalidates the whole portRect. */

#pragma segment Main
void DoZoomWindow(window,part)
	WindowPtr	window;
	short		part;
{
	EraseRect(&window->portRect);
	ZoomWindow(window, part, window == FrontWindow());
	ResizeWindow(window);
} /*  DoZoomWindow */


/* Called when the window has been resized to fix up the controls and content. */
#pragma segment Main
void ResizeWindow(window)
	WindowPtr	window;
{
	AdjustScrollbars(window, true);
	AdjustTE(window);
	InvalRect(&window->portRect);
} /* ResizeWindow */


/* Returns the update region in local coordinates */
#pragma segment Main
void GetLocalUpdateRgn(window,localRgn)
	WindowPtr	window;
	RgnHandle	localRgn;
{
	CopyRgn(((WindowPeek) window)->updateRgn, localRgn);	/* save old update region */
	OffsetRgn(localRgn, window->portBits.bounds.left, window->portBits.bounds.top);
} /* GetLocalUpdateRgn */


/*	This is called when an update event is received for a window.
	It calls DrawWindow to draw the contents of an application window.
	As an efficiency measure that does not have to be followed, it
	calls the drawing routine only if the visRgn is non-empty. This
	will handle situations where calculations for drawing or drawing
	itself is very time-consuming. */

#pragma segment Main
void DoUpdate(window)
	WindowPtr	window;
{
	if ( IsAppWindow(window) ) {
		BeginUpdate(window);				/* this sets up the visRgn */
		if ( ! EmptyRgn(window->visRgn) )	/* draw if updating needs to be done */
			DrawWindow(window);
		EndUpdate(window);
	}
} /*DoUpdate*/


/*	This is called when a window is activated or deactivated.
	It calls TextEdit to deal with the selection. */

#pragma segment Main
void DoActivate(window, becomingActive)
	WindowPtr	window;
	Boolean		becomingActive;
{
	RgnHandle	tempRgn, clipRgn;
	Rect		growRect;
	DocumentPeek doc;
	
	if ( IsAppWindow(window) ) {
		doc = (DocumentPeek) window;
		if ( becomingActive ) {
			/*	since we don't want TEActivate to draw a selection in an area where
				we're going to erase and redraw, we'll clip out the update region
				before calling it. */
			tempRgn = NewRgn();
			clipRgn = NewRgn();
			GetLocalUpdateRgn(window, tempRgn);			/* get localized update region */
			GetClip(clipRgn);
			DiffRgn(clipRgn, tempRgn, tempRgn);			/* subtract updateRgn from clipRgn */
			SetClip(tempRgn);
			TEActivate(doc->docTE);
			SetClip(clipRgn);							/* restore the full-blown clipRgn */
			DisposeRgn(tempRgn);
			DisposeRgn(clipRgn);
			
			/* the controls must be redrawn on activation: */
			(*doc->docVScroll)->contrlVis = kControlVisible;
			(*doc->docHScroll)->contrlVis = kControlVisible;
			InvalRect(&(*doc->docVScroll)->contrlRect);
			InvalRect(&(*doc->docHScroll)->contrlRect);
			/* the growbox needs to be redrawn on activation: */
			growRect = window->portRect;
			/* adjust for the scrollbars */
			growRect.top = growRect.bottom - kScrollbarAdjust;
			growRect.left = growRect.right - kScrollbarAdjust;
			InvalRect(&growRect);
		}
		else {		
			TEDeactivate(doc->docTE);
			/* the controls must be hidden on deactivation: */
			HideControl(doc->docVScroll);
			HideControl(doc->docHScroll);
			/* the growbox should be changed immediately on deactivation: */
			DrawGrowIcon(window);
		}
	}
} /*DoActivate*/


/*	This is called when a mouseDown occurs in the content of a window. */

#pragma segment Main
void DoContentClick(window,event)
	WindowPtr	window;
	EventRecord	*event;
{
	Point		mouse;
	ControlHandle control;
	short		part, value;
	Boolean		shiftDown;
	DocumentPeek doc;
	Rect		teRect;

	if ( IsAppWindow(window) ) {
		SetPort(window);
		mouse = event->where;							/* get the click position */
		GlobalToLocal(&mouse);
		doc = (DocumentPeek) window;
		/* see if we are in the viewRect. if so, we won't check the controls */
		GetTERect(window, &teRect);
		if ( PtInRect(mouse, &teRect) ) {
			/* see if we need to extend the selection */
			shiftDown = (event->modifiers & shiftKey) != 0;	/* extend if Shift is down */
			TEClick(mouse, shiftDown, doc->docTE);
		} else {
			part = FindControl(mouse, window, &control);
			switch ( part ) {
				case 0:							/* do nothing for viewRect case */
					break;
				case inThumb:
					value = GetCtlValue(control);
					part = TrackControl(control, mouse, nil);
					if ( part != 0 ) {
						value -= GetCtlValue(control);
						/* value now has CHANGE in value; if value changed, scroll */
						if ( value != 0 )
							if ( control == doc->docVScroll )
								TEScroll(0, value * (*doc->docTE)->lineHeight, doc->docTE);
							else
								TEScroll(value, 0, doc->docTE);
					}
					break;
				default:						/* they clicked in an arrow, so track & scroll */
					if ( control == doc->docVScroll )
						value = TrackControl(control, mouse, (ProcPtr) VActionProc);
					else
						value = TrackControl(control, mouse, (ProcPtr) HActionProc);
					break;
			}
		}
	}
} /*DoContentClick*/


/* This is called for any keyDown or autoKey events, except when the
 Command key is held down. It looks at the frontmost window to decide what
 to do with the key typed. */

#pragma segment Main
void DoKeyDown(event)
	EventRecord	*event;
{
	WindowPtr	window;
	char		key;
	TEHandle	te;

#ifdef WRITABLE_DOCUMENT
	window = FrontWindow();
	if ( IsAppWindow(window) ) {
		te = ((DocumentPeek) window)->docTE;
		key = event->message & charCodeMask;
		/* we have a char. for our window; see if we are still below TextEdit's
			limit for the number of characters (but deletes are always rad) */
		if ( key == kDelChar ||
				(*te)->teLength - ((*te)->selEnd - (*te)->selStart) + 1 <
				kMaxTELength ) {
			TEKey(key, te);
			AdjustScrollbars(window, false);
			AdjustTE(window);
		} else
			AlertUser(eExceedChar);
	}
#endif
} /*DoKeyDown*/


/*	Calculate a sleep value for WaitNextEvent. This takes into account the things
	that DoIdle does with idle time. */

#pragma segment Main
unsigned long GetSleep()
{
	long		sleep;
	WindowPtr	window;
	TEHandle	te;

	sleep = MAXLONG;						/* default value for sleep */
	if ( !gInBackground ) {
		window = FrontWindow();			/* and the front window is ours... */
		if ( IsAppWindow(window) ) {
			te = ((DocumentPeek) (window))->docTE;	/* and the selection is an insertion point... */
			if ( (*te)->selStart == (*te)->selEnd )
				sleep = GetCaretTime();		/* blink time for the insertion point */
		}
	}
	return sleep;
} /*GetSleep*/


/*	Common algorithm for pinning the value of a control. It returns the actual amount
	the value of the control changed. Note the pinning is done for the sake of returning
	the amount the control value changed. */

#pragma segment Main
void CommonAction(control,amount)
	ControlHandle control;
	short		*amount;
{
	short		value, max;
	
	value = GetCtlValue(control);	/* get current value */
	max = GetCtlMax(control);		/* and maximum value */
	*amount = value - *amount;
	if ( *amount < 0 )
		*amount = 0;
	else if ( *amount > max )
		*amount = max;
	SetCtlValue(control, *amount);
	*amount = value - *amount;		/* calculate the real change */
} /* CommonAction */


/* Determines how much to change the value of the vertical scrollbar by and how
	much to scroll the TE record. */

#pragma segment Main
void CVActionProc(control,part)
	ControlHandle control;
	short		part;
{
	short		amount;
	WindowPtr	window;
	TEPtr		te;
	
	if ( part != 0 ) {				/* if it was actually in the control */
		window = (*control)->contrlOwner;
		te = *((DocumentPeek) window)->docTE;
		switch ( part ) {
			case inUpButton:
			case inDownButton:		/* one line */
				amount = 1;
				break;
			case inPageUp:			/* one page */
			case inPageDown:
				amount = (te->viewRect.bottom - te->viewRect.top) / te->lineHeight;
				break;
		}
		if ( (part == inDownButton) || (part == inPageDown) )
			amount = -amount;		/* reverse direction for a downer */
		CommonAction(control, &amount);
		if ( amount != 0 )
			TEScroll(0, amount * te->lineHeight, ((DocumentPeek) window)->docTE);
	}
} /* VActionProc */


/* Determines how much to change the value of the horizontal scrollbar by and how
much to scroll the TE record. */

#pragma segment Main
void CHActionProc(control,part)
	ControlHandle control;
	short		part;
{
	short		amount;
	WindowPtr	window;
	TEPtr		te;
	
	if ( part != 0 ) {
		window = (*control)->contrlOwner;
		te = *((DocumentPeek) window)->docTE;
		switch ( part ) {
			case inUpButton:
			case inDownButton:		/* a few pixels */
				amount = kButtonScroll;
				break;
			case inPageUp:			/* a page */
			case inPageDown:
				amount = te->viewRect.right - te->viewRect.left;
				break;
		}
		if ( (part == inDownButton) || (part == inPageDown) )
			amount = -amount;		/* reverse direction */
		CommonAction(control, &amount);
		if ( amount != 0 )
			TEScroll(amount, 0, ((DocumentPeek) window)->docTE);
	}
} /* VActionProc */


/* This is called whenever we get a null event et al.
 It takes care of necessary periodic actions. For this program, it calls TEIdle. */

#pragma segment Main
void DoIdle()
{
	WindowPtr	window;

	window = FrontWindow();
	if ( IsAppWindow(window) )
		TEIdle(((DocumentPeek) window)->docTE);
} /*DoIdle*/


/* Draw the contents of an application window. */

#pragma segment Main
void DrawWindow(window)
	WindowPtr	window;
{
	SetPort(window);
	EraseRect(&window->portRect);
	DrawControls(window);
	DrawGrowIcon(window);
	TEUpdate(&window->portRect, ((DocumentPeek) window)->docTE);
} /*DrawWindow*/


/*	Enable and disable menus based on the current state.
	The user can only select enabled menu items. We set up all the menu items
	before calling MenuSelect or MenuKey, since these are the only times that
	a menu item can be selected. Note that MenuSelect is also the only time
	the user will see menu items. This approach to deciding what enable/
	disable state a menu item has the advantage of concentrating all
	the decision-making in one routine, as opposed to being spread throughout
	the application. Other application designs may take a different approach
	that may or may not be as valid. */

#pragma segment Main
void AdjustMenus()
{
	WindowPtr	window;
	MenuHandle	menu;
	long		offset;
	Boolean		undo;
	Boolean		cutCopyClear;
	Boolean		paste;
	TEHandle	te;
	int			i, n;

	window = FrontWindow();

	menu = GetMHandle(mFile);
	if ( gNumDocuments < kMaxOpenDocuments )
		EnableItem(menu, iNew);		/* New is enabled when we can open more documents */
	else
		DisableItem(menu, iNew);
#ifdef CLOSABLE_DOCUMENT
	if ( window != nil )			/* Close is enabled when there is a window to close */
		EnableItem(menu, iClose);
	else
		DisableItem(menu, iClose);
#else
	DisableItem(menu, iClose);
#endif

	menu = GetMHandle(mEdit);
	undo = false;
	cutCopyClear = false;
	paste = false;
	if ( IsDAWindow(window) ) {
		undo = true;				/* all editing is enabled for DA windows */
		cutCopyClear = true;
		paste = true;
	} else if ( IsAppWindow(window) ) {
		te = ((DocumentPeek) window)->docTE;
		if ( (*te)->selStart < (*te)->selEnd )
			cutCopyClear = true;
			/* Cut, Copy, and Clear is enabled for app. windows with selections */
#ifdef WRITABLE_DOCUMENT
		if ( GetScrap(nil, 'TEXT', &offset)  > 0)
			paste = true;			/* if there's any text in the clipboard, paste is enabled */
#endif
	}
	if ( undo )
		EnableItem(menu, iUndo);
	else
		DisableItem(menu, iUndo);
	if ( cutCopyClear ) {
#ifdef WRITABLE_DOCUMENT
		EnableItem(menu, iCut);
		EnableItem(menu, iCopy);
		EnableItem(menu, iClear);
#else
		if (IsDAWindow(window)) {
			EnableItem(menu, iCut);
			EnableItem(menu, iCopy);
			EnableItem(menu, iClear);
		} else {
			DisableItem(menu, iCut);
			EnableItem(menu, iCopy);
			DisableItem(menu, iClear);
		}
#endif
	} else {
		DisableItem(menu, iCut);
		DisableItem(menu, iCopy);
		DisableItem(menu, iClear);
	}
	if ( paste )
		EnableItem(menu, iPaste);
	else
		DisableItem(menu, iPaste);

	menu = GetMHandle(mFonts);
	n = CountMItems(menu);
	for (i = 1; i <= n; i++)
		DisableItem(menu, i);

} /*AdjustMenus*/


/*	This is called when an item is chosen from the menu bar (after calling
	MenuSelect or MenuKey). It does the right thing for each command. */

#pragma segment Main
void DoMenuCommand(menuResult)
	long		menuResult;
{
	short		menuID, menuItem;
	short		itemHit, daRefNum;
	Str255		daName;
	OSErr		saveErr;
	TEHandle	te;
	WindowPtr	window;
	Handle		aHandle;
	long		oldSize, newSize;
	long		total, contig;

	window = FrontWindow();
	menuID = HiWord(menuResult);	/* use macros for efficiency to... */
	menuItem = LoWord(menuResult);	/* get menu item number and menu number */
	switch ( menuID ) {
		case mApple:
			switch ( menuItem ) {
				case iAbout:		/* bring up alert for About */
					itemHit = Alert(rAboutAlert, nil);
					break;
				default:			/* all non-About items in this menu are DAs et al */
					/* type Str255 is an array in MPW 3 */
					GetItem(GetMHandle(mApple), menuItem, daName);
					daRefNum = OpenDeskAcc(daName);
					break;
			}
			break;
		case mFile:
			switch ( menuItem ) {
				case iNew:
					DoNew();
					break;
				case iClose:
					DoCloseWindow(FrontWindow());			/* ignore the result */
					break;
				case iQuit:
					Terminate();
					break;
			}
			break;
		case mEdit:					/* call SystemEdit for DA editing & MultiFinder */
			if ( !SystemEdit(menuItem-1) ) {
				te = ((DocumentPeek) FrontWindow())->docTE;
				switch ( menuItem ) {
					case iCut:
						if ( ZeroScrap() == noErr ) {
#ifndef AUX				/* XXX A/UX omits PurgeSpace support!? */
							PurgeSpace(&total, &contig);
							if ((*te)->selEnd - (*te)->selStart + kTESlop > contig)
								AlertUser(eNoSpaceCut);
							else 
#endif
							    {
								TECut(te);
								if ( TEToScrap() != noErr ) {
									AlertUser(eNoCut);
									ZeroScrap();
								}
							}
						}
						break;
					case iCopy:
						if ( ZeroScrap() == noErr ) {
							TECopy(te);	/* after copying, export the TE scrap */
							if ( TEToScrap() != noErr ) {
								AlertUser(eNoCopy);
								ZeroScrap();
							}
						}
						break;
					case iPaste:	/* import the TE scrap before pasting */
						if ( TEFromScrap() == noErr ) {
							if ( TEGetScrapLen() + ((*te)->teLength -
								((*te)->selEnd - (*te)->selStart)) > kMaxTELength )
								AlertUser(eExceedPaste);
							else {
								aHandle = (Handle) TEGetText(te);
								oldSize = GetHandleSize(aHandle);
								newSize = oldSize + TEGetScrapLen() + kTESlop;
								SetHandleSize(aHandle, newSize);
								saveErr = MemError();
								SetHandleSize(aHandle, oldSize);
								if (saveErr != noErr)
									AlertUser(eNoSpacePaste);
								else
									TEPaste(te);
							}
						}
						else
							AlertUser(eNoPaste);
						break;
					case iClear:
						TEDelete(te);
						break;
				}
			AdjustScrollbars(window, false);
			AdjustTE(window);
			}
			break;
	}
	HiliteMenu(0);					/* unhighlight what MenuSelect (or MenuKey) hilited */
} /*DoMenuCommand*/


/* Create a new document and window. */

#pragma segment Main
void DoNew()
{
	Boolean		good;
	Ptr			storage;
	WindowPtr	window;
	Rect		destRect, viewRect;
	DocumentPeek doc;

	storage = NewPtr(sizeof(DocumentRecord));
	if ( storage != nil ) {
		window = GetNewWindow(rDocWindow, storage, (WindowPtr) -1);
		if ( window != nil ) {
			FontInfo fi;

			gNumDocuments += 1;			/* this will be decremented when we call DoCloseWindow */
			good = false;

			SetPort(window);
			TextFont(monaco);
			TextFace(0);
			TextSize(9);
			GetFontInfo(&fi);

			doc =  (DocumentPeek) window;
			GetTERect(window, &viewRect);
			destRect = viewRect;
#ifdef PARTIALVIEW_DOCUMENT
			destRect.right = destRect.left + kMaxDocWidth;
#endif
			doc->docTE = TENew(&destRect, &viewRect);
			good = doc->docTE != nil;	/* if TENew succeeded, we have a good document */
			if ( good ) {				/* 1.02 - good document? then proceed */

				(*doc->docTE)->txFont = monaco;
				(*doc->docTE)->txSize = 9;
				(*doc->docTE)->txFace = 0;
				(*doc->docTE)->crOnly = -1;
				(*doc->docTE)->lineHeight = fi.ascent + fi.descent + fi.leading;
				(*doc->docTE)->fontAscent = fi.ascent;

				AdjustViewRect(doc->docTE);
				TEAutoView(true, doc->docTE);
				doc->docClik = (ProcPtr) (*doc->docTE)->clikLoop;
				(*doc->docTE)->clikLoop = (ClikLoopProcPtr) AsmClikLoop;
			}
			
			if ( good ) {				/* good document? then get scrollbars */
				doc->docVScroll = GetNewControl(rVScroll, window);
				good = (doc->docVScroll != nil);
			}
			if ( good) {
				doc->docHScroll = GetNewControl(rHScroll, window);
				good = (doc->docHScroll != nil);
			}
			
			if ( good ) {				/* good? then adjust & draw the controls, draw the window */
				/* false to AdjustScrollValues means musn't redraw; technically, of course,
				the window is hidden so it wouldn't matter whether we called ShowControl or not. */
				AdjustScrollValues(window, false);
				ShowWindow(window);
			} else {
				DoCloseWindow(window);	/* otherwise regret we ever created it... */
				AlertUser(eNoWindow);			/* and tell user */
			}
		} else
			DisposPtr(storage);			/* get rid of the storage if it is never used */
	}
} /*DoNew*/


/* Close a window. This handles desk accessory and application windows. */

/*	1.01 - At this point, if there was a document associated with a
	window, you could do any document saving processing if it is 'dirty'.
	DoCloseWindow would return true if the window actually closed, i.e.,
	the user didn't cancel from a save dialog. This result is handy when
	the user quits an application, but then cancels the save of a document
	associated with a window. */

#pragma segment Main
Boolean DoCloseWindow(window)
	WindowPtr	window;
{
	TEHandle	te;

	if ( IsDAWindow(window) )
		CloseDeskAcc(((WindowPeek) window)->windowKind);
	else if ( IsAppWindow(window) ) {
		te = ((DocumentPeek) window)->docTE;
		if ( te != nil )
			TEDispose(te);			/* dispose the TEHandle if we got far enough to make one */
		/*	1.01 - We used to call DisposeWindow, but that was technically
			incorrect, even though we allocated storage for the window on
			the heap. We should instead call CloseWindow to have the structures
			taken care of and then dispose of the storage ourselves. */
		CloseWindow(window);
		DisposPtr((Ptr) window);
		gNumDocuments -= 1;
	}
	return true;
} /*DoCloseWindow*/


/**************************************************************************************
*** 1.01 DoCloseBehind(window) was removed ***

	1.01 - DoCloseBehind was a good idea for closing windows when quitting
	and not having to worry about updating the windows, but it suffered
	from a fatal flaw. If a desk accessory owned two windows, it would
	close both those windows when CloseDeskAcc was called. When DoCloseBehind
	got around to calling DoCloseWindow for that other window that was already
	closed, things would go very poorly. Another option would be to have a
	procedure, GetRearWindow, that would go through the window list and return
	the last window. Instead, we decided to present the standard approach
	of getting and closing FrontWindow until FrontWindow returns NIL. This
	has a potential benefit in that the window whose document needs to be saved
	may be visible since it is the front window, therefore decreasing the
	chance of user confusion. For aesthetic reasons, the windows in the
	application should be checked for updates periodically and have the
	updates serviced.
**************************************************************************************/


/* Clean up the application and exit. We close all of the windows so that
 they can update their documents, if any. */
 
/*	1.01 - If we find out that a cancel has occurred, we won't exit to the
	shell, but will return instead. */

#pragma segment Main
void Terminate()
{
	WindowPtr	aWindow;
	Boolean		closed;
	
	closed = true;
	do {
		aWindow = FrontWindow();				/* get the current front window */
		if (aWindow != nil)
			closed = DoCloseWindow(aWindow);	/* close this window */	
	}
	while (closed && (aWindow != nil));
	if (closed)
		ExitToShell();							/* exit if no cancellation */
} /*Terminate*/


/*	Set up the whole world, including global variables, Toolbox managers,
	menus, and a single blank document. */

/*	1.01 - The code that used to be part of ForceEnvirons has been moved into
	this module. If an error is detected, instead of merely doing an ExitToShell,
	which leaves the user without much to go on, we call AlertUser, which puts
	up a simple alert that just says an error occurred and then calls ExitToShell.
	Since there is no other cleanup needed at this point if an error is detected,
	this form of error- handling is acceptable. If more sophisticated error recovery
	is needed, an exception mechanism, such as is provided by Signals, can be used. */

#pragma segment Initialize
void Initialize()
{
	Handle	menuBar;
	long	total, contig;
	EventRecord event;
	short	count;

	gInBackground = false;

	InitGraf((Ptr) &qd.thePort);
	MacInitFonts();
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(nil);
	InitCursor();

	/*	Call MPPOpen and ATPLoad at this point to initialize AppleTalk,
	 	if you are using it. */
	/*	NOTE -- It is no longer necessary, and actually unhealthy, to check
		PortBUse and SPConfig before opening AppleTalk. The drivers are capable
		of checking for port availability themselves. */
	
	/*	This next bit of code is necessary to allow the default button of our
		alert be outlined.
		1.02 - Changed to call EventAvail so that we don't lose some important
		events. */
	 
	for (count = 1; count <= 3; count++)
		EventAvail(everyEvent, &event);
	
	/*	Ignore the error returned from SysEnvirons; even if an error occurred,
		the SysEnvirons glue will fill in the SysEnvRec. You can save a redundant
		call to SysEnvirons by calling it after initializing AppleTalk. */
	 
	SysEnvirons(kSysEnvironsVersion, &gMac);
	
	/* Make sure that the machine has at least 128K ROMs. If it doesn't, exit. */
	
	if (gMac.machineType < 0) BigBadError(eWrongMachine);
	
	/*	1.02 - Move TrapAvailable call to after SysEnvirons so that we can tell
		in TrapAvailable if a tool trap value is out of range. */
		
	gHasWaitNextEvent = TrapAvailable(_WaitNextEvent, ToolTrap);

	/*	1.01 - We used to make a check for memory at this point by examining ApplLimit,
		ApplicZone, and StackSpace and comparing that to the minimum size we told
		MultiFinder we needed. This did not work well because it assumed too much about
		the relationship between what we asked MultiFinder for and what we would actually
		get back, as well as how to measure it. Instead, we will use an alternate
		method comprised of two steps. */
	 
	/*	It is better to first check the size of the application heap against a value
		that you have determined is the smallest heap the application can reasonably
		work in. This number should be derived by examining the size of the heap that
		is actually provided by MultiFinder when the minimum size requested is used.
		The derivation of the minimum size requested from MultiFinder is described
		in Sample.h. The check should be made because the preferred size can end up
		being set smaller than the minimum size by the user. This extra check acts to
		insure that your application is starting from a solid memory foundation. */
	 
	if ((long) GetApplLimit() - (long) ApplicZone() < kMinHeap) BigBadError(eSmallSize);
	
	/*	Next, make sure that enough memory is free for your application to run. It
		is possible for a situation to arise where the heap may have been of required
		size, but a large scrap was loaded which left too little memory. To check for
		this, call PurgeSpace and compare the result with a value that you have determined
		is the minimum amount of free memory your application needs at initialization.
		This number can be derived several different ways. One way that is fairly
		straightforward is to run the application in the minimum size configuration
		as described previously. Call PurgeSpace at initialization and examine the value
		returned. However, you should make sure that this result is not being modified
		by the scrap's presence. You can do that by calling ZeroScrap before calling
		PurgeSpace. Make sure to remove that call before shipping, though. */
	
	/* ZeroScrap(); */

#ifndef AUX	/* XXX A/UX omits PurgeSpace support!? */
	PurgeSpace(&total, &contig);
	if (total < kMinSpace)
		if (UnloadScrap() != noErr)
			BigBadError(eNoMemory);
		else {
			PurgeSpace(&total, &contig);
			if (total < kMinSpace)
				BigBadError(eNoMemory);
		}
#endif

	/*	The extra benefit to waiting until after the Toolbox Managers have been initialized
		to check memory is that we can now give the user an alert to tell him/her what
		happened. Although it is possible that the memory situation could be worsened by
		displaying an alert, MultiFinder would gracefully exit the application with
		an informative alert if memory became critical. Here we are acting more
		in a preventative manner to avoid future disaster from low-memory problems. */

	menuBar = GetNewMBar(rMenuBar);			/* read menus into menu bar */
	if ( menuBar == nil )
				BigBadError(eNoMemory);
	SetMenuBar(menuBar);					/* install menus */
	DisposHandle(menuBar);
	AddResMenu(GetMHandle(mApple), 'DRVR');	/* add DA names to Apple menu */
	AddResMenu(GetMHandle(mFonts), 'FONT'); /* add font names to Fonts menu */
	DrawMenuBar();

	gNumDocuments = 0;

	/* do other initialization here */

	DoNew();								/* create a single empty document */
} /*Initialize*/


/* Used whenever a, like, fully fatal error happens */
#pragma segment Initialize
void BigBadError(error)
	short error;
{
	AlertUser(error);
	ExitToShell();
}


/* Return a rectangle that is inset from the portRect by the size of
	the scrollbars and a little extra margin. */

#pragma segment Main
void GetTERect(window,teRect)
	WindowPtr	window;
	Rect		*teRect;
{
	*teRect = window->portRect;
	InsetRect(teRect, kTextMargin, kTextMargin);	/* adjust for margin */
	teRect->bottom = teRect->bottom - 15;		/* and for the scrollbars */
	teRect->right = teRect->right - 15;
} /*GetTERect*/


/* Update the TERec's view rect so that it is the greatest multiple of
	the lineHeight that still fits in the old viewRect. */

#pragma segment Main
void AdjustViewRect(docTE)
	TEHandle	docTE;
{
	TEPtr		te;
	
	te = *docTE;
	te->viewRect.bottom = (((te->viewRect.bottom - te->viewRect.top) / te->lineHeight)
							* te->lineHeight) + te->viewRect.top;
} /*AdjustViewRect*/


/* Scroll the TERec around to match up to the potentially updated scrollbar
	values. This is really useful when the window has been resized such that the
	scrollbars became inactive but the TERec was already scrolled. */

#pragma segment Main
void AdjustTE(window)
	WindowPtr	window;
{
	TEPtr		te;
	
	te = *((DocumentPeek)window)->docTE;
	TEScroll((te->viewRect.left - te->destRect.left) -
			GetCtlValue(((DocumentPeek)window)->docHScroll),
			(te->viewRect.top - te->destRect.top) -
				(GetCtlValue(((DocumentPeek)window)->docVScroll) *
				te->lineHeight),
			((DocumentPeek)window)->docTE);
} /*AdjustTE*/


/* Calculate the new control maximum value and current value, whether it is the horizontal or
	vertical scrollbar. The vertical max is calculated by comparing the number of lines to the
	vertical size of the viewRect. The horizontal max is calculated by comparing the maximum document
	width to the width of the viewRect. The current values are set by comparing the offset between
	the view and destination rects. If necessary and we canRedraw, have the control be re-drawn by
	calling ShowControl. */

#pragma segment Main
void AdjustHV(isVert,control,docTE,canRedraw)
	Boolean		isVert;
	ControlHandle control;
	TEHandle	docTE;
	Boolean		canRedraw;
{
	short		value, lines, max;
	short		oldValue, oldMax;
	TEPtr		te;
	
	oldValue = GetCtlValue(control);
	oldMax = GetCtlMax(control);
	te = *docTE;							/* point to TERec for convenience */
	if ( isVert ) {
		lines = te->nLines;
		/* since nLines isn't right if the last character is a return, check for that case */
		if ( *(*te->hText + te->teLength - 1) == kCrChar )
			lines += 1;
		max = lines - ((te->viewRect.bottom - te->viewRect.top) /
				te->lineHeight);
	} else
		max = kMaxDocWidth - (te->viewRect.right - te->viewRect.left);
	
	if ( max < 0 ) max = 0;
	SetCtlMax(control, max);
	
	/* Must deref. after SetCtlMax since, technically, it could draw and therefore move
		memory. This is why we don't just do it once at the beginning. */
	te = *docTE;
	if ( isVert )
		value = (te->viewRect.top - te->destRect.top) / te->lineHeight;
	else
		value = te->viewRect.left - te->destRect.left;
	
	if ( value < 0 ) value = 0;
	else if ( value >  max ) value = max;
	
	SetCtlValue(control, value);
	/* now redraw the control if it needs to be and can be */
	if ( canRedraw || (max != oldMax) || (value != oldValue) )
		ShowControl(control);
} /*AdjustHV*/


/* Simply call the common adjust routine for the vertical and horizontal scrollbars. */

#pragma segment Main
void AdjustScrollValues(window,canRedraw)
	WindowPtr	window;
	Boolean		canRedraw;
{
	DocumentPeek doc;
	
	doc = (DocumentPeek)window;
	AdjustHV(true, doc->docVScroll, doc->docTE, canRedraw);
	AdjustHV(false, doc->docHScroll, doc->docTE, canRedraw);
} /*AdjustScrollValues*/


/*	Re-calculate the position and size of the viewRect and the scrollbars.
	kScrollTweek compensates for off-by-one requirements of the scrollbars
	to have borders coincide with the growbox. */

#pragma segment Main
void AdjustScrollSizes(window)
	WindowPtr	window;
{
	Rect		teRect;
	DocumentPeek doc;
	
	doc = (DocumentPeek) window;
	GetTERect(window, &teRect);							/* start with TERect */
	(*doc->docTE)->viewRect = teRect;
	AdjustViewRect(doc->docTE);							/* snap to nearest line */
	MoveControl(doc->docVScroll, window->portRect.right - kScrollbarAdjust, -1);
	SizeControl(doc->docVScroll, kScrollbarWidth, (window->portRect.bottom - 
				window->portRect.top) - (kScrollbarAdjust - kScrollTweek));
	MoveControl(doc->docHScroll, -1, window->portRect.bottom - kScrollbarAdjust);
	SizeControl(doc->docHScroll, (window->portRect.right - 
				window->portRect.left) - (kScrollbarAdjust - kScrollTweek),
				kScrollbarWidth);
} /*AdjustScrollSizes*/


/* Turn off the controls by jamming a zero into their contrlVis fields (HideControl erases them
	and we don't want that). If the controls are to be resized as well, call the procedure to do that,
	then call the procedure to adjust the maximum and current values. Finally re-enable the controls
	by jamming a $FF in their contrlVis fields. */

#pragma segment Main
void AdjustScrollbars(window,needsResize)
	WindowPtr	window;
	Boolean		needsResize;
{
	DocumentPeek doc;
	
	doc = (DocumentPeek) window;
	/* First, turn visibility of scrollbars off so we won't get unwanted redrawing */
	(*doc->docVScroll)->contrlVis = kControlInvisible;	/* turn them off */
	(*doc->docHScroll)->contrlVis = kControlInvisible;
	if ( needsResize )									/* move & size as needed */
		AdjustScrollSizes(window);
	AdjustScrollValues(window, needsResize);			/* fool with max and current value */
	/* Now, restore visibility in case we never had to ShowControl during adjustment */
	(*doc->docVScroll)->contrlVis = kControlVisible;	/* turn them on */
	(*doc->docHScroll)->contrlVis = kControlVisible;
} /* AdjustScrollbars */


/* Gets called from our assembly language routine, AsmClikLoop, which is in
	turn called by the TEClick toolbox routine. Saves the windows clip region,
	sets it to the portRect, adjusts the scrollbar values to match the TE scroll
	amount, then restores the clip region. */

#pragma segment Main
void CClikLoop ()
{
	WindowPtr	window;
	RgnHandle	region;
	
	window = FrontWindow();
	region = NewRgn();
	GetClip(region);					/* save clip */
	ClipRect(&window->portRect);
	AdjustScrollValues(window, true);	/* pass true for canRedraw */
	SetClip(region);					/* restore clip */
	DisposeRgn(region);
} /* Pascal/C ClikLoop */


/* Gets called from our assembly language routine, AsmClikLoop, which is in
	turn called by the TEClick toolbox routine. It returns the address of the
	default clikLoop routine that was put into the TERec by TEAutoView to
	AsmClikLoop so that it can call it. */

#pragma segment Main
ProcPtr GetOldClikLoop()
{
	return ((DocumentPeek)FrontWindow())->docClik;
} /* GetOldClikLoop */


/*	Check to see if a window belongs to the application. If the window pointer
	passed was NIL, then it could not be an application window. WindowKinds
	that are negative belong to the system and windowKinds less than userKind
	are reserved by Apple except for windowKinds equal to dialogKind, which
	mean it is a dialog.
	1.02 - In order to reduce the chance of accidentally treating some window
	as an AppWindow that shouldn't be, we'll only return true if the windowkind
	is userKind. If you add different kinds of windows to Sample you'll need
	to change how this all works. */

#pragma segment Main
Boolean IsAppWindow(window)
	WindowPtr	window;
{
	short		windowKind;

	if ( window == nil )
		return false;
	else {	/* application windows have windowKinds = userKind (8) */
		windowKind = ((WindowPeek) window)->windowKind;
		return (windowKind == userKind);
	}
} /*IsAppWindow*/


/* Check to see if a window belongs to a desk accessory. */

#pragma segment Main
Boolean IsDAWindow(window)
	WindowPtr	window;
{
	if ( window == nil )
		return false;
	else	/* DA windows have negative windowKinds */
		return ((WindowPeek) window)->windowKind < 0;
} /*IsDAWindow*/


/*	Check to see if a given trap is implemented. This is only used by the
	Initialize routine in this program, so we put it in the Initialize segment.
	The recommended approach to see if a trap is implemented is to see if
	the address of the trap routine is the same as the address of the
	Unimplemented trap. */
/*	1.02 - Needs to be called after call to SysEnvirons so that it can check
	if a ToolTrap is out of range of a pre-MacII ROM. */

#pragma segment Initialize
Boolean TrapAvailable(tNumber,tType)
	short		tNumber;
	TrapType	tType;
{
	if ( ( tType == (unsigned char) ToolTrap ) &&
		( gMac.machineType > envMachUnknown ) &&
		( gMac.machineType < envMacII ) ) {		/* it's a 512KE, Plus, or SE */
		tNumber = tNumber & 0x03FF;
		if ( tNumber > 0x01FF )					/* which means the tool traps */
			tNumber = _Unimplemented;			/* only go to 0x01FF */
	}
	return NGetTrapAddress(tNumber, tType) != GetTrapAddress(_Unimplemented);
} /*TrapAvailable*/


/*	Display an alert that tells the user an error occurred, then exit the program.
	This routine is used as an ultimate bail-out for serious errors that prohibit
	the continuation of the application. Errors that do not require the termination
	of the application should be handled in a different manner. Error checking and
	reporting has a place even in the simplest application. The error number is used
	to index an 'STR#' resource so that a relevant message can be displayed. */

#pragma segment Main
void AlertUser(error)
	short		error;
{
	short		itemHit;
	Str255		message;

	SetCursor(&qd.arrow);
	/* type Str255 is an array in MPW 3 */
	GetIndString(message, kErrStrings, error);
	ParamText(message, "", "", "");
	itemHit = Alert(rUserAlert, nil);
} /* AlertUser */



#define PREFLIGHT(family_name, face, size) \
{\
	short fNum;\
	GrafPtr thePort;\
	EventRecord event;\
	printf("Preflighting %s ...", family_name);\
    getfnum (family_name, &fNum);\
    TextFont (fNum);\
    TextFace (face);\
    TextSize (size);\
	GetPort(&thePort);\
	EraseRect(&thePort->portRect);\
	MoveTo(0, 17);\
	DrawText(theChars, 0, 256);\
	printf("\n");\
    fflush(stdout);\
	GetNextEvent(everyEvent, &event);\
}

static void
PreflightOutlineFonts()
{
#ifdef notdef
	unsigned char theChars[256];
	register unsigned char *p;
	register unsigned ch;

    for (p = theChars, ch = 0; ch <= 255; ++p, ++ch)
        *p = ch;

	PREFLIGHT("courier", 0, 17);
	PREFLIGHT("courier", bold, 17);
	PREFLIGHT("helvetica", 0, 17);
	PREFLIGHT("helvetica", bold, 17);
	PREFLIGHT("symbol", 0, 17);
	PREFLIGHT("times", 0, 17);
	PREFLIGHT("times", bold, 17);
	PREFLIGHT("times", italic, 17);
	PREFLIGHT("times", (bold | italic), 17);
#endif

}

static struct timeval pollTimeval = {0, 0};

typedef unsigned char *pointer;
typedef int Bool;

#include        <sys/param.h>
#include	<osdep.h>
extern Bool NewOutputPending;
extern long ClientsWriteBlocked[]; /* XXX Should be passed in through BlockHandler */

/* Called from character drawing loop to prevent UI lockup for long stretches */
void
MacCheckUI()
{
	EventRecord     event;
	GrafPtr savePort;

	GetPort(&savePort);

	if (GetNextEvent(everyEvent, &event)) {
		/* make sure we have the right cursor before handling the event */
		AdjustCursor(event.where, cursorRgn);
		DoEvent(&event);
	}
	else
		DoIdle(); /* perform idle tasks when it's not our event */

	SetPort(savePort);
}

void
MacBlockHandler(data, pTimeout, pReadmask)
	pointer data, pTimeout, pReadmask;
{
	EventRecord theEvent;

	if (NewOutputPending) /* XXX WaitForSomething should do this earlier! */
		FlushAllOutput();

	while (1) {
		theEvent.what = -1;

		ui_setselect(32, *(int *)pReadmask, ClientsWriteBlocked[0], 0);
		(void) WaitNextEvent(everyEvent, &theEvent, GetSleep(), cursorRgn);
		ui_setselect(32, 0, 0, 0);

		if (theEvent.what != nullEvent) {
			AdjustCursor(theEvent.where, cursorRgn);
			DoEvent(&theEvent);
		} else {
			DoIdle();
			return;
		}
	}
}

void
MacWakeupHandler(data, result, pReadmask)
	pointer data, pReadmask;
	unsigned long result;
{
}

void
MacFontLogInfo(buf)
	char *buf;
{
	TEHandle teH;
	GrafPtr savePort;
	WindowPtr window = FrontWindow();

	if (initDone && window) {
		GetPort(&savePort);
		SetPort(window);
		teH = ((DocumentPeek) window)->docTE;
		TESetSelect(32767, 32767, teH);
		TEInsert(buf, strlen(buf), teH);
		TEPinScroll(0, -32000, teH);
		SetPort(savePort);
	} else
		fprintf(stderr,"%s\n",buf);
}

void
MacFontLogNotice(buf)
	char *buf;
{
	/* Could change font */
	MacFontLogInfo(buf);
}

void
MacFontLogError(buf)
	char *buf;
{
	/* Could change font */
	MacFontLogInfo(buf);
}

static void
MacFontBanner()
{
	char buf[256];

	sprintf(buf, "X Font Server for A/UX\250\r");
	MacFontLogInfo(buf);
	sprintf(buf, "Version Number: %d\r", VENDOR_RELEASE);
	MacFontLogInfo(buf);
	sprintf(buf, "Protocol Major Release Number: %d\r",FS_PROTOCOL);
	MacFontLogInfo(buf);
	sprintf(buf, "Protocol Minor Release Number: %d\r",FS_PROTOCOL_MINOR);
	MacFontLogInfo(buf);
	sprintf(buf, "\r");
	MacFontLogInfo(buf);
}

extern int errno;
extern char *sys_errlist[];
extern int sys_nerr;

void
MacFontPerror(s)
	char *s;
{
	char buf[256];

	if (errno > sys_nerr) {
		sprintf(buf, "%s: errno = %d\r", s, errno);
		MacFontLogError(buf);
	} else {
		sprintf(buf, "%s: %s\r", s, sys_errlist[errno]);
		MacFontLogError(buf);
	}
}
