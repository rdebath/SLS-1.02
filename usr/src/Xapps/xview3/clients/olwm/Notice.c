/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)Notice.c	26.19	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <olgx/olgx.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "screen.h"
#include "notice.h"
#include "globals.h"
#include "mem.h"
#include "events.h"


extern int		PointInRect();
extern SemanticAction 	FindKeyboardAction();

#define	NOTICE_EVENT_MASK	( ButtonPressMask | ButtonReleaseMask \
				  | PointerMotionMask | KeyPressMask \
				  | ExposureMask )
#define	NOTICE_ATTR_MASK	( CWBorderPixel | CWColormap | \
				  CWEventMask | CWSaveUnder )

/* difference between inside beveled box and outside beveled box */
#define BORDER_WIDTH		5	
#define	MIN_BOX_SIDE		( 15 + BORDER_WIDTH )	/* betw side & text */
#define	MIN_BOX_TOP		( 15 + BORDER_WIDTH )	/* betw top & text */
#define	MIN_BOX_BOTTOM		( 15 + BORDER_WIDTH )	/* betw bot & text */
#define MIN_BUTTON_SPACE	15		/* space between buttons */
#define MIN_BUTTON_VSPACE	15		/* space above buttons */
#define MIN_STRING_VSPACE	5		/* space above/below strings */
#define OUTLINE_WIDTH		2		/* thickness of 2D border */

typedef struct {
	int		x;
	int		y;
	unsigned int	width;		/* space taken up by text */
	unsigned int	fullWidth;	/* width including endcaps */
	char		accelerator;	/* mouseless accelerator key */
} noticeButtonDetails;

typedef struct {
	Display			*dpy;
	ScreenInfo		*scrInfo;
	NoticeBox		*noticeBox;
	int			numStrings;
	char			**stringText;
	Window			window;
	unsigned int		buttonHeight;
	unsigned int		fontHeight;
	unsigned int		boxHeight;
	unsigned int		boxWidth;
	int			x;
	int			y;
	int			totalButtonWidth;
	noticeButtonDetails	*buttonInfo;
	int			buttonSelected;
	int			buttonFocus;
	int			buttonDown;
	int			buttonDrawnDown;
	void			(*noticeCallback)();
	int			pointerX,pointerY;
	Bool			ignoreExpose;
	Bool			warped;
} noticeBoxDetails;

static void 		calculateBoxDimensions();
static void 		drawNoticeBox();
static void		noticeDone();
static int		noticeInterposer();

noticeBoxDetails	*CreateNoticeBox();
void			DestroyNoticeBox();
void			ShowNoticeBox();


/******************************************************************
 *			Private Draw Functions
 ******************************************************************/

/*
 * calculateBoxDimensions -- determine size of box needed, etc. based on
 * 		information passed in in noticeBox structure.  Return
 *		values in boxDetails structure.
 */
static void
calculateBoxDimensions( noticeBox, boxDetails )
NoticeBox		*noticeBox;
noticeBoxDetails	*boxDetails;
{
	int		screen = boxDetails->scrInfo->screen;
	unsigned int	displayWidth, displayHeight;
	int		longestStringLength = 0;
	int		totalButtonWidth = 0;
	Graphics_info	*gisButton = boxDetails->scrInfo->gi[BUTTON_GINFO];
	int		ii;
	char		*tok,*str;
static	char		*sep = "\n";
extern	char		*strtok();

	boxDetails->fontHeight = GRV.TextFontInfo->ascent + 
					GRV.TextFontInfo->descent;

	/* calculate the width of the text which appears inside the button */
	for ( ii = 0 ; ii < noticeBox->numButtons ; ii++ ) {
		boxDetails->buttonInfo[ii].width = 
			XTextWidth( GRV.ButtonFontInfo, 
				    noticeBox->buttonText[ii], 
				    strlen(noticeBox->buttonText[ii]) );
		boxDetails->buttonInfo[ii].fullWidth = 
			boxDetails->buttonInfo[ii].width 
				+ 2 * ButtonEndcap_Width(gisButton);
	}

	/* make the key accelerators from the button text string */
	for ( ii = 0 ; ii < noticeBox->numButtons ; ii++ ) {
		boxDetails->buttonInfo[ii].accelerator = 
			tolower(*noticeBox->buttonText[ii]);
	}

	/* add together widths of all the buttons */
	for ( ii = 0 ; ii < noticeBox->numButtons ; ii++ )
		totalButtonWidth += boxDetails->buttonInfo[ii].fullWidth;
	boxDetails->totalButtonWidth = totalButtonWidth;

	/*
	 * Split the msg string into individual strings using newlines
	 */

	/* count the newlines in the msg starting with one */
	for (ii = 1, str = noticeBox->msgText; *str != '\0'; str++) {
		if (*str == '\n')
			ii++;
	}

	/* alloc the string pointer array */
	boxDetails->numStrings = ii;
	boxDetails->stringText = 
		(char **)MemAlloc(boxDetails->numStrings * sizeof(char *));

	/* split the msg string into separate string in stringText array */
	str = MemNewString(noticeBox->msgText);
	for (ii=0, tok=strtok(str,sep); tok; tok=strtok((char *)NULL,sep)) {
		boxDetails->stringText[ii++] = MemNewString(tok);
	}
	MemFree(str);

	/* figure out which descriptive string is longest */
	for ( ii = 0 ; ii < boxDetails->numStrings ; ii++ )
		longestStringLength = 
			MAX( longestStringLength,
			     XTextWidth( GRV.TextFontInfo, 
				         boxDetails->stringText[ii], 
				         strlen(boxDetails->stringText[ii]) ) );

	/* 
	 * REMIND: this calculation assumes all the buttons are
	 * on the same line
	 */
	boxDetails->boxWidth = 2 * MIN_BOX_SIDE 
		   + MAX( longestStringLength,
			  totalButtonWidth + 
			      /* amount of space between all the buttons */
			      MIN_BUTTON_SPACE * (noticeBox->numButtons - 1) );

	boxDetails->buttonHeight = Button_Height(gisButton);

	boxDetails->boxHeight = MIN_BOX_TOP + MIN_BOX_BOTTOM
		    + ( boxDetails->numStrings * boxDetails->fontHeight )
		    /* amount of space vertically between all the strings */
		    + ( (boxDetails->numStrings - 1) * MIN_STRING_VSPACE )
		    + MIN_BUTTON_VSPACE + boxDetails->buttonHeight;
	displayWidth = DisplayWidth( boxDetails->dpy, screen );
	displayHeight = DisplayHeight( boxDetails->dpy, screen );

	/* if not set, create default "origin" for box: centered */
	boxDetails->x = ( noticeBox->boxX == -1 ) 
			    ? (int)( displayWidth - boxDetails->boxWidth )/2 
			    : noticeBox->boxX ;
	boxDetails->y = ( noticeBox->boxY == -1 ) 
			    ? (int)( displayHeight - boxDetails->boxHeight )/2 
			    : noticeBox->boxY ;

}

/*
 * drawLocationCursor
 */
static void
drawLocationCursor(details,btn,erase)
	noticeBoxDetails *details;
	int		btn;
	Bool		erase;
{
static	XPoint	pts[] = { 0,0, 6,11, -12,0 };
	GC	gc ;

	pts[0].x = details->buttonInfo[btn].x +
		   details->buttonInfo[btn].fullWidth/2;
	pts[0].y = details->buttonInfo[btn].y +
		   details->buttonHeight - 7;

	if (erase)
		gc = details->scrInfo->gc[WINDOW_GC];
	else
		gc = details->scrInfo->gc[FOREGROUND_GC];

	XFillPolygon(details->dpy,details->window,
			gc,pts,3,Convex,CoordModePrevious);
}

/*
 * drawButton
 */
static void
drawButton(noticeBox,details,btn,btnState)
NoticeBox		*noticeBox;
noticeBoxDetails	*details;
int			btn;
int			btnState;
{
	Graphics_info	*gisButton = details->scrInfo->gi[BUTTON_GINFO];
	int		buttonState;

	if (noticeBox->defaultButton == btn) {
		buttonState = btnState;
		buttonState |= OLGX_DEFAULT;
	} else {
		buttonState = btnState;
	}

	if (details->buttonFocus == btn)
		drawLocationCursor(details,btn,(buttonState & OLGX_ERASE));

	olgx_draw_button(gisButton,details->window,
			details->buttonInfo[btn].x,details->buttonInfo[btn].y,
			details->buttonInfo[btn].fullWidth,0,
			noticeBox->buttonText[btn],buttonState);
}

/*
 * drawNoticeBox -- draw box outline, strings, and buttons using information
 *		in noticeBox and boxDetails structures which are passed in.
 *		Location of buttons (x, y) are set in boxDetails structure
 *		for use later (mouse warping, determining pointer position).
 */
static void
drawNoticeBox( noticeBox, boxDetails )
NoticeBox		*noticeBox;
noticeBoxDetails	*boxDetails;
{
	int	ii;
	int	buttonX, buttonY;
	int	buttonState;		/* OLGX_NORMAL or OLGX_DEFAULT */
	Graphics_info	*gisText = boxDetails->scrInfo->gi[TEXT_GINFO];
	Graphics_info	*gisButton = boxDetails->scrInfo->gi[BUTTON_GINFO];

	/* frame outline */
	if (GRV.F3dFrames) {

	    /* fill it in, because olgx doesn't fill in 2D */
	    if (!boxDetails->scrInfo->use3D)
		XFillRectangle(boxDetails->dpy, boxDetails->window,
			       boxDetails->scrInfo->gc[WINDOW_GC], 0, 0,
			       boxDetails->boxWidth, boxDetails->boxHeight);

	    olgx_draw_box(gisButton,
		boxDetails->window, 0, 0, 
		boxDetails->boxWidth, boxDetails->boxHeight, 
	      	OLGX_NORMAL, True );

	} else {
	    GC gc = boxDetails->scrInfo->gc[BORDER_GC];

	    XFillRectangle(boxDetails->dpy, boxDetails->window,
			   boxDetails->scrInfo->gc[WINDOW_GC], 0, 0,
			   boxDetails->boxWidth, boxDetails->boxHeight);

	    XFillRectangle(boxDetails->dpy, boxDetails->window, gc,
			   0, 0, boxDetails->boxWidth, OUTLINE_WIDTH);
	    XFillRectangle(boxDetails->dpy, boxDetails->window, gc,
			   0, boxDetails->boxHeight - OUTLINE_WIDTH,
			   boxDetails->boxWidth, OUTLINE_WIDTH);
	    XFillRectangle(boxDetails->dpy, boxDetails->window, gc,
			   0, OUTLINE_WIDTH, OUTLINE_WIDTH,
			   boxDetails->boxHeight - 2*OUTLINE_WIDTH);
	    XFillRectangle(boxDetails->dpy, boxDetails->window, gc,
			   boxDetails->boxWidth - OUTLINE_WIDTH, OUTLINE_WIDTH,
			   OUTLINE_WIDTH,
			   boxDetails->boxHeight - 2*OUTLINE_WIDTH);
	}

	if (boxDetails->scrInfo->use3D) {
	    /* REMIND: this is a hack to draw a chiseled box */
	    olgx_draw_box(gisButton,
		boxDetails->window, BORDER_WIDTH, BORDER_WIDTH, 
		boxDetails->boxWidth - 2 * BORDER_WIDTH, 
		boxDetails->boxHeight - 2 * BORDER_WIDTH, 
		OLGX_INVOKED, False);
	    olgx_draw_box(gisButton,
		boxDetails->window, BORDER_WIDTH+1, BORDER_WIDTH+1,
		boxDetails->boxWidth - 2 * BORDER_WIDTH - 2,
		boxDetails->boxHeight - 2 * BORDER_WIDTH - 2,
		OLGX_NORMAL, False);
	} else {
	    olgx_draw_box(gisButton,
		boxDetails->window, BORDER_WIDTH, BORDER_WIDTH, 
		boxDetails->boxWidth - 2 * BORDER_WIDTH, 
		boxDetails->boxHeight - 2 * BORDER_WIDTH, 
		OLGX_NORMAL, True);
	}

	/* draw descriptive text 
	 * REMIND: all strings are along the left edge (MIN_BOX_SIDE) 
	 */
	for ( ii = 0 ; ii < boxDetails->numStrings ; ii++ )
		olgx_draw_text(gisText,
			boxDetails->window, boxDetails->stringText[ii], 
			MIN_BOX_SIDE,
			/* need to move each line further down the screen */
			MIN_BOX_TOP + GRV.TextFontInfo->ascent * ( ii + 1 )
				+ ( MIN_STRING_VSPACE * ii ),
			0, False, OLGX_NORMAL );

	/* put buttons in - notice that it's a single row */
	/* row of buttons should be centered within available space,
	 * assuming MIN_BUTTON_SPACE between each of the them
	 */
	buttonX = ( boxDetails->boxWidth - (boxDetails->totalButtonWidth 
		        + MIN_BUTTON_SPACE * (noticeBox->numButtons - 1)) )/2;
	/* this calculates from the bottom of the box */
	buttonY = ( boxDetails->boxHeight - 
			( MIN_BOX_BOTTOM + boxDetails->buttonHeight ) );
	for ( ii = 0 ; ii < noticeBox->numButtons ; ii++ )
	{
		if ( noticeBox->defaultButton == ii )
			buttonState = OLGX_DEFAULT;
		else
			buttonState = OLGX_NORMAL;

		/* save button's x, y values for use later */
		boxDetails->buttonInfo[ii].x = buttonX;
		boxDetails->buttonInfo[ii].y = buttonY;

		olgx_draw_button(gisButton,
			boxDetails->window, buttonX, buttonY,
			boxDetails->buttonInfo[ii].fullWidth, 0,
			noticeBox->buttonText[ii], 
			buttonState );

		/* set up buttonX for next button */ 
		buttonX = buttonX + boxDetails->buttonInfo[ii].fullWidth
				+ MIN_BUTTON_SPACE;
	}

	drawLocationCursor(boxDetails,boxDetails->buttonFocus,False);
}

/******************************************************************
 *			Private Event Functions
 ******************************************************************/


/*
 * setButtonFocus - sets the focus button to the passed value and
 *	redraws the necessary buttons.
 */
static void
setButtonFocus(details,newFocus)
noticeBoxDetails	*details;
int			newFocus;
{
	int		oldFocus = details->buttonFocus;

	drawButton(details->noticeBox,details,oldFocus,OLGX_ERASE);
	details->buttonFocus = newFocus;
	drawButton(details->noticeBox,details,newFocus,OLGX_NORMAL);
}

/*
 * moveButtonFocus - moves the focus button in the indicated direction
 *	and wraps around then first and last buttons.
 */
static void
moveButtonFocus(details,dir)
noticeBoxDetails	*details;
int			dir;
{
	NoticeBox	*noticeBox = details->noticeBox;
	int		newFocus = details->buttonFocus;

	if (noticeBox->numButtons == 1)
		return;

	newFocus += dir;

	if (newFocus >= noticeBox->numButtons)
		newFocus = 0;
	else if (newFocus < 0)
		newFocus = noticeBox->numButtons-1;

	setButtonFocus(details,newFocus);
}

/*
 * keyAccelerator - if the key event matches one of the button accelerators
 *	return True and set button to the accelerated button.
 */
static Bool
keyAccelerator(key,details,button)
	XKeyEvent	*key;
	noticeBoxDetails *details;
	int		*button;		/* RETURN */
{
	NoticeBox	*noticeBox = details->noticeBox;
	char		accel,str[10];
	KeySym		keySym;
	int		keyCount,i;

	keyCount = XLookupString(key,str,10,&keySym,NULL);

	if (keyCount != 1)
		return False;

	accel = tolower(str[0]);

	for (i = 0; i < noticeBox->numButtons; i++) {
		if (accel == details->buttonInfo[i].accelerator) {
			*button = i;
			return True;
		}
	}

	return False;
}

/*
 * pointInButton - True if the button is it the passed button number
 */
static Bool
pointInButton(event,details,btn)
	XButtonEvent	*event;
	noticeBoxDetails *details;
	int		btn;
{
	return PointInRect(event->x,event->y,
		details->buttonInfo[btn].x,details->buttonInfo[btn].y,
		details->buttonInfo[btn].fullWidth,details->buttonHeight);
}

/*
 * noticeInterposer - event handler for notices
 */
static int
noticeInterposer(dpy,event,win,details)
Display			*dpy;
XEvent			*event;
void			*win;
noticeBoxDetails	*details;
{
	NoticeBox	*noticeBox = details->noticeBox;
	int		ii,button,buttonState;

	/*
	 * Discard synthetic events
	 */
	if (event->xany.send_event)
		return DISPOSE_USED;

	switch (event->type) {
	case ButtonPress:

		/* first check to see if we're even in the notice box */
		if (!PointInRect(event->xbutton.x,event->xbutton.y,
				 0,0,details->boxWidth,details->boxHeight))
			break;

		/* if on one of the notice buttons, depress it */
		for ( ii = 0 ; ii < noticeBox->numButtons ; ii++ ) {
			if (pointInButton(event,details,ii)) {
				details->buttonDown = ii;
				details->buttonDrawnDown = True;
				drawButton(noticeBox,details,ii,OLGX_INVOKED);
				break;
			}
		}
		break;

	case ButtonRelease:
		if (details->buttonDown < 0)
			break;

		/* only a depressed button can be selected */
		if (pointInButton(event,details,details->buttonDown)) {
			details->buttonSelected = details->buttonDown;
			noticeDone(dpy,details);

		/* else erase the depressed button and now unselected */
		} else {
			drawButton(noticeBox,details,
				   details->buttonDown,OLGX_ERASE);
			details->buttonDown = -1;
			details->buttonDrawnDown = False;
		}
		break;
		
	case MotionNotify:
		if ( details->buttonDown < 0 )
			break;

		/* if moved out of depressed button erase it and cancel */
		if (!pointInButton(event,details,details->buttonDown)) {
			drawButton(noticeBox,details,
				   details->buttonDown,OLGX_ERASE);
			details->buttonDown = -1;
			details->buttonDrawnDown = False;
		}
		break;

	case KeyPress:
		switch (FindKeyboardAction(dpy,event)) {
		case ACTION_EXEC_DEFAULT:
			details->buttonSelected = noticeBox->defaultButton;
			noticeDone(dpy,details);
			break;
		case ACTION_CANCEL:
		case ACTION_STOP:
			details->buttonSelected = NOTICE_CANCEL;
			noticeDone(dpy,details);
			break;
		case ACTION_SELECT:
			details->buttonSelected = details->buttonFocus;
			noticeDone(dpy,details);
			break;
		case ACTION_NEXT_ELEMENT:
		case ACTION_RIGHT:
			moveButtonFocus(details,1);
			break;
		case ACTION_PREVIOUS_ELEMENT:
		case ACTION_LEFT:
			moveButtonFocus(details,-1);
			break;
		case ACTION_FIRST_CONTROL:
			setButtonFocus(details,0);
			break;
		case ACTION_LAST_CONTROL:
			setButtonFocus(details,noticeBox->numButtons-1);
			break;
		default:
			if (keyAccelerator(event,details,&button))
				setButtonFocus(details,button);
			else
				KeyBeep(dpy,event);
			break;
		}
	case KeyRelease:
		return DISPOSE_USED;

	case Expose:
		/*
		 * Ignore the first expose, since we painted as soon as we 
		 * mapped.  Otherwise, we really were exposed, so repaint.
		 */
		if (event->xexpose.count == 0) {
		    if (details->ignoreExpose)
			details->ignoreExpose = False;
		    else
			drawNoticeBox(details->noticeBox,details);
		}
		return DISPOSE_USED;

	default:
		return DISPOSE_DEFER;
	}
	return DISPOSE_USED;
}



/*
 * noticeDone()	-- warp pointer back, release grabs, remove interposer, call 
 * the callback, and clean up the notice window.
 */
static void
noticeDone(dpy,boxDetails)
Display			*dpy;
noticeBoxDetails	*boxDetails;
{
	if (boxDetails->warped)
	    XWarpPointer(dpy,None,boxDetails->scrInfo->rootid,
		      0,0,0,0,boxDetails->pointerX,boxDetails->pointerY);

	if (GRV.ServerGrabs)
	    XUngrabServer(dpy);
	XUngrabKeyboard(dpy,CurrentTime);
	XUngrabPointer(dpy,CurrentTime);

	UninstallInterposer();

	if (boxDetails->noticeCallback)
		(*boxDetails->noticeCallback)(dpy,boxDetails->buttonSelected);

	DestroyNoticeBox(boxDetails);
}


/******************************************************************
 *			Global Functions
 ******************************************************************/

/*
 * CreateNoticeBox -- create the notice box/button details and the window
 */
noticeBoxDetails *
CreateNoticeBox(dpy,screen,noticeBox,callback)
	Display		*dpy;
	int		screen;
	NoticeBox	*noticeBox;
	void		(*callback)();
{
	XSetWindowAttributes	attributes;
	noticeBoxDetails	*boxDetails;
	int			i,defaultButton;

	/* 
	 * Create box details and button details
	 */
	boxDetails = MemNew(noticeBoxDetails);
	boxDetails->buttonInfo = 
		(noticeButtonDetails *)MemAlloc( noticeBox->numButtons 
			* (unsigned int)sizeof(noticeButtonDetails) );
	boxDetails->noticeBox = MemNew(NoticeBox);

	/*
 	 * Init basic box details
	 *  - copy the passed in noticeBox since this is interposition
	 */
	boxDetails->dpy = dpy;
	boxDetails->scrInfo = GetScrInfoOfScreen(screen);
	boxDetails->noticeCallback = callback;
	*boxDetails->noticeBox = *noticeBox;

	boxDetails->noticeBox->buttonText = 
			MemAlloc(noticeBox->numButtons * sizeof(char *));

	for (i = 0; i < noticeBox->numButtons; i++)
		boxDetails->noticeBox->buttonText[i] = 
			MemNewString(noticeBox->buttonText[i]);

	boxDetails->noticeBox->msgText = MemNewString(noticeBox->msgText);

	/* 
	 * Figure out size to make window and where to put it
	 */
	calculateBoxDimensions(boxDetails->noticeBox,boxDetails); 

	/* 
	 * Set up window attributes structure
	 */
	attributes.border_pixel = 0;
	attributes.colormap = boxDetails->scrInfo->colormap;
	attributes.event_mask = NOTICE_EVENT_MASK;
	attributes.save_under = True;

	/* 
	 * Create window
	 */
	boxDetails->window = XCreateWindow(dpy,
					   RootWindow(dpy,screen),
				           boxDetails->x, boxDetails->y, 
					   boxDetails->boxWidth, 
					   boxDetails->boxHeight, 0, 
					   boxDetails->scrInfo->depth,
					   InputOutput,
					   boxDetails->scrInfo->visual, 
				           NOTICE_ATTR_MASK,
				           &attributes );

	boxDetails->buttonSelected = -1;
	boxDetails->buttonFocus = noticeBox->defaultButton;
	boxDetails->buttonDown = -1;
	boxDetails->buttonDrawnDown = False;
	boxDetails->ignoreExpose = True;
	boxDetails->warped = False;

	return boxDetails;
}

/*
 * DestroyNoticeBox -- destroys the notice box/button details and window
 */
void
DestroyNoticeBox(boxDetails)
	noticeBoxDetails	*boxDetails;
{
	int	i;

	XDestroyWindow(boxDetails->dpy,boxDetails->window);

	for (i=0; i<boxDetails->numStrings; i++) 
		MemFree(boxDetails->stringText[i]);
	MemFree(boxDetails->stringText);
	MemFree(boxDetails->buttonInfo);
	for (i=0; i<boxDetails->noticeBox->numButtons; i++)
		MemFree(boxDetails->noticeBox->buttonText[i]);
	MemFree(boxDetails->noticeBox->buttonText);
	MemFree(boxDetails->noticeBox->msgText);
	MemFree(boxDetails->noticeBox);
	MemFree(boxDetails);
}
	
/*
 * ShowNoticeBox -- Brings up the notice box
 */
void
ShowNoticeBox(dpy,details)
	Display			*dpy;
	noticeBoxDetails	*details;
{
	int			defaultButton;
	int			grabstat;

	/*
	 * Map the window, then grab the pointer, the keyboard, and the
	 * server.  Return immediately if we couldn't grab the pointer, but
	 * only issue a warning if we couldn't grab the keyboard.  We need to
	 * map the window first, otherwise the grabs will fail.
	 */
	XMapRaised(dpy,details->window);
	grabstat = XGrabPointer(dpy, details->window, False,
				ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync,
				None, None, CurrentTime);

	if (grabstat != GrabSuccess) {
	    noticeDone(dpy, details);
	    ErrorWarning(gettext("failed to grab pointer"));
	    return;
	}

	grabstat = XGrabKeyboard(dpy, details->window, False, 
				 GrabModeAsync, GrabModeAsync, CurrentTime);
	if (grabstat != GrabSuccess)
	    ErrorWarning(gettext("failed to grab keyboard"));

	if (GRV.ServerGrabs)
	    XGrabServer(dpy);

	/*
	 * Draw notice window immediately.  The first expose event is ignored, 
	 * so there is no redundant repaint.
	 */
	drawNoticeBox(details->noticeBox,details);

	if (GRV.Beep != BeepNever)
		XBell(dpy,100);

	if (GRV.PopupJumpCursor) {
		int			dummyInt;
		unsigned int		dummyUInt;
		Window			dummyWin;

		/* save current mouse position */
		XQueryPointer(dpy,details->scrInfo->rootid,
		       &dummyWin,&dummyWin,
		       &(details->pointerX),&(details->pointerY),
		       &dummyInt,&dummyInt,&dummyUInt);

		/* warp pointer to default button */
		defaultButton = details->noticeBox->defaultButton;

	    	XWarpPointer(dpy,None,details->window,0,0,0,0, 
		      details->buttonInfo[defaultButton].x
		        + details->buttonInfo[defaultButton].fullWidth/2, 
		      details->buttonInfo[defaultButton].y
		        + details->buttonHeight/2 );

		details->warped = True;
	}

	InstallInterposer(noticeInterposer,details);
}



/*
 * UseNoticeBox -- pop up a box which forces the user to answer
 * 		  a question using the buttons
 *
 * Arguments:
 *	dpy	    - pointer to current display
 *	screen	    - index to current screen
 *	noticeBox   - pointer to NoticeBox structure:
 *		numButtons	(number of buttons)
 *		defaultButton	(index into buttonText for mouse warp)
 *		buttonText	(array of strings for button text)
 *		msgText		(msg string for description w/ newlines)
 *		boxX		(box origin (-1 =use default/centered))
 *		boxY		(box origin (-1 =use default/centered))
 *
 *	Default placement of the box is centered in the display
 *	Returns -1 on failure (0 for 0th button, 1 for 1st button, etc.)
 */
void
UseNoticeBoxSync(dpy,screen,noticeBox,callback)
	Display		*dpy;
	int		screen;
	NoticeBox	*noticeBox;
	void		(*callback)();
{
	noticeBoxDetails	*boxDetails;

	boxDetails = CreateNoticeBox(dpy,screen,noticeBox,callback);

	ShowNoticeBox(dpy,boxDetails);
}

/*
 * UseNoticeBox() -- NoticeBox with no callback
 */
int
UseNoticeBox( dpy, screen, noticeBox )
Display		*dpy;
int		screen;
NoticeBox	*noticeBox;
{
	UseNoticeBoxSync(dpy,screen,noticeBox,NULL);
}
	
