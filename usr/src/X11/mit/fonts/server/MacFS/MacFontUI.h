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

#define kPrefSize				512
#define kMinSize				384
	
/* The following constants are used to identify menus and their items. The menu IDs
   have an "m" prefix and the item numbers within each menu have an "i" prefix. */
#define	mApple					128		/* Apple menu */
#define	iAbout					1

#define	mFile					129		/* File menu */
#define	iNew					1
#define	iClose					4
#define	iQuit					12

#define	mEdit					130		/* Edit menu */
#define	iUndo					1
#define	iCut					3
#define	iCopy					4
#define	iPaste					5
#define	iClear					6

#define mFonts                  131     /* Fonts menu */

/*	1.01 - kTopLeft - This is for positioning the Disk Initialization dialogs. */

#define kDITop					0x0050
#define kDILeft					0x0070

/* 1.01 - changed constants to begin with 'k' for consistency, except for resource IDs */
/*	kTextMargin is the number of pixels we leave blank at the edge of the window. */
#define kTextMargin				2

/* kMaxOpenDocuments is used to determine whether a new document can be opened
   or created. We keep track of the number of open documents, and disable the
   menu items that create a new document when the maximum is reached. If the
   number of documents falls below the maximum, the items are enabled again. */
#define	kMaxOpenDocuments		1
	
/*	kMaxDocWidth is an arbitrary number used to specify the width of the TERec's
	destination rectangle so that word wrap and horizontal scrolling can be
	demonstrated. */
#define	kMaxDocWidth			576
	
/* kMinDocDim is used to limit the minimum dimension of a window when GrowWindow
	is called. */
#define	kMinDocDim				64

/*	kControlInvisible is used to 'turn off' controls (i.e., cause the control not
	to be redrawn as a result of some Control Manager call such as SetCtlValue)
	by being put into the contrlVis field of the record. kControlVisible is used
	the same way to 'turn on' the control. */
#define kControlInvisible		0
#define kControlVisible			0xFF

/*	kScrollbarAdjust and kScrollbarWidth are used in calculating
	values for control positioning and sizing. */
#define kScrollbarWidth			16
#define kScrollbarAdjust		(kScrollbarWidth - 1)

/*	kScrollTweek compensates for off-by-one requirements of the scrollbars
 to have borders coincide with the growbox. */
#define kScrollTweek			2
	
/*	kCrChar is used to match with a carriage return when calculating the
	number of lines in the TextEdit record. kDelChar is used to check for
	delete in keyDowns. */
#define kCrChar					13
#define kDelChar				8
	
/*	kButtonScroll is how many pixels to scroll horizontally when the button part
	of the horizontal scrollbar is pressed. */
#define kButtonScroll			4
	
/*	kMaxTELength is an arbitrary number used to limit the length of text in the TERec
	so that various errors won't occur from too many characters in the text. */
#define	kMaxTELength			32000

/* kSysEnvironsVersion is passed to SysEnvirons to tell it which version of the
   SysEnvRec we understand. */
#define	kSysEnvironsVersion		1

/* kOSEvent is the event number of the suspend/resume and mouse-moved events sent
   by MultiFinder. Once we determine that an event is an OSEvent, we look at the
   high byte of the message sent to determine which kind it is. To differentiate
   suspend and resume events we check the resumeMask bit. */
#define	kOSEvent				app4Evt	/* event used by MultiFinder */
#define	kSuspendResumeMessage	1		/* high byte of suspend/resume event message */
#define	kResumeMask				1		/* bit of message field for resume vs. suspend */
#define	kMouseMovedMessage		0xFA	/* high byte of mouse-moved event message */
#define	kNoEvents				0		/* no events mask */

/* 1.01 - kMinHeap - This is the minimum result from the following
	 equation:
			
			ORD(GetApplLimit) - ORD(ApplicZone)
			
	 for the application to run. It will insure that enough memory will
	 be around for reasonable-sized scraps, FKEYs, etc. to exist with the
	 application, and still give the application some 'breathing room'.
	 To derive this number, we ran under a MultiFinder partition that was
	 our requested minimum size, as given in the 'SIZE' resource. */
	 
#define	kMinHeap				 (200 * 1024)
	
/* 1.01 - kMinSpace - This is the minimum result from PurgeSpace, when called
	 at initialization time, for the application to run. This number acts
	 as a double-check to insure that there really is enough memory for the
	 application to run, including what has been taken up already by
	 pre-loaded resources, the scrap, code, and other sundry memory blocks. */
	 
#define	kMinSpace				(20 * 1024)

/*	kExtremeNeg and kExtremePos are used to set up wide open rectangles and regions. */
#define kExtremeNeg				-32768
#define kExtremePos				(32767 - 1)	/* required to address an old region bug */
	
/* kTESlop provides some extra security when pre-flighting edit commands. */
#define	kTESlop					1024

/* The following are indicies into STR# resources. */
#define	eWrongMachine			1
#define	eSmallSize				2
#define	eNoMemory				3
#define	eNoSpaceCut				4
#define	eNoCut					5
#define	eNoCopy					6
#define	eExceedPaste			7
#define	eNoSpacePaste			8
#define	eNoWindow				9
#define	eExceedChar				10
#define	eNoPaste				11
/* Generic Server FatalError */
#define eFatalError				12

#define	rMenuBar	128				/* application's menu bar */
#define	rAboutAlert	128				/* about alert */
#define	rUserAlert	129				/* user error alert */
#define	rDocWindow	128				/* application's window */
#define	rVScroll	128				/* vertical scrollbar control */
#define	rHScroll	129				/* horizontal scrollbar control */
#define	kErrStrings	128				/* error string list */
