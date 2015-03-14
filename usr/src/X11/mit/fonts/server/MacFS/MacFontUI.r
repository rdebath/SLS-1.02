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

#include "systypes.r"
#include "types.r"

#include "MacFontUI.h"

#define verUS	0

type 'vers' {
		hex byte;												/* Major revision in BCD*/
		hex byte;												/* Minor vevision in BCD*/
		hex byte	development = 0x20,							/* Release stage		*/
					alpha = 0x40,
					beta = 0x60,
					final = 0x80, /* or */ release = 0x80;
		hex byte;												/* Non-final release #	*/
		integer		Country;									/* Country code			*/
		pstring;												/* Short version number	*/
		pstring;												/* Long version number	*/
};

/* The SIZE resource definition in /mac/lib/rincludes is faulty in A/UX 2.0. */
/* The one given next works. Ignore the warning caused by re-defining SIZE */

/*----------------------------SIZE MultiFinder Size Information-----------------------*/
type 'SIZE' {
		boolean					dontSaveScreen,					/* for SWITCHER 		*/
								saveScreen;						/*    compatibility		*/
		boolean 				ignoreSuspendResumeEvents,		/* suspend-resume		*/
								acceptSuspendResumeEvents;
		boolean					enableOptionSwitch,				/* for SWITCHER 		*/
								disableOptionSwitch;			/*    compatibility		*/
		boolean					cannotBackground,
								canBackground;					/* Can properly use back-
																   ground null events	*/
		boolean					notMultiFinderAware,			/* activate/deactivate	*/
								multiFinderAware;				/* on resume/suspend	*/
		boolean					backgroundAndForeground,		/* Application does not	*/
								onlyBackground;					/* have a user interface*/
		boolean					dontGetFrontClicks,				/* Get mouse down/up	*/
								getFrontClicks;					/* when suspended		*/
		boolean					ignoreChildDiedEvents,			/* Apps use this.		*/
								acceptChildDiedEvents;			/* Debuggers use this.	*/
		boolean					not32BitCompatible,				/* Works with 24bit addr*/
								is32BitCompatible;				/* Works with 24 or 32	*/
																/* bit addresses		*/
#undef reserved
		boolean					reserved;						/* These seven bits are */		
		boolean					reserved;						/* reserved.  Set them	*/
		boolean					reserved;						/* to "reserved". When	*/
		boolean					reserved;						/* we decide to define	*/
		boolean					reserved;						/* a new flag, your		*/
		boolean					reserved;						/* old resource will 	*/
		boolean					reserved;						/* still compile.		*/
		
		/* Memory sizes are in bytes */
		unsigned longint;										/* preferred mem size	*/
		unsigned longint;										/* minimum mem size		*/

		/* If we ever define one of the seven reserved bits above, the "reserved"
		 enumeration wouldn't appear on the newly defined bit.  By defining "reserved"
		 below, old resource SIZE declarations will still compile. */
#define	reserved		false
};



resource 'vers' (1) {
	0x00, 0x00, release, 0x00,
	verUS,
	"1.00",
	"1.00, Copyright \251 1991 Apple Computer, Inc."
};

/* we use an MBAR resource to conveniently load all the menus */

resource 'MBAR' (rMenuBar, preload) {
	{ mApple, mFile, mEdit, mFonts };		/* four menus */
};


resource 'MENU' (mApple, preload) {
	mApple, textMenuProc,
	0b1111111111111111111111111111101,	/* disable dashed line, enable About and DAs */
	enabled, apple,
	{
		"About MacFS\311",
			noicon, nokey, nomark, plain;
		"-",
			noicon, nokey, nomark, plain
	}
};

resource 'MENU' (mFile, preload) {
	mFile, textMenuProc,
	0b0000000000000000000100000000000,	/* enable Quit only, program enables others */
	enabled, "File",
	{
		"New",
			noicon, "N", nomark, plain;
		"Open",
			noicon, "O", nomark, plain;
		"-",
			noicon, nokey, nomark, plain;
		"Close",
			noicon, "W", nomark, plain;
		"Save",
			noicon, "S", nomark, plain;
		"Save As\311",
			noicon, nokey, nomark, plain;
		"Revert",
			noicon, nokey, nomark, plain;
		"-",
			noicon, nokey, nomark, plain;
		"Page Setup\311",
			noicon, nokey, nomark, plain;
		"Print\311",
			noicon, nokey, nomark, plain;
		"-",
			noicon, nokey, nomark, plain;
		"Quit",
			noicon, "Q", nomark, plain
	}
};

resource 'MENU' (mEdit, preload) {
	mEdit, textMenuProc,
	0b0000000000000000000000000000000,	/* disable everything, program does the enabling */
	enabled, "Edit",
	 {
		"Undo",
			noicon, "Z", nomark, plain;
		"-",
			noicon, nokey, nomark, plain;
		"Cut",
			noicon, "X", nomark, plain;
		"Copy",
			noicon, "C", nomark, plain;
		"Paste",
			noicon, "V", nomark, plain;
		"Clear",
			noicon, nokey, nomark, plain
	}
};

resource 'MENU' (mFonts, preload) {
    mFonts, textMenuProc,
	0b0000000000000000000000000000000,	/* disable everything, program does the enabling */
    enabled, "Fonts",
    {
    }
};


/* this ALRT and DITL are used as an About screen */

resource 'ALRT' (rAboutAlert, purgeable) {
	{140, 180, 260, 456}, rAboutAlert, {
		OK, visible, silent;
		OK, visible, silent;
		OK, visible, silent;
		OK, visible, silent
	};
};

resource 'DITL' (rAboutAlert, purgeable) {
	{ /* array DITLarray: 5 elements */
		/* [1] */
		{88, 184, 108, 264},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{8, 8, 24, 274},
		StaticText {
			disabled,
			"An X Font Server for A/UX\250"
		},
		/* [3] */
		{32, 8, 48, 237},
		StaticText {
			disabled,
			"Copyright \251 1991 Apple Computer"
		},
		/* [4] */
		{56, 8, 72, 136},
		StaticText {
			disabled,
			"Brought to you by:"
		},
		/* [5] */
		{80, 24, 112, 167},
		StaticText {
			disabled,
			"A/UX Engineering"
		}
	}
};


/* this ALRT and DITL are used as an error screen */

resource 'ALRT' (rUserAlert, purgeable) {
	{140, 200, 250, 440},
	rUserAlert,
	{ /* array: 4 elements */
		/* [1] */
		OK, visible, silent,
		/* [2] */
		OK, visible, silent,
		/* [3] */
		OK, visible, silent,
		/* [4] */
		OK, visible, silent
	}
};


resource 'DITL' (rUserAlert, purgeable) {
	{ /* array DITLarray: 3 elements */
		/* [1] */
		{80, 150, 100, 230},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 60, 60, 230},
		StaticText {
			disabled,
			"Error. ^0."
		},
		/* [3] */
		{8, 8, 40, 40},
		Icon {
			disabled,
			2
		}
	}
};


resource 'WIND' (rDocWindow, preload, purgeable) {
	{50, 20, 200, 620},
	zoomDocProc, invisible, noGoAway, 0x0, "MacFS Log"
};


resource 'CNTL' (rVScroll, preload, purgeable) {
	{-1, 585, 136, 601},
	0, visible, 0, 0, scrollBarProc, 0, ""
};


resource 'CNTL' (rHScroll, preload, purgeable) {
	{135, -1, 151, 586},
	0, visible, 0, 0, scrollBarProc, 0, ""
};

resource 'STR#' (kErrStrings, purgeable) {
	{
	"You must run on 512Ke or later";
	"Application Memory Size is too small";
	"Not enough memory to run MacFS";
	"Not enough memory to do Cut";
	"Cannot do Cut";
	"Cannot do Copy";
	"Cannot exceed 32,000 characters with Paste";
	"Not enough memory to do Paste";
	"Cannot create window";
	"Cannot exceed 32,000 characters";
	"Cannot do Paste";
	"Choose Quit to leave MacFS"
	}
};

/* here is the quintessential MultiFinder friendliness device, the SIZE resource */

resource 'SIZE' (-1) {
	dontSaveScreen,
	acceptSuspendResumeEvents,
	enableOptionSwitch,
	canBackground,				/* we can background; we don't currently, but our sleep value */
								/* guarantees we don't hog the Mac while we are in the background */
	multiFinderAware,			/* this says we do our own activate/deactivate; don't fake us out */
	backgroundAndForeground,	/* this is definitely not a background-only application! */
	dontGetFrontClicks,			/* change this is if you want "do first click" behavior like the Finder */
	ignoreChildDiedEvents,		/* essentially, I'm not a debugger (sub-launching) */
	is32BitCompatible,			/* this app should not be run in 32-bit address space */
	reserved,
	reserved,
	reserved,
	reserved,
	reserved,
	reserved,
	reserved,
	kPrefSize * 1024,
	kMinSize * 1024	
};


type 'MOOT' as 'STR ';


resource 'MOOT' (0) {
	"X Font Server for A/UX\251"
};


resource 'BNDL' (128) {
	'MOOT',
	0,
	{
		'ICN#',
		{
			0, 128
		},
		'FREF',
		{
			0, 128
		}
	}
};


resource 'FREF' (128) {
	'APPL',
	0,
	""
};


resource 'ICN#' (128) {
	{ /* array: 2 elements */
		/* [1] */
		$"04 30 40 00 0A 50 A0 00 0B 91 10 02 08 22 08 03"
		$"12 24 04 05 20 28 02 09 40 10 01 11 80 0C 00 A1"
		$"80 03 FF C2 7E 00 FF 04 01 00 7F 04 03 00 1E 08"
		$"04 E0 00 0C 08 E0 00 0A 10 E0 00 09 08 C0 00 06"
		$"04 87 FE 04 02 88 01 04 01 88 00 84 00 88 00 44"
		$"00 88 00 44 00 88 00 C4 01 10 01 88 02 28 03 10"
		$"01 C4 04 E0 00 02 08 00 73 BF FB EE 4C A2 8A 2A"
		$"40 AA AA EA 52 AA AA 24 5E A2 8A EA 73 BE FB 8E",
		/* [2] */
		$"04 30 40 00 0E 70 E0 00 0F F1 F0 02 0F E3 F8 03"
		$"1F E7 FC 07 3F EF FE 0F 7F FF FF 1F FF FF FF BF"
		$"FF FF FF FE 7F FF FF FC 01 FF FF FC 03 FF FF F8"
		$"07 FF FF FC 0F FF FF FE 1F FF FF FF 0F FF FF FE"
		$"07 FF FF FC 03 FF FF FC 01 FF FF FC 00 FF FF FC"
		$"00 FF FF FC 00 FF FF FC 01 FF FF F8 03 EF FF F0"
		$"01 C7 FC E0 00 03 F8 00 73 BF FB EE 7F BE FB EE"
		$"7F BE FB EE 7F BE FB E4 7F BE FB EE 73 BE FB 8E"
	}
};

