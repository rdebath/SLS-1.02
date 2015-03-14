/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident "@(#)resources.c	26.45	91/10/04 SMI"

#ifdef SYSV
#include <sys/types.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/file.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include <olgx/olgx.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "defaults.h"
#include "globals.h"
#include "resources.h"
#include "olcursor.h"
#include "events.h"


/* converters */

static Bool cvtBeepStatus();
static Bool cvtBoolean();
static Bool cvtClickTimeout();
static Bool cvtCursorFont();
static Bool cvtFocusStyle();
static Bool cvtFont();
static Bool cvtIconLocation();
static Bool cvtInteger();
static Bool cvtKey();
static Bool cvtMouseless();
static Bool cvtString();
static Bool cvtStringList();


/* internationalization stuff */

#ifdef OW_I18N_L3
static Bool OLLCUpdated = False;

static void GRVLCInit();
static Bool cvtOLLC();
static Bool cvtOLLCCL();
#endif /* OW_I18N_L3 */


/* updaters */

static void updButtonFont();
       void UpdFocusStyle();			/* yes, this one's global */
static void updGlyphFont();
static void updIconFont();
static void updIconLocation();
static void updMouseless();
static void updString();
static void updStringList();
static void updSync();
static void updTextFont();
static void updTitleFont();
static void updWorkspace();
static void updWindow();
static void updForeground();
static void updBackground();
static void updBorder();


/* resource table */

typedef struct _resourceitem {
    char *instance;
    char *class;
    char *defaultString;
    void *addr;
    Bool (*converter)();
    void (*updater)();
    XrmQuark instanceQ;
    XrmQuark classQ;
} ResourceItem;

ResourceItem ResourceTable[] = {

{   "titleFont",		"TitleFont",
    "-b&h-lucida-bold-r-normal-sans-*-120-*-*-*-*-*-*",
    &(GRV.TitleFontInfo),	cvtFont,		updTitleFont },
{   "textFont",			"TextFont",
    "-b&h-lucida-medium-r-normal-sans-*-120-*-*-*-*-*-*",
    &(GRV.TextFontInfo),	cvtFont,		updTextFont },
{   "buttonFont",		"ButtonFont",
    "-b&h-lucida-medium-r-normal-sans-*-120-*-*-*-*-*-*",
    &(GRV.ButtonFontInfo),	cvtFont,		updButtonFont },
{   "iconFont",			"IconFont",
    "-b&h-lucida-medium-r-normal-sans-*-120-*-*-*-*-*-*",
    &(GRV.IconFontInfo),	cvtFont,		updIconFont },
{   "glyphFont",		"GlyphFont",
    "-sun-open look glyph-*-*-*-*-*-120-*-*-*-*-*-*",
    &(GRV.GlyphFontInfo),	cvtFont,		updGlyphFont },
{   "cursorFont",		"CursorFont",
    "-sun-open look cursor-*-*-*-*-*-120-*-*-*-*-*-*",
    &(GRV.BasicPointer),	cvtCursorFont,		NULL },

{   "foreground",		"Foreground",		"#000000",
    &(GRV.ForegroundColor),	cvtString,		updForeground },
{   "background",		"Background",		"#ffffff",
    &(GRV.BackgroundColor),	cvtString,		updBackground },
{   "reverseVideo",		"ReverseVideo",		"False",
    &(GRV.ReverseVideo),	cvtBoolean,		NULL },
{   "borderColor",		"BorderColor",		"#000000",
    &(GRV.BorderColor),		cvtString,		updBorder },
{   "windowColor",		"WindowColor",		"#cccccc",
    &(GRV.WindowColor),		cvtString,		updWindow },
{   "workspaceColor",		"WorkspaceColor",	"#40a0c0",
    &(GRV.WorkspaceColor),	cvtString,		updWorkspace },
{   "paintWorkspace",		"PaintWorkspace",	"True",
    &(GRV.PaintWorkspace),	cvtBoolean,		NULL },
{   "use3D",			"Use3D",		"True", 
    &(GRV.F3dUsed),		cvtBoolean,		NULL },
{   "setInput",			"SetInput",		"Select",
    &(GRV.FocusFollowsMouse),	cvtFocusStyle,		UpdFocusStyle },
{   "defaultTitle",		"DefaultTitle",		"No Name", 
    &(GRV.DefaultWinName),	cvtString,		updString },
{   "flashFrequency",		"FlashFrequency",	"100000", 
    &(GRV.FlashTime),		cvtInteger,		NULL },
{   "flashTime",		"FlashTime",		"100000",
    &(GRV.FlashTime),		cvtInteger,		NULL },
{   "iconLocation",		"IconLocation", 	"bottom",
    &(GRV.IconPlacement),	cvtIconLocation,	updIconLocation },
{   "focusLenience",		"FocusLenience", 	"False",
    &(GRV.FocusLenience),	cvtBoolean,		NULL },
{   "dragWindow",		"DragWindow", 		"False",
    &(GRV.DragWindow),		cvtBoolean,		NULL },
{   "autoRaise",		"AutoRaise",		"False",
    &(GRV.AutoRaise),		cvtBoolean,		NULL },
{   "autoRaiseDelay",		"AutoRaiseDelay",	"0",
    &(GRV.AutoRaiseDelay),	cvtInteger,		NULL },
{   "dragRightDistance",	"DragRightDistance",	"100",
    &(GRV.DragRightDistance),	cvtInteger,		NULL },
{   "moveThreshold",		"MoveThreshold",	"5",
    &(GRV.MoveThreshold),	cvtInteger,		NULL },
{   "dragThreshold",		"DragThreshold",	"5",
    &(GRV.MoveThreshold),	cvtInteger,		NULL },
{   "clickMoveThreshold",	"ClickMoveThreshold",	"5",
    &(GRV.ClickMoveThreshold),	cvtInteger,		NULL },
{   "multiClickTimeout",	"MultiClickTimeout",	"5",
    &(GRV.DoubleClickTime),	cvtClickTimeout,	NULL },
{   "frontKey",			"FrontKey",		"Any L5",
    &(GRV.FrontKey),		cvtKey,			NULL },
{   "helpKey",			"HelpKey",		"Help",
    &(GRV.HelpKey),		cvtKey,			NULL },
{   "openKey",			"OpenKey",		"Any L7",
    &(GRV.OpenKey),		cvtKey,			NULL },
{   "confirmKey",		"ConfirmKey",		"Return",
    &(GRV.ConfirmKey),		cvtKey,			NULL },
{   "printOrphans",		"PrintOrphans",		"False", 
    &(GRV.PrintOrphans),	cvtBoolean,		NULL },
{   "printAll",			"PrintAll",		"False", 
    &(GRV.PrintAll),		cvtBoolean,		NULL },
{   "synchronize",		"Synchronize",		"False", 
    &(GRV.Synchronize),		cvtBoolean,		updSync },
{   "snapToGrid",		"SnapToGrid",		"False",
    &(GRV.FSnapToGrid),		cvtBoolean,		NULL },
{   "saveWorkspaceTimeout",	"SaveWorkspaceTimeout", "30",
    &(GRV.SaveWorkspaceTimeout), cvtInteger,		NULL },
{   "popupJumpCursor",		"PopupJumpCursor",	"True",
    &(GRV.PopupJumpCursor),	cvtBoolean,		NULL },
{   "cancelKey",		"CancelKey",		"Escape",
    &(GRV.CancelKey),		cvtKey,			NULL },
{   "colorLockKey",		"ColorLockKey",		"Control L2",
    &(GRV.ColorLockKey),	cvtKey,			NULL },
{   "colorUnlockKey",		"ColorUnlockKey",	"Control L4",
    &(GRV.ColorUnlockKey),	cvtKey,			NULL },
{   "colorFocusLocked",		"ColorFocusLocked",	"False",
    &(GRV.ColorLocked),		cvtBoolean,		NULL },
{   "edgeMoveThreshold",	"EdgeMoveThreshold", 	"10",
    &(GRV.EdgeThreshold),	cvtInteger,		NULL },
{   "rubberBandThickness",	"RubberBandThickness",	"2",
    &(GRV.RubberBandThickness),	cvtInteger,		NULL },
{   "beep",			"Beep",			"always",
    &(GRV.Beep),		cvtBeepStatus,		NULL },
{   "pPositionCompat",		"PPositionCompat",	"false",
    &(GRV.PPositionCompat),	cvtBoolean,		NULL },
{   "minimalDecor",		"MinimalDecor",		"",
    &(GRV.Minimals),		cvtStringList,		updStringList },
{   "use3DFrames",		"Use3DFrames",		"False", 
    &(GRV.F3dFrames),		cvtBoolean,		NULL },
{   "use3DResize",		"Use3DResize",		"True",
    &(GRV.F3dResize),		cvtBoolean,		NULL },
{   "refreshRecursively",	"RefreshRecursively",	"True",
    &(GRV.RefreshRecursively),	cvtBoolean,		NULL },
{   "mouseChordTimeout",	"MouseChordTimeout",	"100",
    &(GRV.MouseChordTimeout),	cvtInteger,		NULL },
{   "singleScreen",		"SingleScreen",		"False",
    &(GRV.SingleScreen),	cvtBoolean,		NULL },
{   "autoReReadMenuFile",        "AutoReReadMenuFile",  "True",
    &(GRV.AutoReReadMenuFile),  cvtBoolean,		NULL },
{   "keepTransientsAbove",	"KeepTransientsAbove",	"True",
    &(GRV.KeepTransientsAbove),	cvtBoolean,		NULL },
{   "transientsSaveUnder",	"TransientsSaveUnder",	"True",
    &(GRV.TransientsSaveUnder),	cvtBoolean,		NULL },
{   "transientsTitled",		"TransientsTitled",	"True",
    &(GRV.TransientsTitled),	cvtBoolean,		NULL },
{   "selectWindows",		"SelectWindows",	"True",
    &(GRV.SelectWindows),	cvtBoolean,		NULL },
{   "showMoveGeometry",		"ShowMoveGeometry",	"False",
    &(GRV.ShowMoveGeometry),	cvtBoolean,		NULL },
{   "showResizeGeometry",	"ShowResizeGeometry",	"False",
    &(GRV.ShowResizeGeometry),	cvtBoolean,		NULL },
{   "invertFocusHighlighting",	"InvertFocusHighlighting", "False",
    &(GRV.InvertFocusHighlighting), cvtBoolean,		NULL },
{   "runSlaveProcess",		"RunSlaveProcess",	"True",
    &(GRV.RunSlaveProcess),	cvtBoolean,		NULL },
{   "selectToggleStacking",	"SelectToggleStacking","False",
    &(GRV.SelectToggleStacking),cvtBoolean,		NULL },
{   "flashCount",		"FlashCount",		"6",
    &(GRV.FlashCount),		cvtInteger,		NULL },
{   "defaultIconImage",		"DefaultIconImage",	NULL,
    &(GRV.DefaultIconImage),	cvtString,		NULL },
{   "defaultIconMask",		"DefaultIconMask",	NULL,
    &(GRV.DefaultIconMask),	cvtString,		NULL },
{   "serverGrabs",		"ServerGrabs",		"True",
    &(GRV.ServerGrabs),		cvtBoolean,		NULL },
{   "iconFlashCount",		"IconFlashCount",	"3",
    &(GRV.IconFlashCount),	cvtInteger,		NULL },
{   "selectDisplaysMenu",	"SelectDisplaysMenu",	"False",
    &(GRV.SelectDisplaysMenu),	cvtBoolean,		NULL },
{   "selectionFuzz",		"SelectionFuzz",	"1",
    &(GRV.SelectionFuzz),	cvtInteger,		NULL },
{   "autoInputFocus",		"AutoInputFocus",	"False",
    &(GRV.AutoInputFocus),	cvtBoolean,		NULL },
{   "autoColorFocus",		"AutoColorFocus",	"False",
    &(GRV.AutoColorFocus),	cvtBoolean,		NULL },
{   "colorTracksInputFocus",	"ColorTracksInputFocus","False",
    &(GRV.ColorTracksInputFocus),cvtBoolean,		NULL },
{   "iconFlashOnTime",		"IconFlashOnTime",	"20000",
    &(GRV.IconFlashOnTime),	cvtInteger,		NULL },
{   "iconFlashOffTime",		"IconFlashOffTime",	"1",
    &(GRV.IconFlashOffTime),	cvtInteger,		NULL },
{   "keyboardCommands",		"KeyboardCommands",	"Basic",
    &(GRV.Mouseless),		cvtMouseless,		updMouseless },
{   "raiseOnActivate",		"RaiseOnActivate",	"True",
    &(GRV.RaiseOnActivate),	cvtBoolean,		NULL },
{   "restackWhenWithdraw",	"RestackWhenWithdraw",	"True",
    &(GRV.RestackWhenWithdraw),	cvtBoolean,		NULL },

#ifdef OW_I18N_L3

{   "basicLocale",		"BasicLocale",		NULL,
    &(GRV.LC.BasicLocale),      cvtOLLC,                NULL },
{   "basicLocaleCL",		"BasicLocaleCL",	NULL,
    &(GRV.LC.BasicLocale),      cvtOLLCCL,              NULL },

{   "displayLang",		"DisplayLang",		NULL,
    &(GRV.LC.DisplayLang),      cvtOLLC,                NULL },
{   "displayLangCL",		"DisplayLangCL",	NULL,
    &(GRV.LC.DisplayLang),      cvtOLLCCL,              NULL },

{   "inputLang",		"InputLang",		NULL,
    &(GRV.LC.InputLang),        cvtOLLC,                NULL },
{   "inputLangCL",		"InputLangCL",		NULL,
    &(GRV.LC.InputLang),        cvtOLLCCL,              NULL },

{   "numeric",			"Numeric",		NULL,
    &(GRV.LC.Numeric),          cvtOLLC,                NULL },
{   "numericCL",		"NumericCL",		NULL,
    &(GRV.LC.Numeric),          cvtOLLCCL,              NULL },

{   "dateFormat",		"DateFormat",		NULL,
    &(GRV.LC.DateFormat),       cvtOLLC,                NULL },
{   "dateFormatCL",		"DateFormatCL",		NULL,
    &(GRV.LC.DateFormat),       cvtOLLCCL,              NULL },

#endif /* OW_I18N_L3 */

};

#define NRESOURCEITEMS (sizeof(ResourceTable)/sizeof(ResourceItem))


/* ===== Utilities ======================================================== */


/*
 * Copy a string, converting it to lower case.
 */
static void
strnlower(dest, src, n)
    char *dest;
    char *src;
    int  n;
{
    char *p;

    strncpy(dest, src, n);
    dest[n-1] = '\0';		/* force null termination */

    for (p = dest; *p; ++p)
	if (isupper(*p))
	    *p = tolower(*p);
}


#define BSIZE 100

/*
 * Determine whether value matches pattern, irrespective of case.
 * This routine is necessary because not all systems have strcasecmp().
 */
static Bool
matchString(value, pattern)
    char *value;
    char *pattern;
{
    char buf[BSIZE];

    strnlower(buf, value, BSIZE);
    return (0 == strcmp(buf, pattern));
}


/*
 * Match any of the following booleans: yes, no, 1, 0, on, off, t, nil, 
 * true, false.  Pass back the boolean matched in ret, and return True.  
 * Otherwise, return False.  Matches are case-insensitive.
 */
static Bool
matchBool(value, ret)
    char *value;
    Bool *ret;
{
    char buf[BSIZE];

    strnlower(buf, value, BSIZE);

    if (0 == strcmp(buf, "yes") ||
	0 == strcmp(buf, "on") ||
	0 == strcmp(buf, "t") ||
	0 == strcmp(buf, "true") ||
	0 == strcmp(buf, "1"))
    {
	*ret = True;
	return True;
    }

    if (0 == strcmp(buf, "no") ||
	0 == strcmp(buf, "off") ||
	0 == strcmp(buf, "nil") ||
	0 == strcmp(buf, "false") ||
	0 == strcmp(buf, "0"))
    {
	*ret = False;
	return True;
    }

    return False;
}


/*
 * BoolString() - return Bool based on string, returning the default value if 
 * the string can't be converted.
 */
Bool
BoolString(s, dflt)
	char	*s;
	Bool	dflt;
{
	Bool	b;

	if (matchBool(s,&b))
	    return b;
	else
	    return dflt;
}


/*
 * Match any of the following input focus keywords: followmouse, follow, f, 
 * select, s, click, clicktotype, c.  Pass back True for focusfollows or 
 * False for clicktotype in ret (since FocusFollowsMouse is the global
 * corresponding to this resource), and return True.  
 * Otherwise, return False.
 */
static Bool
matchFocusKeyword(value, ret)
    char *value;
    Bool *ret;
{
    char buf[BSIZE];

    strnlower(buf, value, BSIZE);

    if (0 == strcmp(buf, "followmouse") ||
	0 == strcmp(buf, "follow") ||
	0 == strcmp(buf, "f"))
    {
	*ret = True;
	return True;
    }

    if (0 == strcmp(buf, "select") ||
	0 == strcmp(buf, "click") ||
	0 == strcmp(buf, "clicktotype") ||
	0 == strcmp(buf, "c") ||
	0 == strcmp(buf, "s"))
    {
	*ret = False;
	return True;
    }

    return False;
}


/*
 * Match any of the three possible beep keywords:  always, never, or notices.
 * Pass back the BeepStatus value by reference, and return True, if
 * a match was found; otherwise return False and do not disturb the
 * passed value.
 */
static Bool
matchBeepKeyword(value, ret)
    char *value;
    BeepStatus *ret;
{
	if (matchString(value,"always"))
	{
	    *ret = BeepAlways;
	    return True;
	}
	if (matchString(value,"never"))
	{
	    *ret = BeepNever;
	    return True;
	}
	if (matchString(value,"notices"))
	{
	    *ret = BeepNotices;
	    return True;
	}
	return False;
}


/*
 * Match an icon placement keyword.  Store matched value in ret and return 
 * True, or return False if no match occurred.
 */
static Bool
matchIconPlace( value, ret )
char		*value;
IconPreference	*ret;
{
	if (matchString(value, "top"))
	{
		*ret = AlongTop;
		return True;
	}
	if (matchString(value, "bottom"))
	{
		*ret = AlongBottom;
		return True;
	}
	if (matchString(value, "right"))
	{
		*ret = AlongRight;
		return True;
	}
	if (matchString(value, "left"))
	{
		*ret = AlongLeft;
		return True;
	}
	if (matchString(value, "top-lr"))
	{
		*ret = AlongTop;
		return True;
	}
	if (matchString(value, "top-rl"))
	{
		*ret = AlongTopRL;
		return True;
	}
	if (matchString(value, "bottom-lr"))
	{
		*ret = AlongBottom;
		return True;
	}
	if (matchString(value, "bottom-rl"))
	{
		*ret = AlongBottomRL;
		return True;
	}
	if (matchString(value, "right-tb"))
	{
		*ret = AlongRight;
		return True;
	}
	if (matchString(value, "right-bt"))
	{
		*ret = AlongRightBT;
		return True;
	}
	if (matchString(value, "left-tb"))
	{
		*ret = AlongLeft;
		return True;
	}
	if (matchString(value, "left-bt"))
	{
		*ret = AlongLeftBT;
		return True;
	}

	return False;
}


static Bool
matchMouselessKeyword(str, ret)
    char *str;
    MouselessMode *ret;
{
    if (0 == strcmp(str, "SunView1")) {
	*ret = KbdSunView;
	return True;
    } else if (0 == strcmp(str, "Basic")) {
	*ret = KbdBasic;
	return True;
    } else if (0 == strcmp(str, "Full")) {
	*ret = KbdFull;
	return True;
    }
    return False;
}


/*
 * Parse a key specification of the form
 *
 * [modifier ...] keysym
 *
 * For example, "Control Shift F7".  Returns True if a valid keyspec was
 * parsed, otherwise False.  The modifier mask is returned in modmask, and the
 * keycode is returned in keycode.
 */
static Bool
parseKeySpec(dpy, str, modmask, keycode)
    Display *dpy;
    char *str;
    unsigned int *modmask;
    KeyCode *keycode;
{
    char line[100];
    char *word;
    extern unsigned int FindModiferMask();
    int kc, m;
    int mask = 0;
    int code = 0;
    KeySym ks;

    strcpy(line, str);
    word = strtok(line, " \t");
    if (word == NULL)
	return False;

    while (word != NULL) {
	ks = XStringToKeysym(word);
	if (ks == NoSymbol) {
	    if (strcmp(word, "Any") == 0) {
		mask = AnyModifier;
		word = strtok(NULL, " \t");
		continue;
	    } else if (strcmp(word, "Shift") == 0)
		ks = XK_Shift_L;
	    else if (strcmp(word, "Control") == 0)
		ks = XK_Control_L;
	    else if (strcmp(word, "Meta") == 0)
		ks = XK_Meta_L;
	    else if (strcmp(word, "Alt") == 0)
		ks = XK_Alt_L;
	    else if (strcmp(word, "Super") == 0)
		ks = XK_Super_L;
	    else if (strcmp(word, "Hyper") == 0)
		ks = XK_Hyper_L;
	    else
		return False;
	}
	    
	kc = XKeysymToKeycode(dpy, ks);
	if (kc == 0)
	    return False;

	m = FindModifierMask(kc);
	if (m == 0) {
	    code = kc;
	    break;
	}
	mask |= m;
	word = strtok(NULL, " \t");
    }

    if (code == 0)
	return False;

    *keycode = code;
    *modmask = mask;
    return True;
}


#ifdef OW_I18N_L3

static void
setOLLCPosix()
{
       register OLLCItem       *ollci, *ollci_end;
       register char           *current;


       ollci = &(GRV.LC.BasicLocale);
       ollci_end = &ollci[OLLC_LC_MAX];
       for (ollci++; ollci < ollci_end; ollci++)
       {
               if ((ollci->locale == NULL
                               || ollci->priority >= OLLC_SRC_POSIX)
                 && ollci->posix_category >= 0)
               {
                       ollci->locale = strdup(setlocale(
                                               ollci->posix_category, NULL));
                       OLLCUpdated = True;
               }
      }
}

#endif /* OW_I18N_L3 */


/* ===== Converters ======================================================= */


/*
 * static Bool cvtWhatever(dpy, item, string, addr)
 *
 * The job of the converter is to take a string and convert it into the value
 * appropriate for storage into a global variable.  If the conversion is
 * successful, the value is stored at addr and True is returned.  Otherwise,
 * False is returned.  NOTE: the converted global variable shouldn't have any
 * pointers into the resource database.  If it's necessary to keep a handle on
 * this data, the converter should allocate memory and make a copy.  See also
 * the note about memory allocation in the comment at the top of the updaters
 * section, below.
 */


static Bool
cvtBoolean(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return matchBool(string, (Bool *)addr);
}


static Bool
cvtFont(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    XFontStruct	    **dest = addr;
    XFontStruct	    *info;
    
    info = XLoadQueryFont(dpy, string);

    if (info == NULL)
	return False;

    *dest = info;
    return True;
}


/*
 * cvtCursorFont -- set up ALL cursors from cursor font specified.
 *
 * NOTE that CursorColor and Bg1Color must be set before the cursors!
 *
 * Notice that six cursors are set up (and stored in six separate GRV
 * elements) from this single resource.  REMIND: this is kind of bogus.  
 * Ideally, all six cursors would have fonts and character indexes specifiable 
 * independently.  Further, addr isn't used; GRV is stored directly.
 *
 * REMIND: this appears to have a resource leak, in that cursorFont is loaded 
 * but never unloaded.
 */
static Bool
cvtCursorFont(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    Font	    cursorFont;
    int		    ii;
    Cursor	    *tmpVariable;
    unsigned int    tmpFontIndex;
    unsigned int    defaultIndex;
    XColor	    foreColor, backColor;
    
    cursorFont = XLoadFont(dpy, string);

    /*
     * REMIND: the following doesn't make any sense.  XLoadFont() simply 
     * allocates an ID, sends the LoadFont requst, and returns the ID.  There 
     * is no error indication in the return value from XLoadFont().  This 
     * needs to be fixed.  Perhaps using XLoadQueryFont() would be the right 
     * thing.
     */

    if (cursorFont == NULL)
	return False;

    /*
     * REMIND: in the future, we will probably want to set up some scheme for 
     * customizing cursor colors.  For now, use black and white.
     */

    foreColor.red = foreColor.green = foreColor.blue = 0;	/* black */
    backColor.red = backColor.green = backColor.blue = 65535;	/* white */

    for (ii = 0; ii < NUM_CURSORS; ++ii) {

	switch (ii) {

	case BASICPTR:
	    tmpVariable = &GRV.BasicPointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_left_ptr;
	    break;

	case MOVEPTR:
	    tmpVariable = &GRV.MovePointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_left_ptr;
	    break;

	case BUSYPTR:
	    tmpVariable = &GRV.BusyPointer;
	    tmpFontIndex = OLC_busy;
	    defaultIndex = XC_watch;
	    break;

	case ICONPTR:
	    tmpVariable = &GRV.IconPointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_left_ptr;
	    break;

	case RESIZEPTR:
	    tmpVariable = &GRV.ResizePointer;
	    tmpFontIndex = OLC_beye;
	    defaultIndex = XC_tcross;
	    break;

	case MENUPTR:
	    tmpVariable = &GRV.MenuPointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_sb_right_arrow;
	    break;

	case QUESTIONPTR:
	    tmpVariable = &GRV.QuestionPointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_question_arrow;
	    break;

	case TARGETPTR:
	    tmpVariable = &GRV.TargetPointer;
	    tmpFontIndex = OLC_basic;
	    defaultIndex = XC_circle;
	    break;

	case PANPTR:
	    tmpVariable = &GRV.PanPointer;
	    tmpFontIndex = OLC_panning;
	    defaultIndex = XC_sb_v_double_arrow;
	    break;
	}

	if (cursorFont == 0 ||
	    0 == (*tmpVariable = XCreateGlyphCursor(dpy, cursorFont,
			cursorFont, tmpFontIndex, tmpFontIndex+1, 
			&foreColor, &backColor)))
	{
	    /* use default */
	    *tmpVariable = XCreateFontCursor( dpy, defaultIndex );
#ifdef LATER
	    XRecolorCursor(dpy, tmpVariable, &foreColor, &backColor);
#endif
	}
    }

    return True;
}


/*
 * Converting a string simply means making a copy of it.
 */
static Bool
cvtString(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    char **str = addr;

    if (string == NULL)
	return False;

    *str = MemNewString(string);
    return True;
}


static Bool
cvtFloat(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return (1 == sscanf(string, "%f", (float *)addr));
}


/*
 * Convert an integer.  Note that %i converts from decimal, octal, and 
 * hexadecimal representations.
 */
static Bool
cvtInteger(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return (1 == sscanf(string, "%i", (int *)addr));
}


/*
 * Convert a string representing tenths of a second into milliseconds.
 */
static Bool
cvtClickTimeout(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    int intval;
    int *dest = addr;

    if (1 != sscanf(string, "%d", &intval))
	return False;

    intval *= 100;			/* convert to milliseconds */

    /*
     * It's nearly impossible for typical mouse hardware to generate two
     * clicks in less than 100ms.  We special-case this and make the minimum
     * timeout value be 150ms.
     */
    if (intval < 150)
	intval = 150;

    *dest = intval;
    return True;
}


static Bool
cvtFocusStyle(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return matchFocusKeyword(string, (Bool *)addr);
}


static Bool
cvtBeepStatus(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return matchBeepKeyword(string, (BeepStatus *)addr);
}


static Bool
cvtMouseless(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return matchMouselessKeyword(string, (MouselessMode *)addr);
}


static Bool
cvtIconLocation(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return matchIconPlace(string, (IconPreference *)addr);
}


/*
 * Convert a key specification.  REMIND: this needs to be reconciled with the 
 * key specification stuff in evbind.c.
 */
static Bool
cvtKey(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    KeySpec	    *keyspec = addr;
    unsigned int    modmask;
    KeyCode	    keycode;

    if (!parseKeySpec(dpy, string, &modmask, &keycode))
	return False;

    keyspec->modmask = modmask;
    keyspec->keycode = keycode;
    return True;
}


/*
 * buildStringList -- parse a string into words and build a linked list of 
 * them.
 */
static void
buildStringList(str, pplist)
char *str;
List **pplist;
{
    char *swork, *swork2;
    List *l = NULL_LIST;

    swork2 = swork = MemNewString(str);

    while ((swork2 = strtok(swork2, " \t")) != NULL) {
	l = ListCons(MemNewString(swork2),l);
	swork2 = NULL;
    }
    MemFree(swork);
    *pplist = l;
}


static void *
freeStringList(str,junk)
char *str;
void *junk;
{
	MemFree(str);
	return NULL;
}


static Bool
cvtStringList(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    List **dest = addr;
    List *newl = NULL_LIST;

    buildStringList(string, &newl);
    *dest = newl;
    return True;
}


#ifdef OW_I18N_L3

/*
 * REMIND: somewhat strange.  This function always returns True, so the
 * default value in the Resource Table is never used.  Further, this function 
 * handles both the conversion and update functions itself.
 */
static Bool
_cvtOLLC(dpy, item, string, addr, priority)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    OLLCItem	    *ollcitem = addr;
    char	    *newlocale;

    if (priority < ollcitem->priority)
	return True;

#ifdef notdef
    fprintf(stderr,
"_cvtOLLC locale#%d, newpri=%d, curpri=%d, newlocale %s, curlocale %s\n",
	    ollcitem->posix_category, priority, ollcitem->priority,
	    string, ollcitem->locale);
#endif

    /* don't need to do anything if the new locale is the same as the old */

    if ((string == NULL && ollcitem->locale == NULL) ||
        (string != NULL && ollcitem->locale != NULL &&
	 0 == strcmp(string, ollcitem->locale)))
    {
	return True;
    }

    /* they differ; update the locale */

    if (string == NULL)
	newlocale = NULL;
    else
	newlocale = MemNewString(string);

    if (ollcitem->locale != NULL)
	MemFree(ollcitem->locale);

    ollcitem->locale = newlocale;
    ollcitem->priority = priority;

    OLLCUpdated = True;

#ifdef notdef
    fprintf(stderr, "_cvtOLLC: locale#%d -> %s\n",
	    ollcitem->posix_category, ollcitem->locale);
#endif

    return True;
}


static Bool
cvtOLLC(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return _cvtOLLC(dpy, item, string, addr, OLLC_SRC_RESOURCE);
}


static Bool
cvtOLLCCL(dpy, item, string, addr)
    Display	    *dpy;
    ResourceItem    *item;
    char	    *string;
    void	    *addr;
{
    return _cvtOLLC(dpy, item, string, addr, OLLC_SRC_COMMAND_LINE);
}

#endif /* OW_I18N_L3 */



/* ===== Updaters ========================================================= */


/*
 * static void updWhatever(dpy, item, cur, new);
 *
 * The job of the updater is to compare the current value and newly converted
 * values, and update the current value if they differ.  It is responsible
 * for all changes in global state, such as grabbing and ungrabbing keys.  
 * NOTE: if the converter has allocated memory, the updater must free it 
 * appropriately.  Since the updater is called with old and new values, 
 * exactly one of them should be freed by the updater, otherwise a memory leak 
 * will result.
 */

static void
updString(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
}


static void
updStringList(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    List	    **cur, **new;
{
    ListApply(*cur, freeStringList, NULL);
    ListDestroy(*cur);
    *cur = *new;
}


static void
updWorkspace(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
    SetWorkspaceColor(dpy);
}


static void
updWindow(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
    SetWindowColor(dpy);
}


static void
updForeground(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
    SetForegroundColor(dpy);
}

static void
updBackground(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
    SetBackgroundColor(dpy);
}

static void
updBorder(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    char	    **cur, **new;
{
    MemFree(*cur);
    *cur = *new;
    SetBorderColor(dpy);
}


static void
updSync(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    Bool	    *cur, *new;
{
    if (*cur != *new) {
	(void) XSynchronize(dpy, *new);
	*cur = *new;
    }
}


static void
updTitleFont(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    XFontStruct	    **cur, **new;
{
    XFree((char *) *cur);
    *cur = *new;
    SetTitleFont(dpy);
}


static void
updTextFont(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    XFontStruct	    **cur, **new;
{
    XFree((char *) *cur);
    *cur = *new;
    SetTextFont(dpy);
}


static void
updButtonFont(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    XFontStruct	    **cur, **new;
{
    XFree((char *) *cur);
    *cur = *new;
    SetButtonFont(dpy);
}


static void
updIconFont(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    XFontStruct	    **cur, **new;
{
    XFree((char *) *cur);
    *cur = *new;
    SetIconFont(dpy);
}


static void
updGlyphFont(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    XFontStruct	    **cur, **new;
{
    XFree((char *) *cur);
    *cur = *new;
    SetGlyphFont(dpy);
}


static void
updIconLocation(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    IconPreference  *cur, *new;
{
    if (*cur != *new) {
	*cur = *new;
	SetIconLocation(dpy);
    }
}


static void
updMouseless(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    MouselessMode   *cur, *new;
{
    if (*cur != *new) {
	*cur = *new;
	RefreshKeyGrabs(dpy);
    }
}


/*
 * unconfigureFocus
 *
 * Tell a client to remove any grabs it may have set up according to the focus 
 * mode.  If this client is the focus, tell it to draw in its unfocused state.
 */
static void *
unconfigureFocus(cli)
    Client *cli;
{
    if (cli->framewin == NULL)
	return NULL;
    FrameSetupGrabs(cli, cli->framewin->core.self, False);
    if (cli->isFocus) {
	cli->isFocus = False;
	WinCallDraw((WinGeneric *)cli->framewin);
	cli->isFocus = True;
    }
    return NULL;
}


/*
 * reconfigureFocus
 *
 * Tell a client to restore any grabs it may need for the new focus mode.  If 
 * this client is the focus, tell it to draw using the proper highlighting for 
 * the new focus mode.
 */
static void *
reconfigureFocus(cli)
    Client *cli;
{
    if (cli->framewin == NULL)
	return NULL;
    FrameSetupGrabs(cli, cli->framewin->core.self, True);
    if (cli->isFocus) {
	WinCallDraw((WinGeneric *)cli->framewin);
    }
    return NULL;
}


/*
 * UpdFocusStyle -- change the focus style on the fly
 *
 * If focus style needs updating, call unconfigureFocus on every client.  This
 * will clear grabs and highlighting and such while the old focus mode is
 * still in effect.  Update the global value, and then call reconfigureFocus
 * on every client to set up stuff for the new focus mode.
 *
 * REMIND: This function is global because it's called from FlipFocusFunc in
 * services.c.  This call passes NULL for item.  This needs to be cleaned up.
 */
void
UpdFocusStyle(dpy, item, cur, new)
    Display	    *dpy;
    ResourceItem    *item;
    Bool	    *cur, *new;
{
    if (*cur != *new) {
	ListApply(ActiveClientList, unconfigureFocus, 0);
	*cur = *new;
	ListApply(ActiveClientList, reconfigureFocus, 0);
    }
}


/* ===== Global Functions ================================================= */


/*
 * InitGlobals -- probe OlwmDB for resources and store values into global 
 * variables.  Called once at startup time.
 */
void
InitGlobals(dpy)
    Display	*dpy;
{
    ResourceItem *item;
    int i;
    XrmRepresentation type;
    XrmValue value;
    XrmQuark classes[3];
    XrmQuark instances[3];
    char buf[1000];

    (void) memset((char *) &GRV, 0, sizeof(GRV));

#ifdef OW_I18N_L3
    GRVLCInit();
#endif /* OW_I18N_L3 */

    classes[0] = OpenWinQ;
    instances[0] = TopInstanceQ;
    classes[2] = instances[2] = NULLQUARK;

    for (i = 0; i < NRESOURCEITEMS; ++i) {

	item = &ResourceTable[i];

	classes[1]   = item->classQ    = XrmStringToQuark(item->class);
	instances[1] = item->instanceQ = XrmStringToQuark(item->instance);

	/*
	 * Probe the database.  If the probe fails, or if the probe succeeds 
	 * but the resulting value cannot be converted, convert the default 
	 * value into the global variable.
	 */

	if (!XrmQGetResource(OlwmDB, instances, classes, &type, &value) ||
	    !(*item->converter)(dpy, item, (char *)value.addr, item->addr))
	{
	    (void) (*item->converter)(dpy, item, item->defaultString,
				      item->addr);
	}
    }

    /*
     * Special case for glyph font: if we couldn't find a valid glyph font,
     * it's a fatal error.
     */
    if (GRV.GlyphFontInfo == NULL)
	ErrorGeneral(gettext("can't open glyph font"));
	/*NOTREACHED*/

#ifdef OW_I18N_L3
	setOLLCPosix();
#endif /* OW_I18N_L3 */
}


typedef union _datum {
    int		    intval;
    void	    *pointer;
    KeySpec	    keyspec;
#ifdef OW_I18N_L3
    OLLCItem	    ollcitem;
#endif /* OW_I18N_L3 */
} Datum;


static XrmBinding Bindings[] = { XrmBindTightly, XrmBindTightly };


/*
 * UpdateGlobals -- handle updates to the server's resource database.  Called
 * every time the server's RESOURCE_MANAGER property changes.  stringdb is the
 * contents of this property.  Creates a new database and probes into it.  For
 * entries that have changed, convert the entry into a value and call the
 * update function.  If there is no update function, convert the entry
 * directly into the global variable.  Destroys the new database after 
 * processing it.
 */
void
UpdateGlobals(dpy, stringdb)
    Display		*dpy;
    char		*stringdb;
{
    Datum		datum;
    ResourceItem	*item;
    int			i;
    XrmDatabase		newDB;
    XrmRepresentation	type;
    XrmValue		newvalue, oldvalue;
    XrmQuark		classes[3];
    XrmQuark		instances[3];
    void		*dest;

    newDB = XrmGetStringDatabase(stringdb);

    classes[0] = OpenWinQ;
    instances[0] = TopInstanceQ;
    classes[2] = instances[2] = NULLQUARK;

    for (i = 0; i < NRESOURCEITEMS; ++i) {

	item = &ResourceTable[i];

	classes[1]   = item->classQ;
	instances[1] = item->instanceQ;

	if (!XrmQGetResource(newDB, instances, classes, &type, &newvalue))
	    continue;

	if (XrmQGetResource(OlwmDB, instances, classes, &type, &oldvalue) &&
	    0 == strcmp((char *)newvalue.addr, (char *)oldvalue.addr))
	{
	    /* old and new values the same; ignore */
	    continue;
	}

	XrmQPutStringResource(&OlwmDB, Bindings, instances,
			      (char *)newvalue.addr);

	if (item->updater == NULL) {
	    (void) (*item->converter)(dpy, item, (char *)newvalue.addr,
				      item->addr);
	} else {
	    (void) memset((char *) &datum, 0, sizeof(datum));
	    if ((*item->converter)(dpy, item, (char *)newvalue.addr, &datum))
		(*item->updater)(dpy, item, item->addr, &datum);
	}
    }

    UpdateBindings(dpy, newDB);

#ifdef OW_I18N_L3
    EffectOLLC(dpy);
#endif /* OW_I18N_L3 */

    XrmDestroyDatabase(newDB);
}


#ifdef OW_I18N_L3

 /*
  * GRVLCInit: Here is the table for OPEN LOOK locale and POSIX/ANSI-C
  * locale categories.
  */
static void
GRVLCInit()
{
       GRV.LC.BasicLocale.posix_category       = LC_CTYPE;
       GRV.LC.DisplayLang.posix_category       = LC_MESSAGES;
       GRV.LC.InputLang.posix_category         = -1;
       GRV.LC.Numeric.posix_category           = LC_NUMERIC;
       GRV.LC.DateFormat.posix_category        = LC_TIME;
}


void
EffectOLLC(dpy)
Display       *dpy;
{
       register OLLCItem       *ollci, *ollci_end, *ollci_basic;
       register char           *current;
       register int            require_all_update;
       int                     messages_updated;


       if (!OLLCUpdated)
               return;

       require_all_update = False;
       messages_updated = False;
       ollci_basic = &(GRV.LC.BasicLocale);
       ollci_end = &ollci_basic[OLLC_LC_MAX];

       /*
        * This is silly restriction, but works well for Sundae1.0
        * environment.
        */
       current = setlocale(LC_CTYPE, NULL);
       if (strcmp(current, "C") == 0
        && ollci_basic->locale != NULL
        && strcmp(current, ollci_basic->locale) != 0)
       {
               /*
                * I'm doing LC_ALL rather only LC_CTYPE, becuase
                * there are some locale category which does not
                * covered by current OLLC spec.
                */
#ifdef notdef
 fprintf(stderr, "Basic Locale -> %s\n", ollci_basic->locale);
#endif
               setlocale(LC_ALL, ollci_basic->locale);
               require_all_update = True;
       }


       for (ollci = ollci_basic + 1; ollci < ollci_end; ollci++)
       {
               if (ollci->posix_category < 0)
                       continue;
               current = setlocale(ollci->posix_category, NULL);
               if (strcmp(current, ollci->locale) != 0
               || require_all_update == True)
               {
                       /*
                        * Again, we need following silly restriction
                        * in order to work well with EUC based
                        * environment.
                        */
                      if (strcmp(current, "C") == 0
                        || strcmp(ollci->locale, "C") == 0
                        || strcmp(ollci->locale, ollci_basic->locale) == 0)
                       {
#ifdef notdef
 fprintf(stderr, "locale#%d -> %s\n", ollci->posix_category, ollci->locale);
#endif
                               setlocale(ollci->posix_category,
                                       ollci->locale);
                               if (ollci->posix_category == LC_MESSAGES)
                                       messages_updated = True;
                       }
               }
       }
       OLLCUpdated = False;

       if (messages_updated == True) {
		WindowMenuDestroy(dpy);
		WindowMenuCreate(dpy);
		ReInitUserMenu(dpy,True);
       }
}
#endif /* OW_I18N_L3 */
