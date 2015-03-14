/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)atom.c	26.17	91/09/14 SMI"

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

Atom	AtomAtomPair;
Atom	AtomChangeState;
Atom	AtomClass;
Atom	AtomColorMapWindows;
Atom	AtomDecorAdd;
Atom	AtomDecorClose;
Atom	AtomDecorDel;
Atom	AtomDecorFooter;
Atom	AtomDecorHeader;
Atom	AtomDecorPin;
Atom	AtomDecorResize;
Atom	AtomDelete;
Atom	AtomDeleteWindow;
Atom	AtomDfltBtn;
Atom	AtomLeftFooter;
Atom	AtomLength;
Atom	AtomListLength;
Atom	AtomMenuFull;
Atom	AtomMenuLimited;
Atom	AtomMultiple;
Atom	AtomName;
Atom	AtomNone;
Atom	AtomOlwmTimestamp;
Atom	AtomPinIn;
Atom	AtomPinOut;
Atom	AtomProtocols;
Atom	AtomPushpinState;
Atom	AtomRightFooter;
Atom	AtomSaveYourself;
Atom	AtomShowProperties;
Atom	AtomSunViewEnv;
Atom	AtomTakeFocus;
Atom	AtomTargets;
Atom	AtomTimestamp;
Atom	AtomUser;
Atom	AtomWMClass;
Atom	AtomWMName;
Atom	AtomWMIconName;
Atom	AtomWMHints;
Atom	AtomWMNormalHints;
Atom	AtomWMSizeHints;
Atom	AtomWMTransientFor;
Atom	AtomWMApplyProperties;
Atom	AtomWMResetProperties;
Atom	AtomWMState;
Atom	AtomWTBase;
Atom	AtomWTCmd;
Atom	AtomWTHelp;
Atom	AtomWTNotice;
Atom	AtomWTOther;
Atom	AtomWinAttr;
Atom	AtomWindowBusy;
Atom	AtomSunLedMap;
Atom	AtomSunWMProtocols;
Atom	AtomSunWindowState;
Atom	AtomSunDragDropInterest;
Atom	AtomSunOLWinAttr5;
Atom	AtomDecorIconName;

/***************************************************************************
* Global functions
***************************************************************************/

/*
 * InitAtoms -- initialize the atoms needed to communicate with Open
 *	Look clients
 */
InitAtoms(dpy)
Display	*dpy;
{
	/* ICCCM specific flags */
	AtomColorMapWindows = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
	AtomWMState = XInternAtom(dpy, "WM_STATE", False);
	AtomChangeState = XInternAtom(dpy, "WM_CHANGE_STATE" , False);
	AtomProtocols = XInternAtom(dpy, "WM_PROTOCOLS" , False);
	AtomTakeFocus = XInternAtom(dpy, "WM_TAKE_FOCUS" , False);
	AtomSaveYourself = XInternAtom(dpy, "WM_SAVE_YOURSELF" , False);
	AtomDeleteWindow = XInternAtom(dpy, "WM_DELETE_WINDOW" , False);

	/* Predefined atoms - referenced from ClientDistributeProperties */
	AtomWMName = XA_WM_NAME;
	AtomWMIconName = XA_WM_ICON_NAME;
	AtomWMClass = XA_WM_CLASS;
	AtomWMHints = XA_WM_HINTS;
	AtomWMNormalHints = XA_WM_NORMAL_HINTS;
	AtomWMSizeHints = XA_WM_SIZE_HINTS;
	AtomWMTransientFor = XA_WM_TRANSIENT_FOR;

	/* OpenLook specific flags */
	AtomWinAttr = XInternAtom(dpy, "_OL_WIN_ATTR" , False);
	AtomPushpinState = XInternAtom(dpy, "_OL_PIN_STATE" , False);
	AtomWindowBusy = XInternAtom(dpy, "_OL_WIN_BUSY" , False);
	AtomLeftFooter = XInternAtom(dpy, "_OL_WINMSG_ERROR" , False);
	AtomRightFooter = XInternAtom(dpy, "_OL_WINMSG_STATE" , False);
	AtomShowProperties = XInternAtom(dpy, "_OL_SHOW_PROPS" , False);
	AtomWMApplyProperties = XInternAtom(dpy, "_OL_PROPS_APPLY" , False);
	AtomWMResetProperties = XInternAtom(dpy, "_OL_PROPS_RESET" , False);
	AtomPinOut = XInternAtom(dpy, "_OL_PIN_OUT" , False);
	AtomDecorResize = XInternAtom(dpy, "_OL_DECOR_RESIZE" , False);
	AtomWTBase = XInternAtom(dpy, "_OL_WT_BASE" , False);
	AtomDecorFooter = XInternAtom(dpy, "_OL_DECOR_FOOTER" , False);
	AtomDecorAdd = XInternAtom(dpy, "_OL_DECOR_ADD" , False);
	AtomDecorDel = XInternAtom(dpy, "_OL_DECOR_DEL" , False);
	AtomDecorPin = XInternAtom(dpy, "_OL_DECOR_PIN" , False);
	AtomWTCmd = XInternAtom(dpy, "_OL_WT_CMD" , False);
	AtomPinIn = XInternAtom(dpy, "_OL_PIN_IN" , False);
	AtomNone = XInternAtom(dpy, "_OL_NONE" , False);
	AtomWTNotice = XInternAtom(dpy, "_OL_WT_NOTICE" , False);
	AtomMenuFull = XInternAtom(dpy, "_OL_MENU_FULL" , False);
	AtomDecorHeader = XInternAtom(dpy, "_OL_DECOR_HEADER" , False);
	AtomWTHelp = XInternAtom(dpy, "_OL_WT_HELP" , False);
	AtomMenuLimited = XInternAtom(dpy, "_OL_MENU_LIMITED" , False);
	AtomDecorClose = XInternAtom(dpy, "_OL_DECOR_CLOSE" , False);
	AtomWTOther = XInternAtom(dpy, "_OL_WT_OTHER" , False);
	AtomOlwmTimestamp = XInternAtom(dpy,"_OLWM_TIMESTAMP",False);
	AtomDfltBtn = XInternAtom(dpy, "_OL_DFLT_BTN", False);
	AtomDecorIconName = XInternAtom(dpy, "_OL_DECOR_ICON_NAME", False);

	/* ICCCM selection atoms */
	AtomAtomPair = XInternAtom(dpy,"ATOM_PAIR",False);
	AtomClass = XInternAtom(dpy,"CLASS",False);
	AtomDelete = XInternAtom(dpy,"DELETE",False);
	AtomMultiple = XInternAtom(dpy,"MULTIPLE",False);
	AtomLength = XInternAtom(dpy,"LENGTH",False);
	AtomListLength = XInternAtom(dpy,"LIST_LENGTH",False);
	AtomName = XInternAtom(dpy,"NAME",False);
	AtomTargets = XInternAtom(dpy,"TARGETS",False);
	AtomTimestamp = XInternAtom(dpy,"TIMESTAMP",False);
	AtomUser = XInternAtom(dpy,"USER",False);

	/* SunView environment */
	AtomSunViewEnv = XInternAtom(dpy,"_SUN_SUNVIEW_ENV",False);

	/* Sun window manager atoms */
	AtomSunLedMap = XInternAtom(dpy,"_SUN_LED_MAP",False);
	AtomSunWMProtocols = XInternAtom(dpy,"_SUN_WM_PROTOCOLS",False);
	AtomSunWindowState = XInternAtom(dpy,"_SUN_WINDOW_STATE",False);
	AtomSunOLWinAttr5 = XInternAtom(dpy,"_SUN_OL_WIN_ATTR_5",False);

	AtomSunDragDropInterest =
	    XInternAtom(dpy, "_SUN_DRAGDROP_INTEREST", False);
}

#ifdef notdef
/* XXX remove after properties.c working */
void *
GetWindowProperty(dpy, w, property, long_offset, long_length, req_type, 
    req_fmt, nitems, bytes_after)
Display *dpy;
Window w;
Atom property;
long long_offset, long_length;
Atom req_type;
int req_fmt;
unsigned long *nitems;
unsigned long *bytes_after;
{
	int status;
	unsigned char *prop;
	Atom act_type;
	int act_format;

	status = XGetWindowProperty(dpy, w, property, long_offset, long_length,
		    False, req_type, &act_type, &act_format, nitems,
		    bytes_after, &prop);
	if ((status != Success) || (act_type != req_type))
	{
	    *nitems = 0;
	    return NULL;
	}
	if ((req_fmt != 0) && (act_format != req_fmt))
	{
	    XFree((char *)prop);
	    *nitems = 0;
	    return NULL;
	}
	return (void *)prop;
}
#endif
