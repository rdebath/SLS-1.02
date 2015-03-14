/*
 *      (c) Copyright 1991 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)evbind.c	1.21	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "events.h"
#include "list.h"
#include "mem.h"
#include "kbdfuncs.h"
#include "resources.h"


/* ===== externs ========================================================== */

extern List *ScreenInfoList;

/* ===== private data ===================================================== */

static XrmQuark kbdCmdInstanceQ;
static XrmQuark kbdCmdClassQ;
static XrmQuark modInstanceQ;
static XrmQuark modClassQ;

/* ===== Modifier Binding ================================================= */


unsigned int ModMaskMap[MOD_MASK_COUNT];

typedef struct {
    char *rsrc_name;
    char *dflt_binding;
    int mapindex;
} ModDescriptor;

ModDescriptor ModDescriptorTable[] = {

     /*	    rsrc_name		default		    mapindex */
    {	    "Constrain",	"Control",	    MOD_CONSTRAIN	},
    {	    "WMGrab",		"Alt",		    MOD_WMGRAB		},
    {	    "Reduce",		"Meta",		    MOD_REDUCE		},
    {	    "Invert",		"Shift",	    MOD_INVERT		},
    {	    "SetDefault",	"Control",	    MOD_SETDEFAULT	},
    {	    "Ignore", "Lock,Num_Lock,mod5,Mode_switch", MOD_IGNORE	}

};
#define NMODBINDINGS (sizeof(ModDescriptorTable)/sizeof(ModDescriptor))


/*
 * establishModBindings
 *
 * Read through the modifier descriptor table and fill in the modifier mask 
 * map with modifier masks found in the server's modifier mask map.
 */
static void
establishModBindings(dpy, newDB)
    Display *dpy;
    XrmDatabase newDB;
{
    XrmQuark classlist[4], instlist[4];
    char *s;
    XrmRepresentation rep;
    XrmValue value;
    ModDescriptor *d;
    unsigned int polyStringToModifier();
    static XrmBinding bindings[] =
	{ XrmBindTightly, XrmBindTightly, XrmBindTightly };

    classlist[0] = OpenWinQ;
    classlist[1] = modClassQ;
    classlist[3] = NULLQUARK;

    instlist[0] = TopInstanceQ;
    instlist[1] = modInstanceQ;
    instlist[3] = NULLQUARK;

    for (d = ModDescriptorTable; d < ModDescriptorTable + NMODBINDINGS; ++d) {

	classlist[2] = instlist[2] = XrmStringToQuark(d->rsrc_name);

	if (XrmQGetResource(newDB, instlist, classlist, &rep, &value))
	    s = (char *) value.addr;
	else
	    s = d->dflt_binding;

	ModMaskMap[d->mapindex] = polyStringToModifier(dpy, s);
    }
}


/* ===== Mouse Binding ==================================================== */


/*
 * Table of mouse bindings.
 *
 * REMIND
 * 
 * At a future time, this table will be changeable via resources.  For now,
 * it's specified as a compile-time constant.
 */

typedef struct {
    int state;
    int button;
    SemanticAction action;
} MouseBinding;

MouseBinding MouseBindingTable[] = {
  /*  state,		button,	 action		 */
    { 0,		Button1, ACTION_SELECT },
    { 0,		Button2, ACTION_ADJUST },
    { 0,		Button3, ACTION_MENU   },
    { ButtonToMask(1),	Button2, ACTION_MENU },
    { ButtonToMask(2),	Button1, ACTION_MENU },
    { ShiftMask,	Button1, ACTION_ADJUST },
    { ControlMask,	Button3, ACTION_MENU }, 
    { ControlMask|ButtonToMask(1),	Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(2),	Button1, ACTION_MENU },
};
#define NMOUSEBINDINGS (sizeof(MouseBindingTable)/sizeof(MouseBinding))


/*
 * searchMouseBindings
 * 
 * Search the mouse binding table and return information about what was found.
 * Return values of MATCH_NONE, MATCH_INCOMPLETE, and MATCH_AMBIG mean that no
 * action is available for the given event.  MATCH_PREFIX and MATCH_EXACT
 * indicate that an action is available; this action is returned in the area
 * pointed to by the action parameter.
 */
static MouseMatchState
searchMouseBindings(pe, action)
    XButtonEvent *pe;
    SemanticAction *action;
{
    int i;
    int nexact = 0;	/* number of exact matches */
    int nprefix = 0;	/* number of prefix matches */
    int lastexact = 0;	/* index of last exact match */
    unsigned int modmask = (pe->state | ButtonToMask(pe->button));

    for (i=0; i<NMOUSEBINDINGS; ++i) {
	if (pe->state == MouseBindingTable[i].state &&
	  pe->button == MouseBindingTable[i].button) {
	    lastexact = i;
	    ++nexact;
	} else if (modmask == MouseBindingTable[i].state) {
	    ++nprefix;
	}
    }

    if (nexact == 0 && nprefix == 0)
	return MATCH_NONE;
    if (nexact == 0 && nprefix > 0)
	return MATCH_INCOMPLETE;
    if (nexact > 1)
	return MATCH_AMBIG;

    /* at this point, we know there is exactly one exact match */
    *action = MouseBindingTable[lastexact].action;
    if (nprefix == 0)
	return MATCH_EXACT;
    else
	return MATCH_PREFIX;
}


/*
 * checkChording
 *
 * Scan the input queue for button events that will disambiguate a single
 * action from a chorded action.  If there aren't any events in the queue,
 * wait for them until a certain timeout period has elapsed.  Return value
 * indicates whether a ButtonPress was seen further ahead in the event stream,
 * which indicates that this event is part of a chorded sequence.  The timeout 
 * parameter is updated with the amount of time remaining.
 */
static Bool
checkChording(dpy, timeout, pr)
    Display *dpy;
    struct timeval timeout;
    XButtonEvent *pr;
{
    XEvent e;
    int n;
    
    while (1) {
	/*
	 * Check for data on the connection.  Scan it for disambiguating 
	 * events.  Note that MotionNotify events within the move threshold 
	 * are discarded.
	 */
	n = XEventsQueued(dpy, QueuedAfterReading);
	if (n > 0 && XCheckMaskEvent(dpy,
	ButtonPressMask|ButtonReleaseMask|ButtonMotionMask, &e)) {
	    switch (e.type) {
	    case ButtonPress:
		XPutBackEvent(dpy, &e);
		return True;
	    case ButtonRelease:
		XPutBackEvent(dpy, &e);
		return False;
	    case MotionNotify:
		if (ABS(pr->x_root - e.xmotion.x_root) > GRV.MoveThreshold ||
		    ABS(pr->y_root - e.xmotion.y_root) > GRV.MoveThreshold) {
		    XPutBackEvent(dpy, &e);
		    return False;
		}
		break;
	    }
	}

	if (!AwaitEvents(dpy, &timeout))
	    return False;
    }
}


/*
 * ResolveMouseBinding
 *
 * Given a mouse button press event, determines whether this event completes
 * an event sequence that binds to an action.  If the button press is a prefix
 * of a chording sequence, and this press falls within the chording time of
 * the initial button press, checkChording is called to disambiguate the event
 * stream.  Returns a proper action if the action is complete, otherwise
 * returns ACTION_NONE.  All callers should ensure that no action is taken
 * when this routine returns ACTION_NONE.
 */
SemanticAction
ResolveMouseBinding(dpy, pevent, ignoremask)
    Display *dpy;
    XEvent *pevent;
    unsigned long ignoremask;
{
    MouseMatchState m;
    struct timeval timeout;
    SemanticAction a;
    static Time firstpresstime;
    XEvent e;

    /* copy *pevent to e, masking off ignored bits from the state */
    e = *pevent;
    e.xbutton.state &= ~(ignoremask |
			 ModMaskMap[MOD_IGNORE] |
			 ModMaskMap[MOD_WMGRAB]);

    /* Chording is in msec.  Convert to sec/usec for timeval. */
    timeout.tv_usec = GRV.MouseChordTimeout * 1000;
    if (timeout.tv_usec >= 1000000) {
	timeout.tv_sec = timeout.tv_usec / 1000000;
	timeout.tv_usec %= 1000000;
    } else {
	timeout.tv_sec = 0;
    }

    if (FirstButtonDown(&e)) {
	firstpresstime = e.xbutton.time;
    } else {
	if (e.xbutton.time - firstpresstime > GRV.MouseChordTimeout)
	    return ACTION_NONE;
    }

    m = searchMouseBindings(&e, &a);
    if ((m == MATCH_PREFIX && !checkChording(dpy, timeout, &e))
	|| m == MATCH_EXACT) {
	return a;
    } else {
	return ACTION_NONE;
    }
}


/* ===== Keyboard Binding ================================================= */

/*
 * Table of default keyboard descriptors.  This table contains information 
 * necessary to initialize keyboard bindings and customize them based on 
 * resources.
 */

#define NULLFUNC ((void (*)())0)

extern void HandleHelpKey();

static void keySuspend();
static void keyResume();
static void keyQuoteNext();

typedef struct {
    char *rsrc_name;
    char *dflt_binding;
    void (*function)();
    SemanticAction action;
    unsigned long flags;
} KeyDescriptor;

/* values for flags */
#define KD_IMMUNE	(1<<0)		/* immune to suspension */
#define KD_SUNVIEW	(1<<1)		/* active if mouseless == SunView */
#define KD_BASIC	(1<<2)		/* active if mouseless == basic */
#define KD_FULL		(1<<3)		/* active if mouseless == full */

#define KD_BASIC_FULL	(KD_BASIC | KD_FULL)
#define KD_ALWAYS	(KD_SUNVIEW | KD_BASIC_FULL)

static unsigned long mouselessMaskTable[] = { KD_SUNVIEW, KD_BASIC, KD_FULL };

KeyDescriptor KeyDescriptorTable[] = {

/*
	rsrc_name		    dflt_binding		function
	action			    flags
 */

{
	"Stop",			    "L1,Escape",		NULLFUNC,
	ACTION_STOP,		    KD_ALWAYS
}, {
	"DefaultAction",	"Return,Return+Meta,KP_Enter",	NULLFUNC,
	ACTION_EXEC_DEFAULT,	    KD_ALWAYS
}, {
	"Select",		    "space",			NULLFUNC,
	ACTION_SELECT,		    KD_ALWAYS
}, {
	"Adjust",		    "Insert+Alt",		NULLFUNC,
	ACTION_ADJUST,		    KD_ALWAYS
}, {
	"Menu",			    "space+Alt",		NULLFUNC,
	ACTION_MENU,		    KD_ALWAYS
}, {
	"InputFocusHelp",	    "question,question+Ctrl",	NULLFUNC,
	ACTION_FOCUS_HELP,	    KD_ALWAYS
}, {
	"Up",			    "Up",			NULLFUNC,
	ACTION_UP,		    KD_ALWAYS
}, {
	"Down",			    "Down",			NULLFUNC,
	ACTION_DOWN,		    KD_ALWAYS
}, {
	"Left",			    "Left",			NULLFUNC,
	ACTION_LEFT,		    KD_ALWAYS
}, {
	"Right",		    "Right",			NULLFUNC,
	ACTION_RIGHT,		    KD_ALWAYS
}, {
	"JumpUp",		    "Up+Ctrl",			NULLFUNC,
	ACTION_JUMP_UP,		    KD_ALWAYS
}, {
	"JumpDown",		    "Down+Ctrl",		NULLFUNC,
	ACTION_JUMP_DOWN,	    KD_ALWAYS
}, {
	"JumpLeft",		    "Left+Ctrl",		NULLFUNC,
	ACTION_JUMP_LEFT,	    KD_ALWAYS
}, {
	"JumpRight",		    "Right+Ctrl",		NULLFUNC,
	ACTION_JUMP_RIGHT,	    KD_ALWAYS
}, {
	"RowStart",		    "Home,R7",			NULLFUNC,
	ACTION_ROW_START,	    KD_ALWAYS
}, {
	"RowEnd",		    "End,R13",			NULLFUNC,
	ACTION_ROW_END,		    KD_ALWAYS
}, {
	"DataStart",		    "Home+Ctrl",		NULLFUNC,
	ACTION_DATA_START,	    KD_ALWAYS
}, {
	"DataEnd",		    "End+Ctrl",			NULLFUNC,
	ACTION_DATA_END,	    KD_ALWAYS
}, {
	"FirstControl",		    "bracketleft+Ctrl",		NULLFUNC,
	ACTION_FIRST_CONTROL,	    KD_ALWAYS
}, {
	"LastControl",		    "bracketright+Ctrl",	NULLFUNC,
	ACTION_LAST_CONTROL,	    KD_ALWAYS
}, {
	"NextElement",		    "Tab,Tab+Ctrl",		NULLFUNC,
	ACTION_NEXT_ELEMENT,	    KD_ALWAYS
}, {
	"PreviousElement",	    "Tab+Shift,Tab+Shift+Ctrl",	NULLFUNC,
	ACTION_PREVIOUS_ELEMENT,    KD_ALWAYS
}, {
	"Open",			    "L7+Alt",			NULLFUNC,
	ACTION_OPEN,		    KD_ALWAYS
}, {
	"Help",			    "Help",			HandleHelpKey,
	ACTION_HELP,		    KD_BASIC_FULL
}, {
	"LockColormap",		    "L2+Ctrl",		    KeyLockColormap,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"UnlockColormap",	    "L4+Ctrl",		    KeyUnlockColormap,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"Front",		    "L5+Alt",			KeyFrontFocus,
	ACTION_FRONT,		    KD_BASIC_FULL
}, {
	"FocusToPointer",	    "j+Shift+Alt",	    KeyFocusToPointer,
	ACTION_NONE,		    KD_FULL
}, {
	"NextApp",		    "n+Alt",			KeyNextApp,
	ACTION_NONE,		    KD_FULL
}, {
	"PreviousApp",		    "N+Alt",			KeyPrevApp,
	ACTION_NONE,		    KD_FULL
}, {
	"ToggleInput",		    "t+Alt",			KeyToggleInput,
	ACTION_NONE,		    KD_FULL
}, {
	"NextWindow",		    "w+Alt",			KeyNextWindow,
	ACTION_NONE,		    KD_FULL
}, {
	"PreviousWindow",	    "W+Alt",			KeyPrevWindow,
	ACTION_NONE,		    KD_FULL
}, {
	"TogglePin",		    "Insert+Meta",		KeyTogglePin,
	ACTION_TOGGLE_PIN,	    KD_FULL
}, {
	"SuspendMouseless",	    "z+Alt",			keySuspend,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"ResumeMouseless",	    "Z+Alt",			keyResume,
	ACTION_NONE,		    KD_IMMUNE | KD_BASIC_FULL
}, {
	"QuoteNextKey",		    "q+Alt",			keyQuoteNext,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"Refresh",		    "F8+Alt",			KeyRefresh,
	ACTION_NONE,		    KD_FULL
}, {
	"Back",			    "F5+Alt",			KeyBackFocus,
	ACTION_NONE,		    KD_FULL
}, {
	"OpenClose",		    "F2+Alt",		    KeyOpenCloseFocus,
	ACTION_NONE,		    KD_FULL
}, {
	"FullRestore",		    "F3+Alt",			KeyFullRestore,
	ACTION_NONE,		    KD_FULL
}, {
	"Quit",			    "F9+Alt",			KeyQuit,
	ACTION_NONE,		    KD_FULL
}, {
	"Owner",		    "F10+Alt",			KeyOwner,
	ACTION_NONE,		    KD_FULL
}, {
	"WorkspaceMenu",	    "M+Alt",		    KeyWorkspaceMenu,
	ACTION_NONE,		    KD_FULL
}, {
	"WindowMenu",		    "m+Alt",			KeyWindowMenu,
	ACTION_NONE,		    KD_FULL
}, {
	"Move",			    "F6+Alt",			KeyMove,
	ACTION_NONE,		    KD_FULL
}, {
	"Resize",		    "F7+Alt",			KeyResize,
	ACTION_NONE,		    KD_FULL
}, {
	"Properties",		    "F4+Alt",			KeyProperties,
	ACTION_NONE,		    KD_FULL
}, {
	"OpenClosePointer",	    "L7",		  KeyOpenClosePointer,
	ACTION_NONE,		    KD_ALWAYS
}, {
	"RaiseLower",		    "L5",		 KeyRaiseLowerPointer,
	ACTION_NONE,		    KD_ALWAYS
},
};

#define NUMKEYDESCRIPTORS (sizeof(KeyDescriptorTable)/sizeof(KeyDescriptor))


typedef struct {
    unsigned int modstate;
    KeyCode keycode;
    KeyDescriptor *desc;
} KeyBinding;


typedef struct {
    KeySym sym;
    unsigned int mod;
} modsym;


#define KEYBINDING_TABLE_SIZE 60
#define KEYBINDING_TABLE_INCR 20

static KeyBinding *KeyBindingTable = NULL;
static KeyBinding *activeKey = NULL;
static int bindingTableCount = 0;
static int bindingTableSize = 0;
static Bool suspended = False;
static Bool quotenext = False;


/*
 * Suspension and resumption of Mouseless functions.
 */
static void
keySuspend(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    suspended = True;
}


static void
keyResume(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    suspended = False;
}


Bool
IsMouselessSuspended()
{
    return suspended;
}

static void
keyQuoteNext(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    /*
     * Turn on quotenext on the release.  If we set it on the press,
     * the subsequent release would turn it off!
     */
    if (ke->type != KeyRelease)
	return;
    quotenext = True;
}


/*
 * Add a binding to the key binding table.
 */
static void
addBinding(kc, mod, desc)
    KeyCode kc;
    unsigned int mod;
    KeyDescriptor *desc;
{
    KeyBinding *b;

    if (bindingTableCount == bindingTableSize) {
	bindingTableSize += KEYBINDING_TABLE_INCR;
	KeyBindingTable = MemRealloc(KeyBindingTable,
				     bindingTableSize*sizeof(KeyBinding));
    }

    b = &KeyBindingTable[bindingTableCount];
    b->keycode = kc;
    b->modstate = mod;
    b->desc = desc;
    ++bindingTableCount;
}


/*
 * Keysym aliasing.  Provides aliases for modifier keysyms.  Allows an alias 
 * to represent a mask or to be a synonym for up to two keysyms.  The keysyms 
 * are only looked at if the mask value is zero.
 */

typedef struct {
    char *alias;
    unsigned int mask;
    KeySym sym1, sym2;
} KeysymAlias;

static KeysymAlias KeysymAliasTable[] = {
   /* alias	    mask	    sym1	    sym2 */
    { "Any",	    AnyModifier,    0,		    0 },
    { "Shift",	    ShiftMask,	    0,		    0 },
    { "Lock",	    LockMask,	    0,		    0 },
    { "Control",    ControlMask,    0,		    0 },
    { "Ctrl",	    ControlMask,    0,		    0 },
    { "Ctl",	    ControlMask,    0,		    0 },
    { "Meta",	    0,		    XK_Meta_L,	    XK_Meta_R },
    { "Alt",	    0,		    XK_Alt_L,	    XK_Alt_R },
    { "Super",	    0,		    XK_Super_L,	    XK_Super_R },
    { "Hyper",	    0,		    XK_Hyper_L,	    XK_Hyper_R }
};
#define NUMALIASES (sizeof(KeysymAliasTable)/sizeof(KeysymAlias))


/*
 * Takes a word and presumes that it names a keysym.  Looks up this keysym in
 * the modifier mapping table and returns the corresponding modifier mask.  If
 * the string doesn't name a valid keysym, returns 0.  If the keysym is not a
 * modifier, returns 0.  If the word is "Any", returns AnyModifier.  Several
 * aliases are supported for well-known modifiers, e.g. "Meta" for "Meta_L" or
 * "Meta_R".  REMIND: If a keysym is on several keys, and only some of the
 * keys are modifiers, this function may fail to find the modifier mask.
 */
unsigned int
stringToModifier(dpy, word)
    Display *dpy;
    char *word;
{
    KeySym ks;
    KeyCode kc;
    KeysymAlias *ksa;
    int modnum;

    ks = XStringToKeysym(word);

    if (ks != NoSymbol) {
	kc = XKeysymToKeycode(dpy, ks);

	if (kc == 0)
	    return 0;
	else
	    return FindModifierMask(kc);
    }

    /*
     * It's not a valid keysym name, so try a bunch of aliases.  First,
     * Allow "mod1" ... "mod5" as synonyms for Mod1Mask ... Mod5Mask.
     */

    if (1 == sscanf(word, "mod%d", &modnum) &&
	modnum >= 1 && modnum <= 5)
    {
	return (1 << (Mod1MapIndex + modnum - 1));
    }

    /* look through the alias table for masks or keysyms */

    kc = 0;
    for (ksa = KeysymAliasTable; ksa < KeysymAliasTable + NUMALIASES;
	 ++ksa)
    {
	if (0 == strcmp(word, ksa->alias)) {
	    if (ksa->mask != 0)
		return ksa->mask;

	    kc = XKeysymToKeycode(dpy, ksa->sym1);

	    if (kc == 0)
		kc = XKeysymToKeycode(dpy, ksa->sym2);

	    if (kc == 0)
		return 0;
	    else
		return FindModifierMask(kc);
	}
    }

    return 0;
}


/*
 * Parses a comma-separated string into words, gets the modifier mask for 
 * each, ORs them together and returns the result.
 */
unsigned int
polyStringToModifier(dpy, str)
    Display *dpy;
    char *str;
{
    char buf[200];
    unsigned int result = 0;
    char *word;

    /* make a copy first, because strtok riddles the string with nulls. */
    strcpy(buf, str);
    word = strtok(buf, ",");
    while (word != NULL) {
	result |= stringToModifier(dpy, word);
	word = strtok(NULL, ",");
    }
    return result;
}


#define MAX_MAPPINGS 6

/*
 * Parses a key specification of the form
 *	    keymod[,keymod[,...]]
 * where keymod is
 *	    keysym[+modifier[+...]]
 */
static int
parseKeySpec(dpy, specifier, syms)
    char *specifier;
    modsym *syms;
{
    char spec[200];
    char *mapping[MAX_MAPPINGS];
    char *keysym_string, *mod_string;
    KeySym keysym;
    int k;
    int modmask, newmod;
    int nbound = 0;
    char buffer[200];

    /* make a copy first, because strtok riddles the string with nulls. */
    strcpy(spec, specifier);

    /* break apart comma-separated mappings */
    mapping[0] = strtok(spec, ",");
    for (k = 1; k < MAX_MAPPINGS; ++k) {
	mapping[k] = strtok(NULL, ",");
	if (mapping[k] == NULL)
	    break;
    }

    /* for each mapping, break into keysym and modifier components */
    for (k = 0; k < MAX_MAPPINGS && mapping[k]; ++k) {
	keysym_string = strtok(mapping[k], "+");
	if (!keysym_string) {
	    (void) sprintf(buffer, gettext("bad key mapping `%s'\n"),
			  mapping[k]);
	    ErrorWarning(buffer);
	    continue;
	}
	keysym = XStringToKeysym(keysym_string);
	if (keysym == NoSymbol) {
	    (void) sprintf(buffer, gettext("can't find keysym for `%s'\n"),
			   keysym_string);
	    ErrorWarning(buffer);
	    continue;
	}

	/*
	 * If the keysym is upper case alphabetic, add a shift mask.  If it's 
	 * lower case, convert it to upper case.
	 */
	modmask = 0;
	if (XK_A <= keysym && keysym <= XK_Z)
	    modmask = ShiftMask;
	if (XK_a <= keysym && keysym <= XK_z)
	    keysym -= (XK_a - XK_A);

	while (1) {
	    mod_string = strtok(NULL, "+");
	    if (!mod_string)
		break;
	    newmod = stringToModifier(dpy, mod_string);
	    if (newmod == 0) {
		/*
		 * We couldn't find a modifier; ignore this binding.  We can't 
		 * use continue, because we want to continue an outer loop.
		 */
		goto nobinding;
	    }
	    modmask |= newmod;
	}
	syms[nbound].sym = keysym;
	syms[nbound].mod = modmask;
	++nbound;
    nobinding: ;
    }  /* for each mapping */
    return nbound;
}


/*
 * Run through the table of key descriptors and establish key bindings for
 * each descriptor.  First, the resource database is probed for a customized
 * key binding specification.  If one isn't found, the default key binding
 * specification is used.  Then, this specification is parsed into an array of
 * keysym/modifier pairs.  For each pair, the keyboard mapping table is
 * searched for the keysym and an entry is made into the binding table for
 * each instance of the keysym in the mapping table.  Thus, if a keysym
 * appears on more than one keystation, a key binding entry is created for
 * each.
 *
 * If a resource is found, it is always used.  If no resource is found, the
 * default specification is used only if the current mouseless setting has
 * this binding enabled.  So, setting mouseless to Basic or SunView will 
 * disable most key bindings.  However, if you add a specific resource, it 
 * will always be honored.
 */
static void
establishKeyBindings(dpy)
    Display *dpy;
{
    KeyDescriptor *d;
    KeyBinding *kb;
    modsym syms[MAX_MAPPINGS];
    int nsyms;
    int i, j;
    int keytblsize = (MaxKeyCode-MinKeyCode+1) * KeySymsPerKeyCode;
    XrmQuark classlist[4], namelist[4];
    XrmQuark rep;
    XrmValue value;
    char *keyspec;
    unsigned long mask;

    classlist[0] = OpenWinQ;
    classlist[1] = kbdCmdClassQ;
    classlist[3] = NULLQUARK;

    namelist[0] = TopInstanceQ;
    namelist[1] = kbdCmdInstanceQ;
    namelist[3] = NULLQUARK;

    mask = mouselessMaskTable[GRV.Mouseless];

    for (d=KeyDescriptorTable; d < KeyDescriptorTable+NUMKEYDESCRIPTORS;
	    ++d) {
	classlist[2] = namelist[2] = XrmStringToQuark(d->rsrc_name);
	if (XrmQGetResource(OlwmDB, namelist, classlist, &rep, &value)) {
	    keyspec = (char *) value.addr;
	} else {
	    if (mask & d->flags)
		keyspec = d->dflt_binding;
	    else
		continue;
	}
	nsyms = parseKeySpec(dpy, keyspec, syms);
	for (i=0; i<nsyms; ++i) {
	    for (j=0; j<keytblsize; ++j) {
		if (KbdMap[j] == syms[i].sym) {
		    addBinding(j/KeySymsPerKeyCode+MinKeyCode, syms[i].mod, d);
		    j += KeySymsPerKeyCode - (j % KeySymsPerKeyCode) - 1;
		}
	    }
	}
    }
}
	

/*
 * Issue or release passive grabs for the necessary keys on this particular
 * root window.  Run through the binding table and un/grab the key-modifier
 * combination itself, and also combined with the Lock and NumLock (if any)
 * modifier.  There thus may be up to four actual grabs per key binding.
 */
static void
grabRootKeys(dpy, root, grab)
    Display *dpy;
    Window root;
    Bool grab;		/* True = grab, False = release */
{
    KeyBinding *k;
    unsigned int NumLockMask =
	FindModifierMask(XKeysymToKeycode(dpy, XK_Num_Lock));

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->desc->function != NULLFUNC) {
	    if (grab) {
		XGrabKey(dpy, k->keycode, k->modstate,
			 root, False, GrabModeAsync, GrabModeSync);
		XGrabKey(dpy, k->keycode, k->modstate | LockMask,
			 root, False, GrabModeAsync, GrabModeSync);
	    } else {
		XUngrabKey(dpy, k->keycode, k->modstate, root);
		XUngrabKey(dpy, k->keycode, k->modstate|LockMask, root);
	    }
	    if (NumLockMask != 0) {
		if (grab) {
		    XGrabKey(dpy, k->keycode, k->modstate | NumLockMask,
			     root, False, GrabModeAsync, GrabModeSync);
		    XGrabKey(dpy, k->keycode,
			     k->modstate | NumLockMask | LockMask,
			     root, False, GrabModeAsync, GrabModeSync);
		} else {
		    XUngrabKey(dpy, k->keycode, k->modstate | NumLockMask,
			       root);
		    XUngrabKey(dpy, k->keycode,
			       k->modstate | NumLockMask | LockMask, root);
		}
	    }
	}
    }
}


/*
 * Issue or release passive button grabs on this root window.  Like
 * grabRootKeys, has to deal with Lock and NumLock by issuing up to four
 * separate grabs.  Note: these are synchronous grabs.  This relies on the
 * root event handler to issue an AllowEvents or GrabPointer request.
 */
static void
grabRootButtons(dpy, root, grab)
    Display *dpy;
    Window root;
    Bool grab;		/* True = grab, False = release */
{
    unsigned int NumLockMask =
	FindModifierMask(XKeysymToKeycode(dpy, XK_Num_Lock));
    unsigned int eventmask =
	ButtonPressMask | ButtonMotionMask | ButtonReleaseMask;

    if (ModMaskMap[MOD_WMGRAB] != 0) {
	if (grab) {
	    XGrabButton(dpy, AnyButton,
			ModMaskMap[MOD_WMGRAB],
			root, False, eventmask,
			GrabModeSync, GrabModeSync, None, None);
	    XGrabButton(dpy, AnyButton,
			ModMaskMap[MOD_WMGRAB] | LockMask,
			root, False, eventmask,
			GrabModeSync, GrabModeSync, None, None);
	} else {
	    XUngrabButton(dpy, AnyButton,
			  ModMaskMap[MOD_WMGRAB],
			  root);
	    XUngrabButton(dpy, AnyButton,
			  ModMaskMap[MOD_WMGRAB] | LockMask,
			  root);
	}

	if (NumLockMask != 0) {
	    if (grab) {
		XGrabButton(dpy, AnyButton,
			    ModMaskMap[MOD_WMGRAB] | NumLockMask,
			    root, False, eventmask,
			    GrabModeSync, GrabModeSync, None, None);
		XGrabButton(dpy, AnyButton,
			    ModMaskMap[MOD_WMGRAB] | LockMask | NumLockMask,
			    root, False, eventmask,
			    GrabModeSync, GrabModeSync, None, None);
	    } else {
		XUngrabButton(dpy, AnyButton,
			      ModMaskMap[MOD_WMGRAB] | NumLockMask,
			      root);
		XUngrabButton(dpy, AnyButton,
			      ModMaskMap[MOD_WMGRAB] | LockMask | NumLockMask,
			      root);
	    }
	}
    }
}


/* ===== public functions ================================================= */


/*
 * Given a keyboard event, looks it up in the keyboard binding table.  If a 
 * binding is found, returns the semantic action associated with that key.  If 
 * no binding is found, returns ACTION_NONE.
 */
SemanticAction
FindKeyboardAction(dpy, event)
    Display *dpy;
    XEvent *event;
{
    KeyBinding *k;
    SemanticAction a = ACTION_NONE;
    unsigned long ignore = ModMaskMap[MOD_IGNORE] | AnyButtonMask;

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->keycode == event->xkey.keycode
		&& k->modstate == (event->xkey.state & ~ignore)) {
	    a = k->desc->action;
	    break;
	}
    }
    return a;
}


/*
 * Given a keyboard event, looks it up in the keyboard binding table.
 * If a binding is found, executes the function bound to that key.  Returns 
 * True if a function was found and called, otherwise False.
 */
Bool
ExecuteKeyboardFunction(dpy, event)
    Display *dpy;
    XEvent *event;
{
    KeyBinding *k;
    void (*f)() = NULLFUNC;
    unsigned long ignore = ModMaskMap[MOD_IGNORE] | AnyButtonMask;

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->keycode == event->xkey.keycode
		&& k->modstate == (event->xkey.state & ~ignore)
		&& k->desc->function != NULLFUNC) {
	    f = k->desc->function;
	    break;
	}
    }

    /* If the user pressed the STOP key, clear active key. */

    if (f == NULLFUNC) {
	if (FindKeyboardAction(dpy, event) == ACTION_STOP)
	    activeKey = NULL;
	XAllowEvents(dpy, AsyncKeyboard, event->xkey.time);
	return False;
    }

    /* invariant: k points to a valid key binding */

#ifdef notdef
    /*
     * On the first keypress, stash the active key binding; ignore subsequent
     * keypresses.  Ignore all key releases except the one corresponding to
     * the active binding.
     */
    if (event->type == KeyPress) {
	if (activeKey == NULL)
	    activeKey = k;
	else
	    return False;
    } else {			    /* KeyRelease */
	if (k == activeKey)
	    activeKey = NULL;
	else
	    return False;	    /* ignore it */
    }
#endif

    if (suspended && !(k->desc->flags & KD_IMMUNE)) {
	XAllowEvents(dpy, ReplayKeyboard, event->xkey.time);
	return True;
    }

    if (quotenext) {
	XAllowEvents(dpy, ReplayKeyboard, event->xkey.time);
	quotenext = False;
	return True;
    }

    if (event->type == KeyPress)
	XAllowEvents(dpy, AsyncKeyboard, event->xkey.time);

    (*f)(dpy, event);
    return True;
}


/* ===== Initialization =================================================== */

/*
 * Deal with key grabs on all root windows.  If grab = True, grab the keys; if 
 * grab = False, release the keys.  Note: the screens and the keyboard binding 
 * information must be initialized prior to calling this function.
 */
void
GrabKeys(dpy, grab)
    Display *dpy;
    Bool grab;
{
    List *l = ScreenInfoList;
    ScreenInfo *scr;

    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l))
	grabRootKeys(dpy, scr->rootid, grab);
}


/*
 * Remove all key grabs, zero out the binding table, and rebuild it from the 
 * resource database.  Then, re-establish key grabs.
 */
void
RefreshKeyGrabs(dpy)
    Display *dpy;
{
    GrabKeys(dpy, False);
    bindingTableCount = 0;
    establishKeyBindings(dpy);
    GrabKeys(dpy, True);
}


/*
 * Deal with button grabs on all root windows.  If grab = True, grab the 
 * buttons; if grab = False, release them.
 */
void
GrabButtons(dpy, grab)
    Display *dpy;
    Bool grab;
{
    List *l = ScreenInfoList;
    ScreenInfo *scr;

    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l))
	grabRootButtons(dpy, scr->rootid, grab);
}


/*
 * Remove all button grabs, regenerate the modifier mask table, and
 * re-establish the button grabs.
 */
void
RefreshButtonGrabs(dpy)
    Display *dpy;
{
    GrabButtons(dpy, False);
    establishModBindings(dpy, OlwmDB);
    GrabButtons(dpy, True);
}


/*
 * Update all bindings from a new resource database.  Called whenever the 
 * resource database changes.  
 */
void
UpdateBindings(dpy, newDB)
    Display *dpy;
    XrmDatabase	newDB;
{
    KeyDescriptor *d;
    XrmQuark classlist[4], instlist[4];
    XrmQuark rep;
    XrmValue newvalue, oldvalue;
    Bool regrab = False;
    static XrmBinding bindings[] =
	{ XrmBindTightly, XrmBindTightly, XrmBindTightly };

    GrabButtons(dpy, False);
    establishModBindings(dpy, newDB);
    GrabButtons(dpy, True);

    /* run through the KeyDescriptorTable and probe resources */

    classlist[0] = OpenWinQ;
    classlist[1] = kbdCmdClassQ;
    classlist[3] = NULLQUARK;

    instlist[0] = TopInstanceQ;
    instlist[1] = kbdCmdInstanceQ;
    instlist[3] = NULLQUARK;

    for (d=KeyDescriptorTable; d < KeyDescriptorTable+NUMKEYDESCRIPTORS;
	    ++d) {

	classlist[2] = instlist[2] = XrmStringToQuark(d->rsrc_name);

	if (!XrmQGetResource(newDB, instlist, classlist, &rep, &newvalue))
	    continue;

	if (XrmQGetResource(OlwmDB, instlist, classlist, &rep, &oldvalue) &&
	    0 == strcmp((char *) newvalue.addr, (char *) oldvalue.addr))
	{
	    /* old and new values the same; ignore */
	    continue;
	}

	regrab = True;
	XrmQPutStringResource(&OlwmDB, bindings, instlist,
			      (char *) newvalue.addr);
    }

    if (regrab)
	RefreshKeyGrabs(dpy);
}


/*
 * Initialize the event handling system, but don't do any key grabbing.  This 
 * function is called exactly *once* at startup.
 */
void
InitBindings(dpy)
    Display *dpy;
{
    kbdCmdInstanceQ = XrmStringToQuark("keyboardCommand");
    kbdCmdClassQ    = XrmStringToQuark("KeyboardCommand");

    modInstanceQ    = XrmStringToQuark("modifier");
    modClassQ	    = XrmStringToQuark("Modifier");

    KeyBindingTable = MemCalloc(KEYBINDING_TABLE_SIZE,sizeof(KeyBinding));
    bindingTableSize = KEYBINDING_TABLE_SIZE;
    establishKeyBindings(dpy);
    establishModBindings(dpy, OlwmDB);
}
