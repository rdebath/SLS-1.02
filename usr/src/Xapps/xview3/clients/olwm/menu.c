/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)menu.c	26.52	91/09/14 SMI"

/*
 * This file contains all of the functions for creating and displaying menus.
 *
 * Global Functions:
 * InitMenus	-- initialize menu stuff
 * MenuCreate	-- create a new menu
 * MenuDestroy	-- destroy an existing menu
 * MenuShow	-- display a menu
 */

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <memory.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#define XK_MISCELLANY
#include <X11/keysymdef.h>
#include <olgx/olgx.h>
#include <assert.h>
#include <string.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "events.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "globals.h"

extern unsigned int FindModifierMask();

/* Locals */
static XEvent lastPress;
static int  lastX, lastY, minX;
static WinGeneric *prevColorFocusWindow = NULL;
static MenuTrackMode menuTrackMode;

/*
 * Table of currently active menus.
 * REMIND: perhaps this should be dynamically allocated.
 */
#define MAX_ACTIVE_MENUS	20	/* We hope, more than enough. */
static MenuInfo *menuInfoTable[MAX_ACTIVE_MENUS];
static int  topMenu = 0;	/* Next free menuInfoTable slot. */

/* Calculate fontheight from font info structure. */
#define FONT_HEIGHT(f)		((f)->ascent + (f)->descent)
#define BUTT_FONTHEIGHT		FONT_HEIGHT(GRV.ButtonFontInfo)
#define BUTT_FONTASCENT		(GRV.ButtonFontInfo->ascent)
#define TITLE_FONTASCENT	(GRV.TitleFontInfo->ascent)

#define PUSHPINHEIGHT(g)	(PushPinOut_Height(g) + 4)

/* Label positioning. */
#define TEXT_HEIGHT		FONT_HEIGHT(GRV.ButtonFontInfo)

/* Space between buttons (these should be adjusted for resolution). */
#define BUTT_VSPACE	0	/* There used to be space between buttons. */
#define BUTT_HSPACE	5	/* Space betw button/menumark &
				 * pushpin/title/box */

/* Space above and below the titlebar text, and the space below the
 * last button in the menu.
 */
#define HEAD_VSPACE	4

/*
 * Space for LocCursor
 */
#define LOC_CURSOR_SIZE 6

/* The size of the menu mark is dependant on the font height. */
#define MENUMARK_SIZE 6

#define MENU_HORIZ_OFFSET 3


/*
 * globals
 */
Bool flDoSetDefault;

/********************************************************************************/
static void calcmenusize();
static void recalcCachedMenu();

static void (*syncFunc) ();
static void *syncInfo;
static MenuInfo *findMenuInfo();
static void menuInfoDestroy();
static ButtonInfo *buttonInfoCreate();
static void showMenu();
static Bool menuHandleUpDownMotion();
static Bool menuHandlePress();
static void menuHandleMotion();
static Bool menuHandleRelease();
static MenuInfo *menuSearch();
static MenuLocation checkMenuEvent();
static int  menuHide();
static void unmapChildren();
static void activateButton();
static void setMenuPin();
static void activateSubMenu();
static void drawButton();
static void drawRevButton();
static Bool isClick();

int MenuTrack();
void DrawLocCursor();

#ifdef notdef
/* Defaulting stuff */
extern DefaultsP DefaultsPtr;	/* defined in usermenu.c */



/* REMIND:
 * the resource stuff for saving defaults is not implemented;
 */

void
UpdDefaultPtr(mInfo, index)
    MenuInfo   *mInfo;
    int         index;
{
    DefaultsP   curr = DefaultsPtr;

    while (curr) {
	if (curr->mInfo == mInfo)
	    break;
	curr = curr->next;
    }

    if (curr)
	curr->DefaultButton = index;
}

XrmDatabase
CreateDB()
{
    char       *path;
    char        filename[80];
    extern char *getenv();
    extern XrmDatabase XrmGetFileDataBase();

    if ((path = getenv("OLWMRC")) != NULL)
	return XrmGetFileDatabase(path);

    path = getenv("HOME");
    strcpy(filename, path ? path : "");
    strcat(filename, "/.olwmrc");

    return XrmGetFileDatabase(filename);
}

static void
makeRMName_class(name, class, Name)
    char       *name, *class, *Name;
{
    int         len = strlen(Name);
    int         i, j;

    strcpy(name, "Olwm*Menu.");
    for (i = 0, j = strlen(name); i < len; i++)
	if (isalpha(Name[i]))
	    name[j++] = Name[i];
    name[j] = '\0';
    strcat(name, ".DefaultButton");
    strcpy(class, name);
}

FillDefaultsList(defaultsDB, DefaultsPtr)
    XrmDatabase defaultsDB;
    DefaultsP   DefaultsPtr;
{
    DefaultsP   curr = DefaultsPtr;
    char       *Stype;
    char        name[80];
    char        class[80];
    XrmValue    val;

    while (curr) {
	makeRMName_class(name, class, curr->Name);
	if (XrmGetResource(defaultsDB, name, class, &Stype, &val))
	    curr->DefaultButton = atoi(val.addr);
	curr = curr->next;
    }
}

static int
ApplyDefaults(DefPtr, mInfo)
    DefaultsP   DefPtr;
    MenuInfo   *mInfo;
{
    int         i;
    char *mtit = menuTitle(mInfo);

    if (mtit != NULL && (strcmp(mtit, DefPtr->Name) == 0)) {
	DefPtr->mInfo = mInfo;
	if (DefPtr->DefaultButton < mInfo->menu->buttonCount) {
	    mInfo->menu->buttonDefault = DefPtr->DefaultButton;
	}
	return 1;
    } else {
	for (i = 0; i < mInfo->menu->buttonCount; i++) {
	    Button *pb = mInfo->buttons[i].button;

	    if (pb == NULL) /*button is not visible; it has no entry*/
		continue;

	    if (pb->stacked) {
		if (strcmp(pb->label[pb->which], DefPtr->Name) == 0) {
		    DefPtr->mInfo = mInfo->buttons[i].subMenu;
		    if (DefPtr->DefaultButton < mInfo->buttons[i].subMenu->menu->buttonCount)
			mInfo->buttons[i].subMenu->menu->buttonDefault = DefPtr->DefaultButton;
		    return 1;
		}
	    }
	}
	return 0;
    }
}

/*
 * ApplyMenuDefaults -
 *          read $HOME/.olwmrc or the file specified in OLWMRC env variable.
 * This file has entries in Xrm format. DefaultsPtr points to a list of node,
 * where each node contains the name of a button to which a menu corresponds
 * and a buttonIndex in the menu which should be the default button for that
 * menu. When ApplyMenuDefaults() is called, the buttonIndex field is 0, and
 * the name field contains all buttons which have a menu associated with them
 * These include Window Menu and the WorkSpace Menu.
 */

void
ApplyMenuDefaults(dpy, menuCache)
    Display    *dpy;
    MenuCache  *menuCache;
{
    XrmDatabase defaultsDB;
    MenuInfo   *mInfo;
    DefaultsP   curr;
    int         i;

    defaultsDB = CreateDB();

    if (defaultsDB == NULL)
	return;

    FillDefaultsList(defaultsDB, DefaultsPtr);

    for (curr = DefaultsPtr; curr; curr = curr->next) {
	for (i = 0; i < menuCache->nextSlot; i++) {
	    mInfo = menuCache->menuInfoList[i];
	    if (ApplyDefaults(curr, mInfo))
		break;
	}
    }
}

/*
 * Save defaults for next invocation of wm. Should be called from cleanup()
 * functions when exiting wm.
 */
SaveMenuDefaults()
{
    char       *path;
    char        filename[80];
    FILE       *fd;
    extern char *getenv();
    DefaultsP   curr, temp;
    char        name[80], class[80];
    /* open a file for writing, overwriting existing file */

    if ((path = getenv("OLWMRC")) != NULL) {
	if ((fd = fopen(path, "w")) == NULL) {
	    printf("Cannot create %s\n", path);
	    return;
	}
    } else {
	path = getenv("HOME");
	strcpy(filename, path ? path : "");
	strcat(filename, "/.olwmrc");

	if ((fd = fopen(filename, "w")) == NULL) {
	    printf("Cannot create %s\n", path);
	    return;
	}
    }

    curr = DefaultsPtr;
    while (curr) {
	makeRMName_class(name, class, curr->Name);
	temp = curr;
	curr = curr->next;
	free(temp);
    }
}

#endif


/*
 * whether the menu has a title or not is dependant on
 * if it's pinned
 */
static char *
menuTitle(mInfo)
    MenuInfo *mInfo;
{
    if (mInfo->origmenuInfo != NULL)
	return NULL;
    return mInfo->menu->title;
}

/*
 * SOMETIMES, it is desirable to know when the menu
 * has changed the tracking mode (from click to selecto to drag to
 * select (or rather, vice versa)
 * so that buttons can busify themselves.
 * so ShowStandardMenuSync() will use these to that
 * effect
 */
void (*clickProc)();
void *clickData;

void
SetClickMode(flclick)
    Bool flclick;
{
    menuTrackMode = flclick? MODE_CLICK : MODE_DRAG;
    if (clickProc)
	(*clickProc)(menuTrackMode, clickData);
}

void
SetClickCallback(proc, data)
    void (*proc)();
    void  *data;
{
    clickProc = proc;
    clickData = data;
}

/*
 * ExecButtonAction
 *
 * Given a menu and a button, find the button's action (by searching down the
 * menu tree following defaults, if necessary) and execute it.
 */
void
ExecButtonAction(dpy, winInfo, menuInfo, btn)
    Display    *dpy;
    WinGeneric *winInfo;
    MenuInfo   *menuInfo;
    int         btn;
{
    Menu       *menu = menuInfo->menu;

    if (btn > menu->buttonCount)
	return;

    /* search down the menu tree for defaults */
    while (btn != NOBUTTON) {
	if (btn == PINBUTTON) {
	    /* pin the menu */
	    (void) MakePinMenu(dpy, winInfo, menuInfo);
	    break;
	} else if (menu->buttons[btn]->stacked) {
	    /* this button has a submenu */
	    menu = menu->buttons[btn]->action.submenu;
	    btn = menu->buttonDefault;
	    menuInfo = findMenuInfo(winInfo, menu);
	} else {
	    /* this is a leaf button */
	    break;
	}
    }

    /* if not the same menu then find its corresponding menuinfo */
    if (menuInfo->menu != menu) {
	if ((menuInfo = findMenuInfo(winInfo, menu)) == NULL)
	    return;
    }
    if (BUTTON_INDEX_OK(menuInfo, btn))
	(*menu->buttons[btn]->action.callback) (dpy, winInfo, menuInfo, btn);
}

static void
drawMenuPushpin(dpy, menuInfo)
    Display *dpy;
    MenuInfo *menuInfo;
{
    WinGeneric *winInfo = menuInfo->menuWin;
    Window      win = winInfo->core.self;
    Menu       *menu = menuInfo->menu;
    GC          windowGC = WinGC(winInfo, WINDOW_GC);
    Graphics_info *gisNormal = WinGI(winInfo, NORMAL_GINFO);

    if (menu->hasPushPin) {
	int flags;
	/*
	 * If the menu is already displayed, draw the pushpin grayed out to
	 * indicate that it can't be pinned again.
	 */
	/*
	 * REMIND we have to manually erase the pushpin because OLGX is
	 * broken when it comes to erasing pushpins.
	 */
	XFillRectangle(dpy, win, windowGC,
		       menuInfo->pushPinX, menuInfo->pushPinY,
		       PushPinOut_Width(gisNormal),
		       PUSHPINHEIGHT(gisNormal));

	if (menuInfo->pinnedBrother != NULL)
	    flags = OLGX_PUSHPIN_OUT | OLGX_INACTIVE;
	else
	    if (menuInfo->ringedButton == PINBUTTON)
		flags = OLGX_PUSHPIN_OUT | OLGX_DEFAULT;
	    else
		flags = OLGX_PUSHPIN_OUT;
	
	olgx_draw_pushpin(gisNormal, win,
			  menuInfo->pushPinX,
			  menuInfo->pushPinY,
			  flags);
    }
}

/*
 * this only draws the menu buttons that need have their
 * icky flag set
 */
void
DrawMenuWithHints(dpy, mInfo)
    Display *dpy;
    MenuInfo *mInfo;
{
    Menu       *menu = mInfo->menu;
    int         bindex;

    /* Draw the buttons. */
    for (bindex = 0; bindex < menu->buttonCount; bindex++) {
	ButtonInfo *bi = &mInfo->buttons[bindex];

	if (! bi->flDirty)
	    continue;

	bi->flDirty = False;

	if (! bi->button->visible || bi->button->label[bi->button->which] == NULL)
	    continue;

	drawButton(dpy, mInfo, bindex, (bindex == menu->buttonDefault));
    }
}

void
SetMenuRedrawHints(dpy, ee, mInfo)
    Display *dpy;
    XExposeEvent *ee;
    MenuInfo *mInfo;
{
    Menu       *menu = mInfo->menu;
    int         bindex;

    static Region region = NULL;

    MakeExposeDamage(&region, ee);

    /* Draw the buttons. */
    for (bindex = 0; bindex < menu->buttonCount; bindex++) {
	ButtonInfo *bi = &mInfo->buttons[bindex];

	if (bi->flDirty)
	    continue;

	if (XRectInRegion(region, 
			  bi->buttonX,
			  bi->buttonY,
			  mInfo->maxbuttonWidth,
			  bi->buttonHeight + HEAD_VSPACE))
	    bi->flDirty = True;
    }
    
}


/*
 * Draw menu contents into menu->window.
 */
void
DrawMenu(dpy, mInfo)
    Display    *dpy;
    MenuInfo   *mInfo;
{
    WinGeneric *winInfo = mInfo->menuWin;
    Window      win = winInfo->core.self;
    Menu       *menu = mInfo->menu;
    GC          windowGC = WinGC(winInfo, WINDOW_GC);
    Graphics_info *gisNormal = WinGI(winInfo, NORMAL_GINFO);
    int         bindex;
    char       *mtit;

    /* Draw the basic menu background if this menu isn't pinned */
    if (mInfo->origmenuInfo != NULL || !winInfo->core.client->scrInfo->use3D) {
	XFillRectangle(dpy, win, windowGC, 0, 0,
		       mInfo->menuWidth, mInfo->menuHeight);
    }
    if (mInfo->origmenuInfo == NULL) {
	olgx_draw_box(gisNormal, win, 0, 0,
		      mInfo->menuWidth, mInfo->menuHeight,
		      OLGX_NORMAL, True);
    }
    /* Draw the menu title. */
    if ((mtit = menuTitle(mInfo)) != NULL) {
	drawMenuPushpin(dpy, mInfo);

	olgx_draw_text(gisNormal, win, mtit,
		       mInfo->titleX, mInfo->titleY, 0,
		       False, OLGX_NORMAL);

	olgx_draw_text_ledge(gisNormal, win,
			     BUTT_HSPACE, mInfo->titleHeight - 6,
			     mInfo->menuWidth - (BUTT_HSPACE * 2));
    }
    /* Draw the buttons. */
    for (bindex = 0; bindex < menu->buttonCount; bindex++) {
	ButtonInfo *bi = &mInfo->buttons[bindex];

	bi->flDirty = False;

	if (bi->button == NULL
	    || ! bi->button->visible 
	    || bi->button->label[bi->button->which] == NULL)
	    continue;

	drawButton(dpy, mInfo, bindex, bindex == menu->buttonDefault);
    }
}


/*
 * SetButton: when you want to talk about the default ring,
 * use flsetdefault=true
 */
void
SetButton(dpy, menuInfo, idx, highlight, flsetdefault)
    Display    *dpy;
    MenuInfo   *menuInfo;
    int         idx;
    Bool        highlight;
    Bool        flsetdefault;
{
    /*
     * If flsetdefault is on, draw the button with default ring if it is to
     * be high-lighted.
     */

    if (idx < NOBUTTON)
	return;

    if (flsetdefault) {
	if (highlight) {
	    if (idx != menuInfo->ringedButton) {
		if (menuInfo->ringedButton > NOBUTTON)
		    drawButton(dpy, menuInfo, menuInfo->ringedButton, False);
		if (idx > NOBUTTON)
		    drawButton(dpy, menuInfo, idx, True);
		menuInfo->ringedButton = idx;
	    }
	} else {
	    if (idx == menuInfo->ringedButton) {
		if (idx > NOBUTTON)
		    drawButton(dpy, menuInfo, idx, False);
		menuInfo->ringedButton = NOBUTTON;
	    }
	}
    } else {
	if (highlight) {
	    if (menuInfo->ringedButton == idx) {
		if (idx > NOBUTTON)
		    drawButton(dpy, menuInfo, idx, False);
		menuInfo->ringedButton = NOBUTTON;
	    }
	    if (idx > NOBUTTON)
		drawRevButton(dpy, menuInfo, idx);
	} else if (idx > NOBUTTON)
	    drawButton(dpy, menuInfo, idx, False);
    }
}

Bool
StartMenuGrabs(dpy, winInfo)
    Display *dpy;
    WinGeneric *winInfo;
{
    int grabstat;
    /*
     * Grab the server to prevent anybody from sullying the underlying windows
     * when the menu window is mapped, but only if we're allowed.
     */
    grabstat = XGrabPointer(dpy, WinRootID(winInfo),
			    True,
			    ButtonReleaseMask | ButtonMotionMask | ButtonPressMask,
			    GrabModeAsync, GrabModeAsync,
			    None,
			    GRV.MenuPointer,
			    CurrentTime);

    if (grabstat != GrabSuccess) {
	ErrorWarning(gettext("failed to grab pointer"));
	/*don't worry about sync function*/
	return False;
    }

    /* needed for previewing */
    grabstat = XGrabKeyboard(dpy, WinRootID(winInfo),
			     False,
			     GrabModeAsync, GrabModeAsync,
			     CurrentTime);

    if (grabstat != GrabSuccess)
	ErrorWarning(gettext("failed to grab keyboard"));
	
    if (GRV.ServerGrabs)
	XGrabServer(dpy);

   InstallInterposer(MenuTrack, winInfo);

    return True;
}

/*
 * MenuShow
 * MenuShowSync
 *
 * These functions are the main entry points into the menu tracking system.
 * MenuShow() grabs everything, sets up the event interposer, and returns.
 *
 * REMIND
 * MenuShowSync() sets up an additional callback that is called after the menu
 * action is completed.  This is necessary for the present implementation of
 * pinned menus, which need to have work done after the menu goes down, in
 * addition to the menu button action.  This interface should probably go away
 * once pinned menus are rearchitected.
 *
 * flbutton means that it came from a window menu button; so it should be placed
 * below that button.
 * flkbd means that it was a mouselessly invoked menu; it needs to be placed just 
 * to the right of the button (if any)
 */

void
MenuMakeFirst(mInfo, sfunc, sinfo)
    MenuInfo *mInfo;
    void (*sfunc)();
    void *sinfo;
{
    memset((char *) menuInfoTable, 0, sizeof(MenuInfo *) * MAX_ACTIVE_MENUS);

    topMenu = 0;
    menuInfoTable[topMenu++] = mInfo;

    syncFunc = sfunc;
    syncInfo = sinfo;
}

void
MenuShowSync(dpy, winInfo, menu, pevent, sfunc, sinfo, flkbd, flbutton)
    Display     *dpy;
    WinGeneric  *winInfo;
    Menu        *menu;
    XEvent      *pevent;
    void       (*sfunc) ();
    void        *sinfo;
    Bool         flkbd;
    Bool	 flbutton;
{
    MenuInfo   *menuInfo;
    MenuCache  *menuCache = winInfo->core.client->scrInfo->menuCache;
    Graphics_info *gisButton = WinGI(winInfo, BUTTON_GINFO);
    int         x, y;

    if (! StartMenuGrabs(dpy, winInfo))
	return;

    if (menu->menudirty) {
	/*some attribute of the menu has changed;
	  recalc it's size, and button layoyt*/
	menu->menudirty = False;
	recalcCachedMenu(winInfo, menu);
    }

    menuInfo = findMenuInfo(winInfo, menu);

    if (menuInfo == NULL)
	return;

    MenuMakeFirst(menuInfo, sfunc, sinfo);

    menuInfo->menuWin = (WinGeneric *) menuCache->menuWinList[0];

    if (prevColorFocusWindow == NULL && ColorFocusLocked(menuInfo->menuWin))
	prevColorFocusWindow = ColorFocusWindow(menuInfo->menuWin);

    InstallColormap(dpy, menuInfo->menuWin->core.client->scrInfo->rootwin);

    switch (pevent->type) {
      case ButtonPress:
      case ButtonRelease:
	lastX = minX = pevent->xbutton.x_root;

	if (flbutton) {
	    WinGeneric *deco = ((WinPaneFrame *) winInfo)->winDeco;

	    x = winInfo->core.x;
	    y = winInfo->core.y;

	    if (deco != NULL) { /*should always be the case*/
		x += deco->core.x;
		y += deco->core.y + deco->core.height + HEAD_VSPACE;
	    }
	} else {
	    x = pevent->xbutton.x_root - MENU_HORIZ_OFFSET;
	    y = pevent->xbutton.y_root - (Button_Height(gisButton) +
					  BUTT_VSPACE) / 2;
	}
	flDoSetDefault = (pevent->xbutton.state & ModMaskMap[MOD_SETDEFAULT]);
	break;
      case KeyPress:
      case KeyRelease:
	if (flkbd) {
	    extern Client *CurrentClient;

	    if (CurrentClient->wmState == IconicState) {
		x = CurrentClient->iconwin->core.x +
		    (CurrentClient->iconwin->core.width) / 2;
		y = CurrentClient->iconwin->core.y +
		    (CurrentClient->iconwin->core.height) / 2;
	    } else {
		WinGeneric *deco = ((WinPaneFrame *) winInfo)->winDeco;

		x = winInfo->core.x;
		y = winInfo->core.y;
		if (deco != NULL) {
		    x += deco->core.x + deco->core.width;
		    y += deco->core.y;
		}
	    }
	} else {
	    lastX = minX = pevent->xkey.x_root;

	    x = pevent->xkey.x_root - MENU_HORIZ_OFFSET;
	    y = pevent->xkey.y_root - (Button_Height(gisButton) +
				       BUTT_VSPACE) / 2;
	}
	flDoSetDefault = 
	    (FindModifierMask(pevent->xkey.keycode) == ModMaskMap[MOD_SETDEFAULT]);
	break;
      default:
	return;
    }

    /* Install the first menu */
    menuTrackMode = MODE_CLICK;
    lastPress = *pevent;

    showMenu(dpy, menuInfo, x, y, ! flbutton);
}


/*
 * MenuShow
 */
void
MenuShow(dpy, winInfo, menu, pevent)
    Display    *dpy;
    WinGeneric *winInfo;
    Menu       *menu;
    XEvent     *pevent;
{
    MenuShowSync(dpy, winInfo, menu, pevent, NULL, NULL, False, False);
}


/*
 * PointInRect	-- check if a point is inside a rectangle
 */
int
PointInRect(x, y, rx, ry, rw, rh)
    int         x, y, rx, ry, rw, rh;
{
    return (x >= rx && x < rx + rw) && (y >= ry && y < ry + rh);
}


/*
 * ===========================================================================
 */

/*
 * Local routines
 */

/*
 * findMaxDepth - returns max depth of all menuinfo's in a cache
 */
static int
findMaxDepth(menuCache)
    MenuCache  *menuCache;
{
    MenuInfo   *menuInfo;
    int         i, depth;
    int         nextSlot = menuCache->nextSlot;

    depth = 0;
    for (i = 0; i < nextSlot; i++) {
	menuInfo = menuCache->menuInfoList[i];
	depth = MAX(depth, menuInfo->depth);
    }
    return depth;
}


/*
 * menuInfoCreate -- Create the MenuInfo structure for this menu.
 *		     This is mainly sizing information that is screen specific.
 *		     This will traverse the entire menu/button/submenu tree
 *		     and created all needed structures.
 */
MenuInfo *
MenuInfoCreate(menuCache, winInfo, menu, depth)
    MenuCache  *menuCache;
    WinGeneric *winInfo;
    Menu       *menu;
    int         depth;
{
    MenuInfo   *menuInfo;

    if (menu == NULL)
	return (MenuInfo *) NULL;

    menuInfo = MemNew(MenuInfo);

    menuInfo->depth = depth;

    /* save a pointer to the original/global menu */
    menuInfo->menu = menu;

    calcmenusize(menuInfo, winInfo, menu);

    /* create info each button (which creates any needed submenus */
    menuInfo->buttons = buttonInfoCreate(menuCache, winInfo, menuInfo);

    /* zero out the rest of the fields */
    menuInfo->menuWin = (WinGeneric *) NULL;
    menuInfo->origmenuInfo = (MenuInfo *) NULL;
    menuInfo->pinnedBrother = NULL;

    /* insert it into the list */
    if (menuCache->nextSlot == menuCache->maxSlots - 1) {
	menuCache->maxSlots += 10;
	menuCache->menuInfoList = MemRealloc(menuCache->menuInfoList,
					     sizeof(MenuInfo) * menuCache->maxSlots);
    }
    menuCache->menuInfoList[menuCache->nextSlot++] = menuInfo;

    return menuInfo;
}


/*
 * Set the x,y position of the button excluding the
 * title height (since a pinned menu has no title).
 */
static void
calcbuttonpositions(wi, bi, mi)
    WinGeneric *wi;
    ButtonInfo *bi;
    MenuInfo *mi;
{
    Graphics_info *gisButton = WinGI(wi, BUTTON_GINFO);
    int i;
    int nextY = BUTT_VSPACE;
    int buttonheight = Button_Height(gisButton) + BUTT_VSPACE;

    for (i = 0; i < mi->menu->buttonCount; i++, bi++) {
	if (bi->button == NULL)
	    continue;

	if (bi->subMenu != NULL)
	    calcbuttonpositions(wi, bi->subMenu->buttons, bi->subMenu);

	/* set the core of the ButtonInfo */
	bi->buttonX = BUTT_HSPACE + LOC_CURSOR_SIZE;
	bi->buttonY = nextY;

	/* Move down to next button postion. */
	if (bi->button->label[bi->button->which] == NULL)
	    bi->buttonHeight = buttonheight / 2;
	else
	    bi->buttonHeight = buttonheight;

	nextY += bi->buttonHeight;
    }
}

/*
 * buttonInfoCreate - Create ButtonInfo's for each button in the menu
 *		      Create any submenus found in the buttons.
 */
static
ButtonInfo *
buttonInfoCreate(menuCache, winInfo, menuInfo)
    MenuCache  *menuCache;
    WinGeneric *winInfo;
    MenuInfo   *menuInfo;
{
    int		buttonCount = menuInfo->menu->buttonCount;
    int         bindex;
    ButtonInfo *buttonInfo;
    ButtonInfo *bInfo;

    buttonInfo = MemAlloc(buttonCount * sizeof(ButtonInfo));

    /* Init each ButtonInfo for each button */
    for (bindex = 0; bindex < buttonCount; bindex++) {
	Button *pb = menuInfo->menu->buttons[bindex];

	bInfo = &(buttonInfo[bindex]);

	/* erase everything; if we decide not to use this item (visible == False)
	 * then bInfo->buttons will be NULL; this must be checked before using!
	 */
	memset((void *) bInfo, 0, sizeof(ButtonInfo));

	if (! pb->visible)
	    continue;

	bInfo->button = pb;

	/* If this is a submenu then traverse it */
	if (bInfo->button->stacked && bInfo->button->action.submenu) {
	    bInfo->subMenu = MenuInfoCreate(menuCache, winInfo,
					    bInfo->button->action.submenu,
					    menuInfo->depth + 1);
	} else {
	    bInfo->subMenu = (MenuInfo *) NULL;
	}
    }

    calcbuttonpositions(winInfo, buttonInfo, menuInfo);

    return buttonInfo;
}

/*
 * menuInfoDestroy
 */
static void
menuInfoDestroy(menuInfo)
    MenuInfo   *menuInfo;
{
    if (menuInfo->buttons)
	MemFree(menuInfo->buttons);
    MemFree(menuInfo);
}

/*
 * findMenuInfo
 */
static MenuInfo *
findMenuInfo(winInfo, menu)
    WinGeneric *winInfo;
    Menu       *menu;
{
    MenuCache  *menuCache;
    MenuInfo   *new = (MenuInfo *) NULL;
    int         i;

    menuCache = winInfo->core.client->scrInfo->menuCache;

    for (i = 0; i < menuCache->nextSlot; i++) {
	if (menu == menuCache->menuInfoList[i]->menu) {
	    new = menuCache->menuInfoList[i];
	    break;
	}
    }

    if (new == NULL)
	return (MenuInfo *) NULL;

    new->childActive = False;
    new->pinIn = False;
    new->litButton = NOBUTTON;
    new->ringedButton = new->menu->buttonDefault;
    return new;
}

static void 
calcmenusize(menuInfo, winInfo, menu)
    MenuInfo *menuInfo;
    WinGeneric *winInfo;
    Menu       *menu;
{
    int         i;
    int         maxLabWidth;		/* Width of longest menu label */
    int         menWidth, menHeight;	/* Width and height of menu. */
    int         hasStacked = False;	/* True if there are submenus */
    Graphics_info *gisButton = WinGI(winInfo, BUTTON_GINFO);
    Graphics_info *gisNormal = WinGI(winInfo, NORMAL_GINFO);
    int buttonheight = Button_Height(gisButton) + BUTT_VSPACE;
    int heightofbuttons;

    /* Find longest menu entry and the height of the buttons*/
    heightofbuttons = 0;
    
    for (maxLabWidth = 0, i = 0; i < menu->buttonCount; i++) {
	Button *pb = menu->buttons[i];
	if (! pb->visible)
	    continue;

	if (pb->label[pb->which] == NULL)
	    heightofbuttons += (buttonheight / 2);
	else {
	    maxLabWidth = MAX(maxLabWidth,
			      XTextWidth(GRV.ButtonFontInfo,
					 pb->label[pb->which],
					 strlen(pb->label[pb->which])));
	    heightofbuttons += buttonheight;
	}
	if (pb->stacked)
	    hasStacked = True;

    }

    maxLabWidth += 2 * ButtonEndcap_Width(gisButton);

    /*
     * If any of the buttons have submenus, make space for the menu mark.
     */
    if (hasStacked)
	maxLabWidth += BUTT_HSPACE + MENUMARK_SIZE;

    /* Calculate title parameters. */
    /* it's ok to check title here because we know this is a new
     * (not existing) menu
     */
    if (menu->title != NULL) {
	menuInfo->titleWidth = XTextWidth(GRV.TitleFontInfo,
					  menu->title,
					  strlen(menu->title));
	/*
	 * the +4 is so that there be a tiny bit more space to draw the default
	 * ring around a pushpin.  It is also used in erasing the pushpin, below.
	 */
	menuInfo->titleHeight = HEAD_VSPACE 
	    + MAX(FONT_HEIGHT(GRV.TitleFontInfo), PUSHPINHEIGHT(gisNormal)) 
		+ HEAD_VSPACE + 4;

	if (menu->hasPushPin) {
	    menuInfo->titleX = BUTT_HSPACE + LOC_CURSOR_SIZE +
		PushPinOut_Width(gisNormal) +
		BUTT_HSPACE;
	    /* the +3 is so that the title will match 
	     * up with what the pinned menu looks like
	     */
	    menuInfo->titleY = HEAD_VSPACE + TITLE_FONTASCENT + 3;

	    menWidth = LOC_CURSOR_SIZE + BUTT_HSPACE +
		MAX(maxLabWidth, PushPinOut_Width(gisNormal) + BUTT_HSPACE + 
		    menuInfo->titleWidth) 
		+ BUTT_HSPACE + LOC_CURSOR_SIZE;
	} else {
	    menWidth = LOC_CURSOR_SIZE + BUTT_HSPACE +
		MAX(maxLabWidth, menuInfo->titleWidth) 
		    + BUTT_HSPACE + LOC_CURSOR_SIZE;

	    menuInfo->titleX = (menWidth / 2) -
		(menuInfo->titleWidth / 2);
	    menuInfo->titleY = HEAD_VSPACE + TITLE_FONTASCENT;
	}
	menuInfo->buttonOffset = menuInfo->titleHeight;
    } else {	/* no title */
	menWidth = LOC_CURSOR_SIZE + BUTT_HSPACE +
	    maxLabWidth + BUTT_HSPACE + LOC_CURSOR_SIZE;

	menuInfo->titleX = 0;
	menuInfo->titleY = 0;
	menuInfo->titleWidth = 0;
	menuInfo->titleHeight = 0;
	menuInfo->buttonOffset = HEAD_VSPACE;
    }
    menuInfo->notitleOffset = HEAD_VSPACE;

    /*
     * Menu height is the sum of the buttons, the title height if any, the
     * space above the first button, and the space below the last button.
     */
    menHeight = menuInfo->titleHeight + HEAD_VSPACE + heightofbuttons + HEAD_VSPACE;

    menuInfo->menuWidth = menWidth;
    menuInfo->menuHeight = menHeight;

    /*
     * Place the pushpin. Pushpin is centered vertically in case the font
     * height is smaller than the pushpin height.
     */
    menuInfo->pushPinX = BUTT_HSPACE + LOC_CURSOR_SIZE;
    menuInfo->pushPinY = (menuInfo->titleHeight -  PUSHPINHEIGHT(gisNormal)) / 2;

    /* all buttons have the same width, but the height can differ;
     * it stored in the button itself 
     * the buttons are centered in the menu.
     */
    menuInfo->maxbuttonWidth = menWidth - (BUTT_HSPACE + LOC_CURSOR_SIZE) * 2;
}

static void
updateButtonInfo(wi, mi)
    WinGeneric *wi;
    MenuInfo *mi;
{
    int i;
    ButtonInfo *bi;

    for (bi = mi->buttons, i = 0;  i < mi->menu->buttonCount;  i++, bi++) {
	if (bi->subMenu != NULL) {
	    updateButtonInfo(wi, bi->subMenu);
	} else if (bi->button) {
	    if (! mi->menu->buttons[i]->visible)
		bi->button = NULL;	/*not visible any more*/
	} else {
	    if (mi->menu->buttons[i]->visible)
		bi->button = mi->menu->buttons[i]; /*now visible*/
	}
    }
    calcbuttonpositions(wi, mi->buttons, mi);
}

static void
recalcCachedMenu(win, menu)
    WinGeneric *win;
    Menu *menu;
{
    MenuCache  *menuCache;
    int         i;

    menuCache = win->core.client->scrInfo->menuCache;

    for (i = 0; i < menuCache->nextSlot; i++) {
	MenuInfo *mInfo = menuCache->menuInfoList[i];

	if (menu == mInfo->menu) {
	    /* update the minfo's notion of buttons */
	    updateButtonInfo(win, mInfo);

	    calcmenusize(mInfo, win, menu);
	    break;
	}
    }
}

/*
 *	Assumes that the window menus will tkae up the first MENU_NONE slots
 */
int
DestroyWindowMenuInfo(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    int         i;

    for (i = 0; i < (int) MENU_NONE; i++) {
	menuInfoDestroy(scrInfo->menuCache->menuInfoList[i]);
    }
}

/*
 *	Assumes that Destroy called before Create.
 *	Assumes that root menu info starts in slot MENU_ROOT in the menu cache.
 */
int
CreateUserMenuInfo(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    int         i, maxDepth;

    scrInfo->menuCache->nextSlot = (int) MENU_ROOT;
    (void) MenuInfoCreate(scrInfo->menuCache, scrInfo->rootwin,
			  MenuTable[(int) MENU_ROOT], 1);

    maxDepth = findMaxDepth(scrInfo->menuCache);
    if (maxDepth > scrInfo->menuCache->maxDepth) {
	scrInfo->menuCache->menuWinList = 
	    MemRealloc(scrInfo->menuCache->menuWinList, maxDepth * sizeof(struct _winmenu *));

	for (i = scrInfo->menuCache->maxDepth; i < maxDepth; i++) {
	    scrInfo->menuCache->menuWinList[i] = MakeMenu(dpy, scrInfo->rootwin);
	}
	scrInfo->menuCache->maxDepth = maxDepth;
    }
}

/*
 *	Assumes that root menu info starts in slot MENU_ROOT in the menu cache.
 */
int
DestroyUserMenuInfo(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    int         i;

    for (i = (int) MENU_ROOT; i < scrInfo->menuCache->nextSlot; i++) {
	menuInfoDestroy(scrInfo->menuCache->menuInfoList[i]);
	scrInfo->menuCache->menuInfoList[i] = 0;
    }
}

/*
 * showMenu
 */
static void
showMenu(dpy, menuInfo, x, y, flusedefault)
    Display    *dpy;
    MenuInfo   *menuInfo;
    int         x, y;
    Bool	flusedefault;
{
    int         dpyWidth, dpyHeight;
    Menu       *menu = menuInfo->menu;
    WinRoot    *winRoot;

    if (flusedefault) {
	/*
	 * if menu has a default, line default button with current y; otherwise
	 * line first button of menu up with current y.
	 */
	if (menu->buttonDefault > 0) {
	    y -= menuInfo->buttons[menu->buttonDefault].buttonY;
	}
#ifdef REMIND_PIN_ALIGN
	else if (menu->buttonDefault == PINBUTTON) {
	    /* REMIND align with pin */
	}
#endif	   /* REMIND_PIN_ALIGN */
    }

    /*
     * If menu has a title move y up by titleHeight, else move up by only the
     * space above first button.
     */

    if (menuTitle(menuInfo) != NULL)
	y -= menuInfo->titleHeight;
    else
	y -= HEAD_VSPACE;

    /* Make sure the menu is going to fit on the screen. */
    winRoot = menuInfo->menuWin->core.client->scrInfo->rootwin;
    dpyWidth = winRoot->core.width;
    dpyHeight = winRoot->core.height;
    if ((x + menuInfo->menuWidth) > dpyWidth)
	x = dpyWidth - menuInfo->menuWidth;

    if ((y + menuInfo->menuHeight) > dpyHeight)
	y = dpyHeight - menuInfo->menuHeight;

    if (y < 0)
	y = 0;

    menuInfo->menuX = x;
    menuInfo->menuY = y;

    menuInfo->ignoreNextExpose = True;
    menuInfo->action = ACTION_MENU;

    MapMenuWindow(dpy, menuInfo->menuWin, menuInfo);

    /*
     * Initialize menu brought up using keyboard. Draw the location cursor and
     * hilight the default button
     */
    if (lastPress.type == KeyPress &&
	    menuInfo->menu->buttonDefault != PINBUTTON) {
	drawRevButton(dpy, menuInfo, menuInfo->menu->buttonDefault);
	DrawLocCursor(dpy, menuInfo, menuInfo->menu->buttonDefault, True);
	menuInfo->litButton = menuInfo->menu->buttonDefault;
    }
}

struct brotherVisitInfo {
    Menu *menu;
    int newdef;
};

static void
_setdefault(cli, bfi)
    Client *cli;
    struct brotherVisitInfo *bfi;
{
    WinPinMenu *pinmenu = (WinPinMenu *) cli->framewin->fcore.panewin;
    MenuInfo *mInfo = pinmenu->menuInfo;

    if (mInfo->menu->buttonDefault != bfi->newdef && mInfo->menu == bfi->menu) {
	/*erase old ring*/
	SetButton(cli->dpy, mInfo, bfi->menu->buttonDefault, False, False);

	if (bfi->newdef > NOBUTTON) {
	    /*draw new ring*/
	    SetButton(cli->dpy, mInfo, bfi->newdef, True, True);
	}
    }
}

/*
 * REMIND: this maybe wants to be in client.c
 */
void
VisitPinnedMenuClients(pproc, extra)
    void (*pproc)();
    void *extra;
{
    List	*l = ActiveClientList;
    Client	*cli;

    for (cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l)) {
	if (cli->framewin && cli->framewin->fcore.panewin &&
	    cli->framewin->fcore.panewin->core.kind == WIN_PINMENU) {
	    (*pproc)(cli, extra);
	}
    }
}

void
SetBrothersDefault(dpy, mInfo, new)
    Display *dpy;
    MenuInfo *mInfo;
    int	new;
{
    struct brotherVisitInfo bfi;

    bfi.menu = mInfo->menu;
    bfi.newdef = new;

    VisitPinnedMenuClients(_setdefault, &bfi);

    mInfo->menu->buttonDefault = new;
    SetButton(dpy, mInfo, new, True, False);
#ifdef notdef
    UpdDefaultPtr(mInfo, bindex);
#endif
}




static void
handleMenuKeyPress(dpy, pevent)
    Display    *dpy;
    XEvent     *pevent;
{
    int         bindex;
    MenuInfo   *mInfo;

    flDoSetDefault = (pevent->type == KeyPress);

    mInfo = menuInfoTable[topMenu - 1];

    if (! flDoSetDefault) {
	if (mInfo != NULL && mInfo->litButton != NOBUTTON) {
	    SetBrothersDefault(dpy, mInfo, mInfo->litButton);
	    if (lastPress.type == KeyPress)
		DrawLocCursor(dpy, mInfo, mInfo->litButton, True);
	}
    } else {
	bindex = mInfo->litButton;
	
	/* If on PIN, take out push pin and show it as a default */
	if (bindex == PINBUTTON) {
	    setMenuPin(dpy, mInfo, False, flDoSetDefault);
	    if (mInfo->ringedButton > NOBUTTON)
		SetButton(dpy, mInfo, mInfo->ringedButton, False, False);
	} else if (bindex > NOBUTTON) {
	    if (mInfo->menu->buttonDefault < NOBUTTON) {
		mInfo->ringedButton = bindex;
		drawMenuPushpin(dpy, mInfo);
	    } else if (mInfo->menu->buttonDefault > NOBUTTON) {
		SetButton(dpy, mInfo, mInfo->menu->buttonDefault, False, flDoSetDefault);
	    }
	    if (mInfo->litButton > NOBUTTON) {
		if (lastPress.type == KeyPress)
		    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
		SetButton(dpy, mInfo, mInfo->litButton, False, flDoSetDefault);
	    }
	    SetButton(dpy, mInfo, mInfo->litButton, True, flDoSetDefault);
	    if (lastPress.type == KeyPress)
		DrawLocCursor(dpy, mInfo, mInfo->litButton, True);
	}
    }
}


static Bool
keyEventToItem(dpy, xke)
    Display *dpy;
    XKeyEvent *xke;
{
    MenuInfo *mInfo = menuInfoTable[topMenu - 1];
    Button *pb;
    char c;
    int ct;
    int best;
    int i;

    if (mInfo == NULL)
	return False;

    ct = XLookupString(xke, &c, sizeof(c), NULL, NULL);

    if (ct > 0) {
	best = NOBUTTON;

	if (isupper(c))
	    c = tolower(c);

	if (c == 'p' && isEnabled(mInfo, PINBUTTON))
	    best = PINBUTTON;

	for (i = 0;  i < mInfo->menu->buttonCount;  i++) {
	    pb = mInfo->menu->buttons[i];

	    if (pb->enabled && pb->visible) {
		char t = *pb->label[pb->which];

		if (isupper(t))
		    t = tolower(t);

		if (t == c)
		    if (best == NOBUTTON || i > mInfo->litButton) {
			best = i;
			if (i > mInfo->litButton)
			    break;
		    }
	    }
	}
	if (best == NOBUTTON)
	    best = mInfo->litButton;

	if (BUTTON_INDEX_OK(mInfo, best)) {
	    if (mInfo->pinIn && isEnabled(mInfo, PINBUTTON))
		setMenuPin(dpy, mInfo, False, flDoSetDefault);
	    else if (mInfo->litButton > NOBUTTON)
		DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    else if (mInfo->ringedButton != NOBUTTON) {
		SetButton(dpy, mInfo, mInfo->menu->buttonDefault, False, False);
		mInfo->ringedButton = NOBUTTON;
	    }
	    activateButton(dpy, mInfo, best, flDoSetDefault);
	    DrawLocCursor(dpy, mInfo, best, True);
	    return True;	    
	} else if (best == PINBUTTON) {
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    SetButton(dpy, mInfo, mInfo->litButton, False, False);
	    setMenuPin(dpy, mInfo, True, flDoSetDefault);
	    mInfo->litButton = PINBUTTON;
	    return True;
	}
    }
    return False;
}

/*
 * replaces any occurance of any string in sstr found in buff with c
 */
void
ReplaceChars(buff, sstr, rc)
    char *buff;
    char *sstr;
    char rc;
{
    char *p;
    while ((p = strpbrk(buff, sstr)) != NULL)
	*p = rc;
}

static void
menuHelpCommand(dpy, xke, closure)
    Display *dpy;
    XKeyEvent *xke;
    void *closure;
{
    int bindex;
    MenuInfo *mInfo = menuSearch(xke);
    int status = checkMenuEvent(dpy, mInfo, xke, &bindex);
    char helpbuff[255];
    char *helpstring;
    Button *pb;

    switch (status) {
      case ML_PIN:
	helpstring = "olwm:PushPin";
	break;
      case ML_BUTTON:
      case ML_MENU:
      case ML_BUTTONDISABLED:
	if (! BUTTON_INDEX_OK(mInfo, bindex)) {
	    helpstring = mInfo->menu->helpstring;
	    break;
	}
	pb = mInfo->menu->buttons[bindex];
	helpstring = pb->helpstring[pb->which];
	if (helpstring && status == ML_BUTTONDISABLED) {
	    sprintf(helpbuff, "%s_D", helpstring);
	    helpstring = helpbuff;
	}
	break;
      default:
	helpstring = NULL;
    }

    if (helpstring == NULL) {
	if (mInfo->menu->helpstring != NULL)
	    helpstring = mInfo->menu->helpstring;
	else if (mInfo->menu->title != NULL) {
	    sprintf(helpbuff, "workspace:%s", mInfo->menu->title);
	    ReplaceChars(helpbuff, " \t", '_');
	    helpstring = helpbuff;
	} else
	    helpstring = "workspace:NoHelp";
    }
    menuHide(dpy, closure, False);
    (void) ShowHelpWindow(mInfo->menuWin->core.client->screen,
			  xke->x_root, xke->y_root, helpstring);
#ifdef DEBUG
    fprintf(stderr, "help: %s\n", helpstring);
#endif
}


Bool
MenuHandleKeyEvent(dpy, pevent, win, closure)
    Display *dpy;
    XEvent *pevent;
    WinGeneric *win;
    WinGeneric *closure;
{
    SemanticAction a;
    MenuInfo *mInfo;
    int bindex;
    Bool lastGood = True;
    Bool rval = False;

    if (FindModifierMask(pevent->xkey.keycode) == ModMaskMap[MOD_SETDEFAULT]) {
	handleMenuKeyPress(dpy, pevent);
	return False;
    }

    if (pevent->type == KeyRelease)
	return False;

    a = FindKeyboardAction(dpy, pevent);

    switch (a) {
      case ACTION_SELECT:
      case ACTION_EXEC_DEFAULT:
	mInfo = menuInfoTable[topMenu - 1];
	bindex = NOBUTTON;
	if (mInfo->litButton == NOBUTTON) {
	    bindex = mInfo->menu->buttonDefault;
	    if (isEnabled(mInfo, bindex)) {
		mInfo->litButton = bindex;
		SetButton(dpy, mInfo, mInfo->litButton, True, False);
		XFlush(dpy); /*for effect...*/
	    }
	}
	menuHide(dpy, closure, True);
	break;
      case ACTION_UP:
      case ACTION_DOWN:
      case ACTION_RIGHT:
	mInfo = menuInfoTable[topMenu - 1];
	if (menuHandleUpDownMotion(dpy, pevent, mInfo)) {
	    rval = True;
	    break;
	}
	break;
      case ACTION_LEFT:
      case ACTION_STOP:
	if (topMenu <= 1) {
	    menuHide(dpy, closure, False);
	}
	else {
	    mInfo = menuInfoTable[topMenu - 1];
	    UnmapMenuWindow(dpy, mInfo->menuWin);
	    --topMenu;
	    mInfo = menuInfoTable[topMenu - 1];
	    mInfo->childActive = False;
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, True);

	    /* got rid of a submenu, there's only the pinned menu left,
	     * and it's the one that initiated these mouseless operations,
	     * so we want to uninstall the interposer, and ungrab the server
	     */
	    if (topMenu == 1 && mInfo->menuWin->core.kind == WIN_PINMENU) {
		if (GRV.ServerGrabs)
		    XUngrabServer(dpy);
		
		XUngrabPointer(dpy, CurrentTime);
		XUngrabKeyboard(dpy, CurrentTime);
		XFlush(dpy);
		UninstallInterposer();
	    }
	}
	break;
      case ACTION_NONE:
	if (! keyEventToItem(dpy, pevent))
	    KeyBeep(dpy, pevent);
	break;
      case ACTION_HELP:
	menuHelpCommand(dpy, pevent, closure);
	break;
      default:
	lastGood = False;
    }
    if (lastGood)
	lastPress = *pevent;
	
    return rval;
}
/*
 * eventX, eventY, eventTime
 *
 * Extract the xroot, yroot, or timestamp fields from an event, assuming it's
 * a MotionNotify, ButtonPress, or ButtonRelease.
 */

#define eventX(e)	((e)->type == MotionNotify ? (e)->xmotion.x_root \
						   : (e)->xbutton.x_root )

#define eventY(e)	((e)->type == MotionNotify ? (e)->xmotion.y_root \
			 : (e)->xbutton.y_root )

#define eventTime(e)	((e)->type == MotionNotify ? (e)->xmotion.time \
						   : (e)->xbutton.time )


/*
 * menuTracker
 * Event interposer for menu tracking.
 */

/*ARGSUSED*/
int
MenuTrack(dpy, pevent, win, closure)
    Display    *dpy;
    XEvent     *pevent;
    WinGeneric *win;
    WinGeneric *closure;
{
    MenuInfo   *mInfo = menuInfoTable[topMenu - 1];

    switch (pevent->type) {
      case KeyRelease:
      case KeyPress:
	(void) MenuHandleKeyEvent(dpy, pevent, win, closure);
	break;
      case ButtonPress:
	if (lastPress.type == KeyPress) {
	    if (isEnabled(mInfo, PINBUTTON))
		setMenuPin(dpy, mInfo, False, False);	    
	    if (isEnabled(mInfo, mInfo->litButton)) {
		DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
		/* force redraw of that item */
		if (mInfo->litButton == mInfo->ringedButton)
		    mInfo->ringedButton = NOBUTTON;
		SetButton(dpy, mInfo, mInfo->litButton, True, flDoSetDefault);
	    }
	}
	lastPress = *pevent;
	if (menuHandlePress(dpy, pevent))
	    menuHide(dpy, closure, False);
	break;

      case ButtonRelease:
	if (!AllButtonsUp(pevent))
	    break;
	/* 
	 * if the control key is down, then don't
	 * allow anything to execute, but make sure
	 * that the default item is set for the top menu
	 * REMIND: should maybe we be wanting to do that
	 * thing with showing the defaults for all previous menus?
	 * if so, then shouldn't SetBrothersDefault set the parents 
	 * defaults as well?  No, probably not, but still, we might
	 * want to set the parents defaults somehow on an 
	 * {key,button}up
	 */ 
	if (menuHandleRelease(dpy, pevent)) {
	    if (flDoSetDefault && BUTTON_INDEX_OK(mInfo, mInfo->litButton))
		SetBrothersDefault(dpy, mInfo, mInfo->litButton);
	    menuHide(dpy, closure, ! flDoSetDefault);
	}
	lastPress = *pevent;	/*need to resync it*/
	break;

      case MotionNotify:
	if (!pevent->xmotion.same_screen)
	    break;
	menuHandleMotion(dpy, pevent);
	break;

      case Expose:
	mInfo = menuSearch(pevent);
	if (mInfo == NULL)
	    return DISPOSE_DISPATCH;
	if (mInfo->ignoreNextExpose)
	    mInfo->ignoreNextExpose = False;
	else {
	    DrawMenu(dpy, mInfo);
	    if (mInfo->litButton != NOBUTTON)
		SetButton(dpy, mInfo, mInfo->litButton, True, flDoSetDefault);
	    if (mInfo->pinIn) {
		/*
		 * REMIND This is a trifle odd.  We have to set pinIn to False
		 * because setMenuPin does nothing if pinIn already equals the
		 * value we're setting it to.  The alternative is to code a
		 * call to olgx_draw_pushpin here, which is worse.
		 */
		mInfo->pinIn = False;
		setMenuPin(dpy, mInfo, True, flDoSetDefault);
	    }
	}
	break;

      default:
	return DISPOSE_DEFER;
    }

    /* for pointer events, save the event location */
    switch (pevent->type) {
      case MotionNotify:
	if (! pevent->xmotion.same_screen)
	    break;
	/*fallthrough*/
      case ButtonPress:
      case ButtonRelease:
	lastX = eventX(pevent);
	lastY = eventY(pevent);
	break;
      default:
	break;
    }
    return DISPOSE_USED;
}


SemanticAction
MenuMouseAction(dpy, pevent)
    Display *dpy;
    XEvent *pevent;
{
    SemanticAction a;

    a = ResolveMouseBinding(dpy, pevent, ModMaskMap[MOD_SETDEFAULT]);

    if (GRV.SelectDisplaysMenu && a == ACTION_SELECT)
	a = ACTION_MENU;

    return a;
}

static void
getMenuDim(mInfo, prect)
    MenuInfo *mInfo;
    XRectangle *prect;
{
    WinGeneric *win;

    if (mInfo->menuWin->core.kind == WIN_MENU)
	win = mInfo->menuWin;
    else
	win = mInfo->menuWin->core.parent;

    prect->x = win->core.x;
    prect->y = win->core.y;

    if (mInfo->menuWin->core.kind != WIN_MENU) {
	int xoff = mInfo->menuWin->core.x;
	int yoff = mInfo->menuWin->core.y;
	prect->x += xoff;
	prect->y += yoff;
	prect->width = mInfo->menuWin->core.width;
	prect->height = mInfo->menuWin->core.height;
    } else {
	prect->width = win->core.width;
	prect->height = win->core.height;
    }
}

static Bool
inMenuDent(mInfo, bindex, pevent)
    MenuInfo *mInfo;
    int bindex;
    XEvent *pevent;
{
    int curX;
    Graphics_info *gisNormal;
    Graphics_info *gisButton;
    XRectangle menuDim;

    if (BUTTON_INDEX_OK(mInfo, bindex)) {
	switch (pevent->type) {
	  case MotionNotify:
	    if (pevent->xmotion.same_screen) {
		curX = pevent->xmotion.x_root;
	    }
	    break;
	    
	  case ButtonPress:
	  case ButtonRelease:
	    curX = pevent->xbutton.x_root;
	    break;
	    
	  default:
	    return False;
	}

	gisNormal = WinGI(mInfo->menuWin, NORMAL_GINFO);
	gisButton = WinGI(mInfo->menuWin, BUTTON_GINFO);

	getMenuDim(mInfo, &menuDim);

	if (curX > menuDim.x + mInfo->buttons[bindex].buttonX +
	    mInfo->maxbuttonWidth -
	    ButtonEndcap_Width(gisButton) -
	    MenuMark_Width(gisNormal)) {
	    return True;
	}
    }
    return False;
}

static Bool
menuHandlePress(dpy, pevent)
    Display    *dpy;
    XEvent     *pevent;
{
    int         bindex;
    int         status;
    MenuInfo   *mInfo;

    flDoSetDefault = (pevent->xbutton.state & ModMaskMap[MOD_SETDEFAULT]);

    mInfo = menuSearch(pevent);
    status = checkMenuEvent(dpy, mInfo, pevent, &bindex);
    mInfo->action = MenuMouseAction(dpy, pevent);

    if (mInfo->action != ACTION_SELECT && mInfo->action != ACTION_MENU)
	return False;

    if (isClick(&lastPress, pevent))
	SetClickMode(True);
    
    switch (status) {
      case ML_OFFMENU:
	return True;
      case ML_BUTTONDISABLED:
	break;	/*don't do anything*/
      case ML_PIN:
	unmapChildren(dpy, mInfo);
	setMenuPin(dpy, mInfo, True, flDoSetDefault);
	break;
      case ML_BUTTON:
	/*remove the default ring */
	if (! flDoSetDefault) {
	    if (mInfo->ringedButton != NOBUTTON) {
		SetButton(dpy, mInfo, mInfo->menu->buttonDefault, False, False);
		mInfo->ringedButton = NOBUTTON;
	    }
	}

	unmapChildren(dpy, mInfo);
	minX = eventX(pevent);
	/*FALL THROUGH*/
      default:
	if (isEnabled(mInfo, mInfo->litButton)) {
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    /* force redraw of that item */
	    if (mInfo->litButton == mInfo->ringedButton)
		mInfo->ringedButton = NOBUTTON;
	    SetButton(dpy, mInfo, mInfo->litButton, True, flDoSetDefault);
	}
	activateButton(dpy, mInfo, bindex, flDoSetDefault);
	if (mInfo->action == ACTION_MENU || inMenuDent(mInfo, bindex, pevent))
	    activateSubMenu(dpy, mInfo, bindex, pevent->xbutton.x_root);
	break;
    }
    return False;
}

static void
menuHandleMotion(dpy, pevent)
    Display    *dpy;
    XEvent     *pevent;
{
    XRectangle menuDim;
    int         status;
    int         bindex;
    int         curX;
    int         deltaX;
    Bool        samebutton;
    int         i;
    Graphics_info *gisNormal;
    Graphics_info *gisButton;
    MenuInfo   *mInfo;
    int menuX;

    mInfo = menuSearch(pevent);
    if (mInfo == NULL)
	return;

    gisNormal = WinGI(mInfo->menuWin, NORMAL_GINFO);
    gisButton = WinGI(mInfo->menuWin, BUTTON_GINFO);
    status = checkMenuEvent(dpy, mInfo, pevent, &bindex);
    if (mInfo->action != ACTION_SELECT && mInfo->action != ACTION_MENU)
	return;

    if (lastPress.type == KeyPress && ! flDoSetDefault) {
	if (isEnabled(mInfo, mInfo->litButton)) {
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    drawRevButton(dpy, mInfo, mInfo->litButton);
	}
    }
    if (! isClick(&lastPress, pevent))
	SetClickMode(False);	/*for window buttons*/

    /*
     * If the push pin was in before and this event is not a ML_PIN event, put
     * the pin back out because we are no longer in the pin area.
     */
    if (isEnabled(mInfo, PINBUTTON))
	if ((mInfo->pinIn) && (status != ML_PIN))
	    setMenuPin(dpy, mInfo, False, flDoSetDefault);

    /*
     * I know this is a kludge, but i need it. I want to remove the ring around
     * the default item at this point.
     */
    if ((!flDoSetDefault && (status == ML_BUTTON)) || (status == ML_PIN)) {
	if (mInfo->ringedButton != NOBUTTON) {
	    SetButton(dpy, mInfo, mInfo->menu->buttonDefault, False, False);
	    mInfo->ringedButton = NOBUTTON;
	}
    }
    switch (status) {
      case ML_BUTTON:
	samebutton = (bindex == mInfo->litButton);
	if (mInfo->childActive && ! samebutton)  {
	    unmapChildren(dpy, mInfo);
	    if (mInfo->litButton > NOBUTTON && flDoSetDefault)
		SetButton(dpy, mInfo, mInfo->litButton, False, False);
	}
	curX = pevent->xmotion.x_root;

	activateButton(dpy, mInfo, bindex, flDoSetDefault);

	getMenuDim(mInfo, &menuDim);

	menuX = menuDim.x;

	if (BUTTON_INDEX_OK(mInfo, bindex))
	    if (mInfo->menu->buttons[bindex]->stacked) {
		if (samebutton) {
		    deltaX = curX - minX;
		    minX = MIN(curX, minX);
		} else {
		    deltaX = curX - MAX(lastX, menuX);
		    minX = MIN(curX, lastX);
		}
		
		if ((deltaX > GRV.DragRightDistance) ||
		    (curX > (menuX +
			     mInfo->buttons[bindex].buttonX +
			     mInfo->maxbuttonWidth -
			     ButtonEndcap_Width(gisButton) -
			     MenuMark_Width(gisNormal)))) {
		    (void) activateSubMenu(dpy, mInfo, bindex, pevent->xmotion.x_root);
		    minX = curX;
		}
	    }
	break;

      case ML_PIN:
	setMenuPin(dpy, mInfo, True, flDoSetDefault);
	if (mInfo->childActive) {
	    unmapChildren(dpy, mInfo);
	    if (mInfo->litButton > NOBUTTON && flDoSetDefault)
		SetButton(dpy, mInfo, mInfo->litButton, False, False);
	}
	activateButton(dpy, mInfo, PINBUTTON, flDoSetDefault);
	break;

      case ML_MENU:
      case ML_OFFMENU:
	if (mInfo->childActive)
	    unmapChildren(dpy, mInfo);
	activateButton(dpy, mInfo, NOBUTTON, flDoSetDefault);
	break;

      case ML_BUTTONDISABLED:
	return;
    }	   /* End switch */

    /*
     * Pull down all menus to the right of the current mouse position, except
     * for the initial menu.
     */
    i = topMenu - 1;
    while (i > 0) {
	XRectangle menuDim;
	getMenuDim(menuInfoTable[i], &menuDim);
	if (menuDim.x < pevent->xmotion.x_root)
	    break;
	--i;
    }
    if (i < topMenu - 1) {
	mInfo = menuInfoTable[i];
	unmapChildren(dpy, mInfo);

	topMenu = i + 1;

	status = checkMenuEvent(dpy, mInfo, pevent, &bindex);

	if (status != ML_BUTTON) {
	    activateButton(dpy, mInfo, NOBUTTON, flDoSetDefault);
	} else {
	    /*right now, nothing should be depressed */
	    activateButton(dpy, mInfo, bindex, flDoSetDefault);
	}
    }
}

static Bool
isEnabled(mInfo, item)
    MenuInfo *mInfo;
    int item;
{
    Button *pb;
    if (item > NOBUTTON) {
	pb = mInfo->menu->buttons[item];
	return BUTTON_INDEX_OK(mInfo, item) && pb->enabled && pb->visible;
    }
    if (item == PINBUTTON) {
	return (
		mInfo->menu->hasPushPin
		&& mInfo->pinnedBrother == NULL
		&& mInfo->origmenuInfo == NULL);
    }
    return False;
}

static Bool
alldisabled(mInfo)
    MenuInfo *mInfo;
{
    Bool good;
    int i;

    good = isEnabled(mInfo, PINBUTTON);

    for (i = 0;  i < mInfo->menu->buttonCount && ! good; i++)
	good = isEnabled(mInfo, i);

    return ! good;
}


/*
 * {next,prev}Item are slightly recursive; they rely on themselves to do
 * the right thing
 */

static int
nextItem(mInfo, item)
    MenuInfo *mInfo;
    int item;
{
    if (alldisabled(mInfo))
	return item;

    item++;

    if (item == NOBUTTON)	/*came off of pushpin*/
	item = 0;

    if (item == mInfo->menu->buttonCount)
	item = PINBUTTON;

    if (isEnabled(mInfo, item))
	return item;

    return nextItem(mInfo, item);
}

static int
prevItem(mInfo, item)
    MenuInfo *mInfo;
    int item;
{
    if (alldisabled(mInfo))
	return item;

    item--;

    if (item == NOBUTTON)
	item = PINBUTTON;

    if (item < PINBUTTON)
	item = mInfo->menu->buttonCount - 1;

    if (isEnabled(mInfo, item))
	return item;

    return prevItem(mInfo, item);
}


static Bool
menuHandleUpDownMotion(dpy, pevent, mInfo)
    Display    *dpy;
    XEvent     *pevent;
    MenuInfo   *mInfo;
{
    int         status;
    int         bindex;
    static int (*bumpIndex[])() = {prevItem, nextItem};

    if (mInfo == NULL)
	return False;

    status = FindKeyboardAction(dpy, pevent);

    if (mInfo->litButton == NOBUTTON)
	mInfo->litButton = mInfo->menu->buttonDefault;

    if (status == ACTION_NONE)
	return False;

    /*
     * If the push pin was in before put the pin back out because we are no
     * longer in the pin area.
     */

    if (mInfo->pinIn && isEnabled(mInfo, PINBUTTON))
	setMenuPin(dpy, mInfo, False, flDoSetDefault);
    
    switch (status) {
      case ACTION_UP:
      case ACTION_DOWN:
	assert(ACTION_UP == ACTION_DOWN - 1);

	bindex = (*bumpIndex[status - ACTION_UP])(mInfo, mInfo->litButton);

	if (bindex != mInfo->litButton) {
	    assert(isEnabled(mInfo, bindex));
	    
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    
	    /* One kludge leads to another */
	    if (mInfo->ringedButton != NOBUTTON) {
		SetButton(dpy, mInfo, mInfo->menu->buttonDefault, 0, 0);
		mInfo->ringedButton = NOBUTTON;
	    }

	    if (mInfo->childActive)
		unmapChildren(dpy, mInfo);
	    
	    if (bindex != PINBUTTON)
		activateButton(dpy, mInfo, bindex, flDoSetDefault);
	    else {
		setMenuPin(dpy, mInfo, True, flDoSetDefault);
		activateButton(dpy, mInfo, PINBUTTON, flDoSetDefault);
	    }
	    DrawLocCursor(dpy, mInfo, bindex, True);
	}
	break;

      case ACTION_RIGHT:
	bindex = mInfo->litButton;

	if (BUTTON_INDEX_OK(mInfo, bindex) && mInfo->menu->buttons[bindex]->stacked == 1) {
	    int         menuat;

	    DrawLocCursor(dpy, mInfo, bindex, False);
	    drawRevButton(dpy, mInfo, bindex);

	    if (mInfo->menuWin->core.kind == WIN_MENU)
		menuat = mInfo->menuX + mInfo->menuWidth;
	    else {
		WinGenericFrame *parent = (WinGenericFrame *) mInfo->menuWin->core.parent;
		assert(parent);
		menuat = parent->core.x + parent->core.width - LOC_CURSOR_SIZE;
	    }

	    activateSubMenu(dpy, mInfo, bindex, menuat);
	    return True;
	}
	break;
    }	   /* End switch */
    return False;
}

/*
 * menuHandleRelease
 *
 * Handles ButtonRelease events.  Return value indicates whether the menu
 * stack should taken down.
 */

static      Bool
menuHandleRelease(dpy, pevent)
    Display    *dpy;
    XEvent     *pevent;
{
    int         bindex;
    int         status;
    MenuInfo   *mInfo;
    Bool setClick = False;

    mInfo = menuSearch(pevent);

    if (menuTrackMode == MODE_DRAG) {
	return True;
    } else {
	if (isClick(&lastPress, pevent)) {
	    SetClickMode(True);	/*for window buttons*/
	    setClick = True;
	}

	switch (mInfo->action) {
	  case ACTION_SELECT:
	    return True;	/*always return true on SELECT*/
	  case ACTION_MENU:
	    status = checkMenuEvent(dpy, mInfo, pevent, &bindex);
	    switch (status) {
	      case ML_PIN:
		return True;
	      case ML_BUTTONDISABLED:
		return True;
	      case ML_BUTTON:
		if (mInfo->litButton != NOBUTTON)
		    return True;
		/*fall through*/
	      default:
		return ! setClick;
	    } /*switch(status)*/
	  default:
	    return False;
	}
    }
}


/*
 * menuSearch
 *
 * Given an event, search the stack of active menus for the menu on which this
 * event occurred.  The event must be a ButtonPress, ButtonRelease,
 * MotionNotify, or Expose event.  If the event didn't occur on any of the
 * menus, for the pointer events, the topmost menu in the stack is returned.
 * Otherwise, zero is returned.
 */
static MenuInfo *
menuSearch(event)
    XEvent     *event;
{
    Window      w = 0;
    int         i;

    switch (event->type) {
      case KeyPress:
      case KeyRelease:
	w = event->xkey.subwindow;
	break;
      case ButtonPress:
      case ButtonRelease:
	w = event->xbutton.subwindow;
	if (w == None)
	    w = event->xbutton.window;
	break;
      case MotionNotify:
	if (event->xmotion.same_screen) {
	    w = event->xmotion.subwindow;

	    if (w == None)
		w = event->xbutton.window;
	}
	break;
      case Expose:
	w = event->xexpose.window;
	break;
      default:
	fputs(gettext("olwm: wrong event type passed to menuSearch\n"), stderr);
	return (MenuInfo *) 0;
    }

    for (i = topMenu - 1; i >= 0; --i) {
	if (w == menuInfoTable[i]->menuWin->core.self)
	    return menuInfoTable[i];
    }
    return (event->type == Expose) ? (MenuInfo *) 0 : menuInfoTable[topMenu - 1];
}


/*
 * checkMenuEvent
 *
 * Check a button or motion event against a menu.  Sets the index of the
 * active button (or to NOBUTTON) and returns the pointer location:
 *	ML_BUTTON, ML_PIN, ML_MENU, or ML_OFFMENU.
 */
static MenuLocation
checkMenuEvent(dpy, menuInfo, pevent, bindex)
    Display    *dpy;
    MenuInfo   *menuInfo;
    XEvent     *pevent;
    int        *bindex;
{
    int         i;
    int         yoff = 0;
    Window      hitwindow = 0;
    int         ex, ey;
    Graphics_info *gisNormal = WinGI(menuInfo->menuWin, NORMAL_GINFO);
    int rx, ry;
    XRectangle  menuDim;

    /* menu->title == NULL for pinned menus, as well as title-less ones */
    if (menuTitle(menuInfo) != NULL)
	yoff = menuInfo->titleHeight;
    else
	yoff = HEAD_VSPACE;

    switch (pevent->type) {
      case MotionNotify:
	if (pevent->xmotion.same_screen) {
	    hitwindow = pevent->xmotion.window;
	    ex = pevent->xmotion.x;
	    ey = pevent->xmotion.y;
	    rx = pevent->xmotion.x_root;
	    ry = pevent->xmotion.y_root;
	}
	break;
      case ButtonPress:
      case ButtonRelease:
	hitwindow = pevent->xbutton.window;
	ex = pevent->xbutton.x;
	ey = pevent->xbutton.y;
	rx = pevent->xbutton.x_root;
	ry = pevent->xbutton.y_root;
	break;
      case KeyPress:
      case KeyRelease:
	hitwindow = menuInfo->menuWin->core.self;
	rx = ex = pevent->xkey.x_root;
	ry = ey = pevent->xkey.y_root;
	break;
    }

    /* If the event window is not the menu window. */
    if (hitwindow != menuInfo->menuWin->core.self) {
	if (hitwindow != None) {
	    XTranslateCoordinates(dpy, hitwindow, menuInfo->menuWin->core.self,
				  ex, ey, &ex, &ey, &hitwindow);
	} else {
	    *bindex = NOBUTTON;
	    return ML_OFFMENU;
	}
    }

    getMenuDim(menuInfo, &menuDim);
    if (! PointInRect(rx, ry, menuDim.x, menuDim.y, menuDim.width, menuDim.height))
	return ML_OFFMENU;

    /*
     * Check the event coordinates against each of the buttons. Since the
     * button event is reported relative to root window it must be adjusted for
     * the check.
     */
    for (i = 0; i < menuInfo->menu->buttonCount; i++) {
	/*button is invisible; it has no entry*/
	if (menuInfo->buttons[i].button == NULL)
	    continue;

	if (PointInRect(ex, ey,
			menuInfo->buttons[i].buttonX,
			menuInfo->buttons[i].buttonY + yoff,
			menuInfo->maxbuttonWidth,
			menuInfo->buttons[i].buttonHeight)) {
	    /*
	     * Event is in a button. Is it a button stack and if so, is it in
	     * the right half of the button?
	     */
	    *bindex = i;
	    if (menuInfo->buttons[i].button->enabled)
		return ML_BUTTON;
	    else
		return ML_BUTTONDISABLED;
#ifdef notdef
	    if ((menuInfo->buttons[i].button->stacked) &&
		    ((ex - menuInfo->menuX) > (menuInfo->menuWidth / 2)))
		return S_ACTIVATE;
	    else
		return S_ONBUTTON;
#endif	   /* notdef */
	}
    }

    /* Check the pushpin area. */
    *bindex = NOBUTTON;
    if (isEnabled(menuInfo, PINBUTTON) 
	&& PointInRect(ex, ey, 
		       menuInfo->pushPinX,
		       menuInfo->pushPinY,
		       PushPinOut_Width(gisNormal),
		       PUSHPINHEIGHT(gisNormal)))
	return ML_PIN;

    return ML_MENU;
}


/*
 * menuHide
 *
 * Remove any active menus from the screen, and call the menu callback
 * function as necessary.
 */
static int
menuHide(dpy, winInfo, fldoit)
    Display    *dpy;
    WinGeneric *winInfo;
    Bool fldoit;
{
    int         i;
    MenuInfo   *mInfo = menuInfoTable[topMenu - 1];
    int item = mInfo->litButton;

    /*
     * reinstall the locked colormap, if we saved it
     */
    if (prevColorFocusWindow != NULL) {
	InstallColormap(dpy, prevColorFocusWindow);
	prevColorFocusWindow = NULL;
    }

    if (GRV.ServerGrabs)
	XUngrabServer(dpy);

    /* Unmap any active menus. */
    for (i = topMenu - 1; i >= 0; --i) {
	MenuInfo *mi = menuInfoTable[i];
	WinGeneric *menuWin = mi->menuWin;

	if (menuWin->core.kind == WIN_MENU)
	    UnmapMenuWindow(dpy, menuWin);
	else {
	    DrawLocCursor(dpy, mi, mi->litButton, False);
	    SetButton(dpy, mi, mi->litButton, False, False);
	}
	mi->litButton = NOBUTTON;
    }

    XUngrabPointer(dpy, CurrentTime);
    XUngrabKeyboard(dpy, CurrentTime);
    XFlush(dpy);
    UninstallInterposer();

    /*
     * need to sync before calling the proc,
     * because the proc's purpose might be to
     * destroy all pinned menus!
     */
    if (syncFunc != NULL)
	(*syncFunc) (syncInfo);

    if (fldoit) {
	if (mInfo->pinIn && isEnabled(mInfo, PINBUTTON)) {
	    (void) MakePinMenu(dpy, winInfo, mInfo);
	} else {
	    if (item != NOBUTTON)
		ExecButtonAction(dpy, winInfo, mInfo, item);
	}
    }
}


static void
unmapChildren(dpy, mInfo)
    Display    *dpy;
    MenuInfo   *mInfo;
{
    int         i;

    i = topMenu - 1;
    while (i >= 0 && menuInfoTable[i]->menu != mInfo->menu) {
	UnmapMenuWindow(dpy, menuInfoTable[i]->menuWin);
	--i;
    }
    topMenu = i + 1;
#ifdef DEBUG
    if (i < 0)
	fputs("olwm: warning, internal error in unmapChildren!\n", stderr);
#endif	   /* DEBUG */

    mInfo->childActive = False;
}


static void
activateButton(dpy, mInfo, idx, flsetdefault)
    Display    *dpy;
    MenuInfo   *mInfo;
    int         idx;
    Bool        flsetdefault;
{
    if (mInfo->litButton == idx)
	return;

    /* Unhighlight any highlit button. */

    if (mInfo->litButton > NOBUTTON) {
	SetButton(dpy, mInfo, mInfo->litButton, False, flsetdefault);
	SetButton(dpy, mInfo, mInfo->litButton, False, False);
    } else if (mInfo->litButton == PINBUTTON && isEnabled(mInfo, PINBUTTON)) {
	int         oldstate;
	/* setMenuPin is too restrictive */
	oldstate = mInfo->pinIn;
	mInfo->pinIn = True;
	setMenuPin(dpy, mInfo, False, False);
	mInfo->pinIn = oldstate;
    }
    /* Highlight the new button */

    if (idx > NOBUTTON)
	SetButton(dpy, mInfo, idx, True, flsetdefault);
    else
	SetButton(dpy, mInfo, mInfo->menu->buttonDefault, True, True);

    mInfo->litButton = idx;
}


static void
setMenuPin(dpy, mInfo, state, flsetdefault)
    Display    *dpy;
    MenuInfo   *mInfo;
    Bool        state;
    int         flsetdefault;
{
    GC          windowGC = WinGC(mInfo->menuWin, WINDOW_GC);
    Graphics_info *gisNormal = WinGI(mInfo->menuWin, NORMAL_GINFO);
    Window      win = mInfo->menuWin->core.self;
    int         olgx_flags;

    if (mInfo->pinIn != state) {
	mInfo->pinIn = state;
#ifdef DEBUG
	if (! isEnabled(mInfo, PINBUTTON))
	    fprintf(stderr, "trying to set disabled pin!\n");
#endif

	XFillRectangle(dpy, win, windowGC,
		       mInfo->pushPinX, mInfo->pushPinY,
		       PushPinOut_Width(gisNormal),
		       PUSHPINHEIGHT(gisNormal));

	if (((mInfo->menu->buttonDefault == PINBUTTON) && !state) || flsetdefault)
	    olgx_flags = OLGX_DEFAULT | OLGX_PUSHPIN_OUT;
	else
	    olgx_flags = (state ? OLGX_PUSHPIN_IN : OLGX_PUSHPIN_OUT);

	olgx_draw_pushpin(gisNormal, win,
			  mInfo->pushPinX, mInfo->pushPinY,
			  olgx_flags);
    }
}


/*
 * activateSubMenu
 *
 * Given a MenuInfo struct and a button, activate that button's submenu.
 * It's assumed that the button actually has a submenu.  Note that only the
 * x-location is passed in, while the y-location is calculated.  The reason is
 * that the x-location is determined by the mouse event, while the y-location
 * is always based the location of the parent menu.  If a submenu is already
 * active, do nothing.  This is primarily to prevent the same submenu from
 * being activated again.  This occurs if a submenu is much narrower than its
 * parent, and you pull off the right of the submenu back into the parent.
 */
static void
activateSubMenu(dpy, mInfo, bindex, x)
    Display    *dpy;
    MenuInfo   *mInfo;
    int         bindex;
    int         x;
{
    MenuInfo   *subMenu;
    MenuCache  *menuCache = mInfo->menuWin->core.client->scrInfo->menuCache;
    int ypos;

    if (!mInfo->childActive 
	&& BUTTON_INDEX_OK(mInfo, bindex)
	&& mInfo->buttons[bindex].subMenu) {
	mInfo->childActive = True;

	subMenu = mInfo->buttons[bindex].subMenu;
	subMenu->menuWin = (WinGeneric *) menuCache->menuWinList[topMenu];
	menuInfoTable[topMenu++] = subMenu;
	
	subMenu->childActive = False;
	subMenu->pinIn = False;
	subMenu->litButton = NOBUTTON;
	subMenu->ringedButton = subMenu->menu->buttonDefault;
	
	if (mInfo->menuWin->core.kind == WIN_MENU) {
	    ypos = mInfo->menuY;
	    if (menuTitle(mInfo) == NULL)
		ypos += HEAD_VSPACE;
	    else
		ypos += mInfo->titleHeight;
	} else {
	    WinGenericFrame *parent = (WinGenericFrame *) mInfo->menuWin->core.parent;
	    assert(parent);
	    ypos = mInfo->menuWin->core.y + HEAD_VSPACE;
	    ypos += parent->core.y;
	}

	ypos += mInfo->buttons[bindex].buttonY;

	showMenu(dpy, subMenu, x - MENU_HORIZ_OFFSET, ypos, True);
    }
}


void
DrawLocCursor(dpy, mInfo, bindex, fldraw)
    Display    *dpy;
    MenuInfo   *mInfo;
    int         bindex;
    Bool         fldraw;
{
    Window      wid = mInfo->menuWin->core.self;
    int         y;
    XPoint      Pt[3];
    Button *pb;
    
    /*
     * REMIND: 
     * we don't draw the location cursor for the pushpin
     */
    if (! BUTTON_INDEX_OK(mInfo, bindex))
	return;

    pb = mInfo->buttons[bindex].button;

    if (pb == NULL || pb->label[pb->which] == NULL)
	return;

    if (menuTitle(mInfo) != NULL)
	y = mInfo->titleHeight;
    else
	y = HEAD_VSPACE;

    y += (mInfo->buttons[bindex].buttonY + (mInfo->buttons[bindex].buttonHeight / 2));

    Pt[0].x = 6 + 11;
    Pt[0].y = y;
    Pt[1].x = -11;
    Pt[1].y = -6;
    Pt[2].x = 0;
    Pt[2].y = 12;

    if (fldraw && bindex != PINBUTTON)
	XFillPolygon(dpy, wid, WinGC(mInfo->menuWin, FOREGROUND_GC), Pt,
		     3, Convex, CoordModePrevious);
    else
	XFillPolygon(dpy, wid, WinGC(mInfo->menuWin, WINDOW_GC), Pt,
		     3, Convex, CoordModePrevious);
}

/* Draw a normal button.
 * if fDefault is true, a default ring will be drawn.
 */
/*ARGSUSED*/
static void
drawButton(dpy, menuInfo, idx, fDefault)
    Display    *dpy;
    MenuInfo   *menuInfo;
    int         idx;
    Bool        fDefault;
{
    Button *pb;
    WinGeneric *winInfo = menuInfo->menuWin;
    int         state;

    if (! BUTTON_INDEX_OK(menuInfo, idx))
	return;

    pb = menuInfo->buttons[idx].button;

    state = OLGX_NORMAL | OLGX_ERASE | OLGX_MENU_ITEM;

    if (! pb->enabled)
	state |= OLGX_INACTIVE;

    if (pb->stacked)
	state |= OLGX_HORIZ_MENU_MARK;

    if (fDefault)
	state |= OLGX_DEFAULT;

    olgx_draw_button(WinGI(winInfo, BUTTON_GINFO), winInfo->core.self,
		     menuInfo->buttons[idx].buttonX,
		     menuInfo->buttons[idx].buttonY + menuInfo->buttonOffset,
		     menuInfo->maxbuttonWidth, 0,
		     pb->label[pb->which], state);
}


/*
 * drawRevButton - Draw a reverse video button.
 */
/*ARGSUSED*/
static void
drawRevButton(dpy, menuInfo, idx)
    Display    *dpy;
    MenuInfo   *menuInfo;
    int         idx;
{
    Button *pb;
    int         state;
    WinGeneric *winInfo = menuInfo->menuWin;

    if (! BUTTON_INDEX_OK(menuInfo, idx))
	return;

    pb = menuInfo->buttons[idx].button;

    /* if the button is disabled, do nothing */
    if (! pb->enabled)
	return;

    state = OLGX_INVOKED | OLGX_ERASE |
	((pb->stacked) ? OLGX_HORIZ_MENU_MARK : 0);

    olgx_draw_button(WinGI(winInfo, BUTTON_GINFO), winInfo->core.self,
		     menuInfo->buttons[idx].buttonX,
		     menuInfo->buttons[idx].buttonY + menuInfo->buttonOffset,
		     menuInfo->maxbuttonWidth, 0,
		     pb->label[pb->which], state | OLGX_MENU_ITEM);
}


/*
 * isClick
 *
 * Takes two button events and returns a boolean indicating whether they are
 * close enough (spacially and temporally) to be considered a click.
 */

#define THRESH_DIST   5

static      Bool
isClick(e1, e2)
    XEvent     *e1;
    XEvent     *e2;
{
    return (
	    ABS(eventX(e1) - eventX(e2)) <= GRV.ClickMoveThreshold &&
	    ABS(eventY(e1) - eventY(e2)) <= GRV.ClickMoveThreshold &&
	    eventTime(e2) - eventTime(e1) <= GRV.DoubleClickTime
	);
}


/********************************************************************************/

void
SetMenuDefault(pmenu, def)
    Menu *pmenu;
    int def;
{
    pmenu->buttonDefault = def;
}

Menu *
NewNamedMenu(name, flpin, help)
    char *name;
    Bool flpin;
    char *help;
{
    Menu *pmenu = MemNew(Menu);
    
    if (pmenu != NULL) {
	pmenu->title = name;
	pmenu->buttons = NULL;
	pmenu->buttonCount = 0;
	pmenu->buttonDefault = 0;
	pmenu->hasPushPin = flpin;
	pmenu->helpstring = help;
	pmenu->menudirty = True;
    }

    return pmenu;
}

Bool
AppendMenuItem(pmenu, pitem)
    Menu *pmenu;
    Button *pitem;
{
    if (pmenu == NULL)
	return False;

    if (pmenu->buttonCount++ == 0)
	pmenu->buttons = (Button **) MemNew(Button **);
    else
	pmenu->buttons = (Button **) MemRealloc(pmenu->buttons,
						 (pmenu->buttonCount * sizeof(Button **)));
    
    if (pmenu->buttons == NULL)
	return False;

    pmenu->buttons[pmenu->buttonCount - 1] = pitem;
    
    pmenu->menudirty = True;

    return True;
}

/*
 * caveat: aren't overly useful, since you may have a button used in 
 * multiple menus, in which case the second menu may never get dirtied
 * so you have to handle those special cases [look in GetEnabledMenu()]
 */

void
SetMenuHier(pmenu, itemno, phier)
    Menu *pmenu;
    int itemno;
    Menu *phier;
{
    Button *pb = pmenu->buttons[itemno];

    if (pb->action.submenu != phier) {
	pmenu->menudirty = True;
	pb->action.submenu = phier;
	pb->stacked = (phier != NULL);
    }
}


void
_ToggleEnabled(pmenu, itemno, flenabled)
    Menu *pmenu;
    int itemno;
    Bool flenabled;
{
    ToggleEnabled(pmenu, itemno, flenabled);
}

void
_ToggleItem(pmenu, itemno, which)
    Menu *pmenu;
    int itemno;
    int which;
{
    ToggleItem(pmenu, itemno, which);
}

void
_ToggleVisible(pmenu, itemno, visible)
    Menu *pmenu;
    int itemno;
    int visible;
{
    ToggleVisible(pmenu, itemno, visible);
}

void
_SetMenuTitle(pmenu, s)
    Menu *pmenu;
    char *s;
{
    SetMenuTitle(pmenu, s);
}

void
_DirtyMenu(pmenu)
    Menu *pmenu;
{
    DirtyMenu(pmenu);
}

Menu *
CreateMenu(name, barray, ctbuttons, flpin, help)
    char *name;
    Button **barray;	/*array of button pointers*/
    int ctbuttons;
    Bool flpin;
    char *help;
{
    Menu *new = NewNamedMenu(name, flpin, help);

    if (new != NULL) {
	new->buttonCount = ctbuttons;

	new->buttons = (Button **) MemAlloc(ctbuttons * sizeof(Button **));
	memcpy((void *) new->buttons, (void *) barray, ctbuttons * sizeof(Button **));
    }
    return new;
}

/********************************************************************************/


/*
 * InitScreenMenus
 */
MenuCache  *
InitScreenMenus(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    MenuCache  *menuCache;
    int         index, maxDepth;

    menuCache = MemNew(MenuCache);
    menuCache->maxSlots = 40;	/*intial size*/
    menuCache->nextSlot = 0;
    menuCache->menuInfoList = (MenuInfo **) MemAlloc(sizeof(MenuInfo) * menuCache->maxSlots);

    for (index = 0; index < (int) NUM_MENUS; index++) {
	(void) MenuInfoCreate(menuCache, scrInfo->rootwin, MenuTable[index], 1);
    }

    maxDepth = findMaxDepth(menuCache);

    menuCache->menuWinList = MemAlloc(maxDepth * sizeof(struct _winmenu *));

    for (index = 0; index < maxDepth; index++)
	menuCache->menuWinList[index] = MakeMenu(dpy, scrInfo->rootwin);

    menuCache->maxDepth = maxDepth;

#ifdef notdef
    ApplyMenuDefaults(dpy, menuCache);
#endif

    return menuCache;
}


/*
 * REMIND: does not deal with pushpins being
 * the default, as there are no standard menus that
 * have pushpins at the time of this inception
 */
Bool
DoDefaultMenuAction(win)
    WinGenericFrame *win;
{
    Menu *menu;
    int defitem;
    Button *pb;
   
    /*
     * find the menu associated with this frame,
     * this ensures that the menu is enabled correctly
     */
    menu = GetEnabledMenu(win->core.client, win->fcore.fullsize, False);

    if (!menu)
	return False;

    defitem = win->core.client->wmDecors->def_item;

    if (defitem < 0 || defitem > menu->buttonCount) {
	win->core.client->wmDecors->def_item =	defitem = 0;
    }
    pb = menu->buttons[defitem];

    if (pb->enabled) {
	MenuInfo *menuInfo = findMenuInfo(win, menu);
	assert(menuInfo);
	(*pb->action.callback) (win->core.client->dpy, win, menuInfo, defitem);
    }
    return True;
}


/********************************************************************************/

static Region zeroregion;

void
InitRegions()
{
    zeroregion = XCreateRegion();
}


void
EmptyRegion(r)
    Region r;
{
    if (r != NULL)
	XIntersectRegion(zeroregion, r, r);
}

void
RectRegion(r, x, y, w, h)
    Region r;
    int x, y;
    unsigned int w, h;
{
    XRectangle rect;

    if (r != NULL) {
	rect.x = x;
	rect.y = y;
	rect.width = w;
	rect.height = h;

	XUnionRectWithRegion(&rect, r, r);
    }
}


void
AppendExposeDamage(pr, ee)
    Region *pr;
    XExposeEvent *ee;
{
    if (*pr == NULL)
	*pr = XCreateRegion();

    if (*pr != NULL)
	RectRegion(*pr, ee->x, ee->y, ee->width, ee->height);
}

void
MakeExposeDamage(pr, ee)
    Region *pr;
    XExposeEvent *ee;
{
    if (*pr != NULL)
	EmptyRegion(*pr);

    AppendExposeDamage(pr, ee);
}

