/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "suit.h"

#define STUCK_DOWN "stuck down"

/*  A quick note on the notation used in this file.  The following
    two parts as a unit are refered to as a "pulldown menu" :

    +------+
    | File |           <------- refered to as the "button" because it looks like one
    +------+-----+                (not to be confused with the button widget)
    | Open...    |
    | Close      |
    | Save       |     <------- refered to as the "menu"
    | Save As... |
    | Print      |
    | Exit       |
    +------------+

   (Isn't ASCII art to describe part of a GUI rather ironic?)

             -- Rob

    yes rob, it is :)

             -- dennis

*/


PRIVATE void MoveMenuUnderButton (SUIT_object button, SUIT_object menu)
{
    int bwidth = SUIT_getInteger (menu, BORDER_WIDTH);
    rectangle loc, mloc;
    int xdiff, ydiff;

    loc = SUIT_mapViewportToScreen (button, OBJECT_VIEWPORT(button));
    mloc = SUIT_mapViewportToScreen (menu, OBJECT_VIEWPORT(menu));
    xdiff = mloc.bottom_left.x - loc.bottom_left.x;
    ydiff = mloc.top_right.y - loc.bottom_left.y + 2*bwidth + 1;
    mloc.bottom_left.x -= xdiff;    mloc.top_right.x -= xdiff;
    mloc.bottom_left.y -= ydiff;    mloc.top_right.y -= ydiff;
    SUIT_setViewport (menu, VIEWPORT, SUIT_mapScreenToViewport(menu, mloc));
    SUIT_bringToFront (menu);
}

PRIVATE void TileVertically (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    static boolean canEnter = TRUE; /* this just prevents some needless recursion */

    if (!canEnter)
	return;

    if (SUIT_stringsMatch (propName, NUMBER_OF_CHILDREN) ||
	SUIT_stringsMatch (propName, VIEWPORT)) {
	int numKids = SUIT_numberOfChildren(o);
	int bWidth = SUIT_getInteger (o, BORDER_WIDTH);
	int margin = SUIT_getInteger (o, MARGIN);
	SUIT_viewport vp;
	int Width, Height, i, w, a, d, maxwidth = 0, maxheight = 0;

	vp = OBJECT_VIEWPORT(o);
	Width = vp.top_right.x - vp.bottom_left.x;
	Height = vp.top_right.y - vp.bottom_left.y;

	GP_pushGraphicsState();
	GP_setFont (SUIT_getFont (o, FONT));
	SRGP_inquireTextExtent ("Arbitrary", &w, &a, &d);
	for (i=0; i < numKids; i++) {
	    char *hotkey;
	    int owidth, oheight,  h;  /* the original size */
	    SUIT_object kid = SUIT_getChild(o,numKids-i-1);
	    SUIT_viewport kidvp;

	    kidvp =  SRGP_defRectangle (2*bWidth, i*Height/numKids + bWidth,
					Width - 2*bWidth, (i+1)*Height/numKids - bWidth);
	    SUIT_setBoolean (kid, HAS_BORDER, FALSE);
	    SUIT_setBoolean (kid, SHRINK_TO_FIT, FALSE);
	    SUIT_getObjectSize (kid, &owidth, &oheight);
	    SUIT_setViewport (kid, VIEWPORT, kidvp);
	    SRGP_inquireTextExtent (SUIT_getText (kid, LABEL), &w, &a, &d);
	    h = a + d;
	    if (SUIT_stringsMatch (SUIT_getText (kid, LABEL), "")) {
		w = owidth;
		h = oheight;
	    }
	    hotkey = SUIT_getText (kid, HOTKEY);
	    if (strlen(hotkey) > 0) {
		int wid;
		SRGP_inquireTextExtent (hotkey, &wid, &a, &d);
		w += wid + 6*margin;
	    }
	    if (w > maxwidth)
		maxwidth = w;
	    if (h > maxheight)
		maxheight = h;
	}
	vp.bottom_left.y = vp.top_right.y - ((numKids==0)? 1 : numKids) *
	                                     (maxheight+2*margin+2*bWidth)-1;
	vp.top_right.x = vp.bottom_left.x + ((numKids==0)? 20 : maxwidth)+2*margin+4*bWidth + 1;
	canEnter = FALSE;
	SUIT_setViewport (o, VIEWPORT, vp);
	canEnter = TRUE;
	GP_popGraphicsState();
    }
}



PRIVATE void HighlightChoice (SUIT_object o, int numKids, int choice)
{
    if (choice >= 0 && choice < numKids) {
	SUIT_object kid = SUIT_getChild (o, choice);
	rectangle box;
	int bWidth = SUIT_getInteger (o, BORDER_WIDTH);

	box = SUIT_mapViewportToScreen (kid, OBJECT_VIEWPORT(kid));
	if (SUIT_getBoolean(kid,DISABLED))
	    return;
	box.bottom_left.x -= bWidth;  box.bottom_left.y -= bWidth;
	box.top_right.x += bWidth;    box.top_right.y += bWidth;
	GP_pushGraphicsState();
	GP_beveledBorder (box, SUIT_getColor (o, BORDER_COLOR), TRUE, bWidth);
	GP_popGraphicsState();
    }
}


PRIVATE void UnhighlightChoice (SUIT_object o, int numKids, int choice)
{
    if (choice >= 0 && choice < numKids) {
	SUIT_object kid = SUIT_getChild (o, choice);
	int bWidth = SUIT_getInteger (o, BORDER_WIDTH);
	rectangle box;

	box = SUIT_mapViewportToScreen (kid, OBJECT_VIEWPORT(kid));
	box.bottom_left.x -= bWidth;  box.bottom_left.y -= bWidth;
	box.top_right.x += bWidth;    box.top_right.y += bWidth;
	GP_pushGraphicsState();
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	SRGP_fillRectangleCoord (box.bottom_left.x, box.bottom_left.y, 
				 box.bottom_left.x+bWidth, box.top_right.y);
	SRGP_fillRectangleCoord (box.top_right.x-bWidth, box.bottom_left.y, 
				 box.top_right.x, box.top_right.y);
	SRGP_fillRectangleCoord (box.bottom_left.x+bWidth, box.bottom_left.y, 
				 box.top_right.x-bWidth, box.bottom_left.y+bWidth);
	SRGP_fillRectangleCoord (box.bottom_left.x+bWidth, box.top_right.y-bWidth, 
				 box.top_right.x-bWidth, box.top_right.y);
	GP_popGraphicsState();
    }
}



PRIVATE SUIT_object OverAnotherMenu (SUIT_object menu, SUIT_object button, SUIT_event e)
{
    SUIT_object parent = SUIT_getParent(button);
    point p;
    int i;

    p = e.locator.position;
    for (i=0; i < SUIT_numberOfChildren(parent); i++) {
	SUIT_object kid = SUIT_getChild (parent, i);
	rectangle screen;

	screen = SUIT_mapViewportToScreen (kid, OBJECT_VIEWPORT(kid));

	if (kid == button)
	    continue;

	if (p.x >= screen.bottom_left.x && p.x <= screen.top_right.x &&
	    p.y >= screen.bottom_left.y && p.y <= screen.top_right.y &&
	    SUIT_stringsMatch (OBJECT_CLASS(kid), "pulldown menu"))
	    return kid;
    }
    return NULL;
}



PRIVATE void HitVertical (SUIT_object menu, SUIT_event e)
{
    int numKids = SUIT_numberOfChildren(menu);
    boolean inside = (e.worldLocation.x >= 0.0 && e.worldLocation.x <= 1.0 &&
		      e.worldLocation.y >= 0.0 && e.worldLocation.y <= 1.0);
    int choice = (int) ((1.0 - e.worldLocation.y) * (double)numKids);
    SUIT_object newmenu;
    boolean isPartOfPulldown = (SUIT_getObject(menu, "button") != NULL);
    static boolean wasinside = FALSE;
    static int oldchoice = -1;
    
    if (inside && (choice < 0 || choice >= numKids))
	return;
    
    if (e.type == MOUSE_DOWN) {
	SUIT_reportMouseMotion (menu, UNTIL_MOUSE_UP);
	if (inside)
	    HighlightChoice (menu, numKids, choice);
    }

    else if (e.type == MOUSE_MOTION && (choice != oldchoice || inside != wasinside)) {
	UnhighlightChoice (menu, numKids, oldchoice);
	if (inside)
	    HighlightChoice (menu, numKids, choice);
    }

    else if (e.type == MOUSE_MOTION && !inside && isPartOfPulldown &&
	     (newmenu = OverAnotherMenu (menu, SUIT_getObject (menu, "button"), e)) != NULL) {
	SUIT_setBoolean (menu, VISIBLE, FALSE);
	SUIT_setBoolean (menu, STUCK_DOWN, FALSE);
	if (isPartOfPulldown &&
	    SUIT_stringsMatch (OBJECT_CLASS (SUIT_getParent (SUIT_getObject (menu, "button"))),
			       "stacker")) {
	    SUIT_setBoolean (SUIT_getObject(menu, "button"), HAS_BORDER, FALSE);
	    SUIT_redisplayNotRequired (SUIT_getParent (SUIT_getObject (menu, "button")));  /* hack */
	}
	e.type = MOUSE_DOWN;
	SUIT_hitObject (newmenu, SUIT_adjustEventForObject (e, menu, newmenu));
	return;
    }

    else if (e.type == MOUSE_UP || e.type == CLICK || e.type == KEYSTROKE) {
	if (!isPartOfPulldown)
	    UnhighlightChoice (menu, numKids, oldchoice);
	if (e.type != MOUSE_UP)
	    HighlightChoice (menu, numKids, choice);
	if (!SUIT_getBoolean (menu, STUCK_DOWN) && isPartOfPulldown) {
	    SUIT_setBoolean (menu, VISIBLE, FALSE);
	    if (SUIT_stringsMatch (OBJECT_CLASS(SUIT_getParent (SUIT_getObject (menu, "button"))),
				   "stacker"))
		SUIT_setBoolean (SUIT_getObject(menu, "button"), HAS_BORDER, FALSE);
	}
	GP_setCursor (STANDARD_CURSOR);
	if (inside && numKids > 0) {
	    SUIT_object child = SUIT_getChild(menu, choice);
	    e.type = CLICK;
	    SUIT_performRedisplay();  /* for aesthetics, just incase the callback takes a while */
	    SUIT_hitObject (child, SUIT_adjustEventForObject(e, menu, child));
	    if (!isPartOfPulldown)
		UnhighlightChoice (menu, numKids, choice);
	}
    }
    
    oldchoice = choice;
    wasinside = inside;
}



void TileAppropriately (SUIT_object obj, char *pname, char *ptype, Pointer new, Pointer old);

SUIT_object SUIT_createVerticalMenu (char *name)
{
    static boolean firsttime = TRUE;

    SUIT_object o = SUIT_createObject (name, "menu");
    SUIT_addDisplayToObject (o, "vertical", HitVertical, SUIT_paintChildren);
    SUIT_setBoolean (o, STUCK_DOWN, TRUE);

    /* dennis hack july 9, 1992 6:14 am */
    SUIT_setObject (o, "button", NULL);
    SUIT_registerInterest (o, TileVertically);

    SUIT_setViewport (o, VIEWPORT, OBJECT_VIEWPORT(o));
    if (firsttime) {
	SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, CAN_BE_OPENED, CLASS);
	SUIT_deluxeSetBoolean (o, CACHE_USING_CANVAS, TRUE, CLASS);
	firsttime = FALSE;
    }
    return o;
}


PRIVATE void ShrinkToFit (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, VIEWPORT) ||
	SUIT_stringsMatch (propName, LABEL) ||
	SUIT_stringsMatch (propName, FONT) ||
	SUIT_stringsMatch (propName, SHRINK_TO_FIT))
	if (SUIT_getBoolean (o, SHRINK_TO_FIT)) {
	    int w, a, d, border;
	    GP_setFont (SUIT_getFont (o, FONT));
	    GP_setViewport(SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
	    GP_setWindow (OBJECT_WINDOW(o));
	    GP_inquireTextExtentWithoutMapping (SUIT_getText (o, LABEL), &w, &a, &d);
	    border = SUIT_getInteger (o, MARGIN);
	    SUIT_changeObjectSize (o, w+2*border, a+d+2*border);
	}
}


PRIVATE void AddChildren (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    SUIT_object menu = SUIT_getObject (o, "menu");
    if (SUIT_stringsMatch (propName, NUMBER_OF_CHILDREN)) {
	int num = * (int *) new;
	if (num == 1)
	    SUIT_addChildToObject (menu, SUIT_getChild(o, 0));
    }
    else if (SUIT_stringsMatch (propName, INTERACTIVELY_CREATED))
	SUIT_setBoolean (menu, INTERACTIVELY_CREATED, * (boolean *) new);
}


PRIVATE void PaintPulldown (SUIT_object o)
{
    GP_justifyText (SUIT_getText (o, LABEL), JUSTIFY_CENTER);
}


PRIVATE SUIT_object MenuThatIsDown = NULL;

PRIVATE SUIT_object BringMenuBackUp (SUIT_object o, SUIT_event *e)
{
    *e = SUIT_adjustEventForObject (*e, o, MenuThatIsDown);
    SUIT_setBoolean (MenuThatIsDown, STUCK_DOWN, FALSE);
    if (SUIT_stringsMatch(OBJECT_CLASS(SUIT_getParent(MenuThatIsDown)),"stacker"))
	SUIT_setBoolean (o, HAS_BORDER, FALSE);
    SUIT_unregisterTrapper();
    return MenuThatIsDown;
}



PRIVATE void HitPulldown (SUIT_object o, SUIT_event e)
{
    SUIT_object menu = SUIT_getObject (o, "menu");

    if (e.type == MOUSE_DOWN) {
	MoveMenuUnderButton (o, menu);
	SUIT_setBoolean (menu, VISIBLE, TRUE);
	SUIT_setBoolean (menu, STUCK_DOWN, FALSE);
	if (SUIT_stringsMatch(OBJECT_CLASS(SUIT_getParent(o)),"stacker"))
	    SUIT_setBoolean (o, HAS_BORDER, TRUE);
	GP_setCursor (RIGHT_ARROW_CURSOR);
	e.worldLocation.y = 1.5; /* we're above the menu */
	SUIT_hitObject (menu, e);
    } else if (e.type == CLICK || e.type == KEYSTROKE) {
	MoveMenuUnderButton (o, menu);
	SUIT_setBoolean (menu, VISIBLE, TRUE);
	SUIT_setBoolean (menu, STUCK_DOWN, TRUE);
	if (SUIT_stringsMatch(OBJECT_CLASS(SUIT_getParent(o)),"stacker"))
	    SUIT_setBoolean (o, HAS_BORDER, TRUE);
	GP_setCursor (RIGHT_ARROW_CURSOR);
	MenuThatIsDown = menu;
	SUIT_registerTrapper (BringMenuBackUp);
    }
}



SUIT_object SUIT_createPullDownMenu (char *name)
{
    SUIT_object o = SUIT_createObject (name, "pulldown menu");
    SUIT_object menu;
    static boolean firsttime = TRUE;

    SUIT_addDisplayToObject (o, "standard",  HitPulldown, PaintPulldown);
    SUIT_setBoolean (o, HAS_BORDER, TRUE);

    /* pausch hack: there's a chance that the "companian" will be already
       created, based on the order the .sui file and/or dumped code
       generates objects.  That's because the companian isn't really
       a child, it's just "associated."
       */
    if ((menu = SUIT_name(SUIT_relativeName(o, "menu"))) == NULL)
	menu = SUIT_createVerticalMenu (SUIT_relativeName(o, "menu"));
    SUIT_setBoolean (menu, STUCK_DOWN, FALSE);
    SUIT_setObject (o, "menu", menu);
    SUIT_setObject (menu, "button", o);
    SUIT_setBoolean (menu, VISIBLE, FALSE);

    SUIT_registerInterest (o, ShrinkToFit);
    SUIT_registerInterest (o, AddChildren);
    SUIT_setText (o, LABEL, name);
    if (firsttime) {
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, SHRINK_TO_FIT, CLASS);
	firsttime = FALSE;
    }
    return o;
}



SUIT_object SUIT_addToMenu (SUIT_object obj, char *name, void (*callback) (SUIT_object))
{
    SUIT_object button;

    button = SUIT_createButton (name, callback);
    SUIT_setEnumString (button, ACTIVE_DISPLAY, "standard");
    SUIT_addChildToObject (obj, button);
    return button;
}



SUIT_object SUIT_addToMenuWithHotKey (SUIT_object obj, char *name, void (*callback) (SUIT_object), char *hotkey)
{
    SUIT_object button;

    button = SUIT_createButton (name, callback);
    SUIT_setEnumString (button, ACTIVE_DISPLAY, "button with hotkey");
    SUIT_setText (button, HOTKEY, hotkey);
    SUIT_addChildToObject (obj, button);
    return button;
}
