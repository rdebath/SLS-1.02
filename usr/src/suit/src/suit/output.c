/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "privsuit.h"


#define STRING_BORDER       5
#define BETWEEN(X,X1,X2)	(((X) > (X1)) && ((X) < (X2)))

/* for selection handles */
#define HANDLE_WIDTH	8	
#define HANDLE_GRAVITY	2


void SUIT_clearScreen (void)
{
    rectangle screen;

    screen = SRGP_defRectangle (0, 0, SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL),
				SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL));
    GP_pushGraphicsState ();
    SRGP_setClipRectangle (screen);
    si_clearArea(screen);
    GP_popGraphicsState ();
}



/* This routine returns true if the two parameter viewports, vp and rect,
 * overlap on the screen. */

boolean SUIT_viewportsOverlap (SUIT_viewport a, SUIT_viewport b)
{

    if ( a.bottom_left.y > b.top_right.y )    /* if a is above b, they don't overlap */
	return(FALSE);

    if ( a.top_right.y < b.bottom_left.y )    /* if a is below b, they don't overlap */
	return(FALSE);
    
    if ( a.bottom_left.x > b.top_right.x )    /* if a is right of b, they don't overlap */
	return(FALSE);
    
    if ( a.top_right.x < b.bottom_left.x )    /* if a is left of b, they don't overlap */
	return(FALSE);

    return TRUE;                   /* otherwise they overlap */
}


/* This routine adds a border size to the size of a viewport */

rectangle ViewportPlusBorder (SUIT_object o, int width)
{
    rectangle vp;

    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT(o));
    if (!SUIT_getBoolean (o, DRAW_BORDER_ON_INSIDE)) {
	vp.bottom_left.x -= width;
	vp.bottom_left.y -= width;
	vp.top_right.x += width;
	vp.top_right.y += width;
    }
    return vp;
}



PRIVATE void DrawSimpleBorder (int width, rectangle vp, GP_color color)
{
    SRGP_setLineWidth (width);
    GP_setColor (color);
    vp.bottom_left.x += width/2;
    vp.bottom_left.y += width/2;
    vp.top_right.x -= width/2;
    vp.top_right.y -= width/2;
    SRGP_rectangle (vp);
}



#define USE_RECTANGLE_CUTOFF 2

void SUIT_borderObject (SUIT_object o)
{
    int width = SUIT_getInteger (o, BORDER_WIDTH);
    SUIT_viewport vp;
    char *bordertype = SUIT_getEnumString (o, BORDER_TYPE);

    ASSERT ((o != NULL), (mes, "SUIT_borderObject was called with a null object pointer.\n"));

    if (!SUIT_getBoolean(o, HAS_BORDER) || width <= 0)
	return;

    SRGP_setClipRectangle (CalculateClipRectangle(o));
    vp =  ViewportPlusBorder (o, width);

    GP_pushGraphicsState ();
    if (SUIT_stringsMatch(bordertype, "simple"))
	DrawSimpleBorder (width, vp, SUIT_getColor (o, BORDER_COLOR));
    else if (width < USE_RECTANGLE_CUTOFF)
	GP_unbeveledBorder (vp, SUIT_getColor (o, BORDER_COLOR),
			    SUIT_getBoolean (o, BORDER_RAISED), width);
    else if (SUIT_stringsMatch (bordertype, "motif"))
	GP_beveledBorder (vp, SUIT_getColor (o, BORDER_COLOR),
			  SUIT_getBoolean (o, BORDER_RAISED), width);
    else
	GP_fancyBeveledBorder (vp, SUIT_getColor (o, BORDER_COLOR), width);
    GP_popGraphicsState ();
}



void SUIT_eraseObject (SUIT_object o)
{
    SUIT_viewport vp;

    ASSERT ((o != NULL), (mes, "SUIT_eraseObject was called with a null object pointer.\n"));;
    /* vp = ViewportPlusBorder (o, SUIT_getInteger (o, BORDER_WIDTH)); */
    vp = CalculateClipRectangle(o);

    ENTER (2, (buf, "SUIT_eraseObject(%s)\n", OBJECT_NAME (o)));
    if (si_isVisible(o) && o->has_been_painted) {
#ifdef X_WINDOWS
	int can;
	if (SUIT_getBoolean (o, CACHE_USING_CANVAS) &&
	    SUIT_propertyExists (o, "canvas number", "int", OBJECT) && 
	    (can = SUIT_getInteger (o, "canvas number")) > 0) {
	    rectangle rect;
	    rect = ViewportPlusBorder (o, SUIT_getInteger (o, BORDER_WIDTH));
	    SRGP_copyPixel (can, SRGP_inquireCanvasExtent(can), rect.bottom_left);
	    SRGP_deleteCanvas (can);
	    SUIT_setInteger (o, "canvas number", -1);
	} else {
	    si_clearArea (vp);
	    SUIT_redisplayRequiredInRegion(vp);
	}
#else
	si_clearArea (vp);
	SUIT_redisplayRequiredInRegion(vp);
#endif
    }
    LEAVE (2, (buf, "SUIT_eraseObject(%s)\n", OBJECT_NAME (o)));
}



/* This routine erases all objects on the selected objects list. */

void si_eraseSelected (DynArray selectedObjects)
{
    int i;
    SUIT_object temp;

    for (i = DynHigh (selectedObjects); i >= DynLow (selectedObjects); i--) {
	temp = *((SUIT_object *) DynGet (selectedObjects, i));
	SUIT_eraseObject (temp);
    }
}



/* Given a dynarray of selected objects, this routine calculates and returns
 * the dimensions of the minimum rectangle on the screen that bounds all the
 * objects.
*/

rectangle si_getBoundingRectangle (DynArray objects)
{
    rectangle vp, retval;
    SUIT_object chase;
    int i;

    retval = SRGP_defRectangle (10000, 10000, -10000, -10000);
    for (i = 0; i < DynSize(objects); i++) {
	chase = * (SUIT_object *) DynGet (objects, i);
	vp = SUIT_mapViewportToScreen(chase, OBJECT_VIEWPORT(chase));
	if (vp.bottom_left.x < retval.bottom_left.x)
	    retval.bottom_left.x = vp.bottom_left.x;
	if (vp.bottom_left.y < retval.bottom_left.y)
	    retval.bottom_left.y = vp.bottom_left.y;
	if (vp.top_right.x > retval.top_right.x)
	    retval.top_right.x = vp.top_right.x;
	if (vp.top_right.y > retval.top_right.y)
	    retval.top_right.y = vp.top_right.y;
    }
    return retval;
}



SUIT_viewport SUIT_getScreenViewport (void)
{
    return SUIT_mapViewportToScreen(global.root, OBJECT_VIEWPORT(global.root));
}


/* This routine calls SUIT_moveRectangle with the object's viewport to move
 * the object, updates the screen, and sets the object's viewport property to
 * reflect the move. */

void si_moveObject (SUIT_object obj, deluxe_locator_measure mouse)
{
    SUIT_viewport newViewport, oldViewport;
    SUIT_object openObj;
    int mousex, mousey;
    char buf[500];

    ENTER (2, (buf, "si_moveObject(%s)\n", OBJECT_NAME (obj)));
    ASSERT ((obj != NULL), (mes, "si_moveObject was called with a null object pointer.\n"));
    GP_pushGraphicsState ();

    oldViewport = SUIT_mapViewportToScreen(obj, OBJECT_VIEWPORT(obj));
    newViewport = SUIT_moveRectangle (oldViewport, mouse.position, FALSE);

    mousex = mouse.position.x - oldViewport.bottom_left.x + newViewport.bottom_left.x;
    mousey = mouse.position.y - oldViewport.bottom_left.y + newViewport.bottom_left.y;

    SUIT_setViewport (obj, VIEWPORT, SUIT_mapScreenToViewport(obj, newViewport));
    SUIT_bringToFront(obj);

    openObj = si_mapPointToObject (OpenObjects, mousex, mousey);

    /* Did I just move the object into a different open object? If so, change its parent. */
    if (openObj != NULL && openObj != SUIT_getParent(obj)) {
	int j, whichChild = -1;
	/* Did we stop over a child? */
	for (j = 0; j < SUIT_numberOfChildren (openObj) && whichChild == -1; j++) {
	    SUIT_object kid = SUIT_getChild (openObj, j);
	    SUIT_viewport vp;
	    vp = SUIT_mapViewportToScreen(kid, OBJECT_VIEWPORT(kid));
	    if (vp.bottom_left.x <= mousex && mousex <= vp.top_right.x &&
		vp.bottom_left.y <= mousey && mousey <= vp.top_right.y)
		whichChild = j;
	}
	if (whichChild == -1)
	    SUIT_addChildToObject (openObj, obj);  /* If not, just add this object */
	else
	    si_addChildHere (openObj, obj, whichChild);
	SUIT_redisplayRequired (openObj);
    }
    else if (global.propertyEditorIsActive){
        if (si_overTrashCan(mousex, mousey)) {
            sprintf(buf, "Do you wish to delete the object %s?", obj->name);
	    if (SUIT_askYesNo(buf) == REPLY_YES) {
		SUIT_destroyObject (obj);
	    }
	} else if (si_overInfoButton(mousex, mousey)) { /* give info about object class */
	    char *help = SUIT_getHelp (OBJECT_CLASS(obj), "object create procedure");
	    char *buffer;
	    if (help == NULL)
		help = "Sorry, there is no help for objects of this class.";
	    buffer = (char *) SUIT_malloc (strlen(help)+100);
	    sprintf (buffer, "@b(Object): %s\n@b(Class): %s\n\n%s",
		     OBJECT_NAME(obj), OBJECT_CLASS(obj), help);
	    SUIT_inform (buffer);
	    SUIT_free(buffer);
	} else if (si_overExportButton(mousex, mousey))
	    SUIT_inform ("Exporting an object is not possible: only properties may be exported.");
    }

    GP_popGraphicsState ();
    LEAVE (2, (buf, "si_moveObject(%s)\n", OBJECT_NAME (obj)));
}


PRIVATE void HighLightSelectedWithNoCanvas (int howMany, rectangle *rArray, int xoffset, int yoffset)
{
    int i;

    GP_pushGraphicsState ();

    si_drawInHighlightStyle ();
    for (i = 0; i < howMany; i++)
	SRGP_rectangleCoord (rArray[i].bottom_left.x + xoffset,
			     rArray[i].bottom_left.y + yoffset,
			     rArray[i].top_right.x + xoffset,
			     rArray[i].top_right.y + yoffset);
    GP_popGraphicsState ();
}



/* This routine takes a dynarray of selected objects, highlights them, allows
 * the user to move them as a group around the screen, and then redraws and
 * updates them.
*/


void si_moveGroup (DynArray objects, deluxe_locator_measure original)
{
    rectangle boundingBox;
    point offset, bottomCorner;
    SUIT_object obj, openobj;
    int screenWidth = SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL);
    int screenHeight = SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL);
    SUIT_viewport vp, oldvp;
    deluxe_locator_measure current;
    rectangle *rectArray = (rectangle *) SUIT_malloc (sizeof(rectangle) * DynSize(objects));
    int i;

    GP_pushGraphicsState ();
    boundingBox = si_getBoundingRectangle (objects);
    if (boundingBox.top_right.x == boundingBox.bottom_left.x ||
	boundingBox.top_right.y == boundingBox.bottom_left.y)
	return;

    for (i=0; i < DynSize(objects); i++) {
	obj = * (SUIT_object *) DynGet (objects, i);
	rectArray[i] = SUIT_mapViewportToScreen (obj, OBJECT_VIEWPORT(obj));
    }

    bottomCorner = boundingBox.bottom_left;
    offset.x = original.position.x - boundingBox.bottom_left.x;
    offset.y = original.position.y - boundingBox.bottom_left.y;
    si_drawInHighlightStyle ();
    current = original;

    si_inSample ();
    while (si_buttonsAndModifiersEqual (original, current)) {
	HighLightSelectedWithNoCanvas (DynSize(objects), rectArray,
				       bottomCorner.x - boundingBox.bottom_left.x,
				       bottomCorner.y - boundingBox.bottom_left.y);
	bottomCorner.x = current.position.x - offset.x;
	bottomCorner.y = current.position.y - offset.y;
	if (bottomCorner.x < 0)
	    bottomCorner.x = 0;
	else if (boundingBox.top_right.x + bottomCorner.x - boundingBox.bottom_left.x > screenWidth)
	    bottomCorner.x = screenWidth - boundingBox.top_right.x + boundingBox.bottom_left.x;
	if (bottomCorner.y < 0)
	    bottomCorner.y = 0;
	else if (boundingBox.top_right.y + bottomCorner.y - boundingBox.bottom_left.y > screenHeight)
	    bottomCorner.y = screenHeight - boundingBox.top_right.y + boundingBox.bottom_left.y;
	HighLightSelectedWithNoCanvas (DynSize(objects), rectArray,
				       bottomCorner.x - boundingBox.bottom_left.x,
				       bottomCorner.y - boundingBox.bottom_left.y);
	current = si_waitForAnyChange (current, FALSE);
    }
    HighLightSelectedWithNoCanvas (DynSize(objects), rectArray,
				   bottomCorner.x - boundingBox.bottom_left.x,
				   bottomCorner.y - boundingBox.bottom_left.y);
    si_outSample ();
    
    openobj = si_mapPointToObject (OpenObjects, current.position.x, current.position.y);
    if (openobj == NULL) /* the user let the mouse button up outside our window */
	openobj = global.root;

    si_eraseSelected (objects);
    for (i = DynLow (objects); i < DynSize (objects); i++) {
	rectangle new;
	obj = * (SUIT_object *) DynGet (objects, i);
	oldvp = rectArray[i];
	new = SRGP_defRectangle (oldvp.bottom_left.x + (bottomCorner.x - boundingBox.bottom_left.x),
				 oldvp.bottom_left.y + (bottomCorner.y - boundingBox.bottom_left.y),
				 oldvp.top_right.x + (bottomCorner.x - boundingBox.bottom_left.x),
				 oldvp.top_right.y + (bottomCorner.y - boundingBox.bottom_left.y));
	vp = SUIT_mapScreenToViewport (obj, new);
	SUIT_redisplayRequiredInRegion (oldvp);
	SUIT_setViewport (obj, VIEWPORT, vp);
	SUIT_bringToFront (obj);
	if (openobj != SUIT_getParent (obj))
	    SUIT_addChildToObject (openobj, obj);

    }
    GP_popGraphicsState ();
    SUIT_free (rectArray);
}



/* This routine takes a dynarray of selected objects and marks them all as
* requiring redisplay.
*/

void si_repaintGroup (DynArray objects)
{
    SUIT_object chase;
    int i;

    for (i = DynLow (objects); i < DynSize (objects); i++) {
	chase = *((SUIT_object *) DynGet (objects, i));
	SUIT_redisplayRequired (chase);
    }
}



PRIVATE void DrawHandle (int x1, int y1, int x2, int y2)
{
    SRGP_setColor (SRGP_BLACK);
    SRGP_fillRectangleCoord (x1, y1, x2, y2);
    SRGP_setColor (SRGP_WHITE);
    SRGP_rectangleCoord (x1, y1, x2, y2);
}


PRIVATE void DrawHandles (rectangle r)
{
    int x1 = r.bottom_left.x;
    int y1 = r.bottom_left.y;
    int x2 = r.top_right.x;
    int y2 = r.top_right.y;
    int halfx = x1 + (x2 - x1) / 2;
    int halfy = y1 + (y2 - y1) / 2;

    DrawHandle (x1, y1, x1 + HANDLE_WIDTH, y1 + HANDLE_WIDTH);
    DrawHandle (halfx - HANDLE_WIDTH / 2, y1, halfx + HANDLE_WIDTH / 2, y1 + HANDLE_WIDTH);
    DrawHandle (x2 - HANDLE_WIDTH, y1, x2, y1 + HANDLE_WIDTH);

    DrawHandle (x1, halfy - HANDLE_WIDTH / 2, x1 + HANDLE_WIDTH, halfy + HANDLE_WIDTH / 2);
    DrawHandle (x2 - HANDLE_WIDTH, halfy - HANDLE_WIDTH / 2, x2, halfy + HANDLE_WIDTH / 2);

    DrawHandle (x1, y2 - HANDLE_WIDTH, x1 + HANDLE_WIDTH, y2);
    DrawHandle (halfx - HANDLE_WIDTH / 2, y2 - HANDLE_WIDTH, halfx + HANDLE_WIDTH / 2, y2);
    DrawHandle (x2 - HANDLE_WIDTH, y2 - HANDLE_WIDTH, x2, y2);
}



void si_drawInHighlightStyle (void)
{
    SRGP_setColor (1);
#if defined (IBM_PC) && !defined(_Windows)
    SRGP_setLineStyle (CONTINUOUS);
#else
    SRGP_setLineStyle (DOTTED);
#endif
    SRGP_setWriteMode (WRITE_XOR);
}



void si_highlightRectangle (rectangle r, boolean handles)
{

    GP_pushGraphicsState ();

    SRGP_rectangle (r);
    if (handles)
	DrawHandles (r);
    GP_popGraphicsState ();
}



void si_drawRectangleAsOpen (rectangle r)
{
    int x1 = r.bottom_left.x;
    int y1 = r.bottom_left.y;
    int x2 = r.top_right.x;
    int y2 = r.top_right.y;

    GP_pushGraphicsState ();

    GP_setColor (GP_defColor ("red", BLACK_ON_MONO));
#ifndef _Windows
    /* SRGP for MS Windows does not currently have patterns for pens*/
    GP_setPenStyle (BITMAP_PATTERN_OPAQUE);
    GP_setPenBitmapPattern (4);	/* a perfect every-other pixel */
#endif
    GP_setLineWidth (3);
    SRGP_rectangle (SRGP_defRectangle (x1 + 5, y1 + 5, x2 - 5, y2 - 5));

    GP_popGraphicsState ();
}



PRIVATE int SUIT_createCanvas (int x, int y)
{
    int canvas;

    SRGP_setErrorHandlingMode(NON_FATAL_ERRORS);
    canvas = SRGP_createCanvas (x, y);
    if (SRGP_errorOccurred != 0) { 
	global.NoCanvas = TRUE;
	SRGP_errorOccurred = 0; 
	canvas = -1;
    } else 
	global.NoCanvas = FALSE;
    SRGP_setErrorHandlingMode(FATAL_ERRORS);
    SRGP_useCanvas (0);

    return canvas;
}



int si_overHandle (SUIT_viewport r, point p)
{
    int x1 = r.bottom_left.x;
    int y1 = r.bottom_left.y;
    int x2 = r.top_right.x;
    int y2 = r.top_right.y;
    int width = x2 - x1;
    int height = y2 - y1;
    int handleWidth = HANDLE_WIDTH + HANDLE_GRAVITY;

    int leftx = (p.x > x1 && p.x < x1 + handleWidth);
    int rightx = (p.x > x2 - handleWidth && p.x < x2);
    int middlex = (p.x > x1 + width / 2 - handleWidth / 2 && p.x < x1 + width / 2 + handleWidth / 2);
    int bottomy = (p.y > y1 && p.y < y1 + handleWidth);
    int topy = (p.y > y2 - handleWidth && p.y < y2);
    int middley = (p.y > y1 + height / 2 - handleWidth / 2 && p.y < y1 + height / 2 + handleWidth / 2);
    int retval = NO_DIRECTION;

    if (leftx) {
	if (bottomy)
	    retval = LEFT_DIRECTION | BOTTOM_DIRECTION;
	else if (middley)
	    retval = LEFT_DIRECTION;
	else if (topy)
	    retval = LEFT_DIRECTION | TOP_DIRECTION;
    } else if (middlex) {
	if (bottomy)
	    retval = BOTTOM_DIRECTION;
	else if (topy)
	    retval = TOP_DIRECTION;
    } else if (rightx) {
	if (bottomy)
	    retval = RIGHT_DIRECTION | BOTTOM_DIRECTION;
	else if (middley)
	    retval = RIGHT_DIRECTION;
	else if (topy)
	    retval = RIGHT_DIRECTION | TOP_DIRECTION;
    }
    return retval;
}



point SUIT_mapRelativeLocationToScreen (SUIT_object o, point p)
{
    SUIT_viewport v;
    v = OBJECT_VIEWPORT(o);
    return SRGP_defPoint (o->offset.x + p.x + v.bottom_left.x,
			  o->offset.y + p.y + v.bottom_left.y);
}

point SUIT_mapScreenToRelativeLocation (SUIT_object o, point p)
{
    SUIT_viewport v;
    v = OBJECT_VIEWPORT(o);
    return SRGP_defPoint (p.x - (o->offset.x + v.bottom_left.x),
			  p.y - (o->offset.y + v.bottom_left.y) );
}


rectangle SUIT_mapViewportToScreen (SUIT_object o, SUIT_viewport vp)
{
    return SRGP_defRectangle (o->offset.x + vp.bottom_left.x, o->offset.y + vp.bottom_left.y,
			      o->offset.x + vp.top_right.x, o->offset.y + vp.top_right.y);
}



rectangle SUIT_mapScreenToViewport (SUIT_object o, rectangle scr)
{
    return SRGP_defRectangle (scr.bottom_left.x - o->offset.x, scr.bottom_left.y - o->offset.y,
			      scr.top_right.x - o->offset.x, scr.top_right.y - o->offset.y);
}



SUIT_viewport SUIT_mapToParent (SUIT_object o, double x1, double y1, double x2, double y2)
{
    SUIT_object parent = SUIT_getParent(o);
    int border = SUIT_getInteger (o, BORDER_WIDTH);
    rectangle pvp;
    rectangle r, newvp;

    GP_pushGraphicsState();
    GP_setWindow (SUIT_getWindow(parent, WINDOW));
    pvp = SUIT_mapViewportToScreen(parent, OBJECT_VIEWPORT(parent));
    pvp.bottom_left.x += border;   pvp.top_right.x -= border;
    pvp.bottom_left.y += border;   pvp.top_right.y -= border;
    GP_setViewport (pvp);
    r = GP_mapRectangle (GP_defRectangle (x1, y1, x2, y2));
    newvp = SUIT_mapScreenToViewport (o, r);
    GP_popGraphicsState();
    return newvp;
}



void SUIT_centerInParent (SUIT_object o, double centerx, double centery)
{
    SUIT_object parent = SUIT_getParent(o);
    rectangle r, newvp;
    GP_rectangle gvp;
    double width2, height2;

    GP_pushGraphicsState();
    GP_setWindow (SUIT_getWindow(parent, WINDOW));
    GP_setViewport (SUIT_getViewport(parent, VIEWPORT));
    gvp = GP_unMapRectangle (OBJECT_VIEWPORT(o));
    width2 = (gvp.top_right.x - gvp.bottom_left.x)/2.0;
    height2 = (gvp.top_right.y - gvp.bottom_left.y)/2.0;
    r = GP_mapRectangle (GP_defRectangle (centerx-width2, centery-height2, centerx+width2, centery+height2));
    newvp = SUIT_mapScreenToViewport (o, r);
    GP_popGraphicsState();
    SUIT_setViewport (o, VIEWPORT, newvp);
}



#ifdef PAUSCH_HAS_REMOVED_THIS
PRIVATE void handleWallPaper(SUIT_object o)
{
    
    char buf[1000];
    char *s;
    char *error;
    
    SUIT_callbackFunctionPtr paper = 
	(SUIT_callbackFunctionPtr) 
	    SUIT_getFunctionPointer(o, WALLPAPER_FUNCTION);
    s = SUIT_getText(o, WALLPAPER);
    
    if ( (paper != NULL) || (!SUIT_stringsMatch(s, "")) )
    {
	GP_pushGraphicsState ();
	if ( paper != NULL )
	    paper(o);
	if ( !SUIT_stringsMatch(s, "") )
	{
	    error = si_parseAndDrawWallPaper(o, s);
	    if ( error != NULL )
	    {
		sprintf(buf, "@b(Trouble occured while parsing wallpaper for object '%s'):\n\n%s", OBJECT_NAME(o), error);
	/* 	o->redisplay_required = FALSE; */ /* pausch hack */
		/* SUIT_inform(buf); */
		fprintf(stderr, buf);
	    }
	}
	GP_popGraphicsState ();
    }	
    
    
}
#endif


PRIVATE rectangle CalculateOverlap (SUIT_object obj, boolean countBorder)
{
    SUIT_object parent = SUIT_getParent(obj);
    int border = countBorder? SUIT_getInteger (obj, BORDER_WIDTH) : 0;
    rectangle vp, parentvp;
    rectangle retval;

    vp = SUIT_mapViewportToScreen (obj, OBJECT_VIEWPORT(obj));
    parentvp = SUIT_mapViewportToScreen (parent, OBJECT_VIEWPORT(parent));

    if (vp.top_right.x < parentvp.bottom_left.x || /* if there's no overlap */
	vp.bottom_left.x > parentvp.top_right.x ||
	vp.top_right.y < parentvp.bottom_left.y ||
	vp.bottom_left.y > parentvp.top_right.y)
	return SRGP_defRectangle(0,0,0,0);         /* return an empty clip rectangle */

    /* find the intersection of the viewport and the parent's viewport */
    retval.bottom_left.x = MAX(vp.bottom_left.x - border, parentvp.bottom_left.x);
    retval.bottom_left.y = MAX(vp.bottom_left.y - border, parentvp.bottom_left.y);
    retval.top_right.x = MIN(vp.top_right.x + border, parentvp.top_right.x);
    retval.top_right.y = MIN(vp.top_right.y + border, parentvp.top_right.y);
    return retval;
}



rectangle CalculateClipRectangle (SUIT_object obj)  { return CalculateOverlap (obj, TRUE); }
rectangle CalculateVisiblePortion (SUIT_object obj)  { return CalculateOverlap (obj, FALSE); }


void SUIT_paintObject (SUIT_object o)
{
    SUIT_display *display;
    boolean opti;
    
    ASSERT ((o != NULL), (mes, "SUIT_paintObject was called with a null object pointer.\n"));
    ENTER (2, (buf, "SUIT_paintObject (%s)\n", OBJECT_NAME (o)));
    
    GP_pushGraphicsState ();
    SRGP_setWriteMode (WRITE_REPLACE);
    
    display = si_getDisplay (o, SUIT_getEnumString(o, ACTIVE_DISPLAY));
    
    if (display != NULL && (display->paint != NULL) && si_isVisible(o)) {
	(void) GP_setFont (SUIT_getFont (o, FONT));
	
#ifdef X_WINDOWS
	if (!(o->has_been_painted) && SUIT_getBoolean (o, CACHE_USING_CANVAS)) {
	    rectangle rect;
	    int width, height, can;
	    rect = ViewportPlusBorder (o, SUIT_getInteger (o, BORDER_WIDTH));
	    width = rect.top_right.x - rect.bottom_left.x + 1;
	    height = rect.top_right.y - rect.bottom_left.y + 1;
	    if ((can = SUIT_createCanvas (width, height)) > 0) {
		SUIT_setInteger (o, "canvas number", can);
		SUIT_makePropertyTemporary (o, "canvas number", OBJECT);
		SUIT_lockProperty (o, "canvas number", OBJECT);
		SRGP_useCanvas (can);
		SRGP_copyPixel (0, rect, SRGP_defPoint(0,0));
		SRGP_useCanvas (0);
	    }
	}
#endif

	opti = OBJECT_OPTIMIZED(o);
	if (!opti && SUIT_getBoolean (o, HAS_BACKGROUND)) {
	    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	    /*SRGP_fillRectangle (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o))); */
	    SRGP_fillRectangle (CalculateVisiblePortion(o));
	}

	if (SUIT_getBoolean (o, CLIP_TO_VIEWPORT))
	    SRGP_setClipRectangle (CalculateVisiblePortion(o));

	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	GP_setWindow (SUIT_getWindow (o, WINDOW));
	GP_setViewport (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));

	global.currentlyPainting = TRUE;

	GP_pushGraphicsState();
	display->paint (o);
	GP_popGraphicsState();
	global.currentlyPainting = FALSE;

	if (!opti && SUIT_getBoolean (o, HAS_BORDER))
	    SUIT_borderObject (o);

	o->redisplay_required = FALSE;
	o->has_been_painted = TRUE;
    } else
	o->redisplay_required = FALSE;  /* This keeps ROOT from always needing painting */ 
    GP_popGraphicsState ();

    LEAVE (2, (buf, "SUIT_paintObject(%s)\n", OBJECT_NAME (o)));
}


void SUIT_paintChildren (SUIT_object o)
{
    int i;
    for (i = SUIT_numberOfChildren(o)-1; i >= 0; i--) {
	SUIT_object kid = SUIT_getChild (o, i);
	if (!OBJECT_OPTIMIZED(o))
	    OBJECT_OPTIMIZED(kid) = FALSE;
	SUIT_paintObject(kid);
	if (OBJECT_SELECTED(kid) && si_isVisible(kid))
	    si_highlightRectangle (SUIT_mapViewportToScreen(kid, OBJECT_VIEWPORT(kid)), TRUE);
	if (OBJECT_OPEN(kid) && si_isVisible(kid))
	    si_drawRectangleAsOpen (SUIT_mapViewportToScreen(kid, OBJECT_VIEWPORT(kid)));
    }
}


void SUIT_paintEmployees (SUIT_object o)
{
    char *disp = SUIT_getEnumString (o, ACTIVE_DISPLAY);
    int i;
    for (i = SUIT_numberOfEmployees(o, disp)-1; i >= 0; i--) {
	SUIT_object emp = SUIT_getEmployee (o, disp, i);
	if (!OBJECT_OPTIMIZED(o)) 
	    OBJECT_OPTIMIZED(emp) = FALSE;
	SUIT_paintObject(emp);
    }
}


void si_repaintScreen (void)
{
    SUIT_clearScreen();
    SUIT_allObjectsRequireRedisplay (NULL);
    SUIT_performRedisplay ();
}


PRIVATE rectangle ForceOnScreen (rectangle r)
{
    rectangle screen;
    int width, height;

    screen = SRGP_inquireCanvasExtent(0);
    width = r.top_right.x - r.bottom_left.x;
    height = r.top_right.y - r.bottom_left.y;

    if (r.bottom_left.x < 0)
	r.bottom_left.x = 0;
    else if (r.top_right.x > screen.top_right.x)
	r.bottom_left.x = screen.top_right.x - width;
    if (r.bottom_left.y < 0)
	r.bottom_left.y = 0;
    else if (r.top_right.y > screen.top_right.y)
	r.bottom_left.y = screen.top_right.y - height;
    r.top_right.x = r.bottom_left.x + width;
    r.top_right.y = r.bottom_left.y + height;
    return r;
}



SUIT_viewport SUIT_moveRectangle (SUIT_viewport original, point start, boolean allowOffScreen)
{
    rectangle current;
    deluxe_locator_measure currentTrigger;
    extern boolean IgnoreMouseUp;

    current = original;
    si_inSample ();
    currentTrigger = si_getLocatorMeasure ();

    GP_pushGraphicsState();
    si_drawInHighlightStyle ();
    si_highlightRectangle (current, FALSE);

    while (si_atLeastOneButtonDown (currentTrigger)) {
	int deltaX = currentTrigger.position.x - start.x;
	int deltaY = currentTrigger.position.y - start.y;
	si_highlightRectangle (current, FALSE);
	current = original;
	current.bottom_left.x += deltaX;
	current.top_right.x += deltaX;
	current.bottom_left.y += deltaY;
	current.top_right.y += deltaY;
	if (!allowOffScreen)
	    current = ForceOnScreen (current);
	si_highlightRectangle (current, FALSE);
	currentTrigger = si_waitForAnyChange (currentTrigger, FALSE);
    }
    si_highlightRectangle (current, FALSE);
    GP_popGraphicsState();
    si_outSample (); 

    IgnoreMouseUp = global.currentlyInHitProcedure;

    return current;
}



PRIVATE void si_highlightRectangleInDirection (rectangle r, int direction)
{
    int x1 = r.bottom_left.x;
    int y1 = r.bottom_left.y;
    int x2 = r.top_right.x;
    int y2 = r.top_right.y;
    int halfx = x1 + (x2 - x1) / 2;
    int halfy = y1 + (y2 - y1) / 2;

    GP_pushGraphicsState ();

    si_drawInHighlightStyle ();
    SRGP_rectangle (r);

    if (direction & LEFT_DIRECTION && direction & BOTTOM_DIRECTION) {
	SRGP_fillRectangleCoord (x1, y1, x1 + HANDLE_WIDTH, y1 + HANDLE_WIDTH);
	goto leave;
    }
    if (direction & LEFT_DIRECTION && direction & TOP_DIRECTION) {
	SRGP_fillRectangleCoord (x1, y2 - HANDLE_WIDTH, x1 + HANDLE_WIDTH, y2);
	goto leave;
    }
    if (direction & LEFT_DIRECTION) {
	SRGP_fillRectangleCoord (x1, halfy - HANDLE_WIDTH / 2, x1 + HANDLE_WIDTH, halfy + HANDLE_WIDTH / 2);
	goto leave;
    }
    if (direction & RIGHT_DIRECTION && direction & TOP_DIRECTION) {
	SRGP_fillRectangleCoord (x2 - HANDLE_WIDTH, y2 - HANDLE_WIDTH, x2, y2);
	goto leave;
    }
    if (direction & RIGHT_DIRECTION && direction & BOTTOM_DIRECTION) {
	SRGP_fillRectangleCoord (x2 - HANDLE_WIDTH, y1, x2, y1 + HANDLE_WIDTH);
	goto leave;
    }
    if (direction & RIGHT_DIRECTION) {
	SRGP_fillRectangleCoord (x2 - HANDLE_WIDTH, halfy - HANDLE_WIDTH / 2, x2, halfy + HANDLE_WIDTH / 2);
	goto leave;
    }
    if (direction & BOTTOM_DIRECTION) {
	SRGP_fillRectangleCoord (halfx - HANDLE_WIDTH / 2, y1, halfx + HANDLE_WIDTH / 2, y1 + HANDLE_WIDTH);
	goto leave;
    }
    if (direction & TOP_DIRECTION) {
	SRGP_fillRectangleCoord (halfx - HANDLE_WIDTH / 2, y2 - HANDLE_WIDTH, halfx + HANDLE_WIDTH / 2, y2);
	goto leave;
    }
leave:
    GP_popGraphicsState ();
}



/* This routine resizes the viewport rectangle until the mouse buttons change
 * state, indicating the user is satisfied. */

SUIT_viewport si_resizeRectangleInDirection (SUIT_viewport original, int direction)
{
    rectangle current;
    deluxe_locator_measure startingTrigger, currentTrigger;
    current = original;
    si_inSample ();

    startingTrigger = si_getLocatorMeasure ();

    GP_pushGraphicsState();
    si_highlightRectangle (original, TRUE);
    si_drawInHighlightStyle ();

    currentTrigger = startingTrigger;
    while (si_buttonsEqual (currentTrigger, startingTrigger)) {
	si_highlightRectangleInDirection (current, direction);

	current.bottom_left.x = (direction & LEFT_DIRECTION) ?
	    MIN (currentTrigger.position.x, original.top_right.x - 3 * HANDLE_WIDTH) : original.bottom_left.x;
	current.top_right.x = (direction & RIGHT_DIRECTION) ?
	    MAX (currentTrigger.position.x, original.bottom_left.x + 3 * HANDLE_WIDTH) : original.top_right.x;
	current.bottom_left.y = (direction & BOTTOM_DIRECTION) ?
	    MIN (currentTrigger.position.y, original.top_right.y - 3 * HANDLE_WIDTH) : original.bottom_left.y;
	current.top_right.y = (direction & TOP_DIRECTION) ?
	    MAX (currentTrigger.position.y, original.bottom_left.y + 3 * HANDLE_WIDTH) : original.top_right.y;

	si_highlightRectangleInDirection (current, direction);
	currentTrigger = si_waitForAnyChange (currentTrigger, FALSE);
    }
    si_highlightRectangleInDirection (current, direction);
    /* si_highlightRectangle(current,TRUE); */
    GP_popGraphicsState();
    si_outSample ();
    return (current);
}



SUIT_viewport SUIT_resizeRectangle (SUIT_viewport original)
{
    deluxe_locator_measure mouse;

    GP_pushGraphicsState();
    si_highlightRectangle (original, TRUE); 

    si_outSample ();
    if (SRGP_waitEvent (INDEFINITE) == KEYBOARD)
	while (SRGP_waitEvent (INDEFINITE) == KEYBOARD) ;
    SRGP_getDeluxeLocator (&mouse);
    return si_resizeRectangleInDirection (original, si_overHandle (original, mouse.position));
}



void si_resizeObject (SUIT_object o, int direction)
{
    rectangle newRect;

    ASSERT ((o != NULL), (mes, "si_resizeObject was called with a null object pointer.\n"));
    ENTER (2, (buf, "si_resizeObject(%s)", OBJECT_NAME (o)));
    newRect = si_resizeRectangleInDirection (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)), direction);
    if (SUIT_getBoolean (o, SHRINK_TO_FIT) && !SUIT_stringsMatch(OBJECT_CLASS(o),"type in box")) {
	SUIT_inform ("Sorry, this object may not be resized, since the property SHRINK_TO_FIT is TRUE for this object.");
	SUIT_redisplayRequired(o);
    } else {
	SUIT_eraseObject (o);
	SUIT_setViewport (o, VIEWPORT, SUIT_mapScreenToViewport(o, newRect));
    }
    GP_popGraphicsState();
    LEAVE (2, (buf, "si_resizeObject(%s)", OBJECT_NAME (o)));
}



void si_resizeGroup (DynArray objects, int direction)
{
    int topx, topy, bottomx, bottomy;
    int oldtopx, oldtopy, oldbottomx, oldbottomy;
    int newX1, newX2, newY1, newY2;
    int i;
    SUIT_object chase;
    double ratiox, ratioy;
    rectangle newRect, boundingBox;
    point locator;
    int width, height;
    SUIT_viewport vp, chase_vp;

    ENTER (2, (buf, "resize _group()\n"));

    boundingBox = si_getBoundingRectangle (objects);
    bottomx = boundingBox.bottom_left.x;
    bottomy = boundingBox.bottom_left.y;
    topx = boundingBox.top_right.x;
    topy = boundingBox.top_right.y;

    oldbottomx = bottomx;
    oldbottomy = bottomy;
    oldtopx = topx;
    oldtopy = topy;
    locator.x = (bottomx + topx) / 2;
    locator.y = (bottomy + topy) / 2;

    if (direction & LEFT_DIRECTION)
	locator.x = bottomx;
    if (direction & RIGHT_DIRECTION)
	locator.x = topx;
    if (direction & BOTTOM_DIRECTION)
	locator.y = bottomy;
    if (direction & TOP_DIRECTION)
	locator.y = topy;
    SRGP_setLocatorMeasure (locator);

    vp = SRGP_defRectangle (bottomx, bottomy, topx, topy);
    /* si_highlightRectangle(vp,TRUE); */
    newRect = si_resizeRectangleInDirection (vp, direction);
    bottomx = newRect.bottom_left.x;
    bottomy = newRect.bottom_left.y;
    topx = newRect.top_right.x;
    topy = newRect.top_right.y;
    ratiox = (topx - bottomx) / (double) (oldtopx - oldbottomx);
    ratioy = (topy - bottomy) / (double) (oldtopy - oldbottomy);

    for (i = DynLow (objects); i < DynSize (objects); i++) {
	chase = *((SUIT_object *) DynGet (objects, i));
	chase_vp = SUIT_mapViewportToScreen(chase, OBJECT_VIEWPORT(chase));
	width = chase_vp.top_right.x - chase_vp.bottom_left.x;
	height = chase_vp.top_right.y - chase_vp.bottom_left.y;

	newX1 = ratiox * (chase_vp.bottom_left.x - oldbottomx) + bottomx;
	newY1 = ratioy * (chase_vp.bottom_left.y - oldbottomy) + bottomy;
	newX2 = width * ratiox + newX1;
	newY2 = height * ratioy + newY1;

	SUIT_eraseObject (chase);
	SUIT_setViewport (chase, VIEWPORT, 
			  SUIT_mapScreenToViewport(chase, SRGP_defRectangle (newX1, newY1, newX2, newY2)));
	SUIT_redisplayRequired (chase);
    }

    si_clearArea(vp);
    SUIT_redisplayRequiredInRegion (vp);
    LEAVE (2, (buf, "resize_group()\n"));
}



/* This routine resets an object's size. */

void SUIT_changeObjectSize (SUIT_object o, int width, int height)
{
    SUIT_viewport vp;
    int obj_width, obj_height;
    
    vp = OBJECT_VIEWPORT(o);
    obj_width = vp.top_right.x - vp.bottom_left.x;
    obj_height = vp.top_right.y - vp.bottom_left.y;

    vp.bottom_left.x -= (width - obj_width) / 2;
    vp.bottom_left.y -= (height - obj_height) / 2;
    vp.top_right.x = vp.bottom_left.x + width;
    vp.top_right.y = vp.bottom_left.y + height;
    SUIT_setViewport (o, VIEWPORT, vp);
}



/* This routine changes an object's width while maintaining its aspect ratio. */

void SUIT_changeWidthPreservingRatio (SUIT_object o, int width)
{
    SUIT_viewport vp;
    int obj_width, obj_height;

    vp = SUIT_getViewport (o, VIEWPORT);
    obj_width = vp.top_right.x - vp.bottom_left.x;
    obj_height = vp.top_right.y - vp.bottom_left.y;
    SUIT_changeObjectSize (o, width, width * obj_height / obj_width);
}



/* Similarly, this changes the height while preserving the aspect ratio. */

void SUIT_changeHeightPreservingRatio (SUIT_object o, int height)
{
    SUIT_viewport vp;
    int obj_width, obj_height;

    vp = SUIT_getViewport (o, VIEWPORT);
    obj_width = vp.top_right.x - vp.bottom_left.x;
    obj_height = vp.top_right.y - vp.bottom_left.y;
    SUIT_changeObjectSize (o, height * obj_width / obj_height, height);
}
