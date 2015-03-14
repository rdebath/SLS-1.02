/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "privsuit.h"
#include "scrolbox.h"


#define DRAWING_VIEWPORT "drawing viewport"
#define LEVEL "level"
#define TEMPORARY_PROPERTY_COLOR GP_defColor("white",BLACK_ON_MONO)

/* The property editor maintains some variables, global to this file, which
 * maintain state during an editing session.  They are packed into a single
 * record to indicate that they are "property editor global"
 * 
 * objectBeingEdited: application program object which the property editor was
 * invoked for.
 * 
 * objectsUserCanDropPropertiesOn: the objects (trash can, expose, etc.) are the
 * objects which it makes sense for the user to "drop" a property into.
 * 
 * activeWidget: when the user is altering a property which requires bringing up
 * another widget. (most commonly a Text box, but other widgets can be used,
 * for example for springiness or font).  For all but the text box, this
 * widget is wrapped in the variable 'activeWrapper' which puts an OK/CANCEL
 * dialog box around the widget. Therefore, if peGlobal.activeWrapper is
 * NULL, we know we're using a simple text box. */
    
typedef struct {
    SUIT_object objectBeingEdited;
    char objectsName[500];
    property *propertyBeingEdited;
    int levelBeingEdited;
    DynArray objectsUserCanDropPropertiesOn;
    DynArray exportedObjects;
    SUIT_object activeWidget;
    SUIT_object activeWrapper;
    boolean doneWithTypeSpecific;
    boolean typeSpecificUserPressedOK;
} propertyEditorGlobalVariables;

propertyEditorGlobalVariables peGlobal;
    
    
SUIT_object ObjectBeingEdited(void) { return peGlobal.objectBeingEdited; }
SUIT_object EditorActiveWidget(void) { return peGlobal.activeWidget; }

PRIVATE void MakeVisibleWithin (SUIT_object obj, boolean visible)
{
    SUIT_setBoolean (obj, VISIBLE_WITHIN_PROPERTY_EDITOR, visible);
    SUIT_makePropertyTemporary (obj, VISIBLE_WITHIN_PROPERTY_EDITOR, OBJECT);
}



PRIVATE void MakeStandardColors (SUIT_object o)
{
    SUIT_setColor (o, FOREGROUND_COLOR, GP_defColor("black",BLACK_ON_MONO));
    SUIT_setColor (o, BACKGROUND_COLOR, GP_defColor("grey",WHITE_ON_MONO));
    SUIT_setColor (o, BORDER_COLOR, GP_defColor("grey",BLACK_ON_MONO));
}



/* returns NULL if users CANCELs out */
PRIVATE Pointer PutUpTypeSpecificWidget (SUIT_object o, char *type)
{
    SUIT_viewport vp;
    int width, height;
    Pointer retval = NULL;
    
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    width = vp.top_right.x - vp.bottom_left.x;
    height = vp.top_right.y - vp.bottom_left.y;
    ASSERT ((o != NULL), (mes, "PutUpTypeSpecificWidget recieved a NULL object.\n"));
    peGlobal.activeWidget = SUIT_createOKCancelDialogBox ("property editor gizmo", o, NULL);
    SUIT_setText (o, "property type", type);
    SUIT_forceOnScreen(o);
    MakeStandardColors (peGlobal.activeWidget);
    MakeVisibleWithin (peGlobal.activeWidget, TRUE);
    
    if (width == SUIT_deluxeGetInteger (NULL, DEFAULT_OBJECT_WIDTH, GLOBAL) &&
	height == SUIT_deluxeGetInteger (NULL, DEFAULT_OBJECT_HEIGHT, GLOBAL)) {
	int w, a, d;
	GP_setFont (SUIT_getFont (peGlobal.activeWidget, SUIT_SYSTEM_FONT));
	SRGP_inquireTextExtent ("Cancel", &w, &a, &d);
	SUIT_changeObjectSize (peGlobal.activeWidget, 10 * w, 10 * (a + d));	/* guess at a good size */
    }
    
    if (SUIT_stringsMatch (type, "SUIT_textList") || SUIT_stringsMatch (type, "text"))
	SUIT_setBoolean (peGlobal.activeWidget, GRAB_RETURN_KEY, FALSE);

    SUIT_setText (o, CHAINED_TO_OBJECT, OBJECT_NAME(peGlobal.objectBeingEdited));

    if (SUIT_activateDialogBox (peGlobal.activeWidget) == REPLY_OK) {
	boolean error;
	if (SUIT_stringsMatch(type, "SUIT_textList"))
	    retval = si_readTextList (SUIT_getText (o, CURRENT_VALUE), &error);
	else if (SUIT_stringsMatch(type, "SUIT_functionPointer"))
	    retval = si_readFunctionPtr (SUIT_getText (o, CURRENT_VALUE), &error);
	else
	    retval = si_getType (type)->copy (SUIT_getProperty (o, CURRENT_VALUE, type));
    }
    SUIT_destroyObject (peGlobal.activeWidget);
    peGlobal.activeWidget = NULL;
    peGlobal.propertyBeingEdited = NULL;
    return retval;
}



PRIVATE void setPropertyAndDestroyActiveWidget (SUIT_object o)
{
    Pointer newVal;
    SUIT_level level;
    property *temp;
    
    if (SUIT_stringsMatch(OBJECT_CLASS(o), "dialog box")) {
	o = SUIT_getChild (o, 3); /* get the object we're really interested in */
    }
    temp = si_searchForPropAtLevels (o, CURRENT_VALUE, &level);
    ASSERT ((level == OBJECT), (mes, "setPropertyAndDestroyActiveWidget could not find property CURRENT_VALUE.\n"));
    
    newVal = SUIT_convertType (temp->value, temp->type, peGlobal.propertyBeingEdited->type);
    
    if (newVal != NULL)
	/* set the property to the new value */
	SUIT_setProperty (peGlobal.objectBeingEdited, peGlobal.propertyBeingEdited->name, 
			  peGlobal.propertyBeingEdited->type, newVal, peGlobal.levelBeingEdited);
    
    /* now get rid of the active widget */
    SUIT_destroyObject (peGlobal.activeWidget);
    if (peGlobal.activeWrapper != NULL)
	SUIT_destroyObject (peGlobal.activeWrapper);
    peGlobal.activeWidget = NULL;
    peGlobal.activeWrapper = NULL;
    peGlobal.propertyBeingEdited = NULL;
}


/* this routine is called any time the property editor suspects that the
 * ActiveWidget (either a type in box, or a type-specific widget) is
 * currently on-screen and it could be dangerous for it to stay there.  For
 * example, if a property is about to be copied, that could be bad news, so
 * we just flush first. */
    
PRIVATE void FlushActiveWidgetIfNecessary (void)
{
    if (peGlobal.activeWidget != NULL)
	setPropertyAndDestroyActiveWidget (peGlobal.activeWidget);
}


PRIVATE void ScrollbarCallback (SUIT_object sbar)
{
    FlushActiveWidgetIfNecessary();
}


void ChainedPropertyCallback (SUIT_object o)
{
    property *p = si_searchForProperty (o, OBJECT, CHAINED_TO_OBJECT);
    char *objectName = (p == NULL)? "" : (char *) p->value;
    SUIT_object chainedToObject = SUIT_name (objectName);
    char *propertyName = SUIT_getText (o, CHAINED_TO_PROPERTY);
    char *toType = SUIT_getText (o, CHAINED_TO_PROPERTY_TYPE);
    char *fromType = SUIT_getText (o, CHAINED_FROM_PROPERTY_TYPE);
    Pointer value, convertedValue;
    
    if (SUIT_name (objectName) == NULL) {
	char buf[1000];
	sprintf (buf, "Object \"%s\" controled a property of object \"%s\", but \"%s\" has been destroyed.", OBJECT_NAME (o), objectName, objectName);
	SUIT_inform (buf);
	return;
    }

    value = SUIT_getProperty (o, CURRENT_VALUE, fromType);
    convertedValue = SUIT_convertType (value, fromType, toType);
    if (convertedValue != NULL) {
	SUIT_setProperty (chainedToObject, propertyName, toType, convertedValue, OBJECT);
    }
}



#define MY_BLEND(NEW,OLD,FRACT) ( ((NEW)*FRACT) + ((OLD)*(1.0-(FRACT))) )

void si_animateScreenBorderToObject(SUIT_object o, long currentTime, long totalTime)

{
    
    static	rectangle oldR;		/* original, old rectangle */
    static	rectangle newR;		/* new, final rectangle */
    static	rectangle prevR;

    double	fract;
    rectangle	r;

    if ( currentTime == 0 )	/* first time through */
    {		
	oldR = SUIT_getScreenViewport();
	newR = OBJECT_VIEWPORT(o);
	prevR = oldR;
	SRGP_rectangle(prevR);	
	SRGP_refresh();
	return;
    }

    fract = ( (double) currentTime) / totalTime;
    r.bottom_left.x = MY_BLEND(newR.bottom_left.x,oldR.bottom_left.x,fract);
    r.bottom_left.y = MY_BLEND(newR.bottom_left.y,oldR.bottom_left.y,fract);
    r.top_right.x = MY_BLEND(newR.top_right.x,oldR.top_right.x,fract);
    r.top_right.y = MY_BLEND(newR.top_right.y,oldR.top_right.y,fract);
    
    if ( !SUIT_viewportsEqual(prevR,r) )
    {
	SRGP_rectangle(prevR);	
	SRGP_rectangle(r);	
	SRGP_refresh();
	prevR = r;
    }

}



PRIVATE void moveTheTruck(SUIT_object o, long currentTime, long totalTime)
{
    
    static 	int		prevOffset = 0;
    static 	rectangle	startRect;

    int		currentOffset;
    rectangle	curRect;
    int		width;
    double	timeFraction;
    static 	point	p;    

    if ( currentTime == 0 ) /* first call */
    {
	prevOffset = 0;
	startRect = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
	SRGP_setClipRectangle(startRect);
	return;
    }
    
    timeFraction = ( (double) currentTime) / totalTime;
    width = (startRect.top_right.x - startRect.bottom_left.x) + 1;

    currentOffset = timeFraction * ( (double) (width - 1) );
    
    if ( currentOffset == prevOffset )
	return;

    curRect = startRect;
    curRect.bottom_left.x += prevOffset;
    p.x = startRect.bottom_left.x + currentOffset;
    p.y = startRect.bottom_left.y;

    SRGP_copyPixel(0, curRect, p);
    SRGP_refresh();	/* force it to happen on screen */
    prevOffset = currentOffset;
}
    
    
PRIVATE void doExposureAnimation(SUIT_object newGuy)
{
    
    /* the animations run a little faster after the first time */
    static	boolean firstTime = TRUE;
    SUIT_object o = SUIT_name("property editor export button");
    
    GP_pushGraphicsState();
    si_animateOverTime(moveTheTruck, o, firstTime ? 1000 : 500); 
    GP_popGraphicsState();		       
    
    SUIT_redisplayRequired(o); /* clean up after ourselves */
    
    /* now draw attention to the new guy */

    GP_pushGraphicsState();
    SRGP_setColor(1);
    SRGP_setWriteMode (WRITE_XOR);
    si_animateOverTime(si_animateScreenBorderToObject, newGuy, firstTime ? 1000 : 500);
    GP_popGraphicsState();		       

    firstTime = FALSE;

}



PRIVATE void actuallyExposeProperty (SUIT_object o, property * p)
{
    
    SUIT_object new = NULL;
    char buf[1000];
    SUIT_type *suitType;
    char *widgetClass, *expoName;
    char *chainedFromType = p->type;
    double *temp;
       
    suitType = si_getType (p->type);
    
    widgetClass = suitType->widgetClass;
    
    /* if he doesn't have any way to export himself, let him use a text box,
     * which is better than nothing */
    if (widgetClass == NULL || SUIT_stringsMatch(p->type,"SUIT_textList")) {
	sprintf (buf, "Sorry, there is no easy way to export a property of that type (@b(%s)).  Would you like to export it as a text box?", p->type);
	if (SUIT_askYesNo (buf) == REPLY_YES) {
	    widgetClass = "type in box";
	    chainedFromType = "text";
	} else
	    return;
    }
    sprintf (buf, "%s's \"%s\"", OBJECT_NAME (o), p->name);
    if (SUIT_stringsMatch (p->type, "int"))
	chainedFromType = "double";
    
    new = SUIT_createObjectByClass (buf, widgetClass);
    ASSERT ((new != NULL), (mes, "actuallyExposeProperty: got back NULL\n"));
    expoName = SUIT_copyString(OBJECT_NAME(new));
    DynAdd (peGlobal.exportedObjects, (void *)&expoName);
    
    SUIT_centerObjectOnScreen (new);
    MakeVisibleWithin (new, TRUE);
    
    SUIT_setFunctionPointer (new, CALLBACK_FUNCTION, (SUIT_functionPointer) ChainedPropertyCallback);
    SUIT_makePropertyPermanent (new, CALLBACK_FUNCTION, OBJECT);
    
    SUIT_setText (new, CHAINED_TO_OBJECT, OBJECT_NAME (o));
    SUIT_setText (new, CHAINED_TO_PROPERTY, p->name);
    SUIT_setText (new, CHAINED_TO_PROPERTY_TYPE, p->type);
    SUIT_setText (new, CHAINED_FROM_PROPERTY_TYPE, chainedFromType);
    SUIT_setBoolean (new, INTERACTIVELY_CREATED, TRUE);
    SUIT_makePropertyTemporary (new, INTERACTIVELY_CREATED, OBJECT);
    
    if (SUIT_stringsMatch (p->type, "int"))
	SUIT_setDouble (new, GRANULARITY, 1.0);
    
    temp = (double *) SUIT_copyData (SUIT_convertType (p->value, p->type, 
				     chainedFromType), sizeof (double));
    if (SUIT_stringsMatch (widgetClass, "bounded value")) {
	double value = * (double *) temp;
	SUIT_setDouble (new, MINIMUM_VALUE, value - 10.0);
	SUIT_setDouble (new, MAXIMUM_VALUE, value + 10.0);
    }
    SUIT_setProperty (new, CURRENT_VALUE, chainedFromType,
		      (Pointer) SUIT_convertType (p->value, p->type, chainedFromType), OBJECT);
    
    if (SUIT_stringsMatch (widgetClass, "type in box"))
	SUIT_setBoolean (new, ANY_KEYSTROKE_TRIGGERS, TRUE);
    
    p->locked = TRUE;
    doExposureAnimation(new);
    SUIT_redisplayRequired(SUIT_name("object properties"));
}



PRIVATE void exposeProperty (SUIT_object o, char *propertyName, char *propertyType)
{
    SUIT_level level;
    property *p = si_searchForPropAtLevels (o, propertyName, &level);
    
    if (p->locked) {
	SUIT_inform("Sorry, that property is already exported.");
	return;
    }
    FlushActiveWidgetIfNecessary ();
    actuallyExposeProperty (o, p);
    
}


PRIVATE void DrawLock (int x, int y, GP_color lcolor)
{
    GP_setColor (lcolor);
    SRGP_fillRectangleCoord (x, y, x + 6, y + 5);
    SRGP_ellipseArc (SRGP_defRectangle (x, y + 1, x + 6, y + 9), 0.0, 180.0);
    SRGP_setColor (SRGP_WHITE);
    SRGP_pointCoord (x + 3, y + 3);
}



PRIVATE int fillBufferWithPropertyPrefix (char *name_string, property * prop)
{
    int width, height, descent, i;
    
    strcpy (name_string, prop->name);
    for (i=0; i < strlen(name_string); i++)
	if (name_string[i] >= 'a' && name_string[i] <= 'z')
	    name_string[i] += 'A'-'a';
	else if (name_string[i] == ' ')
	    name_string[i] = '_';
    if (!(prop->permanent))
	strcat (name_string, " (temporary)");
    strcat (name_string, ": ");
    SRGP_inquireTextExtent (name_string, &width, &height, &descent);
    return width;
}



#define LOCK_MARGIN 9
PRIVATE void DisplayPropertyOnScreen (SUIT_object o, property * prop, SUIT_viewport vp)
{
    int x = vp.bottom_left.x;
    int y = vp.bottom_left.y;
    
    int width;
    char *value_string;
    char name_string[1000];
    char *color_string;
    GP_color	backgroundColor;
    
    value_string = si_getType (prop->type)->convertToAscii (prop->value);
    if (prop->locked)
	DrawLock (x, y, GP_defColor ("black", BLACK_ON_MONO));
    
    x += LOCK_MARGIN;
    width = fillBufferWithPropertyPrefix (name_string, prop);
    
    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    if (!(prop->permanent) && (SRGP_inquireCanvasDepth () != 1))
	GP_setColor (TEMPORARY_PROPERTY_COLOR);
    
    SRGP_text (SRGP_defPoint (x, y), name_string);
    
    x += width;
    
    if (SUIT_caseInsensitiveMatch (prop->type, "GP_color")) {
	GP_color col;
	col = * (GP_color *) prop->value;
	if (BILEVEL_DISPLAY)
	    SRGP_text (SRGP_defPoint (x, y), col.blackOnMonochrome? "black" : "white");
	else {
	    /* draw in its own color, unless this matches the background */
	    backgroundColor = SUIT_getColor (o, "background color");
	    if ((col.colorName != NULL) && (col.colorName != ""))
		if (!SUIT_caseInsensitiveMatch (col.colorName, backgroundColor.colorName))
		    GP_setColor (col);
	    
	    color_string = col.colorName;
	    SRGP_text (SRGP_defPoint (x, y), color_string);
	}
    } else
	SRGP_text (SRGP_defPoint (x, y), value_string);
}



/* CompareProps is used by the DynQsort function to sort the property list */
PRIVATE int CompareProps (property * first, property * second)
{
    return (SUIT_caseInsensitiveCompare (first->name, second->name));
}


PRIVATE void DrawProperty (SUIT_object o, int num, SUIT_viewport vp)
{
    DynArray theList = SUIT_getDynArray (SUIT_getParent(o), LIST);
    property *temp = (property *) DynGet (theList, num);
    DisplayPropertyOnScreen (o, temp, vp);
}



PRIVATE void paintPropEd (SUIT_object o)
{
    int i;
    DynArray origList, myList;
    boolean showTemporaryProperties;
    SUIT_object parent = SUIT_getParent(o);
    
    origList = si_getPropertyList (peGlobal.objectBeingEdited, SUIT_getInteger (o, LEVEL));
    myList = DynCreate (sizeof (property), DynSize (origList));
    showTemporaryProperties = SUIT_deluxeGetBoolean (NULL, SHOW_TEMPORARY_PROPERTIES, GLOBAL);
    
    for (i = DynLow (origList); i <= DynHigh (origList); i++) {
	property *temp = (property *) DynGet (origList, i);
	if (showTemporaryProperties || temp->permanent)
	    DynAdd (myList, (void *) temp);
    }
    
    DynQsort (myList, DynLow (myList), DynHigh (myList), CompareProps);
    SUIT_suspendMarkingRedisplay (parent);
    SUIT_setDynArray (parent, LIST, myList);
    SUIT_makePropertyTemporary (parent, LIST, OBJECT);
    SUIT_resumeMarkingRedisplay (parent);
    sb_makeListConsistent (o);
    sb_genericPaintScrollableBox (o, DrawProperty);
    o->redisplay_required = FALSE;	/* minor hack, but necessary */
}


PRIVATE SUIT_object CatchKeys (SUIT_object o, SUIT_event *ev)
{
    if (ev->type == KEYSTROKE) {
	if (ev->keyboard == 27) { /* the user typed Escape */
	    SUIT_destroyObject (peGlobal.activeWidget);
	    if (peGlobal.activeWrapper != NULL)
    		SUIT_destroyObject (peGlobal.activeWrapper);
	    peGlobal.activeWidget = NULL;
	    peGlobal.activeWrapper = NULL;
	}
	else if (ev->keyboard == '\r') {
	    setPropertyAndDestroyActiveWidget (peGlobal.activeWidget);
	}
	return peGlobal.activeWidget;
    } else 
	return o;
}



PRIVATE int dropAmount (char *buf)
{
    int w, a, d;
    SRGP_inquireTextExtent(buf, &w, &a, &d);
    return d;
}


/* pausch: this macro is copied from scrolbox.c */
#define SIBLING_SCROLLER(O) (SUIT_getChild (SUIT_getParent((O)), 0) )

PRIVATE void UseSimpleTextBox (SUIT_object o, int row, property * prop)
{
    char buf[150];
    SUIT_viewport vp;
    int	heightOfText;
    int theMargin; 
    double theDouble;
    SUIT_viewport theDrawingViewport;
    rectangle	theRectangle;

    /* pausch hack:
       NOTE: these three variables have been introduced in order to
       avoid a compiler bug on the sun3 platform.  I still don't know
       why, but when we put these values in directly as the parameters
       to sb_listSubViewport, it causes a bug... yuck.  At least this works.
       */
    heightOfText = sb_textHeight(o);
    theMargin = SUIT_getInteger(o, MARGIN);
    theDouble = SUIT_getDouble(SIBLING_SCROLLER(o), CURRENT_VALUE);
    theDrawingViewport = SUIT_getViewport(o, DRAWING_VIEWPORT);
    theRectangle = SUIT_mapViewportToScreen(o, theDrawingViewport);

    vp = sb_listSubViewport (o, row, theRectangle, 
			  theDouble, heightOfText, theMargin);
    
    peGlobal.activeWidget = SUIT_createTypeInBox ("property editor type in box", 
						  setPropertyAndDestroyActiveWidget);

    SUIT_setFont (peGlobal.activeWidget, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
    SUIT_setText (peGlobal.activeWidget, "property type", "text");
    SUIT_setBoolean (peGlobal.activeWidget, CACHE_USING_CANVAS, TRUE);
    MakeStandardColors (peGlobal.activeWidget);
    MakeVisibleWithin (peGlobal.activeWidget, TRUE);
    SUIT_setText (peGlobal.activeWidget, CURRENT_VALUE,
		  SUIT_convertType (prop->value, prop->type, "text"));
    
    vp.bottom_left.x += fillBufferWithPropertyPrefix (buf, prop) + LOCK_MARGIN;
    vp.bottom_left.y -= dropAmount (buf) + SUIT_getInteger (o, MARGIN);
    SUIT_setViewport (peGlobal.activeWidget, VIEWPORT, vp);
    
    SUIT_bringToFront (peGlobal.activeWidget);
    
    peGlobal.propertyBeingEdited = prop;
    peGlobal.levelBeingEdited = SUIT_getInteger (o, LEVEL);
    
    SUIT_registerTrapper (CatchKeys);
    {
	DynArray inputList = DynCreate (sizeof (SUIT_object), 2);
	SUIT_object temp = SUIT_name (SUIT_PROPERTY_EDITOR);
	DynAdd (inputList, (void *) &(peGlobal.activeWidget));
	DynAdd (inputList, (void *) &temp);
	
	while (peGlobal.activeWidget != NULL)
	{
	    SUIT_limitedCheckAndProcessInput (INDEFINITE, inputList);
	}
    }
    SUIT_unregisterTrapper();
    
    peGlobal.propertyBeingEdited = NULL;
}


/* types we want to use text box for in the property editor */
PRIVATE boolean JustUseTextBoxForType (property *prop)
{
    return (SUIT_stringsMatch (prop->type, "int") ||
	    SUIT_stringsMatch (prop->type, "double") ||
	    (SUIT_stringsMatch (prop->type, "text") && strchr((char*)prop->value,'\n') == NULL));
}



/* this routine is called when the property editor is putting up a
 * type-specific widget (font dialog box, springiness, etc.) which the user
 * will use to specify a new type */
    
PRIVATE void UseTypeSpecificDialogBox (SUIT_object o, property * prop)
{
    SUIT_object specific;
    Pointer result;
    char *class = si_getType (prop->type)->widgetClass;
    char *type = prop->type;

    if (SUIT_stringsMatch (type, "text"))
	class = "text editor";

    specific = SUIT_createObjectByClass ("type specific", class);
    ASSERT ((specific != NULL), (mes, "UseTypeSpecificDialogBox recieved a NULL object from SUIT_createObjectByClass.\n"));

    if (SUIT_stringsMatch (type, "text"))
	SUIT_setText (specific, CURRENT_VALUE, (char *)prop->value);
    if (SUIT_stringsMatch (type, "SUIT_textList"))
	SUIT_setText (specific, CURRENT_VALUE, si_writeTextList(prop->value));
    else if (SUIT_stringsMatch (type, "SUIT_functionPointer"))
	SUIT_setText (specific, CURRENT_VALUE, si_writeFunctionPtr(prop->value));
    else
	SUIT_setProperty (specific, CURRENT_VALUE, type, prop->value, OBJECT);
    
    peGlobal.propertyBeingEdited = prop;
    result = PutUpTypeSpecificWidget (specific, type);
    if (result != NULL)
	SUIT_setProperty (peGlobal.objectBeingEdited, prop->name, type, result, SUIT_getInteger (o, LEVEL));
}



PRIVATE void hitItem (SUIT_object o, int row, property * prop)
{
    /* we special case a few types */
    if (SUIT_stringsMatch (prop->type, "boolean")) {
	SUIT_deluxeSetBoolean (peGlobal.objectBeingEdited, prop->name,
			       !*((boolean *) (prop->value)), SUIT_getInteger (o, LEVEL));
    } else if (SUIT_stringsMatch (prop->type, "DynArray") ||
	       SUIT_stringsMatch (prop->type, "SUIT_object")) {
	char buf[100];
	sprintf (buf, "Sorry, properties of type %s may not be edited.",prop->type);
	SUIT_inform (buf);
    } else if (SUIT_stringsMatch (prop->type, "GP_color") && BILEVEL_DISPLAY) {
	GP_color col;
	col = *((GP_color *) prop->value);
	col.blackOnMonochrome = !(col.blackOnMonochrome);
	SUIT_deluxeSetColor (peGlobal.objectBeingEdited, prop->name, col, SUIT_getInteger (o, LEVEL));
    } else if (JustUseTextBoxForType (prop))
	UseSimpleTextBox (o, row, prop);
    else {
	SUIT_type *suitType;
	char *widgetClass;
	
	suitType = si_getType (prop->type);
	widgetClass = suitType->widgetClass;
	if (!VALIDSTRING (widgetClass))
	    UseSimpleTextBox (o, row, prop);
	
	else
	    UseTypeSpecificDialogBox (o, prop);
    }
    
    SUIT_redisplayRequired (o);
}


/* This function copies a property, given the level to copy to and a pointer
 * to the property to be copied. */

PRIVATE void copyProperty (SUIT_object from, property * prop, SUIT_object to)
{
    int level;
    Pointer val;
    
    if (SUIT_stringsMatch (OBJECT_NAME(from), "global properties: list") &&
	SUIT_stringsMatch (prop->name, "active display")) {
	SUIT_inform ("Please do not copy @i(active display) from the global level.");
	return;
    }

    level = SUIT_getInteger (from, LEVEL);
    if (level == GLOBAL)
	val = SUIT_deluxeGetProperty (NULL, prop->name, prop->type, GLOBAL);
    else
	val = SUIT_deluxeGetProperty (peGlobal.objectBeingEdited, prop->name, prop->type, level);
    
    level = SUIT_getInteger (to, LEVEL);
    
    SUIT_setProperty (peGlobal.objectBeingEdited, prop->name, prop->type, val, level);
}



point SUIT_drag (void (*graphicsCallback)(point))
{
    deluxe_locator_measure currentTrigger;
    extern boolean IgnoreMouseUp;
    
    si_inSample ();
    currentTrigger = si_getLocatorMeasure ();
    
    GP_pushGraphicsState();
    si_drawInHighlightStyle ();
    
    while (si_atLeastOneButtonDown (currentTrigger)) {
	point hold;
	hold.x = currentTrigger.position.x; /* This is to make the RS6000's stoopid C compiler happy. */
	hold.y = currentTrigger.position.y; /* It doesn't enjoy copying records. */
	graphicsCallback(hold);
	currentTrigger = si_waitForAnyChange (currentTrigger, FALSE);
	graphicsCallback(hold);
    }
    GP_popGraphicsState();
    si_outSample (); 
    
    IgnoreMouseUp = global.currentlyInHitProcedure;
    
    return (currentTrigger.position);
}


PRIVATE char *textWeAreDragging = "if you see this, it's a bug";
PRIVATE point textDragOffset;

PRIVATE void textDrawingCallback(point p)
{

/* roddy and dennis hack... sorry rob :) */
#if defined (IBM_PC)
    int width, ascent, descent;
    rectangle hack;

    GP_inquireTextExtentWithoutMapping (textWeAreDragging, &width, &ascent, &descent);
    hack.bottom_left.x = p.x + textDragOffset.x-2;
    hack.bottom_left.y = p.y + textDragOffset.y-2;
    hack.top_right.x = hack.bottom_left.x + width;
    hack.top_right.y = hack.bottom_left.y + ascent+descent;

    SRGP_rectangle (hack);
#else 
    point t;
    t = p;
    t.x += textDragOffset.x;
    t.y += textDragOffset.y;
    SRGP_text(t, textWeAreDragging);
#endif
}


point SUIT_dragTextWithOffset (char *text, int x, int y)
{
    textWeAreDragging = text;
    textDragOffset.x = x;
    textDragOffset.y = y;
    return( SUIT_drag(textDrawingCallback) );
}


point SUIT_dragText (char *text)
{
    return SUIT_dragTextWithOffset(text, 0, 0);
}



/* This function handles the rectangle movement and end object placement for
 * copying and deleting properties. */

PRIVATE point movePropertyRect (SUIT_object o, SUIT_viewport orig_vp, deluxe_locator_measure inputLocator, property *prop)
{
    char *value_string;
    int xOffset, yOffset;
    point retval;
    
    if (SUIT_stringsMatch (prop->type, "GP_color")) {
	GP_color c;
	c = * (GP_color *) prop->value;
	if (BILEVEL_DISPLAY)
	    value_string = SUIT_copyString(c.blackOnMonochrome? "black" : "white");
	else
	    value_string = SUIT_copyString(c.colorName);
    } else {
	value_string = SUIT_copyString(si_getType (prop->type)->convertToAscii (prop->value));
	if (strlen(value_string)>56)
	    strcpy (value_string+50, " ...");   /* only drag the first 50 characters of the value */
    }

    xOffset = - (inputLocator.position.x - orig_vp.bottom_left.x);
    xOffset +=	 LOCK_MARGIN;

    yOffset = - (inputLocator.position.y - orig_vp.bottom_left.y);
    

    /* pausch hack:  if we don't do this, the user may become confused
       if they click on a property and it disappears until they move
       the mouse
       */
    xOffset += 2;
    yOffset += 2;

    {
	char buf[1000];
	(void) fillBufferWithPropertyPrefix (buf, prop);
	strcat(buf, value_string);
	GP_setFont (SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
	retval = SUIT_dragTextWithOffset(buf, xOffset, yOffset);
    }
    
    SUIT_free (value_string);

    return (retval);
}


PRIVATE boolean notLegalToThrowAway (char *name)
{  /* I presume we may put other things in here at some point ... pausch */
    return (SUIT_caseInsensitiveMatch(name, ACTIVE_DISPLAY) ||
	    SUIT_caseInsensitiveMatch(name, DIRECTION));
}


PRIVATE boolean IsProtectedGlobal (char *name)
{
    return (SUIT_caseInsensitiveMatch(name, ANIMATED) ||
	    SUIT_caseInsensitiveMatch(name, BACKGROUND_COLOR) ||
	    SUIT_caseInsensitiveMatch(name, BORDER_COLOR) ||
	    SUIT_caseInsensitiveMatch(name, BORDER_RAISED) ||
	    SUIT_caseInsensitiveMatch(name, BORDER_TYPE) ||
	    SUIT_caseInsensitiveMatch(name, BORDER_WIDTH) ||
	    SUIT_caseInsensitiveMatch(name, CLIP_TO_VIEWPORT) ||
	    SUIT_caseInsensitiveMatch(name, DEFAULT_OBJECT_WIDTH) ||
	    SUIT_caseInsensitiveMatch(name, DEFAULT_OBJECT_HEIGHT) ||
	    SUIT_caseInsensitiveMatch(name, DRAW_BORDER_ON_INSIDE) ||
	    SUIT_caseInsensitiveMatch(name, FONT) ||
	    SUIT_caseInsensitiveMatch(name, FOREGROUND_COLOR) ||
	    SUIT_caseInsensitiveMatch(name, HAS_BACKGROUND) ||
	    SUIT_caseInsensitiveMatch(name, HAS_BORDER) ||
	    SUIT_caseInsensitiveMatch(name, MARGIN) ||
	    SUIT_caseInsensitiveMatch(name, SHOW_TEMPORARY_PROPERTIES) ||
	    SUIT_caseInsensitiveMatch(name, SHRINK_TO_FIT) ||
	    SUIT_caseInsensitiveMatch(name, SPRINGINESS) ||
	    SUIT_caseInsensitiveMatch(name, SUIT_SYSTEM_FONT) ||
	    SUIT_caseInsensitiveMatch(name, VIEWPORT) ||
	    SUIT_caseInsensitiveMatch(name, VISIBLE) ||
	    SUIT_caseInsensitiveMatch(name, WINDOW));
}


PRIVATE void hitPropEd (SUIT_object o, SUIT_event e)
{
    
    DynArray myList = SUIT_getDynArray (SUIT_getParent(o), LIST);
    int row = sb_mapListEvent (o, e.locator.position);
    SUIT_object dest_o;
    point post_move;
    SUIT_viewport drawvp, sublist;
    static boolean canEnter = TRUE;
    
    ASSERT ((myList != NULL), (mes, "myList was NULL in hitPropEd\n"));
    
    if (!canEnter) /* This is to prevent two gizmos from being on the screen simultaneously.        */
	return;	   /* We don't get this for free (despite the fact that the gizmos are dialog boxes)*/
		   /* because we had to make special consessions to dialog boxes in order to be able*/
                   /* to edit them.  In other words, you must give input only to a dialog box when  */
                   /* one's on screen, unless you're in the property editor.  This canEnter stuff is*/
                   /* due to this exception.                                                        */

    canEnter = TRUE;		/* maybe this canEnter stuff can go?  Rob */
    /* canEnter = FALSE; */

    if (e.type != MOUSE_UP) {
	GP_pushGraphicsState ();
	
	if (row != -1) {
	    property *prop;
	    
	    ASSERT ((row >= DynLow (myList)) && (row <= DynHigh (myList)),
		    (mes, "bad row (%d) in hitPropEd\n", row));
	    
	    FlushActiveWidgetIfNecessary ();
	    
	    prop = (property *) DynGet (myList, row);
	    
	    /* a blatant special case, for when user hits property currently
	     * being edited: */
	    
	    if ((peGlobal.propertyBeingEdited != NULL) &&
		(SUIT_stringsMatch (prop->name, peGlobal.propertyBeingEdited->name)) &&
		(peGlobal.levelBeingEdited == SUIT_getInteger (o, LEVEL)))
		return;
	    
	    drawvp = SUIT_mapViewportToScreen(o, SUIT_getViewport(o, DRAWING_VIEWPORT));
	    sublist = sb_listSubViewport (o, row, drawvp, 
				       SUIT_getDouble(SIBLING_SCROLLER(o), CURRENT_VALUE),
				       sb_textHeight(o), SUIT_getInteger(o, MARGIN));
	    post_move = movePropertyRect (o, sublist, e.locator, prop);
	    
	    dest_o = si_mapPointToObject (peGlobal.objectsUserCanDropPropertiesOn, post_move.x, post_move.y);
	    
	    if (dest_o == SUIT_getParent (o)) {
		int new_row = sb_mapListEvent (o, post_move);
		if (new_row == row) {
		    if (prop->locked)
			SUIT_inform ("Sorry, that property is locked, so you may not edit it.");
		    else {
			hitItem (o, row, prop);
		    }
		}
	    } else {
		if (dest_o == SUIT_name ("property editor gadget collection"))
		    if ((dest_o = si_mapPointToObject (SUIT_getChildren(dest_o), 
						       post_move.x, post_move.y)) == NULL)
			SUIT_inform ("Sorry, but it appears you just dragged a property @i(near) the bins, but missed getting it @i(into) any of them.  By dragging properties into these bins, you can get information about properties, export properties, and destroy properties.");
		
		if (dest_o == SUIT_name ("property editor trash can")) {
		    if (prop->locked)
			SUIT_inform ("Sorry, that property is locked, so you may not throw it away.");
		    else if (SUIT_getInteger (o, LEVEL) == GLOBAL &&
			     IsProtectedGlobal(prop->name))
			SUIT_inform ("Sorry, but you may not throw away global level properties.");			
		    else if ( notLegalToThrowAway(prop->name) )
			SUIT_inform ("Sorry, but you may not throw that property away.");
		    else {
			SUIT_eraseProperty (peGlobal.objectBeingEdited, prop->name, 
					    SUIT_getInteger (o, LEVEL));
			SUIT_redisplayRequired(o);
		    }
		} else if (dest_o == SUIT_name ("property editor export button")) {
		    if (SUIT_getInteger (o, LEVEL) != OBJECT)
			SUIT_inform ("Sorry, you may only export a property from the object level.\nPlease copy the property to the object level before exporting.");
		    else
			exposeProperty (peGlobal.objectBeingEdited, prop->name, prop->type);
		} else if (dest_o == SUIT_name ("property editor info button")) {
		    char *help = SUIT_getHelp (OBJECT_CLASS(peGlobal.objectBeingEdited), prop->name);
		    char *buffer;
		    if (help == NULL)
			help = SUIT_getHelp ("all", prop->name);
		    if (help == NULL)
			help = "Sorry, there is no help for this property.";
		    buffer = (char *) SUIT_malloc (strlen(help)+100);
		    sprintf (buffer, "@b(Property): %s\n@b(Type): %s\n\n%s", prop->name, prop->type, help);
		    SUIT_inform (buffer);
		    SUIT_free(buffer);
		} else if (
			   (dest_o == SUIT_name ("global properties")) ||
			   (dest_o == SUIT_name ("class properties")) ||
			   (dest_o == SUIT_name ("object properties"))
			   ) {
		    /* this is a pausch hack: we know the guy we really want
		     * to copy to is inside the bulletin board, in the second
		     * position */
		    copyProperty (o, prop, SUIT_getChild (dest_o, 1));
		    SUIT_redisplayRequired (dest_o);
		}
	    }
	}
	/* SUIT_redisplayRequired (o);   why? why? why? in God's name, why? */
	
	GP_popGraphicsState ();
    }

    canEnter = TRUE;
}



PRIVATE SUIT_object CreatePropEdList (char *objectName)
{
    SUIT_object o;
    boolean firsttime = TRUE;
    
    o = SUIT_createObject (objectName, "property list");
    SUIT_addDisplayToObject (o, "standard", hitPropEd, paintPropEd);
    
    if (firsttime) {
	SUIT_deluxeSetDouble (o, TEXT_SPACING, 1.5, CLASS);
	SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
	firsttime = FALSE;
    }
    SUIT_registerInterest (o, sb_listInterestCallback);
    return (o);
}



PRIVATE SUIT_object SUIT_createPropEd (int level, char *objectName)
{
    SUIT_object o, lister, scroller;
    static boolean firsttime = TRUE;
    
    o = SUIT_createBulletinBoardWithClass (objectName, "scrollable property list");
      SUIT_setText (o, LABEL, "");	/* this is so the child can inherit
					 * it -- Randy */
      SUIT_changeObjectSize (o, 200, 200);	/* this is necessary for
						 * making child/springiness
						 * work (hack) */
      /* we must create this as temporary, so subsequent gets won't make it
       * permanent */
      SUIT_setDynArray (o, LIST, DynCreate (sizeof (property), 1));
              /* one day this should be made a SUIT_textList */
      SUIT_makePropertyTemporary (o, LIST, OBJECT);
    

    scroller = SUIT_createBoundedValue (SUIT_relativeName(o, "scrollbar"), ScrollbarCallback);
      SUIT_registerInterest(scroller, sb_listScrollerCallback);
      SUIT_addChildToObject (o, scroller);
      SUIT_setEnumString (scroller, ACTIVE_DISPLAY, "scroll bar");
      SUIT_setDouble (scroller, GRANULARITY, 1.0);
      /* SUIT_setDouble (scroller, CURRENT_VALUE, SUIT_getDouble (scroller, MAXIMUM_VALUE)); */
      SUIT_setBoolean (scroller, BORDER_RAISED, FALSE);
      SUIT_setSpringiness (scroller, SPRINGINESS, LEFT_SPRINGINESS | VERTICAL_SPRINGINESS);
      SUIT_setViewport (scroller, VIEWPORT, SUIT_mapToParent (scroller, 0.94, 0.0, 1.0, 1.0));
    
    lister = CreatePropEdList (SUIT_relativeName(o, "list"));
      SUIT_addChildToObject (o, lister);
      SUIT_setInteger (lister, LEVEL, level);
      SUIT_setSpringiness (lister, SPRINGINESS, HORIZONTAL_SPRINGINESS | VERTICAL_SPRINGINESS);
      SUIT_setViewport (lister, VIEWPORT, SUIT_mapToParent (lister, 0.0, 0.0, 0.89, 1.0));
    
    if (firsttime) {
	firsttime = FALSE;
	SUIT_deluxeSetBoolean (o, HAS_BACKGROUND, FALSE, CLASS);
	SUIT_deluxeSetBoolean (o, HAS_BORDER, FALSE, CLASS);
    }
    return o;
}



#define NESTING_FUDGE 0.01

PRIVATE void NestInPropertyEditor (SUIT_object pe, SUIT_object o, double x1, double y1, double x2, double y2)
{
    SUIT_viewport newvp;
    
    ASSERT ((o != NULL), (mes, "NestInPropertyEditor: NULL child parameter\n"));
    
    SUIT_addChildToObject (pe, o);
    
    GP_pushGraphicsState();
    GP_setWindow (SUIT_getWindow (pe, WINDOW));
    GP_setViewport (SUIT_getViewport (pe, VIEWPORT));
    newvp = GP_mapRectangle (GP_defRectangle (x1 + NESTING_FUDGE, y1 + NESTING_FUDGE,
					      x2 - NESTING_FUDGE, y2 - NESTING_FUDGE));
    SUIT_setViewport (o, VIEWPORT, newvp);
    GP_popGraphicsState();
}



PRIVATE SUIT_object MainPropertyEditor = NULL;

PRIVATE void ChangeFont (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, SUIT_SYSTEM_FONT))
	SUIT_setFont (MainPropertyEditor, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
}



PRIVATE void MakeMainPropertyEditor (void)
{
    SUIT_object o = SUIT_createBulletinBoardWithClass (SUIT_PROPERTY_EDITOR, "property editor");
    
    int width = SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL);
    int height = SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL);
    
    OBJECT_PERMANENT(o) = FALSE;
    SUIT_setBoolean (o, VISIBLE, FALSE);
    SUIT_deluxeSetBoolean (o, HAS_BORDER, FALSE, CLASS);
    SUIT_makePropertyTemporary (o, HAS_BORDER, CLASS);
    SUIT_setViewport (o, VIEWPORT, SRGP_defRectangle (5, 5, width - 5, height - 5));
    SUIT_setFont (o, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
    SUIT_setSpringiness (o, SPRINGINESS, ALL_SPRINGINESS);
    MakeStandardColors (o);
    MainPropertyEditor = o;
    SUIT_registerInterestInGlobal (ChangeFont);
}



PRIVATE void PropEd (SUIT_object o)
{
    char buf[500];
    
    if (o == peGlobal.objectBeingEdited) {
	SUIT_inform ("You are currently editing this object.");
	return;
    }
    if (o == SUIT_name(SUIT_PROPERTY_EDITOR) || o == peGlobal.activeWidget) {
	SUIT_inform ("Sorry, the property editor may not be edited.");
	return;
    }
    if (global.propertyEditorIsActive && (SUIT_stringsMatch(OBJECT_CLASS(o), "dialog box") ||
			    SUIT_stringsMatch(OBJECT_CLASS(SUIT_getParent(o)), "dialog box"))) {
	SUIT_inform ("Sorry, this dialog box may not be edited.");
	return;
    }
    
    SUIT_clearScreen();

    MakeVisibleWithin (o, TRUE);
    MakeVisibleWithin (global.root, FALSE);
    MakeVisibleWithin (SUIT_name(SUIT_PROPERTY_EDITOR), TRUE);

    SUIT_resetScrollableListToTop (SUIT_name("global properties"));

    sprintf (buf, "@bold((2) Class Properties:) %s", OBJECT_CLASS(o));
    SUIT_setText (SUIT_name ("class properties"), LABEL, buf);
    SUIT_resetScrollableListToTop (SUIT_name("class properties"));

    sprintf (buf, "@bold((1) Object Properties:) %s", OBJECT_NAME (o));
    SUIT_setText (SUIT_name ("object properties"), LABEL, buf);
    SUIT_resetScrollableListToTop (SUIT_name("object properties"));

    SUIT_setFont (SUIT_name("property editor DONE"), FONT, SUIT_deluxeGetFont(NULL, SUIT_SYSTEM_FONT, GLOBAL));
    
    SUIT_sendToBack (SUIT_name (SUIT_PROPERTY_EDITOR));
    
    global.propertyEditorIsActive = TRUE;
    peGlobal.objectBeingEdited = o;
    strcpy (peGlobal.objectsName, OBJECT_NAME(o));
    peGlobal.propertyBeingEdited = NULL;
    peGlobal.exportedObjects = DynCreate(sizeof(char *), 1);
}


PRIVATE void ClosePropertyEditor (SUIT_object unused)  { SUIT_closePropertyEditor(); }


void SUIT_initPropertyEditor (void)
{
    SUIT_object o, export, info, trash;
    static boolean FirstInit = TRUE;
    SUIT_object pe;
    
    if (FirstInit)
	FirstInit = FALSE;
    else
	return;
    
    MakeVisibleWithin (global.root, FALSE);
    
    MakeMainPropertyEditor ();
    
    peGlobal.objectBeingEdited = NULL;
    peGlobal.activeWidget = NULL;
    peGlobal.activeWrapper = NULL;
    
    peGlobal.objectsUserCanDropPropertiesOn = DynCreate (sizeof (SUIT_object), 5);
    pe = SUIT_name (SUIT_PROPERTY_EDITOR);
    
    o = SUIT_createBulletinBoardWithClass ("property editor gadget collection", "p.e.collection");
    SUIT_setBoolean (o, BORDER_RAISED, TRUE);
    NestInPropertyEditor (pe, o, 0.5, 0.0, 0.9, 0.2);
    DynAdd (peGlobal.objectsUserCanDropPropertiesOn, &o);
    info = SUIT_createInfoButton ("property editor info button");
    SUIT_addChildToObject (o, info);
#define MY_MARGIN   0.05
    SUIT_setViewport (info, VIEWPORT, SUIT_mapToParent(info, MY_MARGIN, 2*MY_MARGIN,
						       0.33-MY_MARGIN/2, 1.0-2*MY_MARGIN));
    export = SUIT_createExportButton ("property editor export button");
    SUIT_addChildToObject (o, export);
    SUIT_setViewport (export, VIEWPORT, SUIT_mapToParent(export, 0.33+MY_MARGIN/2, 2*MY_MARGIN,
							 0.66-MY_MARGIN/2, 1.0-2*MY_MARGIN));
    trash = SUIT_createTrashCan ("property editor trash can");
    SUIT_addChildToObject (o, trash);
    SUIT_setViewport (trash, VIEWPORT, SUIT_mapToParent(trash, 0.66+MY_MARGIN/2, 2*MY_MARGIN,
							1.0-MY_MARGIN, 1.0-2*MY_MARGIN));
#undef MY_MARGIN
    
    o = SUIT_createButton ("property editor DONE", ClosePropertyEditor);
    SUIT_setText (o, LABEL, "OK");
    NestInPropertyEditor (pe, o, 0.9, 0.0, 1.0, 0.2);
    DynAdd (peGlobal.objectsUserCanDropPropertiesOn, &o);
    
    o = SUIT_createPropEd (GLOBAL, "global properties");
    SUIT_setText (SUIT_name ("global properties"), LABEL, "@bold((3) Global Properties)");
    NestInPropertyEditor (pe, o, 0.5, 0.22, 1.0, 0.98);
    DynAdd (peGlobal.objectsUserCanDropPropertiesOn, &o);
    
    o = SUIT_createPropEd (CLASS, "class properties");
    NestInPropertyEditor (pe, o, 0.0, 0.0, 0.5, 0.49);
    DynAdd (peGlobal.objectsUserCanDropPropertiesOn, &o);
    
    o = SUIT_createPropEd (OBJECT, "object properties");
    NestInPropertyEditor (pe, o, 0.0, 0.51, 0.5, 0.98);
    DynAdd (peGlobal.objectsUserCanDropPropertiesOn, &o);
    
    SUIT_establishPropertyEditor (PropEd);
}


PRIVATE void ResetScrollbars (void)
{
    SUIT_object o;
    
    o = SUIT_name ("global properties");
    SUIT_setDouble (SUIT_getChild(o, 0), CURRENT_VALUE, 0.0);
    SUIT_setInteger (SUIT_getChild(o, 1), SCROLL_POSITION, 0);

    o = SUIT_name ("class properties");
    SUIT_setDouble (SUIT_getChild(o, 0), CURRENT_VALUE, 0.0);
    SUIT_setInteger (SUIT_getChild(o, 1), SCROLL_POSITION, 0);

    o = SUIT_name ("object properties");
    SUIT_setDouble (SUIT_getChild(o, 0), CURRENT_VALUE, 0.0);
    SUIT_setInteger (SUIT_getChild(o, 1), SCROLL_POSITION, 0);
}



PRIVATE void RecursivelyRemoveVisibleWithin (SUIT_object obj)
{
    int i;
    if (obj != global.root &&
	SUIT_propertyExists (obj, VISIBLE_WITHIN_PROPERTY_EDITOR, "boolean", OBJECT))
	SUIT_eraseProperty(obj, VISIBLE_WITHIN_PROPERTY_EDITOR, OBJECT);
    for (i=0; i < SUIT_numberOfChildren(obj); i++)
	RecursivelyRemoveVisibleWithin (SUIT_getChild(obj, i));
}



void SUIT_closePropertyEditor (void)
{
    /* Because the current object was set to be visible at the object level,
     * if it is to become invisible ever again, it must have this property
     * removed from the OBJECT level */
    
    FlushActiveWidgetIfNecessary ();
    RecursivelyRemoveVisibleWithin (global.root);
    
    DynDestroy (peGlobal.exportedObjects);
    
    ResetScrollbars();
    
    SUIT_clearScreen ();
    global.propertyEditorIsActive = FALSE;
    peGlobal.objectBeingEdited = NULL;
    SUIT_allObjectsRequireRedisplay (NULL);
}


PRIVATE boolean OverButton (int x, int y, char *buttonName)
{
  SUIT_object destobj;
  
  destobj = si_mapPointToObject (peGlobal.objectsUserCanDropPropertiesOn, x, y);
  if (destobj == SUIT_name ("property editor gadget collection")) {
      destobj = si_mapPointToObject (SUIT_getChildren(destobj), x, y); 
      if (destobj == SUIT_name (buttonName)) 	
	  return TRUE;
      else 
	  return FALSE;
  } else 
      return FALSE;  
}



boolean si_overTrashCan(int x, int y)
{
    return OverButton(x, y, "property editor trash can");
}



boolean si_overExportButton(int x, int y)
{
    return OverButton(x, y, "property editor export button");
}



boolean si_overInfoButton(int x, int y)
{
    return OverButton(x, y, "property editor info button");
}
