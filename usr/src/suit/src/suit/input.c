/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "privsuit.h"

boolean SUIT_isAncestor (SUIT_object parent, SUIT_object kid)
{
    SUIT_object scan;
    for (scan = kid; scan != parent && scan != NULL; scan = SUIT_getParent (scan));
    return (scan == parent);
}


/* This routine sets the input mode to sample.  Avoid using it; SRGP flushes
 * events when you do. */

void si_inSample (void)
{
    if (!global.sampleMode) {
	SRGP_setInputMode (LOCATOR, SAMPLE);
	global.sampleMode = TRUE;
    }
}



/* This routine sets the input mode to event.  Avoid using it; SRGP flushes
 * events when you do. */

void si_outSample (void)
{
    if (global.sampleMode)
	SRGP_setInputMode (LOCATOR, EVENT);
    global.sampleMode = FALSE;
}



void SUIT_registerTrapper (SUIT_trapperPtr trapper)
{
    if (global.trapperFunctions == NULL)
	global.trapperFunctions = DynCreate (sizeof (SUIT_trapperPtr), 1);
    DynAdd (global.trapperFunctions, (void *) &trapper);
}


void SUIT_unregisterTrapper (void)
{
    ASSERT ((global.trapperFunctions != NULL), (mes, "SUIT_unregisterTrapper called, but no trappers are registered.\n"));
    DynDelete (global.trapperFunctions, DynHigh (global.trapperFunctions));
}



boolean si_buttonsEqual (deluxe_locator_measure first, deluxe_locator_measure second)
{
    return ((first.button_chord[LEFT_BUTTON] == second.button_chord[LEFT_BUTTON]) &&
	    (first.button_chord[MIDDLE_BUTTON] == second.button_chord[MIDDLE_BUTTON]) &&
    (first.button_chord[RIGHT_BUTTON] == second.button_chord[RIGHT_BUTTON]));
}



PRIVATE boolean si_positionsEqual (point a, point b, int tolerance)
{
    return (ABS (a.x - b.x) < tolerance) && (ABS (a.y - b.y) < tolerance);
}



boolean si_atLeastOneButtonDown (deluxe_locator_measure measure)
{
    return ((measure.button_chord[LEFT_BUTTON] == DOWN) ||
	    (measure.button_chord[MIDDLE_BUTTON] == DOWN) ||
	    (measure.button_chord[RIGHT_BUTTON] == DOWN));
}



boolean si_modifiersEqual (deluxe_locator_measure first, deluxe_locator_measure second)
{
    return ((first.modifier_chord[SHIFT] == second.modifier_chord[SHIFT]) &&
	(first.modifier_chord[CONTROL] == second.modifier_chord[CONTROL]) &&
	    (first.modifier_chord[META] == second.modifier_chord[META]));
}



boolean si_buttonsAndModifiersEqual (deluxe_locator_measure first, deluxe_locator_measure second)
{
    return (si_buttonsEqual (first, second) && si_modifiersEqual (first, second));
}



/* This routine filters out "uninteresting" input events (events which occur
 * when the cursor is not moving but a button is down) so that the input
 * event queue will not fill up. */

deluxe_locator_measure si_waitForAnyChange (deluxe_locator_measure original, boolean considerAnimation)
{
    deluxe_locator_measure retval;

    si_inSample ();

    SRGP_sampleDeluxeLocator (&retval);
    if (!(considerAnimation && global.animated))
	while ((original.position.x == retval.position.x) &&
	       (original.position.y == retval.position.y) &&
	       si_buttonsAndModifiersEqual (original, retval))
	    SRGP_sampleDeluxeLocator (&retval);

    return (retval);
}



deluxe_locator_measure si_getLocatorMeasure (void)
{
    deluxe_locator_measure retval;

    ENTER (3, (buf, "si_getLocatorMeasure()\n"));

    if (global.sampleMode)
	SRGP_sampleDeluxeLocator (&retval);
    else
	SRGP_getDeluxeLocator (&retval);

    LEAVE (3, (buf, "si_getLocatorMeasure()\n"));
    return (retval);
}


#define BETWEEN(A,B,C)    ((A)<(B) && (B)<(C))
#define TOLERANCE     10

/* Is the first viewport completely enclosed within the second? */

/* boolean si_viewportWithinViewport (SUIT_viewport vp1, SUIT_viewport vp2) {
 * return (BETWEEN (vp2.bottom_left.x - TOLERANCE, vp1.bottom_left.x,
 * vp2.top_right.x + TOLERANCE) && BETWEEN (vp2.bottom_left.x - TOLERANCE,
 * vp1.top_right.x, vp2.top_right.x + TOLERANCE) && BETWEEN
 * (vp2.bottom_left.y - TOLERANCE, vp1.bottom_left.y, vp2.top_right.y +
 * TOLERANCE) && BETWEEN (vp2.bottom_left.y - TOLERANCE, vp1.top_right.y,
 * vp2.top_right.y + TOLERANCE)); } */



PRIVATE void CommandOpenObject (SUIT_object o)
{
    if (o == NULL) {
	SUIT_inform ("You just typed SUIT-O, but the mouse cursor was not over any object and there was no single object selected.  SUIT-O can be used to open\nan object, so that its children may be accessed.");
	return;
    }
    if (SUIT_getBoolean (o, CAN_BE_OPENED))
	si_openObject (o);
    else {
	char buf[80];
	sprintf (buf, "Sorry, but object \"%s\" (class \"%s\") may not be opened.", OBJECT_NAME (o), OBJECT_CLASS (o));
	SUIT_inform (buf);
    }
}



PRIVATE void CommandCloseObject (SUIT_object o)
{
    if (o == global.root) {
	SUIT_inform ("You just typed SUIT-K, but there were no open objects.  SUIT-K can be used to close an object that has been previously opened.");
	return;
    }
    if (SUIT_getBoolean (o, CAN_BE_OPENED))
	si_closeObject (o);
}



void si_giveInfo (SUIT_object o)
{
    if (o == NULL)		/* Don't give info for Root */
	SUIT_inform ("You just typed SUIT-I, but the mouse cursor was not over any object and there was no single object selected.  SUIT-I can be used to\nget information about a single object.");
    else {
	char buf[1000];
	int i, numKids = SUIT_numberOfChildren (o);
	sprintf (buf, "@bold(Name): \"%s\"\n@bold(Class): \"%s\"\n@bold(Active Display): \"%s\"",
		 OBJECT_NAME (o), OBJECT_CLASS (o), SUIT_getEnumString (o, ACTIVE_DISPLAY));
	if (numKids > 0) {
	    strcat (buf, "\n@u(                                                       )\n\n@b(Children):\n");
	    for (i = 0; i < numKids; i++) {
		SUIT_object kid = SUIT_getChild (o, i);
		char buf2[80];
		sprintf (buf2, "     \"%s\" (%s)\n", OBJECT_NAME (kid), OBJECT_CLASS (kid));
		strcat (buf, buf2);
	    }
	}
	SUIT_inform (buf);
    }
}



char *si_generateCopyName (char *name)
{
    char *copyOf = "copy of ";
    int times = 1;
    char *retval = (char *) SUIT_malloc (strlen(name) + times*strlen(copyOf) + 1);

    strcpy (retval, copyOf);
    strcat (retval, name);
    while (SUIT_name(retval) != NULL) {
	int i;
	times++;
	SUIT_free (retval);
	retval = (char *) SUIT_malloc (strlen(name) + times*strlen(copyOf) + 1);
	strcpy (retval, copyOf);
	for (i=1; i < times; i++)
	    strcat (retval, copyOf);
	strcat (retval, name);
    }
    return retval;
}



void si_duplicateObject (SUIT_object child)
{
    char newname[150];
    char *suggested = si_generateCopyName(OBJECT_NAME(child));
    SUIT_object newobj;

    if (SUIT_getString("New object's name:", suggested, newname, 150) == REPLY_CANCEL)
	return;
    newobj = SUIT_copyObject (child, newname);
    SUIT_bringToFront (newobj);
    SUIT_free (suggested);
}




void si_editObject (SUIT_object child)
{
    static boolean firstTime = TRUE;

    if (firstTime) {
	SUIT_initPropertyEditor ();
	firstTime = FALSE;
    }
    if ((child != NULL) && (global.propertyEditor != NULL))
	global.propertyEditor (child);
    else {
	SUIT_inform ("You just typed SUIT-E, but the mouse cursor was not over any object and there was no single object selected.  SUIT-E can be used to edit an object's properties.");
    }
}


PRIVATE void MoveOrResize (SUIT_object o, deluxe_locator_measure current)
{
    static boolean alreadyUp = FALSE;
    int dir;

    if (OBJECT_SELECTED (o) &&
    (dir = si_overHandle (SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o)),
			  current.position)) != NO_DIRECTION) {
	if (DynSize (SelectedObjects) > 1)
	    si_resizeGroup (SelectedObjects, dir);
	else
	    si_resizeObject (o, dir);
    } else {
	if ((DynSize (SelectedObjects) > 0) && OBJECT_SELECTED (o))
	    si_moveGroup (SelectedObjects, current);
	else {
	    if (SUIT_stringsMatch (OBJECT_NAME (o), SUIT_PROPERTY_EDITOR)) {
		if (!alreadyUp) {
		    alreadyUp = TRUE;
		    SUIT_inform ("To manipulate properties,\
 use the mouse @u(without) depressing the SUIT key combination.");
		    alreadyUp = FALSE;
		}
		return;
	    }
	    si_moveObject (o, current);
	}
    }
}



PRIVATE void RecursivelyClose (SUIT_object o)
{
    int i;
    for (i = 0; i < SUIT_numberOfChildren (o); i++)
	RecursivelyClose (SUIT_getChild (o, i));
    SUIT_deselectObject (o);
    if (!OBJECT_OPEN (o) || o == global.root)
	return;
    SUIT_eraseObject (o);
    OBJECT_OPEN (o) = FALSE;
    SUIT_redisplayRequired (o);
    OBJECT_OPTIMIZED (o) = FALSE;
}


PRIVATE void RebuildOpenListRecursively (SUIT_object o)
{
    DynArray kids;
    SUIT_object child;
    int i;

    if (o == NULL)
	return;
    if ((kids = SUIT_getChildren (o)) != NULL)
	for (i = DynLow (kids); i <= DynHigh (kids); i++) {
	    child = *((SUIT_object *) DynGet (kids, i));
	    RebuildOpenListRecursively (child);
	}
    if (OBJECT_OPEN (o))
	DynAdd (OpenObjects, (void *) &o);
}



PRIVATE void RebuildOpenList (void)
{
    DynDestroy (OpenObjects);
    OpenObjects = DynCreate (sizeof (SUIT_object), 1);
    RebuildOpenListRecursively (global.root);
}


PRIVATE void si_closeAll (void)
{
    RecursivelyClose (global.root);
    si_openObject (global.root);
    RebuildOpenList ();
}



PRIVATE void si_deselectAndCloseAll (void)
{
    si_deselectAll ();
    si_closeAll ();
}



PRIVATE void si_toggleSelection (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "ToggleSelection was called with a null object pointer.\n"));
    if (o == global.root)
	return;
    if (OBJECT_SELECTED (o))
	SUIT_deselectObject (o);
    else
	SUIT_selectObject (o);
}



PRIVATE void si_handleClick (SUIT_object child)
{
    if (child == NULL)
	si_deselectAndCloseAll ();
    else {
	boolean unsel = !OBJECT_SELECTED (child);
	/* we don't want to deselect objects when a dialog box is selected -
	 * the dialog box may have been created to aquire parameters to a
	 * function to be performed on the selected objects */
	if (!SUIT_stringsMatch (OBJECT_CLASS (child), "dialog box"))
	    si_deselectAll ();
	if (unsel)
	    si_toggleSelection (child);
    }
}



PRIVATE void si_selectGroup (DynArray selectedObjects, int initx, int inity)
{
    SUIT_object o;
    SUIT_viewport vp;
    int finalx, finaly, x, y, i, j;
    deluxe_locator_measure measure;

    si_outSample ();
    si_deselectAll ();
    SUIT_performRedisplay ();
    SRGP_setLocatorEchoRubberAnchor (SRGP_defPoint (initx, inity));
    SRGP_setLocatorEchoType (RUBBER_RECT);
    SRGP_waitEvent (INDEFINITE);
    SRGP_getDeluxeLocator (&measure);
    SRGP_setLocatorEchoType (CURSOR);

    si_lessAndGreater (initx, measure.position.x, &x, &finalx);
    si_lessAndGreater (inity, measure.position.y, &y, &finaly);

    for (i = DynLow (OpenObjects); i <= DynHigh (OpenObjects); i++) {
	o = *(SUIT_object *) DynGet (OpenObjects, i);
	for (j = 0; j < SUIT_numberOfChildren (o); j++) {
	    SUIT_object child = SUIT_getChild (o, j);
	    vp = SUIT_mapViewportToScreen (child, OBJECT_VIEWPORT (child));
	    if (vp.bottom_left.x >= x && vp.bottom_left.y >= y &&
		vp.top_right.x <= finalx && vp.top_right.y <= finaly && si_isVisible(child) && !OBJECT_OPEN(child)) {
		OBJECT_SELECTED (child) = TRUE;
		SUIT_redisplayRequired (child);
		OBJECT_OPTIMIZED (o) = FALSE;
		DynAdd (selectedObjects, (void *) &child);
	    }
	}
    }
}



PRIVATE void si_handleMouseDown (SUIT_object o, deluxe_locator_measure current)
{
    if (o == NULL)
	si_selectGroup (SelectedObjects, current.position.x, current.position.y);
    else
	MoveOrResize (o, current);
}



PRIVATE void si_handleMouseMotion (SUIT_object o, deluxe_locator_measure current)
{
    if (o == NULL)
	si_selectGroup (SelectedObjects, current.position.x, current.position.y);
    else
	MoveOrResize (o, current);
}


PRIVATE DynArray si_selectAll (SUIT_object o, DynArray selectedObjects)
{
    DynArray children;
    int i;
    SUIT_object temp;
    /* int x1, x2, y1, y2; */

    si_closeAll ();
    children = SUIT_getChildren (o);
    for (i = DynLow (children); i <= DynHigh (children); i++) {
	temp = *((SUIT_object *) DynGet (children, i));
	if (si_isVisible(temp)) {
	    OBJECT_SELECTED (temp) = TRUE;
	    SUIT_redisplayRequired (temp);
	    OBJECT_OPTIMIZED (o) = FALSE;
	    DynAdd (selectedObjects, (void *) &temp);
	}
    }
    return selectedObjects;
}





#define CONTROL_A  (char)1
#define CONTROL_B  (char)2
#define CONTROL_C  (char)3
#define CONTROL_D  (char)4
#define CONTROL_E  (char)5
#define CONTROL_F  (char)6
#define CONTROL_I  (char)9
#define CONTROL_J  (char)10
#define CONTROL_K  (char)11
#define CONTROL_L  (char)12
#define CONTROL_M  (char)13
#define CONTROL_N  (char)14
#define CONTROL_O  (char)15
#define CONTROL_Q  (char)17
#define CONTROL_R  (char)18
#define CONTROL_S  (char)19
#define CONTROL_T  (char)20
#define CONTROL_V  (char)22
#define CONTROL_W  (char)23
#define CONTROL_X  (char)24
#define CONTROL_Z  (char)26

PRIVATE void si_processSUITCommand (SUIT_object o, char ch, deluxe_locator_measure current,
				    SUIT_eventType type, DynArray activeObjects)
{
    SUIT_object child;
    SUIT_object firstObject = NULL;
    int button = current.button_of_last_transition;
    int i;

    if (DynSize(activeObjects) > 0)
	firstObject = * (SUIT_object *) DynGet (activeObjects, 0);

    /* Control chars are handled here in case people are using CONTROL-SHIFT. */

    /* pausch bug fix: o can be NULL if they press down inside the X window,
     * drag outside it, and then let up. */
    if (o == NULL)
	child = NULL;
    else {
	child = si_mapPointToObject (SUIT_getChildren (o), current.position.x, current.position.y);
	if (child == NULL)
	    child = si_mapPointToObject (
	      SUIT_getEmployees (o, SUIT_getEnumString (o, ACTIVE_DISPLAY)),
				    current.position.x, current.position.y);
    }

    if (type == KEYSTROKE)
	switch (ch) {
	  case 'a':
	  case 'A':
	  case CONTROL_A:
	    si_commandAlign (NULL);/* the argument here goes unused */
	    break;

	  case 'b':
	  case 'B':
	  case CONTROL_B:
	    if (child == NULL) {
		if (DynSize (SelectedObjects) > 0) {
		    for (i = DynLow (SelectedObjects); i < DynSize (SelectedObjects); i++)
			SUIT_sendToBack (*((SUIT_object *) DynGet (SelectedObjects, i)));
		} else
		    SUIT_inform ("You just typed SUIT-B, but the mouse cursor was not over any object and there were no objects selected.  SUIT-B over a widget sends that widget to the very back of the stack of widgets.");
	    } else
		SUIT_sendToBack (child);
	    break;

	  case 'd':
	  case 'D':
	  case CONTROL_D:
	    if (DynSize (SelectedObjects) > 0) {
		DynArray listCopy = DynCreate(sizeof(SUIT_object), 1);
		int j;
		for (j=0; j < DynSize(SelectedObjects); j++)
		    DynAdd (listCopy, DynGet(SelectedObjects, j));
		for (j=0; j < DynSize(listCopy); j++) {
		    SUIT_object sel = * (SUIT_object *) DynGet(listCopy, j);
		    si_duplicateObject (sel);
		}
		DynDestroy (listCopy);
	    } else if (child != NULL)
		si_duplicateObject (child);
	    else
		SUIT_inform ("You just typed SUIT-D, but the mouse cursor was not over any object and there were no objects selected.  SUIT-D can be used to duplicate SUIT objects.");
	    break;
	    
	  case 'c':
	  case 'C':
	  case CONTROL_C:
	    if (child == NULL) {
		if (DynSize (SelectedObjects) > 0)
		    si_cycleGroup ();
		else
		    SUIT_inform ("You just typed SUIT-C, but the mouse cursor was not over any object and there were no objects selected.  SUIT-C can be used to cycle the object under the mouse cursor to another display style.");
	    } else
		SUIT_cycleObject (child);
	    break;

	  case 'e':
	  case 'E':
	  case CONTROL_E:
	    if (DynSize(activeObjects) == 1 &&
		SUIT_stringsMatch (OBJECT_CLASS(firstObject), "dialog box") &&
		!SUIT_isAncestor (firstObject, child))
		SUIT_inform ("Please respond to the dialog box before editing this object.");
	    else if (child == NULL) {
		if ((DynSize (SelectedObjects) == 1) & (!global.propertyEditorIsActive))
		    si_editObject (*((SUIT_object *) DynGet (SelectedObjects, DynLow (SelectedObjects))));
		else
		    si_editObject (child);
	    } else
		si_editObject (child);
	    break;

	  case 'f':
	  case 'F':
	  case CONTROL_F:
	    if (child == NULL) {
		if (DynSize (SelectedObjects) > 0) {
		    for (i = DynLow (SelectedObjects); i < DynSize (SelectedObjects); i++)
			SUIT_bringToFront (*((SUIT_object *) DynGet (SelectedObjects, i)));
		} else
		    SUIT_inform ("You just typed SUIT-F, but the mouse cursor was not over any object and there were no objects selected.  SUIT-F over a widget sends that widget to the front of the stack of widgets.");
	    } else
		SUIT_bringToFront (child);
	    break;

	  case 'i':
	  case 'I':
	  case CONTROL_I:
	    if (child == NULL) {
		if (DynSize (SelectedObjects) == 1)
		    si_giveInfo (*((SUIT_object *) DynGet (SelectedObjects, DynLow (SelectedObjects))));
		else
		    si_giveInfo (child);
	    } else
		si_giveInfo (child);
	    break;

	  case 'k':
	  case 'K':
	  case CONTROL_K:
	    if (child == NULL) {
		if (DynSize (OpenObjects) > 0)
		    for (i = DynLow (OpenObjects); i < DynSize (OpenObjects); i++)
			CommandCloseObject (*((SUIT_object *) DynGet (OpenObjects, i)));
		else
		    CommandCloseObject (child);
	    } else
		CommandCloseObject (SUIT_getParent(child));
	    break;

	  case 'm':
	  case 'M':
	  case CONTROL_M:
	    si_showSUITMenu (current.position);
	    break;

	  case 'n':
	  case 'N':
	  case CONTROL_N:
	    si_commandCreateObject (NULL);	/* the argument here goes unused */
	    break;

	  case 'o':
	  case 'O':
	  case CONTROL_O:
	    if (child == NULL) {
	        if ((o != global.root) & (o->open))
		    SUIT_inform("You just typed SUIT-O but the mouse curser was over an object that is already open.\n");
		else 
		    if (DynSize (SelectedObjects) == 1)
			CommandOpenObject (*((SUIT_object *) DynGet (SelectedObjects, DynLow (SelectedObjects))));
		    else
			CommandOpenObject (child);
	    } else
		CommandOpenObject (child);
	    break;

	    /* CommandOpenObject (child); break; */

	  case 'q':
	  case 'Q':
	  case CONTROL_Q:
	    si_quitLikeDoneButton (NULL);
	    break;

	  case 'r':
	  case 'R':
	  case CONTROL_R:
	    si_repaintScreen ();
	    break;

	  case 's':
	  case 'S':
	  case CONTROL_S:
	    if (child != NULL)
		si_toggleSelection (child);
	    else if (DynSize (SelectedObjects) > 0)
		si_deselectAll ();
	    else
		si_selectAll (global.root, SelectedObjects);
	    break;

	  case 'v':
	  case 'V':
	  case CONTROL_V:{
		char buf[100];
		sprintf (buf, "@c(SUIT Version %d.%d)\n\n@c(brought to you by)\n@c(the University of Virginia)", SUIT_VERSION, SUIT_REVISION);
		SUIT_inform (buf);
		break;
	    }

	  case 'x':
	  case 'X':
	  case CONTROL_X:
	    si_commandDestroyObject (NULL);	/* the argument here goes unused */
	    break;

	  default:{
		char buf[80];
		sprintf (buf, "Sorry, SUIT-%c is not a valid command. Use SUIT-M for a menu.",
			 (ch >= CONTROL_A && ch <= CONTROL_Z) ? ch - CONTROL_A + 'A' : ch);
		SUIT_inform (buf);
		break;
	    }
	}
    else if (button == LEFT_BUTTON) {
	switch (type) {
	  case MOUSE_MOTION:
	    si_handleMouseMotion (child, current);
	    break;
	  case MOUSE_DOWN:
	    si_handleMouseDown (child, current);
	    break;
	  case CLICK:
	    si_handleClick (child);
	    break;
	  case MOUSE_UP:
	  case KEYSTROKE:
	    /* nada */
	    break;
	}
    }
}



PRIVATE void InitializeLocatorMeasure (deluxe_locator_measure * lm)
{
    lm->button_chord[LEFT_BUTTON] = UP;
    lm->button_chord[MIDDLE_BUTTON] = UP;
    lm->button_chord[RIGHT_BUTTON] = UP;
    lm->modifier_chord[SHIFT] = 0;
    lm->modifier_chord[CONTROL] = 0;
    lm->modifier_chord[META] = 0;
}


PRIVATE void InitializeKeyboardMeasure (deluxe_keyboard_measure * kbd, char *myBuf, int bufLen)
{
    kbd->buffer = myBuf;
    kbd->buffer[0] = 255;	/* we used to initialize this to '\0'. did
				 * something break? */
    kbd->buffer_length = bufLen;
}


PRIVATE SUIT_event CreateEvent (SUIT_object o, SUIT_eventType type, deluxe_locator_measure mouse, char keystroke)
{
    SUIT_event event;

    if (o != NULL) {
	rectangle rect;
	rect = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o));
	event.relativePixelLocation.x = mouse.position.x - rect.bottom_left.x;
	event.relativePixelLocation.y = mouse.position.y - rect.bottom_left.y;
	GP_setWindow (OBJECT_WINDOW (o));
	GP_setViewport (rect);
	event.worldLocation = GP_unMapPoint (mouse.position);
    }
    event.type = type;
    event.button = mouse.button_of_last_transition;
    event.keyboard = keystroke;
    event.locator = mouse;
    return event;
}



void SUIT_reportMouseMotion (SUIT_object o, SUIT_mouseMotion type)
{
    if (type == UNTIL_MOUSE_UP)
	global.objectGetsMouse = o;
}




SUIT_event SUIT_adjustEventForObject (SUIT_event starter, SUIT_object parent, SUIT_object child)
{
    SUIT_event event;
    SUIT_viewport vp_child, vp_parent;

    if (parent == NULL)
	parent = global.root;
    vp_child = SUIT_mapViewportToScreen (child, OBJECT_VIEWPORT (child));
    vp_parent = SUIT_mapViewportToScreen (parent, OBJECT_VIEWPORT (parent));
    event.relativePixelLocation.x = starter.relativePixelLocation.x - (vp_child.bottom_left.x - vp_parent.bottom_left.x);
    event.relativePixelLocation.y = starter.relativePixelLocation.y - (vp_child.bottom_left.y - vp_parent.bottom_left.y);
    GP_setWindow (OBJECT_WINDOW (child));
    GP_setViewport (SUIT_mapViewportToScreen (child, OBJECT_VIEWPORT (child)));
    event.worldLocation = GP_unMapPoint (SRGP_defPoint (event.relativePixelLocation.x + vp_child.bottom_left.x,
		   event.relativePixelLocation.y + vp_child.bottom_left.y));
    event.type = starter.type;
    event.keyboard = starter.keyboard;
    event.locator = starter.locator;
    return event;
}



void si_openObject (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "si_openObject was called with a null object pointer.\n"));
    if (o == SUIT_name (SUIT_PROPERTY_EDITOR))
	return;			/* Don't allow the property editor to be open */
    OBJECT_OPEN (o) = TRUE;
    SUIT_redisplayRequired (o);
    OBJECT_OPTIMIZED (o) = FALSE;
    if (OBJECT_SELECTED (o))
	SUIT_deselectObject (o);
    RebuildOpenList ();
}


void si_closeObject (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "si_closeObject was called with a null object pointer.\n"));
    if (o == global.root)	/* never close root */
	return;
    RecursivelyClose (o);
    RebuildOpenList ();
}



void SUIT_selectObject (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_selectObject was called with a null object pointer.\n"));
    if (o == SUIT_name (SUIT_PROPERTY_EDITOR))
	return;			/* Don't allow the property editor to be
				 * selected */
    if (OBJECT_SELECTED (o))
	return;
    if (OBJECT_OPEN (o))
	si_closeObject (o);
    OBJECT_SELECTED (o) = TRUE;
    SUIT_redisplayRequired (o);
    OBJECT_OPTIMIZED (o) = FALSE;
    DynAdd (SelectedObjects, (void *) &o);
}



void si_selectObjectAndDeselectParent (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "si_selectObjectAndDeselectParent was called with a null object pointer.\n"));
    SUIT_selectObject (o);
    if (SUIT_getParent (o) != global.root)
	SUIT_deselectObject (SUIT_getParent (o));
}



void SUIT_deselectObject (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_deselectObject was called with a null object pointer.\n"));
    if (!OBJECT_SELECTED (o))
	return;
    SUIT_eraseObject (o);
    OBJECT_SELECTED (o) = FALSE;
    SUIT_redisplayRequired (o);
    OBJECT_OPTIMIZED (o) = FALSE;
    DeleteObjectFromList (SelectedObjects, o);
}



void si_deselectAll (void)
{
    while (DynSize (SelectedObjects) > 0)
	SUIT_deselectObject (*((SUIT_object *) DynGet (SelectedObjects, DynHigh (SelectedObjects))));
}



/* si_mapPointToObject could call this function to save space.  I figured
 * this way saves time though.   Nat 2/18/91 */

boolean SUIT_pointInObject (SUIT_object obj, int x, int y)
{
    SUIT_viewport vp;
    if (obj == NULL)
	return FALSE;
    vp = ViewportPlusBorder (obj, SUIT_getInteger(obj, BORDER_WIDTH));
    return (x >= vp.bottom_left.x && x <= vp.top_right.x &&
	    y >= vp.bottom_left.y && y <= vp.top_right.y);
}



PRIVATE SUIT_object si_whoIsOverMe (SUIT_object o, SUIT_object me)
{
    int i;
    SUIT_object child = NULL;
    SUIT_viewport vp;

    vp = SUIT_mapViewportToScreen (me, OBJECT_VIEWPORT (me));

    if (o == NULL)
	return NULL;
    else if (o == me)
	return me;
    else if (SUIT_viewportsOverlap (SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o)), vp) &&
	     (o != global.root))
	return o;
    else
	for (i = 0; i < SUIT_numberOfChildren (o); i++) {
	    child = si_whoIsOverMe (SUIT_getChild (o, i), me);
	    if (child != NULL)
		return child;
	}
    return child;
}



boolean SUIT_isAnyoneOverMe (SUIT_object me)
{
    return (me != si_whoIsOverMe (global.root, me));
}



SUIT_object si_mapPointToObject (DynArray objectList, int x, int y)
{
    int i;
    SUIT_object obj;

    ENTER (3, (buf, "si_mapPointToObject(%d,%d)\n", x, y));
    if (objectList == NULL)
	return (SUIT_object) NULL;
    for (i = DynLow (objectList); i <= DynHigh (objectList); i++) {
	obj = *((SUIT_object *) DynGet (objectList, i));
	
/* used to read this way
	if ((obj == global.root || si_isVisible(obj) ||
	     (global.propertyEditorIsActive && SUIT_isAncestor (obj, ObjectBeingEdited()))) &&
	     SUIT_pointInObject (obj, x, y)) {
*/	     

	if ((obj == global.root || si_isVisible(obj)) && SUIT_pointInObject (obj, x, y)) {
	    LEAVE (3, (buf, "si_mapPointToObject(%d,%d)\n", x, y));
	    return obj;
	}
    }
    LEAVE (3, (buf, "si_mapPointToObject(%d,%d) ==> NULL\n", x, y));
    return (SUIT_object) NULL;
}



PRIVATE SUIT_object SearchDown (SUIT_object o, point p)
{
    SUIT_object retval;

    if ((retval = si_mapPointToObject (SUIT_getChildren (o), p.x, p.y)) == NULL)
	if ((retval = si_mapPointToObject (SUIT_getEmployees (o, SUIT_getEnumString (o, ACTIVE_DISPLAY)),
					   p.x, p.y)) == NULL)
	    return o;
    return SearchDown (retval, p);
}



SUIT_object SUIT_mapPointToObject (point p)
{
    return SearchDown (global.root, p);
}



void SUIT_passEventDown (SUIT_object o, SUIT_event e)
{
    int x, y;
    SUIT_object whichChild;
    char *disp = SUIT_getEnumString (o, ACTIVE_DISPLAY);

    x = e.locator.position.x;
    y = e.locator.position.y;

    whichChild = si_mapPointToObject (SUIT_getChildren (o), x, y);
    if (whichChild != NULL)
	SUIT_hitObject (whichChild, SUIT_adjustEventForObject (e, o, whichChild));
    else {
	whichChild = si_mapPointToObject (SUIT_getEmployees (o, disp), x, y);
	if (whichChild != NULL)
	    SUIT_hitObject (whichChild, SUIT_adjustEventForObject (e, o, whichChild));
    }
}


#define MOUSE_TOLERANCE     4	/* pixels */


/* note: TimeForClick is in microseconds */

PRIVATE SUIT_eventType si_whichClickEvent (time_t TimeForClick, deluxe_locator_measure original, GP_time initial)
{
    deluxe_locator_measure mouse;
    GP_time now;
    point start;
    int whichButton = original.button_of_last_transition;

    now = initial;
    start = original.position;

    si_inSample ();
    SRGP_sampleDeluxeLocator (&mouse);

    while (mouse.button_chord[whichButton] != UP &&
	   GP_timeDifference (now, initial) < TimeForClick &&
	   si_positionsEqual (mouse.position, start, MOUSE_TOLERANCE)) {
	now = GP_getCurrentTime ();
	SRGP_sampleDeluxeLocator (&mouse);
    }
    SRGP_sampleDeluxeLocator (&mouse);
    si_outSample ();

    if (mouse.button_chord[whichButton] == DOWN)
	return MOUSE_DOWN;
    return CLICK;
}



void SUIT_checkAndProcessInput (int numSecs)
{
    SUIT_limitedCheckAndProcessInput (numSecs, SUIT_getChildren (global.root));
}



void SUIT_callPropertyEditor (SUIT_object o)
{
    global.propertyEditor (o);
}


boolean IgnoreMouseUp = FALSE;	/* This fixes the extra mouse-up in the
				 * property editor */


void SUIT_limitedCheckAndProcessInput (int numSecs, DynArray activeObjects)
{
    static deluxe_locator_measure previousLocator;
    static boolean sampling = FALSE;
    static boolean firstTime = TRUE;
    static char kbdBuffer[2];
    SUIT_object obj = NULL, firstObject = NULL;
    deluxe_keyboard_measure keyboard;
    deluxe_locator_measure current;
    int whichDevice;
    SUIT_eventType type = CLICK;/* initialize it to make the compiler happy */
    SUIT_event event;
    SUIT_object objBeforeTrapper = NULL;

    ENTER (1, (buf, "SUIT_limitedCheckAndProcessInput()\n"));

    SUIT_performRedisplay ();	/* SLOW */

    if (DynSize (activeObjects) > 0)
	firstObject = * (SUIT_object *) DynGet (activeObjects, 0);

    InitializeKeyboardMeasure (&keyboard, kbdBuffer, 2);
    if (firstTime) {
	InitializeLocatorMeasure (&previousLocator);
	global.objectGetsMouse = NULL;
	firstTime = FALSE;
    }

    whichDevice = SRGP_waitEvent (0);	/* peek to see if there's an event
					 * waiting */

    if (whichDevice == NO_DEVICE && !sampling)
	whichDevice = SRGP_waitEvent ((global.animated && numSecs == INDEFINITE) ? 0 : numSecs);

    if (whichDevice == NO_DEVICE && sampling) {
	/* si_inSample (); */
	current = si_waitForAnyChange (previousLocator, TRUE);
	/* si_outSample(); */
	if (si_buttonsEqual (current, previousLocator) &&
	    si_atLeastOneButtonDown (current)) {
	    whichDevice = LOCATOR;
	    type = MOUSE_MOTION;
	} else if (!si_atLeastOneButtonDown (current)) {
	    whichDevice = LOCATOR;
	    type = MOUSE_UP;
	    sampling = FALSE;
	    si_outSample ();
	}
	previousLocator = current;
    }

    else if (whichDevice == LOCATOR) {
	SRGP_getDeluxeLocator (&current);
	if (current.button_chord[current.button_of_last_transition] == UP)
	    type = MOUSE_UP;
	else
	    type = si_whichClickEvent (500, current, GP_getCurrentTime ());
	sampling = (type == MOUSE_DOWN);
	if (!sampling)
	    si_outSample ();
    }

    else if (whichDevice == KEYBOARD) {
	type = KEYSTROKE;
	SRGP_getDeluxeKeyboard (&keyboard);
	current.position = keyboard.position;
	current.modifier_chord[SHIFT] = keyboard.modifier_chord[SHIFT];
	current.modifier_chord[CONTROL] = keyboard.modifier_chord[CONTROL];
	current.modifier_chord[META] = keyboard.modifier_chord[META];
    }

    
    if (whichDevice != NO_DEVICE) {
	if (current.modifier_chord[SHIFT] && current.modifier_chord[CONTROL]) {

	    /* the user is talking to SUIT */

	    if (global.interactiveToolsAvailable) {
		obj = si_mapPointToObject (OpenObjects, current.position.x, current.position.y);
		si_processSUITCommand (obj, keyboard.buffer[0], current, type, activeObjects);
	    } else {
		char buf[200];
		sprintf (buf, "Sorry, but SUIT's interactive tools are disabled,\
because this application was initialized with code, not a .sui file.  \
This program was developed with:\n\n@c(SUIT Version %d.%d)\n\n\
@c(brought to you by)\n@c(the University of Virginia)",
			 SUIT_VERSION, SUIT_REVISION);
		SUIT_inform (buf);
	    }
	}

	else {

	    /* the user is talking to the application */
	    
	    if (global.objectGetsMouse != NULL)
		obj = global.objectGetsMouse;
	    else if (obj == global.root || obj == NULL ||
		     !SUIT_pointInObject (obj, current.position.x, current.position.y) ||
		     SUIT_isAnyoneOverMe (obj))
		obj = si_mapPointToObject (activeObjects, current.position.x, current.position.y);
	    
	    if (type == MOUSE_UP)
		global.objectGetsMouse = NULL;
	    
	    event = CreateEvent (obj, type, current, keyboard.buffer[0]);
	    
	    if (global.trapperFunctions != NULL) {
		int i;
		objBeforeTrapper = obj;
		for (i=DynHigh(global.trapperFunctions); obj == objBeforeTrapper && i >= 0; i--) {
		    SUIT_trapperPtr trap = * (SUIT_trapperPtr *) DynGet(global.trapperFunctions, i);
		    obj = trap (obj, &event);
		}
	    }
	    
	    if (obj == NULL && global.propertyEditorIsActive &&
		!(objBeforeTrapper != NULL && obj == NULL)) {
		obj = SUIT_name (SUIT_PROPERTY_EDITOR);
		event = CreateEvent (obj, type, current, keyboard.buffer[0]);
	    }
	    
	    if (event.type == MOUSE_UP && IgnoreMouseUp) {
		obj = NULL;
		IgnoreMouseUp = FALSE;
	    }
	    if (obj != NULL)
		SUIT_hitObject (obj, event);
	}
	
    }
    /* } while ((whichDevice = SRGP_waitEvent(0)) != NO_DEVICE); */
    
    si_actuallyDestroyObjects ();
    
    LEAVE (1, (buf, "SUIT_limitedCheckAndProcessInput()\n"));
}


char *SUIT_eventName (SUIT_event evt)
{
    switch (evt.type) {
	case MOUSE_DOWN: return "MOUSE_DOWN";
	case MOUSE_UP: return "MOUSE_UP";
	case MOUSE_MOTION: return "MOUSE_MOTION";
	case CLICK: return "CLICK";
	case KEYSTROKE: return "KEYSTROKE";
    }
    return ""; /* should never get here -- this is just to make the compiler happy */
}
