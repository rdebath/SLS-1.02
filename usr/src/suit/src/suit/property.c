/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "privsuit.h"


/* Dummy Functions: These functions are used with DynFindIndex: it only
 * allows searching for an element, so these allow us to create an element of
 * the appropriate type which contains the desired name.
 * 
 * DANGER: As you can see, this dummy guy shouldn't be used in any real
 * capacity: we only assign the pointer you pass, so that could obviously
 * cause trouble if you really used the pointer returned.
 * 
 * Modified by R. Pausch March 8, 1991. */



property *DummyProperty (char *name)
{
    static property dummy;
    dummy.name = name;
    return (&dummy);
}

SUIT_class **DummyNamedPropertyList (char *name)
{
    static SUIT_class dummy;
    static SUIT_class *ptr = &dummy;
    dummy.name = name;
    return (&ptr);
}

SUIT_type *DummyType (char *name)
{
    static SUIT_type dummy;
    dummy.name = name;
    return (&dummy);
}

SUIT_display *DummyDisplay (char *name)
{
    static SUIT_display dummy;
    dummy.name = name;
    return (&dummy);
}

nameTableEntry *DummyNameTableEntry (char *name)
{
    static nameTableEntry dummy;
    dummy.name = name;
    return (&dummy);
}

/* Compare functions:
 * 
 * These allow us to use DynFindIndex and therefore DynSort on these types. */

#define STRCMP  CaseInsensitiveMatch



int CompareProperty (void *first, void *second)
{
    property *f = (property *) first;
    property *s = (property *) second;
    return STRCMP(f->name, s->name);
}

int CompareNamedPropertyList (void *first, void *second)
{
    SUIT_class *f = * (SUIT_class **) first;
    SUIT_class *s = * (SUIT_class **) second;
    return (STRCMP(f->name, s->name));
}

int CompareTypes (void *first, void *second)
{
    SUIT_type *f = (SUIT_type *) first;
    SUIT_type *s = (SUIT_type *) second;
    return (STRCMP(f->name, s->name));
}

int CompareDisplays (void *first, void *second)
{
    SUIT_display *f = (SUIT_display *) first;
    SUIT_display *s = (SUIT_display *) second;
    return (STRCMP(f->name, s->name));
}

int CompareNameTableEntry (void *first, void *second)
{
    nameTableEntry *f = (nameTableEntry *) first;
    nameTableEntry *s = (nameTableEntry *) second;
    return (STRCMP(f->name, s->name));
}

int CompareCallbackFunctionPtr (void *first, void *second)
{
    SUIT_callbackFunctionPtr *f = (SUIT_callbackFunctionPtr *) first;
    SUIT_callbackFunctionPtr *s = (SUIT_callbackFunctionPtr *) second;
    return (((unsigned) (*f)) - ((unsigned) (*s)));
}




#define DEFAULT_CLICK_TIME             0.5
#define DEFAULT_DOUBLE_CLICK_TIME      0.5

char *SUIT_borderTypes[] = { "simple", "motif", "fancy motif" };

void si_initializeProperties (void)
{
    ENTER (1, (buf, "si_initializeProperties\n"));

    SUIT_deluxeSetBoolean (global.root, CLIP_TO_VIEWPORT, TRUE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, HAS_BORDER, TRUE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, HAS_BACKGROUND, TRUE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, DRAW_BORDER_ON_INSIDE, FALSE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, BORDER_RAISED, TRUE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, VISIBLE, TRUE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, SHRINK_TO_FIT, FALSE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, ANIMATED, FALSE, GLOBAL);
    SUIT_deluxeSetBoolean (global.root, SHOW_TEMPORARY_PROPERTIES, FALSE, GLOBAL);
    
    SUIT_deluxeSetBoolean (global.root, CACHE_USING_CANVAS, FALSE, GLOBAL);
      SUIT_makePropertyTemporary (global.root, CACHE_USING_CANVAS, GLOBAL);
      SUIT_lockProperty(global.root, CACHE_USING_CANVAS, GLOBAL);

    SUIT_deluxeSetColor (global.root, BORDER_COLOR, GP_defColor ("grey", BLACK_ON_MONO), GLOBAL);
    SUIT_deluxeSetColor (global.root, BACKGROUND_COLOR, GP_defColor ("grey", WHITE_ON_MONO), GLOBAL);
    SUIT_deluxeSetColor (global.root, FOREGROUND_COLOR, GP_defColor ("black", BLACK_ON_MONO), GLOBAL);

    SUIT_deluxeSetInteger (global.root, BORDER_WIDTH, 2, GLOBAL);
    SUIT_deluxeSetInteger (global.root, SCREEN_WIDTH, DEFAULT_SCREEN_WIDTH, GLOBAL);
      SUIT_makePropertyTemporary (global.root, SCREEN_WIDTH, OBJECT);
      SUIT_lockProperty (global.root, SCREEN_WIDTH, GLOBAL);
    SUIT_deluxeSetInteger (global.root, SCREEN_HEIGHT, DEFAULT_SCREEN_HEIGHT, GLOBAL);
      SUIT_makePropertyTemporary (global.root, SCREEN_HEIGHT, OBJECT);
      SUIT_lockProperty (global.root, SCREEN_HEIGHT, GLOBAL);
    SUIT_deluxeSetInteger (global.root, DEFAULT_OBJECT_HEIGHT, 80, GLOBAL);
    SUIT_deluxeSetInteger (global.root, DEFAULT_OBJECT_WIDTH, 80, GLOBAL);
    SUIT_deluxeSetInteger (global.root, MARGIN, 5, GLOBAL);

    SUIT_deluxeSetWindow (global.root, WINDOW, GP_defRectangle (0.0, 0.0, 1.0, 1.0), GLOBAL);
    SUIT_lockProperty(global.root, WINDOW, GLOBAL);

    SUIT_deluxeSetSpringiness (global.root, SPRINGINESS, ALL_SPRINGINESS, GLOBAL);
    SUIT_deluxeSetFont (global.root, FONT, GP_defFont ("times", "", 12.0), GLOBAL);
    SUIT_deluxeSetFont (global.root, SUIT_SYSTEM_FONT, GP_defFont ("helvetica", "", 14.0), GLOBAL);
    SUIT_deluxeSetEnum (global.root, BORDER_TYPE, SUIT_defEnum("motif", 3, SUIT_borderTypes), GLOBAL);

    LEAVE (1, (buf, "si_initializeProperties\n"));
}




SUIT_propertyCallback propertyIteratorCallback;

PRIVATE void propertyCallout(SUIT_object o)
{    
    int i;
    DynArray proplist = o->props;
    
    for (i = DynLow(proplist); i <= DynHigh(proplist); i++)
    {
	property *p = (property *) DynGet (proplist, i);
	propertyIteratorCallback(OBJECT, OBJECT_NAME(o), p->name, p->type, p->value, p->permanent); 
    }
    
}	

/* this function iterates over all properties stored in the system */
void SUIT_iterateOverAllProperties (SUIT_propertyCallback callback)
{
    int i, j;
    DynArray proplist = global.root->props;
    
    for (i = DynLow(proplist); i <= DynHigh(proplist); i++) {
	property *p = (property *) DynGet (proplist, i);
	callback(GLOBAL, NULL, p->name, p->type, p->value, p->permanent ); 
    }
    
    for (j = DynLow (global.classProperties); j <= DynHigh(global.classProperties); j++) {
     	SUIT_class *class = * (SUIT_class **) DynGet (global.classProperties, j);
 	proplist = class->props;
	for (i = DynLow(proplist); i <= DynHigh(proplist); i++) 	
	{ 	 
	    property *p = (property *) DynGet (proplist, i);
	    callback(CLASS, class->name, p->name, p->type, p->value, p->permanent); 	
	} 
    }
    
    propertyIteratorCallback = callback;
    SUIT_iterateOverObjects(propertyCallout);
    
}

/* si_getType: This function is used to return a pointer to the correct
 * SUIT_type. These types are stored in a dynamic array, which is initialized
 * at startup. The types list will eventually be stored in the deftypes unit,
 * rather than in the global structure. */

SUIT_type *si_getType (char *typeName)
{
    int slot;
    SUIT_type *retval;

    ENTER (5, (buf, "si_getType(%s)\n", typeName));
    ASSERT ((VALIDSTRING (typeName)), (mes, "si_getType called with invalid string (typeName).\n"));
    slot = DynFindIndex (global.types, DummyType (typeName), CompareTypes);
    if (slot != DYN_NOT_FOUND)
	retval = (SUIT_type *) DynGet (global.types, slot);
    else {
	ASSERT ((FALSE), (mes, "si_getType failed to find the type %s\n", typeName));
	retval = NULL;
    }
    LEAVE (5, (buf, "si_getType(%s)\n", typeName));
    return (retval);
}


SUIT_display *si_getDisplay (SUIT_object o, char *displayName)
{
    int slot;
    SUIT_display *retval;

    ENTER (4, (buf, "si_getDisplay(%s)\n", displayName));

    ASSERT ((VALIDSTRING (displayName)), (mes, "si_getDisplay called with invalid string (displayName).\n"));
    ASSERT ((o != NULL), (mes, "si_getDisplay was called with a null object pointer.\n"));

    if (SUIT_stringsMatch ("none", displayName))
	return NULL;
    slot = DynFindIndex (o->displays, DummyDisplay (displayName), CompareDisplays);
    ASSERT ((slot != DYN_NOT_FOUND), (mes, "si_getDisplay failed to find the display \"%s\"\n", displayName));
    retval = (SUIT_display *) DynGet (o->displays, slot);
    LEAVE (4, (buf, "si_getDisplay(%s)\n", displayName));
    return (retval);
}


/* si_searchForClass This functions returns the class property list
 * corresponding to a given class name (character string).  The class
 * property lists are stored in a dynamic array.  */

SUIT_class *si_searchForClass (char *name)
{
    int slot = DynFindIndex (global.classProperties, DummyNamedPropertyList (name), CompareNamedPropertyList);
    ASSERT ((VALIDSTRING (name)), (mes, "si_searchForClass called with invalid string (%s).\n", name));
    if (slot == DYN_NOT_FOUND)
	return NULL;
    else
	return * (SUIT_class **) DynGet (global.classProperties, slot);
}



SUIT_class *FindClassAndAddItIfNecessary (char *name)
{
    SUIT_class *ptr;

    ENTER (4, (buf, "FindClassAndAddItIfNecessary(%s)\n", name));
    ASSERT ((VALIDSTRING (name)), (mes, "FindClassAndAddItIfNecessary called with an invalid string (%s).\n", name));
    ptr = si_searchForClass (name);

    if (ptr == NULL) {
	SUIT_class *temp = (SUIT_class *) SUIT_malloc(sizeof(SUIT_class));
	temp->name = SUIT_createRCString (name);
	temp->props = DynCreate (sizeof (property), 5);
	temp->interestCallbacks = NULL;
	DynAdd (global.classProperties, (void *) &temp);
	ptr = si_searchForClass (name);
    }

    LEAVE (4, (buf, "FindClassAndAddItIfNecessary(%s)\n", name));
    return (ptr);
}



/* FindProperty: This function returns a pointer to a property given a list
 * of properties and the name of the property.  If it is not found, NULL is
 * returned. */

property *FindProperty (DynArray list, char *name)
{
    int slot;
    property *retval;

    ENTER (5, (buf, "FindProperty(dynarray,%s)\n", name));
    ASSERT ((VALIDSTRING (name)), (mes, "FindProperty was called with an invalid property name (%s).\n",(name==NULL)?"NULL":name));
    slot = DynFindIndex (list, DummyProperty (name), CompareProperty);

    if (slot != DYN_NOT_FOUND)
	retval = (property *) DynGet (list, slot);
    else
	retval = (property *) NULL;

    LEAVE (5, (buf, "FindProperty(dynarray,%s) ==> %s\n", name, (retval == NULL) ? "NOT found" : "found"));
    return (retval);
}



DynArray si_getPropertyList (SUIT_object obj, SUIT_level level)
{
    switch (level) {
      case OBJECT:   return obj->props;
      case CLASS:    return obj->classInfo->props;
      case GLOBAL:   return global.root->props;
    }
    return NULL;  /* will never get here */
}


/* si_searchForProperty looks at the specified level for a property.  If it
 * exists it returns it, otherwise it returns NULL. */

property *si_searchForProperty (SUIT_object obj, SUIT_level level, char *propertyName)
{
    property *theProp;

    ENTER (5, (buf, "si_searchForProperty(%s,%s,%s)\n", OBJECT_NAME(obj), SUIT_levelName(level), propertyName));
    ASSERT ((VALIDSTRING (propertyName)), (mes, "si_searchForProperty was called with an invalid property name (%s).\n",(propertyName==NULL)?"NULL":propertyName));

    theProp = FindProperty (si_getPropertyList (obj, level), propertyName);
    LEAVE (5, (buf, "si_searchForProperty(%s,%s,%s)\n", OBJECT_NAME(obj), SUIT_levelName(level), propertyName));
    return theProp;
}



boolean SUIT_propertyExists (SUIT_object obj, char *propertyName,
			     char *propertyType, SUIT_level level)
{
    property *prop = si_searchForProperty (obj, level, propertyName);
    if (prop)
	return SUIT_stringsMatch(propertyType, prop->type) ? TRUE : FALSE;
    return FALSE;
}



/* This routine looks for a property of an object, using the standard SUIT search:
     object level --> class level --> parent's object level --> ... --> global level
*/ 

property *si_searchForPropAtLevels (SUIT_object obj, char *propertyName, SUIT_level *level)
{
    property *guy = NULL;

    ENTER (6, (buf, "si_searchForPropAtLevels(%s,%s,ptr)\n",OBJECT_NAME(obj),
		propertyName));
    if (obj == global.root)
	obj = NULL;
    *level = GLOBAL;
    if (obj != NULL) {
	guy = si_searchForProperty (obj, OBJECT, propertyName);
	*level = OBJECT;
	if (guy == NULL) {
	    guy = si_searchForProperty (obj, CLASS, propertyName);
	    *level = CLASS;
	    if (guy == NULL) {
		SUIT_object parent = SUIT_getParent(obj);
		while (guy == NULL && parent != global.root) {
		    guy = si_searchForProperty (parent, OBJECT, propertyName);
		    parent = SUIT_getParent(parent);
		}
		*level = GLOBAL;
	    }
	}
    }
    if (guy == NULL)
	guy = si_searchForProperty (global.root, OBJECT, propertyName);
    LEAVE (6, (buf, "si_searchForPropAtLevels(%s,%s,%s) ==> %s\n",OBJECT_NAME(obj),
		propertyName,SUIT_levelName(*level),guy?"ptr":"NULL"));
    return guy;
}




/* This function is very similiar to the function getPermanentObjectInClass, perhaps
 * these should be combined into a tree utility which allows searching
 * through the tree doing a particular test.  Nat  3/5/91

 just wanted to add that the goto wansn't my idea ;^)  Rob  */

PRIVATE SUIT_object getAnimatedObject (SUIT_object o)
{
    int i, j, k;
    SUIT_object temp, obj = NULL;
    SUIT_display disp;

    if (o == NULL)
	return (NULL);
    else if (SUIT_getBoolean (o, ANIMATED))
	return (o);
    else {
	for (i = 0; i < SUIT_numberOfChildren (o); i++) {
	    temp = SUIT_getChild (o, i);
	    obj = getAnimatedObject (temp);
	    if (obj != NULL)
		goto end;
	}
	for (j = DynLow (o->displays); j < DynSize (o->displays); j++) {
	    disp = *(SUIT_display *) DynGet (o->displays, j);
	    for (k = 0; k < SUIT_numberOfEmployees (o, disp.name); k++) {
		obj = getAnimatedObject (SUIT_getEmployee (o, disp.name, k));
		if (obj != NULL)
		    goto end;
	    }
	}
    }
end:return (obj);
}



/* This routine keeps track of whether any object is animated.  For efficiency, we just keep
   a list of animated objects.  If the list is empty, no animated objects.  Furthermore for
   efficiency, we don't check to see if an object's already on the list when we add it; we do
   however check for duplicates when we delete an object from the list.
*/

PRIVATE DynArray AnimatedObjects = NULL;

PRIVATE void si_adjustAnimation (SUIT_object o)
{
    if (AnimatedObjects == NULL)
	AnimatedObjects = DynCreate(sizeof(SUIT_object), 1);

    if (SUIT_getBoolean (o, ANIMATED))
	DynAdd (AnimatedObjects, (void *) &o);
    else
	DeleteObjectFromList (AnimatedObjects, o);
    global.animated = (DynSize(AnimatedObjects) > 0);
}


/* This routine flags redisplay. If the level is GLOBAL, then all object are flagged for redisplay;
   if it's CLASS, all objects in the given object's class are flagged; if it's OBJECT, then the
   given object is flagged. 
*/

PRIVATE void MarkAppropriateRedisplay (SUIT_object obj, SUIT_level level)
{
    switch (level) {
      case GLOBAL: SUIT_allObjectsRequireRedisplay (NULL); break;
      case CLASS: SUIT_allObjectsRequireRedisplay (obj->classInfo->name); break;
      case OBJECT: SUIT_redisplayRequired (obj); break;
    }
}



PRIVATE property createProp (char *propertyName, char *propertyType, Pointer newValue)
{
    SUIT_type *type = si_getType (propertyType);

    property newGuy;
    newGuy.name = SUIT_createRCString (propertyName);
    newGuy.type = SUIT_createRCString (propertyType);
    
    newGuy.value = type->copy (newValue);
    return (newGuy);
}



PRIVATE void HandleBorderSizeChange (SUIT_object o, int oldwidth)
{
    SUIT_viewport vp;
    vp = ViewportPlusBorder (o, oldwidth);
    ENTER (2, (buf, "SUIT_eraseObject(%s)\n", OBJECT_NAME (o)));
    if (si_isVisible(o) && !SUIT_getBoolean(o, DRAW_BORDER_ON_INSIDE)) {
	si_clearArea (vp);
	SUIT_redrawLocalSection (o, vp);
    }
    LEAVE (2, (buf, "SUIT_eraseObject(%s)\n", OBJECT_NAME (o)));
}



PRIVATE void HandleBorderChange (SUIT_object obj)
{
    if (si_isVisible(obj) && !SUIT_getBoolean(obj, DRAW_BORDER_ON_INSIDE))
	SUIT_eraseObject (obj);
}



PRIVATE void FixChildrensOffsets (SUIT_object o, SUIT_viewport vp)
{
    int i;

    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	SUIT_viewport kvp;
	kvp = OBJECT_VIEWPORT(kid);
	kid->offset = SRGP_defPoint (o->offset.x + vp.bottom_left.x, o->offset.y + vp.bottom_left.y);
	FixChildrensOffsets (kid, kvp);
    }
    if (o->displays)
	for (i=0; i<=DynHigh(o->displays); i++) {
	    SUIT_display *disp = (SUIT_display *) DynGet(o->displays, i);
	    int j;
	    if (disp->employees != NULL)
		for (j=0; j<=DynHigh(disp->employees); j++) {
		    SUIT_object emp = * (SUIT_object *) DynGet (disp->employees, j);
		    SUIT_viewport evp;
		    evp = OBJECT_VIEWPORT(emp);
		    emp->offset = SRGP_defPoint (o->offset.x + vp.bottom_left.x, o->offset.y + vp.bottom_left.y);
		    FixChildrensOffsets (emp, evp);
		}
	}
}



/*
  This very ugly routine handles resizing children using their springiness. When ever
  SUIT_setProperty is called to change an object's VIEWPORT property and that new viewport
  has a different width or height than the old one, this routine is called to resize that 
  object's children. This routine is tricy since, in order to change the childrens' viewports,
  it too must call SUIT_setProperty, which will in turn call this routine (indirect recursion).
  This recursion is desirable but makes the rest of the routine ugly.
*/


PRIVATE void FixChildrensViewports (SUIT_object o, SUIT_viewport parentvp, SUIT_viewport old)
{
    static point prevOffset = { -1, -1 };
    int i;
    static level = 0;

    if (level == 0)
	prevOffset = o->offset;
    
    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	SUIT_viewport kvp;
	SUIT_springiness spring;
	SUIT_viewport oldscrn, scrn, kidscrn, newvp, adj;
	point holdOffset;
	
	oldscrn = SRGP_defRectangle (old.bottom_left.x + prevOffset.x, old.bottom_left.y + prevOffset.y,
				     old.top_right.x + prevOffset.x, old.top_right.y + prevOffset.y);
	scrn = SRGP_defRectangle (parentvp.bottom_left.x + o->offset.x, parentvp.bottom_left.y + o->offset.y,
				  parentvp.top_right.x + o->offset.x, parentvp.top_right.y + o->offset.y);
	kvp = OBJECT_VIEWPORT(kid);
	kidscrn = SRGP_defRectangle (kvp.bottom_left.x + kid->offset.x, kvp.bottom_left.y + kid->offset.y,
				     kvp.top_right.x + kid->offset.x, kvp.top_right.y + kid->offset.y);
	holdOffset = prevOffset;
	prevOffset = kid->offset;
	kid->offset = SRGP_defPoint (o->offset.x + parentvp.bottom_left.x,
				     o->offset.y + parentvp.bottom_left.y);
	spring = SUIT_getSpringiness(kid,SPRINGINESS);
	adj = SUIT_adjustForSpringiness(oldscrn,scrn,kidscrn,spring);
	newvp = SUIT_mapScreenToViewport (kid, adj);
	
	level++;
	
	/* The following line causes indirect recursion, since it is a call to SUIT_property,
	   which will eventually lead back to this routine.  */
	
	SUIT_setViewport (kid, VIEWPORT, newvp);  
	
	level--;
	prevOffset = holdOffset;
    }
    for (i=0; i<=DynHigh(o->displays); i++) {
	SUIT_display *disp = (SUIT_display *) DynGet(o->displays, i);
	int j;
	if (disp->employees != NULL)
	    for (j=0; j<=DynHigh(disp->employees); j++) {
		SUIT_object kid = * (SUIT_object*) DynGet(disp->employees,j);
		SUIT_viewport kvp;
		SUIT_springiness spring;
		SUIT_viewport oldscrn, scrn, kidscrn, newvp, adj;
		point holdOffset;
		
		oldscrn = SRGP_defRectangle (old.bottom_left.x + prevOffset.x,
					     old.bottom_left.y + prevOffset.y,
					     old.top_right.x + prevOffset.x,
					     old.top_right.y + prevOffset.y);
		scrn = SRGP_defRectangle (parentvp.bottom_left.x + o->offset.x,
					  parentvp.bottom_left.y + o->offset.y,
					  parentvp.top_right.x + o->offset.x,
					  parentvp.top_right.y + o->offset.y);
		kvp = OBJECT_VIEWPORT(kid);
		kidscrn = SRGP_defRectangle (kvp.bottom_left.x + kid->offset.x,
					     kvp.bottom_left.y + kid->offset.y,
					     kvp.top_right.x + kid->offset.x,
					     kvp.top_right.y + kid->offset.y);
		holdOffset = prevOffset;
		prevOffset = kid->offset;
		kid->offset = SRGP_defPoint (o->offset.x + parentvp.bottom_left.x,
					     o->offset.y + parentvp.bottom_left.y);
		spring = SUIT_getSpringiness(kid,SPRINGINESS);
		adj = SUIT_adjustForSpringiness(oldscrn,scrn,kidscrn,spring);
		newvp = SUIT_mapScreenToViewport (kid, adj);
		
		level++;
		
		/* The following line causes indirect recursion, since it is a call to SUIT_property,
		   which will eventually lead back to this routine.  */
		
		SUIT_setViewport (kid, VIEWPORT, newvp);  
		
		level--;
		prevOffset = holdOffset;
	    }
    }
}



/* This is is a fairly benign hack. Setting the viewport property has the side effect of erasing
   the old object. However, when we are simply making a viewport be relative to a new parent
   (e.g. during SUIT_addChildToObject), this erasing is not only unnecessary, but will be calculated
   incorrectly.  This flag prevents the erasing from taking place.  A cleaner solution would be
   to keep the object's previous offset around (either as an entry in the SUIT_object struct or as
   a property, but this is easier.  -- Rob
*/
boolean OnlyAdjustingViewportAndOffset = FALSE;


PRIVATE boolean SUIT_handleViewport (SUIT_object obj, SUIT_viewport vp, SUIT_viewport *old)
{
    ASSERT ((vp.bottom_left.x <= vp.top_right.x ||
	     vp.bottom_left.y <= vp.top_right.y),
	    (mes, "SUIT_setProperty was called with an invalid viewport (%d %d %d %d)\n",
	     vp.bottom_left.x, vp.bottom_left.y, vp.top_right.x, vp.top_right.y));

    if (obj == NULL)
	obj = global.root;

    if (old != NULL &&
	(vp.top_right.x - vp.bottom_left.x != old->top_right.x - old->bottom_left.x ||
	 vp.top_right.y - vp.bottom_left.y != old->top_right.y - old->bottom_left.y))
	FixChildrensViewports (obj, vp, *old);
    else
	FixChildrensOffsets (obj, vp);
    if (srgpHasBegun && hintsFileRead && (obj->has_been_painted) && old != NULL) {
	if (!OnlyAdjustingViewportAndOffset && !SUIT_viewportsEqual(vp, *old)) {
	    SUIT_eraseObject (obj);
	    return TRUE;
	}
    }
    return FALSE;
}



PRIVATE boolean SUIT_setVisibilityBeforeSet (SUIT_object o, boolean visible)
{
    if (srgpHasBegun && !visible && o->has_been_painted) {
	if (SUIT_getBoolean (o, VISIBLE)) {
	    SUIT_eraseObject (o);
	    o->has_been_painted = FALSE; /* just recently added -- Rob */
	    return (TRUE);
	}
    }
    return (FALSE);
}



/*
PRIVATE void SUIT_setVisibilityAfterSet (SUIT_object o)
{
    o->has_been_painted = FALSE;
    SUIT_redrawLocalSection (o, SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
}
*/



void SUIT_setProperty (SUIT_object obj, char *propertyName, char *propertyType,
		       Pointer propertyPtr, SUIT_level level)
{
    property *replacing; /* the old property in the list we're replacing */
    SUIT_type *type;	 /* information about the type propertyType */
    DynArray theList;
    boolean possibleSideEffect = TRUE;
    int slot;

    ENTER (5, (buf, "SUIT_setProperty(%s,%s,%s,%s)\n", OBJECT_NAME(obj), 
	       propertyName, propertyType, SUIT_levelName(level)));
    ASSERT ((VALIDSTRING (propertyName)),
	    (mes, "SUIT_setProperty was called with an invalid string (propertyName).\n"));
    ASSERT ((VALIDSTRING (propertyType)),
	    (mes, "SUIT_setProperty was called with an invalid string (propertyType).\n"));
    ASSERT ((propertyPtr != NULL),
	    (mes, "SUIT_setProperty was called with a NULL property pointer.\n"));
    ASSERT (((level == GLOBAL) || (obj != NULL)),
	    (mes, "SUIT_setProperty was called with a null object and level not equal to global.\n"));
    
    if (level == GLOBAL || obj == NULL)
	obj = global.root;

    theList = si_getPropertyList (obj, level);
    slot = DynFindIndex (theList, DummyProperty (propertyName), CompareProperty);
    replacing = (slot == DYN_NOT_FOUND)? NULL : (property *) DynGet (theList, slot);
    type = si_getType (propertyType);

    if (!replacing || type->compare (replacing->value, propertyPtr) != 0) {
	property *old;  /* the property who's value was used (maybe inherited) */
	SUIT_level oldlevel;
	property new;
	Pointer oldValue = NULL;  /* the value of the above property */
	boolean afterSetVisibility = FALSE;

	ASSERT ((replacing == NULL || SUIT_stringsMatch(propertyType, replacing->type)),
		(mes, "TYPE MISMATCH: SUIT_setProperty was called with\n\
*     property name \"%s\" of type \"%s\" \n\
*     for object \"%s\" and found\n\
*     a property with that name but of the wrong type (\"%s\").\n\
*     This can happen, for example, when SUIT_setInteger is used to set\n\
*     a property of type double, etc. Because of this type mismatch,\n\
*     the results of this operation may be (very) incorrect.\n", propertyName, propertyType, OBJECT_NAME(obj), replacing->type));


	/* create the new property */
	new.name = SUIT_createRCString (propertyName);
	new.type = SUIT_createRCString (propertyType);
	new.value = type->copy (propertyPtr);
	new.permanent = replacing? replacing->permanent : TRUE;
	new.locked = replacing? replacing->locked : FALSE;

	/* find the value that used to be used (used for interest routines) */
	if (global.interestOn) {
	    old = si_searchForPropAtLevels (obj, propertyName, &oldlevel);
	    oldValue = old? type->copy(old->value) : NULL;
	}

	/* check for some special cases */
	if (SUIT_stringsMatch (propertyName, VIEWPORT))
	    SUIT_handleViewport (obj, *(SUIT_viewport*)propertyPtr, (SUIT_viewport*)oldValue);
	else if (SUIT_stringsMatch (propertyName, VISIBLE))
	    afterSetVisibility = SUIT_setVisibilityBeforeSet (obj, *(boolean *) propertyPtr);
	else if (SUIT_stringsMatch (propertyName, HAS_BORDER) && !(*(boolean *)propertyPtr) &&
		 obj->has_been_painted)
	    HandleBorderChange (obj);
	else if (oldValue != NULL && SUIT_stringsMatch (propertyName, BORDER_WIDTH) &&
		 obj->has_been_painted && (*(int*)propertyPtr) < (*(int*)oldValue))
	    HandleBorderSizeChange (obj, * (int *) oldValue);
	else
	    possibleSideEffect = FALSE;

	if (possibleSideEffect) {
	    /* one of the special case routines might have mucked with a property,
	       thereby invalidating the following two variables ==> re-evaluate them */
	    slot = DynFindIndex (theList, DummyProperty (propertyName), CompareProperty);
	    replacing = (slot == DYN_NOT_FOUND)? NULL : (property *) DynGet (theList, slot);
	}

	/* change the property in the property list */
	if (replacing) {
	    type->destroy (replacing->value);
	    DynDelete (theList, slot);
	}
	DynAdd (theList, (void *) &new);

	/* check for interest routines */
	if (global.interestOn)
	    if (oldValue && oldlevel < level) {
		if (type->compare (oldValue, new.value) != 0)
		    si_checkCallbacks (obj, level, &new, oldValue);
		else if (oldlevel == CLASS && level == OBJECT)
		    si_checkChildrensCallbacks (obj, &new, oldValue);
	    } else
		si_checkCallbacks (obj, level, &new, oldValue);
	
	/* mark redisplay (unless we're not supposed to) */
	if (obj->mark_redisplay)
	    MarkAppropriateRedisplay (obj, level);

	/* if (SUIT_stringsMatch (propertyName, VISIBLE) && afterSetVisibility)
	    SUIT_setVisibilityAfterSet (obj);
	else*/
	if (SUIT_stringsMatch (propertyName, ANIMATED))
	    si_adjustAnimation (obj);

	if (oldValue)
	    type->destroy (oldValue);
    }

    LEAVE (5, (buf, "SUIT_setProperty(%s,%s,%s,%s)\n",OBJECT_NAME(obj),
	       propertyName, propertyType, SUIT_levelName(level)));
}



/*--------------------------------------------------------------------------------*/
/*              The type-specific SUIT_setProperty calls                          */
/*--------------------------------------------------------------------------------*/


void SUIT_setInteger (SUIT_object obj, char *name, int value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setInteger was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name,"int", (Pointer) &value, OBJECT);
}

    
void SUIT_setFunctionPointer (SUIT_object obj, char *name, SUIT_functionPointer value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setFunctionPointer was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name, "SUIT_functionPointer", (Pointer) &value, OBJECT);
}

    
void SUIT_setDouble (SUIT_object obj, char *name, double value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setDouble was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name,"double", (Pointer) &value, OBJECT);
}

    
void SUIT_setText (SUIT_object obj, char *name, char *value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setText was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name, "text", (Pointer) value, OBJECT);
}

    
void SUIT_setFont (SUIT_object obj, char *name, GP_font value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setFont was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name, "GP_font", (Pointer) &value, OBJECT);
}

    
void SUIT_setColor (SUIT_object obj, char *name, GP_color value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setColor was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj, name, "GP_color", (Pointer) &value, OBJECT);
}

    
void SUIT_setViewport (SUIT_object obj, char *name, SUIT_viewport value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setViewport was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj,name,VIEWPORT,(Pointer) &value, OBJECT);
}

    
void SUIT_setWindow (SUIT_object obj, char *name, SUIT_window value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setWindow was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty(obj,name,WINDOW,(Pointer) &value, OBJECT);
}

    
void SUIT_setObject (SUIT_object obj, char *name, SUIT_object value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setObject was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj, name, "SUIT_object", (Pointer) &value, OBJECT);
    SUIT_makePropertyTemporary (obj, name, OBJECT);
}

    
void SUIT_setDynArray (SUIT_object obj, char *name, DynArray value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setDynArray was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj, name, "DynArray", (Pointer) &value, OBJECT);
    SUIT_makePropertyTemporary (obj, name, OBJECT);
}


void SUIT_setSpringiness (SUIT_object obj, char *name, SUIT_springiness value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setSpringiness was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj, name, "SUIT_springiness", (Pointer) &value, OBJECT);
}

    
void SUIT_setEnum (SUIT_object obj, char *name, SUIT_enum value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setEnum was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj,name, "SUIT_enum", (Pointer) &value, OBJECT);
}


void SUIT_setBoolean (SUIT_object obj, char *name, boolean value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setBoolean was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj, name, "boolean", (Pointer) &value, OBJECT);
}


void SUIT_setTextList (SUIT_object obj, char *name, SUIT_textList value)
{
    ASSERT ((obj != NULL), (mes, "SUIT_setTextList was called with a NULL object.  Use SUIT_deluxeSetProperty to set global properties.\n"));
    SUIT_setProperty (obj, name, "SUIT_textList", (Pointer) &value, OBJECT);
}



/*--------------------------------------------------------------------------------*/
/*      The "deluxe" type-specific SUIT_setProperty calls                         */
/*--------------------------------------------------------------------------------*/


void SUIT_deluxeSetInteger (SUIT_object obj, char *name, int value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"int", (Pointer) &value, level);
}

     
void SUIT_deluxeSetFunctionPointer (SUIT_object obj, char *name, SUIT_functionPointer value, SUIT_level level)
{
    SUIT_setProperty(obj, name, "SUIT_functionPointer", (Pointer) &value, level);
}

    
void SUIT_deluxeSetBoolean (SUIT_object obj, char *name, boolean value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"boolean", (Pointer) &value, level);
}

    
void SUIT_deluxeSetDouble (SUIT_object obj, char *name, double value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"double", (Pointer) &value, level);
}

    
void SUIT_deluxeSetText (SUIT_object obj, char *name, char *value, SUIT_level level)
{
    SUIT_setProperty(obj, name, "text", (Pointer) value, level);
}

    
void SUIT_deluxeSetFont (SUIT_object obj, char *name, GP_font value, SUIT_level level)
{
    SUIT_setProperty(obj, name, "GP_font", (Pointer) &value, level);
}

   
void SUIT_deluxeSetColor (SUIT_object obj, char *name, GP_color value, SUIT_level level)
{
    SUIT_setProperty(obj, name, "GP_color", (Pointer) &value, level);
}

    
void SUIT_deluxeSetViewport (SUIT_object obj,char *name, SUIT_viewport value, SUIT_level level)
{
    SUIT_setProperty(obj, name,VIEWPORT,(Pointer) &value, level);
}

    
void SUIT_deluxeSetWindow (SUIT_object obj, char *name, SUIT_window value, SUIT_level level)
{
    SUIT_setProperty(obj, name,WINDOW,(Pointer) &value, level);
}

    
void SUIT_deluxeSetObject (SUIT_object obj, char *name, SUIT_object value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"SUIT_object",(Pointer) &value, level);
    SUIT_makePropertyTemporary (obj, name, level);
}

    
void SUIT_deluxeSetDynArray (SUIT_object obj, char *name, DynArray value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"DynArray",(Pointer) &value, level);
    SUIT_makePropertyTemporary (obj, name, level);
}

    
void SUIT_deluxeSetSpringiness (SUIT_object obj, char *name, SUIT_springiness value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"SUIT_springiness",(Pointer) &value, level);
}

    
void SUIT_deluxeSetEnum (SUIT_object obj, char *name, SUIT_enum value, SUIT_level level)
{
    SUIT_setProperty(obj, name,"SUIT_enum",(Pointer) &value, level);
}


void SUIT_deluxeSetTextList (SUIT_object obj, char *name, SUIT_textList value, SUIT_level level)
{
    SUIT_setProperty (obj, name, "SUIT_textList", (Pointer) &value, level);
}




Pointer SUIT_deluxeGetProperty (SUIT_object obj, char *propertyName, char *propertyType, SUIT_level level)
{
    property *theProp;

    ENTER (5, (buf, "SUIT_deluxeGetProperty(%s, %s, %s, %s)\n",
	       OBJECT_NAME(obj),propertyName, propertyType, SUIT_levelName(level)));
    ASSERT ((VALIDSTRING (propertyName)), (mes, "SUIT_deluxeGetProperty was called with an invalid string (propertyName).\n"));
    ASSERT ((VALIDSTRING (propertyType)), (mes, "SUIT_deluxeGetProperty was called with an invalid string (propertyType).\n"));
    if (level == GLOBAL) {
	if (obj != NULL)
	    ASSERT (FALSE, (mes, "SUIT_deluxeGetProperty was called with a non-null object pointer and level equal to global.\n"));
    } else {
	if (obj == NULL)
	    ASSERT (FALSE, (mes, "SUIT_deluxeGetProperty was called with a null object pointer and level not equal to global.\n"));
    }

    theProp = si_searchForProperty (obj, level, propertyName);
    if (theProp == NULL) {	/* it wasn't there */
        SUIT_type *theType = si_getType (propertyType);
	Pointer propertyPtr = theType->defaultValue();
	SUIT_setProperty (obj, propertyName, propertyType, propertyPtr, level);
	/* 'theList' is guaranteed to still be good here ... */
	theProp = si_searchForProperty (obj, level, propertyName);
    }

    ASSERT ((SUIT_stringsMatch(propertyType, theProp->type)),
	    (mes, "TYPE MISMATCH: SUIT_deluxeGetProperty was asked to find\n\
*     a property named \"%s\" of type \"%s\" for object \"%s\"\n\
*     and found a property with that name but of type \"%s\".\n\
*     This usually happens when a user asks for a property with\n\
*     the wrong type, for example, by using SUIT_getInteger to get\n\
*     a property of type text.  Note that, because of this type\n\
*     mismatch, the results of this operation may be (very) wrong.\n", propertyName, propertyType,
	     OBJECT_NAME(obj), theProp->type));
    LEAVE (5, (buf, "SUIT_deluxeGetProperty(%s, %s, %s, %s)\n",
	       OBJECT_NAME(obj), propertyName, propertyType, SUIT_levelName(level)));
    return (theProp->value);
}


#define IN_SECONDS(T)  ( (double)T.secs + (double)T.millisecs/1000.0 )

double timetot = 0.0;


Pointer SUIT_getProperty (SUIT_object obj, char *propertyName, char *propertyType)
{
    Pointer propertyPtr;
    property *prop;
    SUIT_level level;

    ENTER (5, (buf, "SUIT_getProperty(%s,%s,%s)\n", OBJECT_NAME (obj), propertyName, propertyType));
    ASSERT ((VALIDSTRING (propertyName)),
	    (mes, "SUIT_getProperty was called with an invalid string (propertyName).\n"));
    ASSERT ((VALIDSTRING (propertyType)),
	    (mes, "SUIT_getProperty was called with an invalid string (propertyType).\n"));
    ASSERT ((obj != NULL), (mes, "SUIT_getProperty(NULL,%s,%s) was called with a NULL object.  Use SUIT_deluxeGetProperty to get a global property.\n",propertyName,propertyType));

    if (obj == NULL)
	obj = global.root;

    prop = si_searchForPropAtLevels (obj, propertyName, &level);
    if (prop == NULL) {  /* now create it */
	boolean temp = global.currentlyPainting;
	SUIT_type *propType = si_getType (propertyType);
	propertyPtr = propType->defaultValue();
	global.currentlyPainting = FALSE;
	if (obj == global.root)
	    SUIT_setProperty (obj, propertyName, propertyType, propertyPtr, GLOBAL);
	else
	    SUIT_setProperty (obj, propertyName, propertyType, propertyPtr, CLASS);
	global.currentlyPainting = temp;
	LEAVE (5, (buf, "SUIT_getProperty(%s,%s,%s) ==> done\n", OBJECT_NAME (obj), propertyName, propertyType));
	return propertyPtr;
    } else {
	ASSERT ((SUIT_stringsMatch(propertyType, prop->type)),
		(mes, "TYPE MISMATCH: SUIT_getProperty was asked to find\n\
*     a property named \"%s\" of type \"%s\"\n\
*     for object \"%s\" and found a property with that name\n\
*     at the %s level, but of type \"%s\".  This usually happens when\n\
*     a user asks for a property with the wrong type, for example, by\n\
*     using SUIT_getInteger to get a property of type text.  Note that,\n\
*     because of this type mismatch, the results of this operation\n\
*     may be (very) wrong.\n", propertyName, propertyType, OBJECT_NAME(obj),
		 SUIT_levelName(level), prop->type));
	LEAVE (5, (buf, "SUIT_getProperty(%s,%s,%s)\n", OBJECT_NAME (obj), propertyName, propertyType));
	return prop->value;
    }
}



/*--------------------------------------------------------------------------------*/
/*              The type-specific SUIT_setProperty calls                          */
/*--------------------------------------------------------------------------------*/


int SUIT_getInteger (SUIT_object obj, char *name)
{
    return ( * ( (int *) SUIT_getProperty (obj, name, "int") ) );
}
 
   
SUIT_functionPointer SUIT_getFunctionPointer (SUIT_object obj, char *name)
{
     return ( * ( (SUIT_functionPointer *) SUIT_getProperty (obj, name, "SUIT_functionPointer") ) );
}

    
boolean SUIT_getBoolean (SUIT_object obj, char *name)
{
    return ( * ( (boolean *) SUIT_getProperty (obj, name, "boolean") ) );
}

    
double SUIT_getDouble (SUIT_object obj, char *name)
{
    return ( * ( (double *) SUIT_getProperty (obj, name,"double") ) );
}
    

char *SUIT_getText (SUIT_object obj, char *name)
{
    return ( (char *) SUIT_getProperty (obj, name, "text") );
}

    
GP_font SUIT_getFont (SUIT_object obj, char *name)
{
    return ( * (GP_font *) SUIT_getProperty (obj, name, "GP_font") );
}

    
GP_color SUIT_getColor (SUIT_object obj, char *name)
{
    return ( * ( (GP_color *) SUIT_getProperty (obj, name, "GP_color") ) );
}

    
SUIT_viewport SUIT_getViewport (SUIT_object obj,char *name)
{
    return ( * ( (SUIT_viewport *) SUIT_getProperty (obj, name,VIEWPORT) ) );
}

    
SUIT_window SUIT_getWindow (SUIT_object obj,char *name)
{
    return ( * ( (SUIT_window *) SUIT_getProperty (obj, name,WINDOW) ) );
}

    
SUIT_object SUIT_getObject (SUIT_object obj, char *name)
{
    return ( * ( (SUIT_object *) SUIT_getProperty (obj, name, "SUIT_object") ) );
}

    
DynArray SUIT_getDynArray (SUIT_object obj, char *name)
{
    return ( * ( (DynArray *) SUIT_getProperty (obj, name, "DynArray") ) );
}

    
SUIT_springiness SUIT_getSpringiness (SUIT_object obj, char *name)
{
    return ( * ( (SUIT_springiness *) SUIT_getProperty (obj, name, "SUIT_springiness") ) );
}


SUIT_enum SUIT_getEnum (SUIT_object obj, char *name)
{
    return ( * ( (SUIT_enum *) SUIT_getProperty (obj, name, "SUIT_enum") ) );
}


SUIT_textList SUIT_getTextList (SUIT_object obj, char *name)
{
    SUIT_textList list = * (SUIT_textList *) SUIT_getProperty (obj, name, "SUIT_textList");
    return SUIT_copyTextList(list);
}



/*--------------------------------------------------------------------------------*/
/*           The "deluxe" type-specific SUIT_setProperty calls                    */
/*--------------------------------------------------------------------------------*/


int SUIT_deluxeGetInteger (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (int *) SUIT_deluxeGetProperty(obj, name, "int", level) ) );
}

    
SUIT_functionPointer SUIT_deluxeGetFunctionPointer (SUIT_object obj, char *name, SUIT_level level)
{
    return (* ((SUIT_functionPointer *) SUIT_deluxeGetProperty(obj, name, "SUIT_functionPointer", level)));
}

    
boolean SUIT_deluxeGetBoolean (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (boolean *) SUIT_deluxeGetProperty(obj, name, "boolean", level) ) );
}

    
double SUIT_deluxeGetDouble (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (double *) SUIT_deluxeGetProperty(obj, name, "double", level) ) );
}

    
char *SUIT_deluxeGetText (SUIT_object obj, char *name, SUIT_level level)
{
    return ( (char *) SUIT_deluxeGetProperty(obj, name, "text", level) );
}

    
GP_font SUIT_deluxeGetFont (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * (GP_font *) SUIT_deluxeGetProperty(obj, name, "GP_font", level) );
}

    
GP_color SUIT_deluxeGetColor (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * (GP_color *) SUIT_deluxeGetProperty(obj, name, "GP_color", level) );
}

    
SUIT_viewport SUIT_deluxeGetViewport (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (SUIT_viewport *) SUIT_deluxeGetProperty(obj, name, VIEWPORT, level) ) );
}

    
SUIT_window SUIT_deluxeGetWindow (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (SUIT_window *) SUIT_deluxeGetProperty(obj, name, WINDOW, level) ) );
}

    
SUIT_object SUIT_deluxeGetObject (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (SUIT_object *) SUIT_deluxeGetProperty(obj, name, "SUIT_object", level) ) );
}

    
DynArray SUIT_deluxeGetDynArray (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (DynArray *) SUIT_deluxeGetProperty(obj, name, "DynArray", level) ) );
}

    
SUIT_springiness SUIT_deluxeGetSpringiness (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (SUIT_springiness *) SUIT_deluxeGetProperty(obj, name, "SUIT_springiness", level) ) );
}


SUIT_enum SUIT_deluxeGetEnum (SUIT_object obj, char *name, SUIT_level level)
{
    return ( * ( (SUIT_enum *) SUIT_deluxeGetProperty(obj, name, "SUIT_enum", level) ) );
}


SUIT_textList SUIT_deluxeGetTextList (SUIT_object obj, char *name, SUIT_level level)
{
    SUIT_textList list = * (SUIT_textList *) SUIT_deluxeGetProperty(obj, name, "SUIT_textList", level);
    return SUIT_copyTextList(list);
}


/*--------------------------------------------------------------------------------*/



/* CreateProp This is a function which halfway creates a property for the
 * erase property function. The reason this is necessary is that we need a
 * dummy property.  The situation occurs that a property is erased from all
 * levels.  Do we call the interest callbacks? If the default value woould be
 * different from the old value then we must call the interest callbacks.
 * This requires a property though, but none exist and I can't change the
 * state of the system by creating a real property, therefore it is necessary
 * to create a dummy one.  */


/* FreeProperty This routine is only used within property.c, and it is used
 * to free the memory used by a given property. */

PRIVATE void FreeProperty (DynArray propertyList, char *propertyName)
{
    int existingLoc;
    ASSERT ((VALIDSTRING (propertyName)), (mes, "FreeProperty was called with an invalid string (propertyName).\n"));
    existingLoc = DynFindIndex (propertyList, DummyProperty (propertyName), CompareProperty);

    if (existingLoc != DYN_NOT_FOUND) {
	property *temp = (property *) DynGet (propertyList, existingLoc);
	SUIT_type *theType = si_getType (temp->type);

	theType->destroy (temp->value);
	/* SUIT_free (temp->name); */
	/* SUIT_free (temp->type); */
	DynDelete (propertyList, existingLoc);
    }
}


/* SUIT_eraseProperty This function removes a property from a given level of
 * the tree. Properties may be erased from the global(root), class, or
 * object level.  The objects are redisplayed as needed. */

void SUIT_eraseProperty (SUIT_object o, char *propertyName, SUIT_level level)
{
    Pointer newValue, oldValue=NULL;
    SUIT_level newlevel;
    property *p=NULL, *newprop;
    SUIT_type *oldType=NULL;
    char *propName = SUIT_createRCString (propertyName);
    DynArray propList = NULL;

    ASSERT ((o != NULL), (mes, "SUIT_eraseProperty was called with a null object pointer.\n"));
    ASSERT ((VALIDSTRING (propertyName)), (mes, "SUIT_eraseProperty was called with an invalid string (propertyName).\n"));

    /* this is inefficient, search for property does a Dynfind and then erase
     * property does another one.  - Nat  2/18/91 */

    if (global.interestOn)
	p = si_searchForProperty (o, level, propName);


    if (p != NULL) {
	char *type = SUIT_copyString(p->type);
	
	if (global.interestOn) {
	    oldType = si_getType (type);
	    oldValue = oldType->copy (p->value);
	}

	propList = si_getPropertyList (o, level);
	FreeProperty (propList, propName);
	newprop = si_searchForPropAtLevels (o, propName, &newlevel);
	if (newprop == NULL) {
	    property temp;
	    newlevel = CLASS;
	    newValue = oldType->defaultValue();
	    temp = createProp (propName, type, newValue);
	    newprop = &temp;
	} else
	    newValue = newprop->value;

	if (global.interestOn && (newValue != NULL) && (newlevel < level)) {
	    if (oldType->compare (newValue, oldValue) != 0)
		si_checkCallbacks (o, level, newprop, oldValue);
	    else if (level == OBJECT)
		si_checkChildrensCallbacks (o, newprop, oldValue);
	} else
	    si_checkCallbacks (o, level, newprop, oldValue);
	MarkAppropriateRedisplay (o, level);

	if (SUIT_stringsMatch (propName, ANIMATED))
	    si_adjustAnimation (o);

	SUIT_free (type);
    }
    
    /* SUIT_free (propName); */
    if ((global.interestOn) &&p != NULL)
	oldType->destroy (oldValue);
}



/* This routine is needed when starting up to allow the property editor to
 * funtion. */

void SUIT_establishPropertyEditor (PropedFunction funct)
{
    global.propertyEditor = funct;
}




void SUIT_suspendMarkingRedisplay (SUIT_object o)   { o->mark_redisplay = FALSE; }
void SUIT_resumeMarkingRedisplay (SUIT_object o)    { o->mark_redisplay = TRUE;  }



void SUIT_unlockProperty (SUIT_object o, char *propertyName, SUIT_level level)
{
    SUIT_level unused;
    property *prop = si_searchForPropAtLevels(o, propertyName, &unused); 
    ASSERT( (prop != NULL), (mes, "SUIT_unlockProperty(%s, %s, %d): property did not exist\n", OBJECT_NAME(o), propertyName, level) ); 
    prop->locked = FALSE;
}



void SUIT_lockProperty (SUIT_object o, char *propertyName, SUIT_level level)
{
    SUIT_level unused;
    property *prop = si_searchForPropAtLevels(o, propertyName, &unused); 
    ASSERT( (prop!=NULL), (mes, "SUIT_lockProperty(%s, %s, %d): property did not exist\n", OBJECT_NAME(o), propertyName, level) ); 
    prop->locked = TRUE;
}



boolean SUIT_propertyIsLocked(SUIT_object o, char *propertyName, SUIT_level level)
{
    SUIT_level unused;
    property *prop = si_searchForPropAtLevels(o, propertyName, &unused); 
    ASSERT( (prop!=NULL),(mes, "SUIT_propertyIsLocked(%s, %s, %s): property did not exist\n", OBJECT_NAME(o), propertyName, SUIT_levelName(level)) ); 
    return prop->locked;
}



void SUIT_makePropertyPermanent (SUIT_object o, char *name, SUIT_level level)
{
    property *prop = si_searchForProperty (o, level, name);
    if (prop != NULL) {
	if (SUIT_stringsMatch(prop->type, "DynArray") ||
	    SUIT_stringsMatch(prop->type, "SUIT_object")) {
	    ASSERT (FALSE, (mes, "SUIT_makePropertyPermanent: properties of type %s must not be made permanent, since there is no way to save them to the .sui file.\n", prop->type));
	}
	else
	    prop->permanent = TRUE;
    }
    else
	ASSERT (FALSE, (mes, "SUIT_makePropertyPermanent: property \"%s\" does not exist for object \"%s\" at the %s level\n",name,OBJECT_NAME(o),SUIT_levelName(level)));
}



void SUIT_makePropertyTemporary (SUIT_object o, char *name, SUIT_level level)
{
    property *prop = si_searchForProperty (o, level, name);
    if (prop != NULL)
	prop->permanent = FALSE;
    else
	ASSERT (FALSE, (mes, "SUIT_makePropertyTemporary: property \"%s\" does not exist for object \"%s\" at the %s level\n",name,OBJECT_NAME(o),SUIT_levelName(level)));
}



void SUIT_setEnumString (SUIT_object o, char *propName, char *enumString)
{
    SUIT_enum current;
    current = SUIT_getEnum (o, propName);
    SUIT_setEnumSelection (&current, enumString);
    SUIT_setEnum (o, propName, current);
}



void SUIT_deluxeSetEnumString (SUIT_object o, char *propName, char *enumString, SUIT_level level)
{
    SUIT_enum current;
    current = SUIT_getEnum (o, propName);
    SUIT_setEnumSelection (&current, enumString);
    SUIT_deluxeSetEnum (o, propName, current, level);
}




char *SUIT_getEnumString (SUIT_object o, char *propName)
{
    return SUIT_getEnumSelection (SUIT_getEnum (o, propName));
}



char *SUIT_deluxeGetEnumString (SUIT_object o, char *propName, SUIT_level level)
{
    return SUIT_getEnumSelection (SUIT_deluxeGetEnum (o, propName, level));
}


void si_tickleAllViewports (SUIT_object o)
{
    SUIT_level level;
    int i;
    property *prop = si_searchForPropAtLevels (o, VIEWPORT, &level);

    if (prop != NULL)
	si_checkCallbacks (o, level, prop, NULL);
    for (i=0; i < SUIT_numberOfChildren(o); i++)
	si_tickleAllViewports(SUIT_getChild(o,i));
}
