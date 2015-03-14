/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#define SUITMAIN
#include "privsuit.h"


void si_createRoot (void)
{
    nameTableEntry newNameEntry;

    global.root = (SUIT_object) SUIT_malloc (sizeof (SUIT_objectRecord));

    global.root->name = SUIT_createRCString ("ROOT");
    global.root->classInfo = FindClassAndAddItIfNecessary ("ROOT");
    global.root->props = DynCreate (sizeof (property), 20);
    global.root->displays = DynCreate (sizeof (SUIT_display), 1);
    global.root->children = DynCreate (sizeof (SUIT_object), 1);
    global.root->parent = NULL;
    global.root->mark_redisplay = TRUE;
    global.root->redisplay_required = TRUE;
    global.root->has_been_painted = FALSE;
    global.root->optimized_paint = FALSE;
    global.root->objectInterest = NULL;
    global.root->offset = SRGP_defPoint(0,0);
    OBJECT_OPEN(global.root) = TRUE; 
    OBJECT_SELECTED(global.root) = FALSE;
    OBJECT_PERMANENT(global.root) = TRUE;

    newNameEntry.name = global.root->name;	/* no need to copy string */
    newNameEntry.ptr = global.root;
    DynAdd (global.nameTable, (void *) &newNameEntry);

    SUIT_setInteger (global.root, NUMBER_OF_CHILDREN, 0);
      SUIT_makePropertyTemporary (global.root, NUMBER_OF_CHILDREN, OBJECT);
      SUIT_lockProperty (global.root, NUMBER_OF_CHILDREN, OBJECT);
    SUIT_setBoolean (global.root, CAN_BE_OPENED, FALSE);
      SUIT_makePropertyTemporary (global.root, CAN_BE_OPENED, OBJECT);
      SUIT_lockProperty (global.root, CAN_BE_OPENED, OBJECT);

    /* The following enforces that *all* object have at least one display. */
    SUIT_addDisplayToObject (global.root, "standard", NULL, NULL);
    SUIT_lockProperty(global.root, ACTIVE_DISPLAY, OBJECT);
    SUIT_makePropertyTemporary(global.root, ACTIVE_DISPLAY, OBJECT);
}



SUIT_object SUIT_createObject (char *name, char *className)
{
    SUIT_object newObject;
    nameTableEntry newNameEntry;
    int width, height;
    SUIT_viewport vp;

    ENTER (1, (buf, "SUIT_createObject(%s,%s)\n", name, className));
    if (!si_SUITInitialized()) {
	ASSERT ((si_SUITInitialized()), (mes, "SUIT_createObject was called before SUIT was initialized with a\n*     call to SUIT_init or SUIT_deluxeInit.\n"));
        fprintf(stderr, "This error is fatal: your program has been terminated.\n");
	exit(0);
    }
	
    ASSERT ((VALIDSTRING (name)), (mes, "SUIT_createObject was called with an invalid string (name).\n"));
    ASSERT ((VALIDSTRING (className)), (mes, "SUIT_createObject was called with an invalid string (className).\n"));
    if (SUIT_name(name) != NULL)
	si_actuallyDestroyObjects (); /* may need to flush the destroyed objects */

    ASSERT ((SUIT_name (name) == NULL), (mes, "SUIT_createObject was called with the name of an existing object ('%s').\n", name));

    newObject = (SUIT_object) SUIT_malloc (sizeof (SUIT_objectRecord));

    newObject->name = SUIT_createRCString (name);
    newObject->classInfo = FindClassAndAddItIfNecessary (className);

    newObject->open = FALSE;
    newObject->selected = FALSE;
    newObject->permanent = TRUE;
    newObject->redisplay_required = FALSE;
    newObject->optimized_paint = FALSE;
    newObject->has_been_painted = FALSE;
    newObject->mark_redisplay = TRUE;
    newObject->objectInterest = NULL;
    newObject->props = DynCreate (sizeof (property), 5);
    newObject->displays = DynCreate (sizeof (SUIT_display), 1);
    newObject->children = NULL;
    newObject->parent = NULL;
    newObject->offset = SRGP_defPoint(0,0);

    SUIT_addChildToObject (global.root, newObject);

    /* storage efficiency: we've made a safe copy of the name,so we can reuse */
    newNameEntry.name = newObject->name;	/* no need to copy string */
    newNameEntry.ptr = newObject;
    DynAdd (global.nameTable, (void *) &newNameEntry);

    /* now add any properties the object needs */
    /* SUIT_deluxeSetBoolean (newObject, CAN_BE_OPENED, FALSE, OBJECT);
       SUIT_makePropertyTemporary (newObject, CAN_BE_OPENED, OBJECT); */
    
    width = SUIT_getInteger (newObject, DEFAULT_OBJECT_WIDTH);
    height = SUIT_getInteger (newObject, DEFAULT_OBJECT_HEIGHT);
    vp.bottom_left.x = rand () % (SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL) - width - 1);
    vp.bottom_left.y = rand () % (SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL) - height - 1);
    vp.top_right.x = vp.bottom_left.x + width;
    vp.top_right.y = vp.bottom_left.y + height;

    SUIT_setProperty (newObject, VIEWPORT, VIEWPORT, (Pointer) & vp, OBJECT);
    SUIT_deluxeSetInteger (newObject, NUMBER_OF_CHILDREN, 0, OBJECT);
    SUIT_makePropertyTemporary (newObject, NUMBER_OF_CHILDREN, OBJECT);
    SUIT_lockProperty (newObject, NUMBER_OF_CHILDREN, OBJECT);
    if (global.propertyEditorIsActive) {
	SUIT_deluxeSetBoolean(newObject, VISIBLE_WITHIN_PROPERTY_EDITOR, TRUE, OBJECT);
	SUIT_makePropertyTemporary (newObject, VISIBLE_WITHIN_PROPERTY_EDITOR, OBJECT);
    }

    SUIT_bringToFront(newObject);
    LEAVE (1, (buf, "SUIT_createObject(%s,%s)\n", name, className));

    return newObject;
}



PRIVATE char *ReplaceString (char *string, char *old, char *new)
{
    /* replace every occurance of old inside string with new */
    char *copy = SUIT_copyString(string), *hold = copy;
    char *loc = strstr(copy, old);
    char *retval = SUIT_malloc (strlen(string)*strlen(new)/strlen(old)+1); /* guess how big it is */

    strcpy (retval, "");
    while (loc != NULL) {
	loc[0] = '\0';
	strcat (retval, copy);
	strcat (retval, new);
	copy = loc + strlen(old);
	loc = strstr(copy, old);
    }
    strcat (retval, copy);
    SUIT_free (hold);
    return retval;
}



PRIVATE SUIT_object CopyObjectRec (SUIT_object obj, char *newName, boolean moveViewport)
{
    SUIT_viewport vp;
    int i;

    SUIT_object newobj = SUIT_createObject (newName, OBJECT_CLASS(obj));

    newobj->open = obj->open;
    newobj->has_been_painted = FALSE;
    newobj->redisplay_required = TRUE;
    newobj->optimized_paint = FALSE;
    newobj->mark_redisplay = obj->mark_redisplay;
    newobj->permanent = obj->permanent;
    newobj->offset = obj->offset;

    if (obj->selected) {
	SUIT_deselectObject (obj);
	SUIT_selectObject (newobj);
    } else
	newobj->selected = FALSE;

    /* copy all the display styles */
    for (i=0; i < DynSize(obj->displays); i++) {
	SUIT_display *disp = (SUIT_display *) DynGet(obj->displays, i);
	int j;
	SUIT_addDisplayToObject (newobj, disp->name, disp->hit, disp->paint);
	if (disp->employees != NULL)
	    for (j=0; j < DynSize(disp->employees); j++) {
		SUIT_object emp = * (SUIT_object *) DynGet (disp->employees, j);
		char *newname = ReplaceString (emp->name, obj->name, newobj->name);
		if (SUIT_stringsMatch(newname,emp->name)) {
		    SUIT_free (newname);
		    newname = si_generateCopyName(emp->name);
		}
		SUIT_addEmployeeToDisplay (newobj, disp->name, CopyObjectRec(emp, newname, FALSE));
		SUIT_free (newname);
	    }
    }

    /* copy all the (object level) properties */
    for (i=0; i < DynSize(obj->props); i++) {
	property *p = (property *) DynGet (obj->props, i);
	SUIT_setProperty (newobj, p->name, p->type, p->value, OBJECT);
    }

    /* copy all the children */
    if (obj->children == NULL)
	obj->children = NULL;
    else
	for (i=0; i < DynSize(obj->children); i++) {
	    SUIT_object child = SUIT_getChild(obj, i);
	    char *newname = ReplaceString (child->name, obj->name, newobj->name);
	    if (SUIT_stringsMatch(newname,child->name)) {
		SUIT_free (newname);
		newname = si_generateCopyName(child->name);
	    }
	    SUIT_addChildToObject (newobj, CopyObjectRec(child, newname, FALSE));
	    SUIT_free (newname);
	}

    /* copy all the interests */
    if (obj->objectInterest == NULL)
	newobj->objectInterest = NULL;
    else
	for (i=0; i < DynSize(obj->objectInterest); i++) {
	    SUIT_objectInterestCallback f = * (SUIT_objectInterestCallback *)
		                                            DynGet(obj->objectInterest, i);
	    SUIT_registerInterest (newobj, f);
	}
    
    /* "copy" the parent field */
    SUIT_addChildToObject (obj->parent, newobj);

    /* do the Mac trick of offseting by a few pixels */
    if (moveViewport) {
	vp = SUIT_getViewport (newobj, VIEWPORT);
	vp.bottom_left.x += 5;
	vp.bottom_left.y -= 5;
	vp.top_right.x += 5;
	vp.top_right.y -= 5;
	SUIT_setViewport (newobj, VIEWPORT, vp);
    }
    
    return newobj;
}


SUIT_object SUIT_copyObject (SUIT_object obj, char *newName)
{
    return CopyObjectRec (obj, newName, TRUE);
}


PRIVATE  SUIT_object ClassDummyObject = NULL;

SUIT_object SUIT_dummyObjectInClass (char *className)
{
    if (ClassDummyObject == NULL)
	ClassDummyObject = (SUIT_object) SUIT_malloc (sizeof (SUIT_objectRecord));
    ClassDummyObject->name = "Class Dummy Object";
    ClassDummyObject->classInfo = FindClassAndAddItIfNecessary (className);
    ClassDummyObject->props = DynCreate (sizeof (property), 1);
    ClassDummyObject->parent = global.root;
    ClassDummyObject->permanent = FALSE; /* pausch addition */
    ClassDummyObject->has_been_painted = FALSE;
    ClassDummyObject->children = NULL;
    ClassDummyObject->displays = NULL;
    return ClassDummyObject;
}



PRIVATE void SetActiveDisplayProperty (SUIT_object o)
{
    DynArray names = DynCreate (sizeof(char*), 1);
    int i;
    SUIT_enum dispnames;

    for (i=DynLow(o->displays); i <= DynHigh(o->displays); i++) {
	SUIT_display *disp = (SUIT_display *) DynGet(o->displays, i);
	DynAdd (names, (void *) &disp->name);
    }
    dispnames.choices = names;
    dispnames.currentChoice = DynHigh(names);
    SUIT_setEnum (o, ACTIVE_DISPLAY, dispnames);
    DynDestroy(names);
}



void SUIT_addDisplayToObject (SUIT_object o, char *displayName, SUIT_hitProcedure hitproc,
			      SUIT_paintProcedure paintproc)
{
    SUIT_display newDisplay;

    ASSERT ((o != NULL), (mes, "SUIT_addDisplayToObject was called with a null object pointer.\n"));
    ASSERT ((VALIDSTRING (displayName)), (mes, "SUIT_addDisplayToObject was called with an invalid string (displayName).\n"));

    ENTER (1, (buf, "SUIT_addDisplayToObject( %s, %s, hitproc,paintproc)\n", OBJECT_NAME (o), displayName));

    newDisplay.name = SUIT_createRCString (displayName);
    newDisplay.hit = hitproc;
    newDisplay.paint = paintproc;
    newDisplay.employees = NULL;

    DynAdd (o->displays, (void *) &newDisplay);

    SetActiveDisplayProperty (o);

    LEAVE (1, (buf, "SUIT_addDisplayToObject( %s, %s, hitproc,paintproc)\n", OBJECT_NAME (o), displayName));
}



PRIVATE void RemoveChild (SUIT_object child)
{
    SUIT_object mom = SUIT_getParent(child);
    DynArray siblings = SUIT_getChildren(mom);

    DeleteObjectFromList (siblings, child);
    if (mom == global.root)
	SUIT_suspendMarkingRedisplay(global.root); /* no need to repaint everybody */
    SUIT_deluxeSetInteger (mom, NUMBER_OF_CHILDREN, DynSize (mom->children), OBJECT);
    if (mom == global.root)
	SUIT_resumeMarkingRedisplay(global.root);
}



PRIVATE void ChangeViewportAndOffset (SUIT_object parent, SUIT_object child)
{
    extern boolean OnlyAdjustingViewportAndOffset;
    rectangle parentvp, newvp;

    parentvp = OBJECT_VIEWPORT(parent);
    newvp = SUIT_mapViewportToScreen(child, OBJECT_VIEWPORT(child));

    child->offset = SRGP_defPoint(parentvp.bottom_left.x + parent->offset.x, 
				  parentvp.bottom_left.y + parent->offset.y);
    newvp = SUIT_mapScreenToViewport (child, newvp);
    OnlyAdjustingViewportAndOffset = TRUE; /* this is a kludge... so sue me -- Rob */
    SUIT_setViewport (child, VIEWPORT, newvp);
    OnlyAdjustingViewportAndOffset = FALSE;
}



PRIVATE void ForceInsideParent (SUIT_object parent, SUIT_object child)
{
    extern boolean OnlyAdjustingViewportAndOffset;
    SUIT_viewport kidvp;
    int kwidth, kheight, pwidth, pheight;

    kidvp = OBJECT_VIEWPORT(child);
    kwidth = kidvp.top_right.x - kidvp.bottom_left.x;
    kheight = kidvp.top_right.y - kidvp.bottom_left.y;

    SUIT_getObjectSize (parent, &pwidth, &pheight);
    if (kidvp.bottom_left.x < 0) {
	kidvp.bottom_left.x = 0;
	kidvp.top_right.x = kwidth;
    } else if (kidvp.top_right.x > pwidth) {
	kidvp.bottom_left.x = pwidth - kwidth;
	kidvp.top_right.x = pwidth;
    }
    if (kidvp.bottom_left.y < 0) {
	kidvp.bottom_left.y = 0;
	kidvp.top_right.y = kheight;
    } else if (kidvp.top_right.y > pheight) {
	kidvp.bottom_left.y = pheight - kheight;
	kidvp.top_right.y = pheight;
    }

    OnlyAdjustingViewportAndOffset = TRUE; /* this is a kludge... so sue me -- Rob */
    SUIT_setViewport (child, VIEWPORT, kidvp);
    OnlyAdjustingViewportAndOffset = FALSE;
}



void si_addChildToObject (SUIT_object o, SUIT_object child, boolean changeVP)
{
    ASSERT ((o != NULL), (mes, "SUIT_addChildToObject was called with a null parent object pointer.\n"));
    ASSERT ((child != NULL), (mes, "SUIT_addChildToObject was called with a null child object pointer.\n"));

    if (o == child->parent) {	  /* the work is already done */
        SUIT_sendToBack (child);  /* just perform the side-effect and return */
	return;
    }

    /* first we must remove it from its parent's list of children (if it has a parent) */
    if (child->parent)
	RemoveChild (child);

    /* now add it to its new parent's list of children */
    if (o->children == NULL)
	o->children = DynCreate (sizeof (SUIT_object), 1);
    DynAdd (o->children, (void *) &child);
    child->parent = o;

    if (changeVP)
	ChangeViewportAndOffset (o, child);
    SRGP_refresh();
    ForceInsideParent (o, child);  SRGP_refresh();

    /* this is done last since a someone might have an interest in this property
       and we want all the work to be done by the time the interest routine is called */
    SUIT_deluxeSetInteger (o, NUMBER_OF_CHILDREN, DynSize (o->children), OBJECT);
}



void SUIT_addChildToObject (SUIT_object o, SUIT_object child)
{
    si_addChildToObject (o, child, TRUE);
}




void si_addChildHere (SUIT_object openobj, SUIT_object o, int which)
{
    DynArray newkids = DynCreate (sizeof (SUIT_object), 1);
    int j;
    for (j = DynLow (openobj->children); j <= which; j++) {
	SUIT_object kid = SUIT_getChild (openobj, j);
	DynAdd (newkids, (void *) &kid);
    }
    RemoveChild (o);
    DynAdd (newkids, (void *) &o);
    for (j = which + 1; j <= DynHigh (openobj->children); j++) {
	SUIT_object kid = SUIT_getChild (openobj, j);
	DynAdd (newkids, (void *) &kid);
    }
    DynDestroy (openobj->children);
    openobj->children = newkids;
    o->parent = openobj;
    ChangeViewportAndOffset (openobj, o);
    SUIT_deluxeSetInteger (openobj, NUMBER_OF_CHILDREN, DynSize (openobj->children), OBJECT);
}



void SUIT_removeChild (SUIT_object child)
{
    /* SUIT_object o = SUIT_getParent(child), root = global.root; */

    ASSERT ((child != NULL), (mes, "SUIT_removeChild was called with a null child object pointer.\n"));
    SUIT_addChildToObject (global.root, child);	/* this is really what you're doing */
    /*
    RemoveChild (child);

    o->parent = root;
    DynAdd (root->children, (void *)&child);
    */
}



DynArray SUIT_getChildren (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_getChildren was called with a null object pointer.\n"));
    return o->children;
}



int SUIT_numberOfChildren (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_numberOfChildren was called with a null object pointer.\n"));
    if (o->children == NULL)
	return 0;
    return DynSize (o->children);
}


SUIT_object SUIT_getChild (SUIT_object o, int whichChild)
{
    SUIT_object retval;
    ASSERT ((o != NULL), (mes, "SUIT_getChild was called with a null object pointer.\n"));
    ASSERT ((whichChild >= 0), (mes, "SUIT_getChild was called with negative child number (%d).\n", whichChild));
    ASSERT ((whichChild <= DynHigh (o->children)), (mes, "SUIT_getChild was called with child number that is too large (%d).\n", whichChild));
    ENTER (3, (buf, "SUIT_getChild(%s,%d)\n", OBJECT_NAME(o),whichChild));
    retval = * (SUIT_object *) DynGet (o->children, whichChild);
    LEAVE (3, (buf, "SUIT_getChild(%s,%d) ==> %s\n", OBJECT_NAME(o),whichChild,OBJECT_NAME(retval)));
    return retval;
}


/*
PRIVATE void RemoveEmp (char *disp, SUIT_object emp)
{
    SUIT_object o = SUIT_getParent(emp);

    DeleteObjectFromList (SUIT_getEmployees (o, disp), emp);
}
*/



void SUIT_addEmployeeToDisplay (SUIT_object o, char *displayName, SUIT_object employee)
{
    SUIT_display *display;

    ASSERT ((o != NULL), (mes, "SUIT_addEmployeeToDispla was called with a null parent object pointer.\n"));
    ASSERT ((employee != NULL), (mes, "SUIT_addEmployeeToDispla was called with a null child object pointer.\n"));

    /* we must remove it from it former parent's list of children */
    RemoveChild (employee);

    display = si_getDisplay (o, displayName);
    if (display->employees == NULL)
	display->employees = DynCreate (sizeof (SUIT_object), 1);
    DynAdd (display->employees, (void *) &employee);
    employee->parent = o;
    ChangeViewportAndOffset (o, employee);
    /* SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, OBJECT); */
}



void SUIT_removeEmployee (char *disp, SUIT_object emp)
{
    /* SUIT_object o = SUIT_getParent(emp), root = global.root; */

    ASSERT ((emp != NULL), (mes, "SUIT_removeEmployee was called with a null child object pointer.\n"));
    SUIT_addChildToObject (global.root, emp); /* this is really what you're doing */

    /*
    RemoveEmp (disp, emp);

    o->parent = root;
    DynAdd (root->children, (void *)&emp);
    */
}



DynArray SUIT_getEmployees (SUIT_object o, char *displayName)
{
    SUIT_display *display;

    ASSERT ((o != NULL), (mes, "SUIT_getEmployees was called with a null object pointer.\n"));
    if (o == global.root)
	return (DynArray) NULL;
    display = si_getDisplay (o, displayName);
    ASSERT ((display != NULL), (mes, "SUIT_getEmployees was called with a non-existent display.\n"));
    return display->employees;
}


int SUIT_numberOfEmployees (SUIT_object o, char *displayName)
{
    SUIT_display *display;

    ASSERT ((o != NULL), (mes, "SUIT_numberOfEmployees was called with a null object pointer.\n"));
    if (o == global.root)
	return 0;
    display = si_getDisplay (o, displayName);
    ASSERT ((display != NULL), (mes, "SUIT_numberOfEmployees was called with a non-existent display (%s).\n", displayName));
    if (display->employees == NULL)
	return 0;
    return DynSize (display->employees);
}


SUIT_object SUIT_getEmployee (SUIT_object o, char *displayName, int whichEmp)
{
    SUIT_display *display;

    ASSERT ((o != NULL), (mes, "SUIT_getEmployees was called with a null object pointer.\n"));
    if (o == global.root)
	return (SUIT_object) NULL;
    display = si_getDisplay (o, displayName);

    ASSERT ((display != NULL), (mes, "SUIT_getEmployees was called with a non-existent display (%s).\n", displayName));
    ASSERT ((whichEmp >= 0), (mes, "SUIT_getEmployee was called with bad employee number (%d).\n", whichEmp));
    ASSERT ((whichEmp <= DynHigh (display->employees)), (mes, "SUIT_getEmployee was called with bad employee number (%d).\n", whichEmp));

    return *(SUIT_object *) DynGet (display->employees, whichEmp);
}


SUIT_object SUIT_getParent (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_getParent was called with a null object pointer.\n"));
    return o->parent;
}



SUIT_object SUIT_name (char *name)
{
    int slot;

    ASSERT((name != NULL), (mes, "SUIT_name was called with a NULL pointer\n"));
    ENTER (3, (buf, "SUIT_name(%s)\n", name));
    slot = DynFindIndex (global.nameTable, DummyNameTableEntry (name), CompareNameTableEntry);
    if (slot != DYN_NOT_FOUND) {
	nameTableEntry *temp = DynGet (global.nameTable, slot);
	ASSERT (((temp->ptr) != NULL), (mes, "SUIT_name received a pointer to a name table entry which has no name.\n"));
	LEAVE (1, (buf, "SUIT_name(%s) ==> object\n", name));
	return (temp->ptr);
    }
    LEAVE (3, (buf, "SUIT_name(%s) ==> NULL\n", name));
    return ((SUIT_object) NULL);
}



void SUIT_writeSUIFile (char *filename)
{
    ASSERT ((VALIDSTRING (filename)), (mes, "SUIT_name was called with an invalid string (fileName).\n"));
    si_writeHints (filename);
}



/* This routine gracefully ends a SUIT application. */
void SUIT_done (SUIT_saveStatus saveStat, SUIT_exitStatus exitStat)
{
    ENTER (1, (buf, "SUIT_done(%d,%d)\n", saveStat,exitStat));
    global.interestOn = FALSE;
    if (saveStat == SAVE_SUI_FILE && global.interactiveToolsAvailable)
	/* pausch: don't write a .sui if the interactive tools are off */
	SUIT_writeSUIFile (global.hintsfile);
    fclose (trace.tracefile);
    SRGP_end ();
    LEAVE (1, (buf, "SUIT_done(%d,%d)\n", saveStat,exitStat));
    if (exitStat == EXIT_APPLICATION)
	exit(0);
}



PRIVATE void MarkAllObjectsRecursively (SUIT_object o, char *className)
{
    int i;  

    if ((className == NULL) || SUIT_caseInsensitiveMatch (className, OBJECT_CLASS(o))) {
	OBJECT_OPTIMIZED(o) = FALSE;
	SUIT_redisplayRequired (o);
    }
    for (i=0; i < SUIT_numberOfChildren(o); i++)
	MarkAllObjectsRecursively (SUIT_getChild (o, i), className);
}


void SUIT_allObjectsRequireRedisplay (char *className)
{
    ENTER (2, (buf, "suit_all_require_redisplay\n"));
    if (global.root->mark_redisplay)
	MarkAllObjectsRecursively (global.root, className);
    LEAVE (2, (buf, "SUIT_allObjectsRequireRedisplay()\n"));
}



PRIVATE void RedrawSection (DynArray kids, rectangle rect)
{
    SUIT_object o;
    int i;

    if (kids == NULL)
	return;
    for (i = DynLow (kids); i <= DynHigh (kids); i++) {
	o = *((SUIT_object *) DynGet (kids, i));
	if ((!o->redisplay_required) && si_isVisible(o)) {
	    /* SUIT_viewport vp = ViewportPlusBorder (o, SUIT_getInteger(o, BORDER_WIDTH)); */
	    SUIT_viewport vp;
	    vp = CalculateVisiblePortion (o);
	    if (SUIT_viewportsOverlap (rect, vp)) {
		int can;
		OBJECT_OPTIMIZED(o) = FALSE;
		o->redisplay_required = TRUE;
#ifdef X_WINDOWS
		if (SUIT_getBoolean (o, CACHE_USING_CANVAS) && 
		    (can = SUIT_getInteger (o, "canvas number")) > 0) {
		    /* invalidate the cache */
		    SRGP_deleteCanvas (can);
		    SUIT_setInteger (o, "canvas number", -1);
		}
#endif
		SUIT_redisplayRequiredInRegion (vp);
	    }
	}
    }
}



void SUIT_redisplayRequiredInRegion (rectangle rect)
{
    ENTER (3, (buf, "SUIT_redisplayRequiredInRegion(%d,%d,%d,%d)\n", rect.bottom_left.x, rect.bottom_left.y, rect.top_right.x, rect.top_right.y));
    RedrawSection (SUIT_getChildren(global.root), rect);
    LEAVE (3, (buf, "SUIT_redisplayRequiredInRegion(%d,%d,%d,%d)\n", rect.bottom_left.x, rect.bottom_left.y, rect.top_right.x, rect.top_right.y));
}



void SUIT_redrawLocalSection (SUIT_object o, rectangle rect)
{
    ENTER (3, (buf, "SUIT_redrawLocalSection(%d,%d,%d,%d)\n", rect.bottom_left.x, rect.bottom_left.y, rect.top_right.x, rect.top_right.y));
    RedrawSection (SUIT_getChildren(SUIT_getParent(o)), rect);
    LEAVE (3, (buf, "SUIT_redrawLocalSection(%d,%d,%d,%d)\n", rect.bottom_left.x, rect.bottom_left.y, rect.top_right.x, rect.top_right.y));
}



void SUIT_redrawObjectsAbove (SUIT_object me)
{
    SUIT_object parent;
    /* SUIT_viewport vp = SUIT_mapViewportToScreen(me, OBJECT_VIEWPORT(me)); */
    SUIT_viewport vp;
    int i;

    ENTER (3, (buf, "SUIT_redrawobjectsAbove(%s)\n", OBJECT_NAME (me)));
    vp = CalculateVisiblePortion(me);
    for (parent = SUIT_getParent(me); parent != NULL; me = parent, parent = SUIT_getParent(parent))
	for (i=0; i < SUIT_numberOfChildren(parent); i++) {
	    SUIT_object sibling = SUIT_getChild(parent, i);
	    if (sibling == me)
		break;
	    if (si_isVisible(sibling) && SUIT_viewportsOverlap (vp, CalculateVisiblePortion(sibling))) {
   /* SUIT_viewportsOverlap (vp, SUIT_mapViewportToScreen(sibling, OBJECT_VIEWPORT(sibling)))) { */
		int can;
		SUIT_redisplayRequired(sibling);
		OBJECT_OPTIMIZED(sibling) = FALSE;
#ifdef X_WINDOWS
		if (SUIT_getBoolean (sibling, CACHE_USING_CANVAS) && 
		    (can = SUIT_getInteger (sibling, "canvas number")) > 0) {
		    /* invalidate the cache */
		    SRGP_deleteCanvas (can);
		    SUIT_setInteger (sibling, "canvas number", -1);
		}
#endif
	    }
	}
    LEAVE (3, (buf, "SUIT_redrawobjectsAbove(%s)\n", OBJECT_NAME (me)));
}



void SUIT_bringToFront (SUIT_object o)
{
    DynArray siblings, new;
    SUIT_object peer;
    int i;

    ASSERT ((o != NULL), (mes, "SUIT_bringToFront was called with a null object pointer.\n"));
    ASSERT ((o != global.root), (mes, "SUIT_bringToFront was called with Root.\n"));
    siblings = (o->parent)->children;
    new = DynCreate (sizeof (SUIT_object), DynSize(siblings)+1);
    DynAdd (new, (void *) &o);
    for (i = 0; i < SUIT_numberOfChildren (o->parent); i++) {
	peer = SUIT_getChild (o->parent, i);
	if (peer != o)
	    DynAdd (new, (void *) &peer);
    }
    (o->parent)->children = new;
    DynDestroy (siblings);
    OBJECT_OPTIMIZED(o) = FALSE;
    SUIT_redisplayRequired (o);
}



void SUIT_sendToBack (SUIT_object o)
{
    DynArray siblings;

    ASSERT ((o != NULL), (mes, "SUIT_sendToBack was called with a null object pointer.\n"));
    siblings = (o->parent)->children;
    DeleteObjectFromList (siblings, o);
    DynAdd (siblings, (void *) &o);
    OBJECT_OPTIMIZED(o) = FALSE;
    SUIT_redisplayRequired (o);
    SUIT_redrawLocalSection (o, SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
}



PRIVATE void RecursivelyDestroy (SUIT_object o)
{
    SUIT_object parent;
    int i;

    if (SUIT_propertyExists (o, CHAINED_TO_OBJECT, "text", OBJECT)) {
	char *chainedObjectName = SUIT_getText (o, CHAINED_TO_OBJECT);
	SUIT_object chainedObject = SUIT_name(chainedObjectName);
	if (chainedObject && SUIT_propertyExists (o, CHAINED_TO_PROPERTY, "text", OBJECT))
	    SUIT_unlockProperty (chainedObject, SUIT_getText(o, CHAINED_TO_PROPERTY), OBJECT);
    }
    
    /* destroy all the object's children */
    if (o->children != NULL) {
	while (DynSize (o->children) > 0) {
	    SUIT_object kid = *(SUIT_object *) DynGet (o->children, DynHigh (o->children));
	    RecursivelyDestroy (kid);
	}
	DynDestroy (o->children);
    }
    
    /* destroy all the object's displays and their employees */
    while (DynSize (o->displays) > 0) {
	SUIT_display *disp = (SUIT_display *) DynGet (o->displays, DynHigh (o->displays));
	/* destoy all this display's employees */
	if (disp->employees != NULL) {
	    while (DynSize (disp->employees) > 0) {
		SUIT_object emp = *(SUIT_object *) DynGet (disp->employees, DynHigh (disp->employees));
		RecursivelyDestroy (emp);
		DynDelete (disp->employees, DynHigh (disp->employees));
	    }
	    DynDestroy (disp->employees);
	}
	/* SUIT_free (disp->name); */
	DynDelete (o->displays, DynHigh (o->displays));
    }
    DynDestroy (o->displays);
    
    /* now delete o from o's parent's list of kids */
    parent = SUIT_getParent (o);
    if (parent != NULL && parent->children != NULL)
	DeleteObjectFromList (parent->children, o);

    /* destroy the object's entry in the name table */
    i = DynFindIndex (global.nameTable, DummyNameTableEntry (o->name), CompareNameTableEntry);
    if (i != DYN_NOT_FOUND) {
	nameTableEntry *tempName = (nameTableEntry *) DynGet (global.nameTable, i);
	/* SUIT_free (tempName->name); */
	tempName->ptr = (SUIT_object) NULL;
	DynDelete (global.nameTable, i);
    }
    
    /* destroy all the object's properties */
    while (DynSize (o->props) > 0) {
	property *prop = (property *) DynGet (o->props, DynHigh (o->props));
	/* SUIT_free (prop->name); */
	/* SUIT_free (prop->type); */
	SUIT_free (prop->value);
	DynDelete (o->props, DynHigh (o->props));
    }
    DynDestroy (o->props);
    
    /* destroy all of the objectInterest */
    if (o->objectInterest != NULL)
	DynDestroy (o->objectInterest);
    
    /* finally destroy the object itself */
    SUIT_free (o);
}



PRIVATE DynArray DestroyedObjects = NULL;


void SUIT_destroyObject (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_destroyObject was called with a null object pointer.\n"));

    if (SUIT_stringsMatch (OBJECT_NAME (o), "SUIT system menu")) {
	SUIT_inform ("Please do @u(not) destroy the SUIT system menu.");
	return;
    }
    if (SUIT_stringsMatch (OBJECT_NAME (o), SUIT_PROPERTY_EDITOR)) {
	SUIT_inform ("Please do @u(not) destroy the SUIT property editor.");
	return;
    }

    if (o == ObjectBeingEdited()) {
	GP_setCursor (STANDARD_CURSOR);
	SUIT_inform ("Since this object will be destroyed, the property editor will be exited.");
	SUIT_closePropertyEditor ();
    }

    SUIT_eraseObject (o);

    /* the following line helps us keep track whether any object is still animated */
    SUIT_setBoolean (o, ANIMATED, FALSE);
    SUIT_deselectObject (o);
    si_closeObject (o);

    /* now delete o from o's parent's list of kids */
    RemoveChild (o);
    /*
    parent = SUIT_getParent (o);
    if (parent->children != NULL)
	DeleteObjectFromList (parent->children, o);
    */
    
    if (DestroyedObjects == NULL)
	DestroyedObjects = DynCreate(sizeof(SUIT_object), 1);
    DynAdd (DestroyedObjects, (void *)&o);
}



void si_actuallyDestroyObjects (void)
{
    int i;
    if (DestroyedObjects == NULL)
	return;
    for (i=0; i < DynSize(DestroyedObjects); i++) {
	SUIT_object zap = * (SUIT_object *) DynGet(DestroyedObjects,i);
	RecursivelyDestroy (zap);
    }
    DynDestroy (DestroyedObjects);
    DestroyedObjects = NULL;
}



void SUIT_cycleObject (SUIT_object o)
{
    char *name;
    SUIT_object employee;
    int i;
    SUIT_viewport oldViewport;
    SUIT_enum displays;

    ENTER (2, (buf, "SUIT_cycleObject(%s)\n", OBJECT_NAME (o)));
    ASSERT ((o != NULL), (mes, "SUIT_cycleObject was called with a null object pointer.\n"));
    ASSERT ((DynSize (o->displays) > 0), (mes, "SUIT_cycleObject was called with an object which has no implementations.\n"));
    if (DynSize(o->displays) == 1) {
	char buf[80];
	sprintf (buf, "Sorry, but object \"%s\" (class \"%s\") has only one display style.",OBJECT_NAME(o),OBJECT_CLASS(o));
	SUIT_inform (buf);
	return;
    }

    /* first, save the old vp so we can do screen refresh */
    oldViewport = SUIT_getViewport (o, VIEWPORT);
    SUIT_eraseObject (o);

    displays = SUIT_getEnum (o, ACTIVE_DISPLAY);
    name = * (char **) DynGet(displays.choices, displays.currentChoice);
    for (i = 0; i < SUIT_numberOfEmployees (o, name); i++) {
	employee = SUIT_getEmployee (o, name, i);
	SUIT_setBoolean (employee, VISIBLE, FALSE);
	SUIT_paintObject (employee);
    }

    displays.currentChoice = (displays.currentChoice + 1) % DynSize(displays.choices);
    SUIT_setEnum (o, ACTIVE_DISPLAY, displays);

    name = * (char **) DynGet(displays.choices, displays.currentChoice);
    for (i = 0; i < SUIT_numberOfEmployees (o, name); i++) {
	employee = SUIT_getEmployee (o, name, i);
	/* just let'm inherit the property */
	SUIT_eraseProperty (employee, VISIBLE, OBJECT);
    }

    OBJECT_OPTIMIZED(o) = FALSE;
    SUIT_redrawLocalSection (o, SUIT_mapViewportToScreen(o, oldViewport));
    SUIT_redisplayRequired (o);
    LEAVE (2, (buf, "SUIT_cycleObject(%s)\n", OBJECT_NAME (o)));
}



void si_cycleGroup (void)
{
    int i;

    for (i = DynLow (SelectedObjects); i < DynSize (SelectedObjects); i++)
	SUIT_cycleObject (*((SUIT_object *) DynGet (SelectedObjects, i)));
}



void SUIT_hitObject (SUIT_object o, SUIT_event event)
{
    SUIT_display *disp;

    ASSERT ((o != NULL), (mes, "SUIT_hitObject was called with a null object pointer.\n"));
    ENTER (3, (buf, "SUIT_hitObject on %s with rx=%d ry=%d type=%d\n",
	       OBJECT_NAME (o), event.relativePixelLocation.x, event.relativePixelLocation.y, event.type));

    disp = si_getDisplay (o, SUIT_getEnumString(o, ACTIVE_DISPLAY));

    if (disp->hit != NULL) {
	global.currentlyInHitProcedure = TRUE;
	disp->hit (o, event);
	global.currentlyInHitProcedure = FALSE;
    }
    LEAVE (3, (buf, "SUIT_hitObject on %s\n", OBJECT_NAME (o)));
}



void SUIT_redisplayRequired (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_redisplayRequired was called with a null object pointer.\n"));
    ENTER (1, (buf, "SUIT_redisplayRequired(%s)\n", OBJECT_NAME (o)));
    if (o->redisplay_required) {
	LEAVE (1, (buf, "SUIT_redisplayRequired(%s)\n", OBJECT_NAME (o)));
	return;
    }
    o->redisplay_required = TRUE;
    if (si_isVisible(o) && o->has_been_painted)
	SUIT_redrawObjectsAbove (o);
    LEAVE (1, (buf, "SUIT_redisplayRequired(%s)\n", OBJECT_NAME (o)));
}


void SUIT_redisplayNotRequired (SUIT_object o)
{
    ASSERT ((o != NULL), (mes, "SUIT_redisplayNotRequired was called with a null object pointer.\n"));
    ENTER (1, (buf, "SUIT_redisplayNotRequired(%s)\n", OBJECT_NAME (o)));
    o->redisplay_required = FALSE;
    LEAVE (1, (buf, "SUIT_redisplayNotRequired(%s)\n", OBJECT_NAME (o)));
}



PRIVATE void PerformRedisplayRecursively (SUIT_object o)
{
    int i, redisplay = o->redisplay_required;
    boolean optim = (o == global.root) || OBJECT_OPTIMIZED(o);

    if (redisplay || SUIT_getBoolean (o, ANIMATED))
	SUIT_paintObject (o);

    for (i = SUIT_numberOfChildren (o) - 1; i >= 0; i--) {
	SUIT_object child = SUIT_getChild (o, i);
	if (redisplay && !optim) /* If the parent wasn't optimized paint, don't let the kids be */
	    child->optimized_paint = FALSE;
	PerformRedisplayRecursively (child);
    }

    if (o != global.root) {
	char *display = SUIT_getEnumString (o, ACTIVE_DISPLAY);
	for (i = SUIT_numberOfEmployees (o, display) - 1; i >= 0; i--) {
	    SUIT_object emp = SUIT_getEmployee (o, display, i);
	    if (redisplay && !optim)
		emp->optimized_paint = FALSE;
	    PerformRedisplayRecursively (emp);
	}
    }

    if (redisplay && OBJECT_SELECTED(o) && si_isVisible(o))
	si_highlightRectangle (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)), TRUE);

    if (redisplay && o != global.root && OBJECT_OPEN(o) && si_isVisible(o))
	si_drawRectangleAsOpen (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
}


void SUIT_performRedisplay (void)
{
    ENTER (1, (buf, "SUIT_performRedisplay()\n"));
    PerformRedisplayRecursively (global.root);
    LEAVE (1, (buf, "SUIT_performRedisplay()\n"));
}


void SUIT_registerClass (char *className, SUIT_object (*createProc)(char *), char *helpText)
{
    SUIT_class *classList;
    property newGuy;
    char *pname = "object create procedure";
    char *ptype = "SUIT_functionPointer";
    int slot;

    newGuy.name = SUIT_createRCString (pname);
    newGuy.type = SUIT_createRCString (ptype);
    newGuy.value = (Pointer) createProc;
    newGuy.permanent = TEMPORARY;
    newGuy.locked = TRUE;

    SUIT_registerHelp(className, "object create procedure", helpText);
    classList = FindClassAndAddItIfNecessary (className);
    ASSERT ((classList != NULL), (mes, "SUIT_setProperty received a null class pointer from FindClassAndAddItIfNecessary.\n"));

    slot = DynFindIndex (classList->props, DummyProperty (pname), CompareProperty);
    if (slot != DYN_NOT_FOUND)
	DynDelete (classList->props, slot);
    DynAdd (classList->props, &newGuy);

/*  This way is cleaner but doesn't seem to work.  Boo.  -- Rob

    (void) FindClassAndAddItIfNecessary (className);
    SUIT_deluxeSetFunctionPointer (SUIT_dummyObjectInClass(className), "object create procedure",
				   (SUIT_functionPointer)createProc, CLASS);
    SUIT_makePropertyTemporary (SUIT_dummyObjectInClass(className), "object create procedure",
				"SUIT_functionPointer", CLASS);
*/			    
}


PRIVATE char *RelativeNameBuffer = NULL;
PRIVATE int  RelativeNameBufferLength = 0;

char *SUIT_relativeName (SUIT_object o, char *name)
{
    ASSERT ((strlen(OBJECT_NAME(o))+strlen(name)+2 < 1000), (mes, "SUIT_relativeName has a static char buffer which is not big enough to accommodate. This is a bug in SUIT.\n"));
 	if (RelativeNameBuffer == NULL) {
 		RelativeNameBuffer = (char *) SUIT_malloc (200);
 		RelativeNameBufferLength = 200;
 	}
 	if (strlen(OBJECT_NAME(o)) + strlen(name) + 3 > RelativeNameBufferLength) {
		SUIT_free (RelativeNameBuffer);
 		RelativeNameBuffer = (char *) SUIT_malloc (strlen(OBJECT_NAME(o)) + strlen(name) + 3);
 		RelativeNameBufferLength = strlen(OBJECT_NAME(o)) + strlen(name) + 3;
 	}
    strcpy (RelativeNameBuffer, OBJECT_NAME(o));
    strcat (RelativeNameBuffer, ": ");
    strcat (RelativeNameBuffer, name);
    return RelativeNameBuffer;
}


void SUIT_makeObjectTemporary (SUIT_object object)
{
    OBJECT_PERMANENT(object) = FALSE;
}


void SUIT_makeObjectPermanent (SUIT_object object)
{
    OBJECT_PERMANENT(object) = TRUE;
}



SUIT_object SUIT_getSibling (SUIT_object obj, char* siblingName)
{
    return SUIT_name (SUIT_relativeName (SUIT_getParent(obj), siblingName));
}
