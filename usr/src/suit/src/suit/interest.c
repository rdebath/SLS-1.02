/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "privsuit.h"



PRIVATE void callInterestRoutines (SUIT_object obj, property *newProperty, Pointer oldValue)
{
    int i;
    SUIT_objectInterestCallback f;
    SUIT_level unused;

    ENTER (5, (buf, "callInterestRoutines(%s,ptr,ptr)\n", OBJECT_NAME(obj)));
    if (obj->objectInterest != NULL) {
	for (i = DynLow (obj->objectInterest); i <= DynHigh (obj->objectInterest); i++) {
	    f = * (SUIT_objectInterestCallback *) DynGet (obj->objectInterest, i);
	    f (obj, newProperty->name, newProperty->type, newProperty->value, oldValue);

	    /* Because interest routines can recursively trigger themselves, the handle we have to
	       the new property value may no longer be correct.  To be safe, we have to look it up
	       again. */
	    newProperty = si_searchForPropAtLevels (obj, newProperty->name, &unused);
	    if (!newProperty)
		break;
	}
    }
    LEAVE (5, (buf, "callInterestRoutines(%s,ptr,ptr)\n", OBJECT_NAME(obj)));
}




PRIVATE void forObjectsInClass (SUIT_object o, char *className, property *newProperty, Pointer oldValue)
{
    int i;

    ENTER (5, (buf, "forObjectsInClass(%s,%s,ptr,ptr)\n", OBJECT_NAME(o),className));
    if (SUIT_stringsMatch (OBJECT_CLASS(o), className) &&
	!SUIT_propertyExists (o, newProperty->name, newProperty->type, OBJECT))
	callInterestRoutines(o, newProperty, oldValue);
    for (i = 0; i < SUIT_numberOfChildren (o); i++)
	forObjectsInClass (SUIT_getChild (o, i), className, newProperty, oldValue);
    LEAVE (5, (buf, "forObjectsInClass(%s,%s,ptr,ptr)\n", OBJECT_NAME(o),className));
}



PRIVATE void handleClass (SUIT_object o, property *newProperty, Pointer oldValue)
{
    ASSERT ((o != NULL), (mes, "handleClass called with a NULL object\n"));
    ENTER (5, (buf, "handleClass(%s,ptr,ptr)\n", OBJECT_NAME(o)));
    forObjectsInClass (global.root, OBJECT_CLASS(o), newProperty, oldValue);
    LEAVE (5, (buf, "handleClass(%s,ptr,ptr)\n", OBJECT_NAME(o)));
}



PRIVATE void handleObjectAndChildren (SUIT_object o, property *newProperty, Pointer oldValue)
{
    int i;

    ENTER (5, (buf, "handleObjectAndChildren(%s,ptr,ptr)\n", OBJECT_NAME(o)));

    callInterestRoutines (o, newProperty, oldValue);

    for (i = 0; i < SUIT_numberOfChildren (o); i++) {
	SUIT_object kid = SUIT_getChild (o, i);
	if (!SUIT_propertyExists (kid, newProperty->name, newProperty->type, CLASS) &&
	    !SUIT_propertyExists (kid, newProperty->name, newProperty->type, OBJECT)) /* i.e. if inheriting */
	    handleObjectAndChildren (kid, newProperty, oldValue);
    }

    LEAVE (5, (buf, "handleObjectAndChildren(%s,ptr,ptr)\n", OBJECT_NAME(o)));
}



PRIVATE void handleGlobal (SUIT_object o, property *newProperty, Pointer oldValue)
{
    ENTER (5, (buf, "handleGlobal(%s,ptr,ptr)\n", OBJECT_NAME(o)));
    handleObjectAndChildren (o, newProperty, oldValue);
    LEAVE (5, (buf, "handleGlobal(%s,ptr,ptr)\n", OBJECT_NAME(o)));
}



PRIVATE void handleMemberOfClass (SUIT_object o, property *new, Pointer old)
{
    DynArray funcs = o->classInfo->interestCallbacks;
    int i;
    SUIT_level unused;

    if (funcs == NULL)
	return;
    
    for (i=0; i < DynSize(funcs); i++) {
	SUIT_objectInterestCallback f = * (SUIT_objectInterestCallback *) DynGet (funcs, i);
	f (o, new->name, new->type, new->value, old);

	/* Because interest routines can recursively trigger themselves, the handle we have to
	   the new property value may no longer be correct.  To be safe, we have to look it up
	   again. */

	new = si_searchForPropAtLevels (o, new->name, &unused);
	if (!new)
	    break;
    }
}



PRIVATE void handleInterestInClassRec (SUIT_object o, SUIT_class *cl, property *new, Pointer old)
{
    int i;
    if (o->classInfo == cl)
	handleMemberOfClass (o, new, old);
    for (i=0; i < SUIT_numberOfChildren(o); i++)
	handleInterestInClassRec (SUIT_getChild(o,i), cl, new, old);
}



PRIVATE boolean HaveClassInterests = FALSE;

PRIVATE void handleInterestInClass (SUIT_object o, SUIT_level level, property *new, Pointer old)
{
    ENTER (5, (buf, "handleInterestInClass(%s,%s,ptr,ptr)\n", OBJECT_NAME(o),SUIT_levelName(level)));
    if (o != global.root && o->classInfo->interestCallbacks == NULL) {
	LEAVE (5, (buf, "handleInterestInClass(%s,%d,ptr,ptr)\n", OBJECT_NAME(o),level));
	return;
    }

    switch (level) {
      case OBJECT: handleMemberOfClass (o, new, old); break;
      case CLASS:  handleInterestInClassRec (global.root, o->classInfo, new, old); break;
      case GLOBAL: {
	  int i;
	  if (!HaveClassInterests) {
	      LEAVE (5, (buf, "handleInterestInClass(%s,%d,ptr,ptr)\n", OBJECT_NAME(o),level));
	      return;
	  }
	  for (i=0; i < DynSize (global.classProperties); i++) {
	      SUIT_class *cl = * (SUIT_class **) DynGet(global.classProperties, i);
	      if (cl->interestCallbacks != NULL) {
		  SUIT_object member = SUIT_getOneObjectFromClass(cl->name);
		  if (member != NULL)
		      handleInterestInClass (member, CLASS, new, old);
	      }
	  }
	  break;
      }
    }
    LEAVE (5, (buf, "handleInterestInClass(%s,%s,ptr,ptr)\n", OBJECT_NAME(o),SUIT_levelName(level)));
}



void si_checkCallbacks (SUIT_object o, SUIT_level level, property *newProp,
			Pointer oldValue)
{
    ENTER (5, (buf, "si_checkCallbacks(%s,%s,ptr,ptr)\n", OBJECT_NAME(o), SUIT_levelName(level)));
    ASSERT ((((level == GLOBAL) && (o == NULL)) || (o != NULL)),
	    (mes, "si_checkCallbacks was called with a non-null object pointer and level equal to global.\n"));

    switch (level) {
      case GLOBAL:  handleGlobal (global.root, newProp, oldValue);      break;
      case CLASS:   handleClass (o, newProp, oldValue);                 break;
      case OBJECT:  handleObjectAndChildren (o, newProp, oldValue);     break;
    }
    handleInterestInClass (o, level, newProp, oldValue);

    LEAVE (5, (buf, "si_checkCallbacks(%s,%s,ptr,ptr)\n", OBJECT_NAME(o), SUIT_levelName(level)));
}


void si_checkChildrensCallbacks (SUIT_object o, property *newProp, Pointer oldValue)
{
    int i;

    /* a property has been copied from the parent's CLASS level where it can't be seen
       to its OBJECT level where it can, so we need to check the kids callbacks */

    for (i = 0; i < SUIT_numberOfChildren (o); i++) {
	SUIT_object temp = SUIT_getChild (o, i);
	if (!SUIT_propertyExists (temp, newProp->name, newProp->type, OBJECT))
	    handleObjectAndChildren (temp, newProp, oldValue);
    }
}


/****************************  Object Interest Stuff *********************************/



void SUIT_registerInterest (SUIT_object o, SUIT_objectInterestCallback callback)
{
    int slot;

    ASSERT((callback != NULL), (mes, "SUIT_registerInterest was called with a NULL callback.\n"));
    ASSERT((o != NULL), (mes, "SUIT_registerInterest was called with a NULL object.\n"));

    if (o->objectInterest == NULL)
	o->objectInterest = DynCreate (sizeof (SUIT_objectInterestCallback), 1);
    slot = DynFindIndex (o->objectInterest, (void *) &callback, CompareCallbackFunctionPtr);
    if (slot == DYN_NOT_FOUND)
	DynAdd (o->objectInterest, (void *) &callback);
}



void SUIT_registerInterestInClass (char *classname, SUIT_objectInterestCallback callback)
{
    SUIT_class *class;
    int slot;

    ASSERT((callback != NULL), (mes, "SUIT_registerInterestInClass was called with a NULL callback.\n"));

    class = si_searchForClass (classname);
    ASSERT((class != NULL), (mes, "SUIT_registerInterestInClass was called with a non-existent class (%s)\n",classname));

    if (class->interestCallbacks == NULL)
	class->interestCallbacks = DynCreate (sizeof (SUIT_objectInterestCallback), 1);
    slot = DynFindIndex (class->interestCallbacks, (void *) &callback, CompareCallbackFunctionPtr);
    if (slot == DYN_NOT_FOUND)
	DynAdd (class->interestCallbacks, (void *) &callback);
    HaveClassInterests = TRUE;
}



void SUIT_registerInterestInGlobal (SUIT_objectInterestCallback callback)
{
    ASSERT((callback != NULL), (mes, "SUIT_registerInterestInGlobal was called with a NULL callback.\n"));
    SUIT_registerInterest (global.root, callback);
}
