/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#define ORIGINAL_HEIGHT "original height"
#define ORIGINAL_WIDTH "original width"
#define CHILDRENS_NAMES "childrens names"


PRIVATE void TileHorizontallyWithShrink (SUIT_object o)
{
    int width=0, maxHeight=0, i, x=0;

    if (SUIT_numberOfChildren(o) == 0)
	return;
    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	int bwidth = SUIT_getBoolean(kid, DRAW_BORDER_ON_INSIDE)? 0 : SUIT_getInteger(o,BORDER_WIDTH);
	int w = SUIT_getInteger (kid, ORIGINAL_WIDTH) + 2*bwidth;
	int h = SUIT_getInteger (kid, ORIGINAL_HEIGHT) + 2*bwidth;
	if (h > maxHeight)
	    maxHeight = h;
	width += w;
    }
    SUIT_changeObjectSize (o, width-1, maxHeight-1);
    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	int bwidth = SUIT_getBoolean(kid, DRAW_BORDER_ON_INSIDE)? 0 : SUIT_getInteger(o,BORDER_WIDTH);
	int w = SUIT_getInteger (kid, ORIGINAL_WIDTH);
	int h = SUIT_getInteger (kid, ORIGINAL_HEIGHT);
	SUIT_viewport vp;
	vp.bottom_left.x = x + bwidth;  vp.top_right.x = vp.bottom_left.x + w - 1;
	vp.bottom_left.y = (maxHeight-h)/2;      vp.top_right.y = vp.bottom_left.y + h - 1;
	SUIT_setViewport (kid, VIEWPORT, vp);
	x += w + 2*bwidth;
    }
}



PRIVATE boolean CanChangeOriginal = TRUE;


PRIVATE void TileVerticallyWithShrink (SUIT_object o)
{
    int height=0, maxWidth=0, i, y=0;

    if (SUIT_numberOfChildren(o) == 0)
	return;
    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	int bwidth = SUIT_getBoolean(kid, DRAW_BORDER_ON_INSIDE)? 0 : SUIT_getInteger(o,BORDER_WIDTH);
	int w = SUIT_getInteger (kid, ORIGINAL_WIDTH) + 2*bwidth;
	int h = SUIT_getInteger (kid, ORIGINAL_HEIGHT) + 2*bwidth;
	if (w > maxWidth)
	    maxWidth = w;
	height += h;
    }
    SUIT_changeObjectSize (o, maxWidth-1, height-1);
    for (i=0; i < SUIT_numberOfChildren(o); i++) {
	SUIT_object kid = SUIT_getChild(o,i);
	int bwidth = SUIT_getBoolean(kid, DRAW_BORDER_ON_INSIDE)? 0 : SUIT_getInteger(o,BORDER_WIDTH);
	int w = SUIT_getInteger (kid, ORIGINAL_WIDTH);
	int h = SUIT_getInteger (kid, ORIGINAL_HEIGHT);
	SUIT_viewport vp;
	vp.bottom_left.x = (maxWidth-w)/2;      vp.top_right.x = vp.bottom_left.x + w - 1;
	vp.bottom_left.y = y + bwidth;  vp.top_right.y = vp.bottom_left.y + h - 1;
	SUIT_setViewport (kid, VIEWPORT, vp);
	y += h + 2*bwidth;
    }
}



PRIVATE void Tile (SUIT_object o, boolean isVertical)
{
    SUIT_viewport vp;
    int Width, Height, i;
    int numKids = SUIT_numberOfChildren(o);
    int bWidth = SUIT_getInteger (o, BORDER_WIDTH);
    
    vp = OBJECT_VIEWPORT(o);
    Width = vp.top_right.x - vp.bottom_left.x;
    Height = vp.top_right.y - vp.bottom_left.y;
    for (i=0; i < numKids; i++) {
	SUIT_object kid = SUIT_getChild(o,numKids-i-1);
	SUIT_viewport kidvp;
	if (isVertical)
	    kidvp = SRGP_defRectangle (2*bWidth, i*Height/numKids + bWidth,
					Width - 2*bWidth, (i+1)*Height/numKids - bWidth);
	else
	    kidvp = SRGP_defRectangle (i*Width/numKids + bWidth, 2*bWidth, 
					(i+1)*Width/numKids - bWidth, Height - 2*bWidth);
	SUIT_setViewport (kid, VIEWPORT, kidvp);
    }
}



PRIVATE void TileHorizontally (SUIT_object o)   { Tile(o, FALSE); }
PRIVATE void TileVertically (SUIT_object o)     { Tile(o, TRUE); }


void TileAppropriately (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old);

PRIVATE void ChildChangesSize (SUIT_object kid, char *propName, char *propType, Pointer new, Pointer old)
{
    SUIT_object parent = SUIT_getParent(kid);

    if (SUIT_stringsMatch (propName, VIEWPORT) && SUIT_getBoolean(parent,SHRINK_TO_FIT) &&
	CanChangeOriginal) {
	int width, height;
	int owidth = SUIT_getInteger (kid, ORIGINAL_WIDTH);
	int oheight = SUIT_getInteger (kid, ORIGINAL_HEIGHT);

	SUIT_getObjectSize (kid, &width, &height);
	if (width != owidth || height != oheight) {
	    SUIT_setInteger (kid, ORIGINAL_WIDTH, width);
	    SUIT_setInteger (kid, ORIGINAL_HEIGHT, height);
	    TileAppropriately (parent, VIEWPORT, VIEWPORT, NULL, NULL);
	}
    }
}




#define INT(PTR)        ( * (int *) PTR )

void TileAppropriately (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    boolean numKidsChanged = SUIT_stringsMatch (propName, NUMBER_OF_CHILDREN);

    if (numKidsChanged && (old == NULL || INT(new) == INT(old) + 1)) {
	/* we are adding a new child */
	SUIT_object newkid = NULL;
	int width, height, i, j;
	SUIT_textList kidNames = SUIT_getTextList (o, CHILDRENS_NAMES);
	for (i=0; i < SUIT_numberOfChildren(o); i++) {
	    SUIT_object kid = SUIT_getChild(o, i);
	    boolean found = FALSE;
	    for (j=0; !found && j < SUIT_sizeOfTextList(kidNames); j++)
		if (SUIT_stringsMatch (SUIT_itemInTextList (kidNames, j), OBJECT_NAME(kid)))
		    found = TRUE;
	    if (!found) {
		newkid = kid;
		break;
	    }
	}
	if (newkid == NULL)
	    return;
	SUIT_getObjectSize (newkid, &width, &height);
	SUIT_setInteger (newkid, ORIGINAL_WIDTH, width);
	SUIT_lockProperty (newkid, ORIGINAL_WIDTH, OBJECT);
	SUIT_setInteger (newkid, ORIGINAL_HEIGHT, height);
	SUIT_lockProperty (newkid, ORIGINAL_HEIGHT, OBJECT);
	SUIT_registerInterest (newkid, ChildChangesSize);

	/* keep a list of the names of my children, so I know when I lose one which one is gone */
	SUIT_appendToTextList (kidNames, OBJECT_NAME(newkid));
	SUIT_setTextList (o, CHILDRENS_NAMES, kidNames);
	SUIT_lockProperty (o, CHILDRENS_NAMES, OBJECT);
	SUIT_freeTextList (kidNames);
    }
    else if (numKidsChanged && INT(new) == INT(old) - 1) {
	/* we're losing a child */
	SUIT_object oldkid = NULL;
	SUIT_textList kidNames = SUIT_getTextList (o, CHILDRENS_NAMES);
	boolean found = FALSE;
	int i, j;

	/* first find that child */
	for (i=0; i < SUIT_sizeOfTextList(kidNames); i++) {
	    found = FALSE;
	    for (j=0; !found && j < SUIT_numberOfChildren(o); j++)
		if (SUIT_stringsMatch (SUIT_itemInTextList (kidNames, i),
				       OBJECT_NAME(SUIT_getChild(o, j))))
		    found = TRUE;
	    if (!found)	{	/* found the missing one! */
		oldkid = SUIT_name (SUIT_itemInTextList (kidNames, i));
		break;
	    }
	}
	if (oldkid != NULL) {
	    SUIT_eraseProperty (oldkid, ORIGINAL_WIDTH, OBJECT);
	    SUIT_eraseProperty (oldkid, ORIGINAL_HEIGHT, OBJECT);
	}
	SUIT_deleteFromTextList (kidNames, i);
	SUIT_setTextList (o, CHILDRENS_NAMES, kidNames);
	SUIT_freeTextList (kidNames);
    }
    if (SUIT_stringsMatch (propName, NUMBER_OF_CHILDREN) ||
	SUIT_stringsMatch (propName, SHRINK_TO_FIT) ||
	SUIT_stringsMatch (propName, VIEWPORT) ||
	SUIT_stringsMatch (propName, ACTIVE_DISPLAY)) {
	char *disp = SUIT_getEnumString (o, ACTIVE_DISPLAY);
	CanChangeOriginal = FALSE;
	if (SUIT_stringsMatch (disp, "vertical stacking"))
	    if (SUIT_getBoolean (o, SHRINK_TO_FIT))
		TileVerticallyWithShrink(o);
	    else
		TileVertically(o);
	else if (SUIT_stringsMatch (disp, "horizontal stacking"))
	    if (SUIT_getBoolean (o, SHRINK_TO_FIT))
		TileHorizontallyWithShrink(o);
	    else
		TileHorizontally(o);
	else
	    ASSERT (FALSE, (mes, "TileAppropriately received unknown display style \"%s\"\n", disp));
	CanChangeOriginal = TRUE;
    }
}



/* 
  This type of object is different than most.  All of its display
  styles share the same hit and paint procedures.  What makes the
  displays appear distinct is the interest callback TileAppropriately.
  It rearranges the children based on the current display style.
*/


SUIT_object SUIT_createStacker (char *name)
{
    static boolean firsttime = TRUE;
    SUIT_textList emptyList = SUIT_defTextList (NULL, 0);

    SUIT_object o = SUIT_createObject (name, "stacker");
    SUIT_addDisplayToObject (o, "horizontal stacking", SUIT_passEventDown, SUIT_paintChildren);
    SUIT_addDisplayToObject (o, "vertical stacking", SUIT_passEventDown, SUIT_paintChildren);
    SUIT_setTextList (o, CHILDRENS_NAMES, emptyList);
      SUIT_makePropertyTemporary (o, CHILDRENS_NAMES, OBJECT);
    if (firsttime) {
	SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, CAN_BE_OPENED, CLASS);
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	firsttime = FALSE;
    }
    SUIT_registerInterest (o, TileAppropriately);
    return o;
}



SUIT_object SUIT_createMenuBar (char *name)
{
    SUIT_object o = SUIT_createStacker (name);
    SUIT_setEnumString (o, ACTIVE_DISPLAY, "horizontal stacking");
    SUIT_setBoolean (o, HAS_BORDER, FALSE);
    return o;
}
