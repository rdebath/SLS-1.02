/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"


#define TEXT_LIST	"text list"
#define MAXIMUM_WIDTH	"maximum width"
#define LAST_WIDTH      "last width"
#define LAST_HEIGHT     "last height"




/* Given a string which is too long to print in the widget, this routine
   breaks up the string into the first portion of the string that can 
   be printed (orig) and the rest of the string (returned).
   */

PRIVATE char *FindSubstringThatFits (char *orig, double *ascent, double *descent, double *maxWidth)
{
    char *scan, *lastscan = NULL;
    double width;
    
    scan = strchr (orig, '\n');
    if (scan != NULL) {
	scan[0] = '\0';
	GP_inquireTextExtent (orig, &width, ascent, descent);
	*maxWidth = MAX(*maxWidth, width);
	if (width < 0.96)  /* If everything up to the next CR fits, split it at the CR */
	    return scan+1;
	else
	    scan[0] = '\n';		
    }
    
    GP_inquireTextExtent (orig, &width, ascent, descent);
    *maxWidth = MAX(*maxWidth, width);
    if (width < 0.96)  /* If the whole thing fits, no need to split it at all */
	return NULL;
    
    width = 0.0;
    scan = strchr (orig, ' ');
    while (width < 0.96 && scan != NULL) {
	scan[0] = '\0';
	GP_inquireTextExtent (orig, &width, ascent, descent);
	*maxWidth = MAX(*maxWidth, width);
	scan[0] = ' ';
	lastscan = scan;
	scan = strchr (++scan, ' ');
    }
    for (scan = lastscan-1; *scan != ' ' && scan > orig; scan--) ;
    if (scan == orig)
	scan = lastscan;
    scan[0] = '\0';
    return scan+1;
}



PRIVATE void breakUpText (SUIT_object obj)
{
    char *textstring = SUIT_getText (obj, LABEL);
    char *copy = (char *) SUIT_malloc (strlen(textstring)+2), *hold = copy, *rest;
    SUIT_textList newlist = SUIT_defTextList (NULL, 0);
    double asc, desc, maxw = 0.0;
    
    GP_pushGraphicsState();
    GP_setViewport (SUIT_mapViewportToScreen (obj, OBJECT_VIEWPORT (obj)));
    GP_setWindow (OBJECT_WINDOW(obj));
    GP_setFont (SUIT_getFont (obj, FONT));
    
    strcpy (copy, textstring);
    strcat (copy, " ");
    
    rest = FindSubstringThatFits (copy, &asc, &desc, &maxw);
    SUIT_appendToTextList (newlist, copy);
    while (rest != NULL) {
	copy = rest;
	rest = FindSubstringThatFits (copy, &asc, &desc, &maxw);
	SUIT_appendToTextList (newlist, copy);
    }
    
    SUIT_setDouble (obj, MAXIMUM_WIDTH, maxw);
    SUIT_makePropertyTemporary (obj, MAXIMUM_WIDTH, OBJECT);
    SUIT_setTextList (obj, TEXT_LIST, newlist);
    SUIT_makePropertyTemporary (obj, TEXT_LIST, OBJECT);
    SUIT_setInteger (obj, NUMBER_OF_LINES, SUIT_sizeOfTextList(newlist));
    SUIT_makePropertyTemporary (obj, NUMBER_OF_LINES, OBJECT);
    SUIT_free(newlist);
    SUIT_free (hold);
    
    GP_popGraphicsState();
}



PRIVATE void FindNewArrangement (SUIT_object obj, char *pname, char *ptype, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (pname, VIEWPORT)) {
	int width = SUIT_getInteger (obj, LAST_WIDTH);
	int height = SUIT_getInteger (obj, LAST_HEIGHT);
	/* pausch hack: dec doesn't allow automatic agg. initialization */
	SUIT_viewport vp;
	vp = OBJECT_VIEWPORT(obj);
	if (width != vp.top_right.x - vp.bottom_left.x ||
	    height != vp.top_right.y - vp.bottom_left.y) {
	    SUIT_setInteger (obj, LAST_WIDTH, vp.top_right.x - vp.bottom_left.x);
	    SUIT_setInteger (obj, LAST_HEIGHT, vp.top_right.y - vp.bottom_left.y);
	    breakUpText (obj);
	}
    }
    else if (SUIT_stringsMatch (pname, LABEL) ||
	     SUIT_stringsMatch (pname, FONT))
	breakUpText (obj);
}




PRIVATE void PaintText (SUIT_object o)
{
    double y = 0.98, w, asc, desc;
    double spacing = SUIT_getDouble (o, LINE_SPACING);
    int i = SUIT_getInteger (o, STARTING_LINE);
    SUIT_textList list = SUIT_getTextList (o, TEXT_LIST);

    GP_inquireTextExtent ("Ay", &w, &asc, &desc);
    
    y -= (asc + desc) * spacing;
    for (; i < SUIT_sizeOfTextList(list); i++) {
	GP_text (GP_defPoint (0.02, y), SUIT_itemInTextList(list, i));
	y -= (asc + desc) * spacing;
    }

    SUIT_free(list);
}



void SUIT_optimizeSizeForText (SUIT_object o, int width)
{
    double w, asc, desc;
    double spacing = SUIT_getDouble (o, LINE_SPACING);
    int numLines;
    int height, wid;

    SUIT_changeObjectSize (o, width, 50);

    numLines = SUIT_getInteger (o, NUMBER_OF_LINES);
    GP_pushGraphicsState();
    GP_setWindow (SUIT_getWindow (o, WINDOW));
    GP_setViewport (SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
    GP_setFont (SUIT_getFont(o, FONT));

    GP_inquireTextExtent ("Ay", &w, &asc, &desc);
    wid = MIN(width, GP_mapWidth(SUIT_getDouble(o, MAXIMUM_WIDTH))+20);
    height = GP_mapHeight ((asc + desc) * spacing * (numLines + 1));

    SUIT_changeObjectSize (o, wid, height);

    GP_popGraphicsState();
}



SUIT_object SUIT_createTextBox (char *name, char *text)
{
    SUIT_object retval = SUIT_createObject (name, "text box");
    SUIT_addDisplayToObject (retval, "standard", NULL, PaintText);
    SUIT_registerInterest (retval, FindNewArrangement);
    SUIT_setText (retval, LABEL, text);
    SUIT_setDouble (retval, LINE_SPACING, 1.0);
    SUIT_setInteger (retval, STARTING_LINE, 0);
      SUIT_makePropertyTemporary (retval, STARTING_LINE, OBJECT);
    SUIT_setInteger (retval, LAST_WIDTH, 0);
      SUIT_makePropertyTemporary (retval, LAST_WIDTH, OBJECT);
    SUIT_setInteger (retval, LAST_HEIGHT, 0);
      SUIT_makePropertyTemporary (retval, LAST_HEIGHT, OBJECT);
    return retval;
}
