/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "suit.h"
#include "scrolbox.h"
#if defined(_Windows) || defined(SGI_X)
#include <ctype.h>
#endif

#define TEMPORARY_BACKWARD_COMPATIBILITY_HACK

#if defined (TEMPORARY_BACKWARD_COMPATIBILITY_HACK)
#define SIBLING_SCROLLBAR(O)   (SUIT_name (SUIT_relativeName (SUIT_getParent(O), "scrollbar") ) )
#define SIBLING_LIST(O)        (SUIT_name (SUIT_relativeName (SUIT_getParent(O), "list") ) )
#else
#define SIBLING_SCROLLBAR(O)   (SUIT_getChild (SUIT_getParent(O), 0) )
#define SIBLING_LIST(O)        (SUIT_getChild (SUIT_getParent(O), 1) )
#endif

#define LIST_HAS_LABEL(O)     (!SUIT_stringsMatch(SUIT_getText((SUIT_getParent(O)),LABEL),""))


#define DRAWING_VIEWPORT "drawing viewport"


PRIVATE DynArray GetList (SUIT_object obj, boolean *canDestroy)
{
    if (SUIT_propertyExists (obj, LIST, "SUIT_textList", OBJECT) ||
	SUIT_propertyExists (obj, LIST, "SUIT_textList", CLASS)) {
	*canDestroy = TRUE;
	return (DynArray) SUIT_getTextList (obj, LIST);
    }
    *canDestroy = FALSE;
    return SUIT_getDynArray (obj, LIST);
}



int sb_textHeight (SUIT_object listWidget)
{
    int width, ascent, descent;
    double temp;
    
    GP_pushGraphicsState();
    (void) GP_setFont(SUIT_getFont(listWidget, FONT) );
    SRGP_inquireTextExtent ("Ay", &width, &ascent, &descent);
    GP_popGraphicsState();
    
    temp = (ascent + descent) * SUIT_getDouble(listWidget, TEXT_SPACING);
    return (int) temp;
}



/*  returns -1 if the event hits no row  */

int sb_mapListEvent (SUIT_object listWidget, point p)
{
    int ht = sb_textHeight(listWidget);
    rectangle vp;
    int	retval;
    
    vp = SUIT_mapViewportToScreen(listWidget, SUIT_getViewport(listWidget, DRAWING_VIEWPORT));
    if ((p.y >= vp.bottom_left.y) && (p.y <= vp.top_right.y)) {
	boolean canFree;
	DynArray list = GetList(SUIT_getParent(listWidget), &canFree);
	retval = (int)SUIT_getDouble(SIBLING_SCROLLBAR(listWidget), CURRENT_VALUE) +
	    (vp.top_right.y - p.y - ht/2) / ht;

	/* pausch: we had a problem "falling off" the bottom... this fixes the bug. */
	retval = MIN(retval, DynHigh(list) );
	if (canFree)
	    SUIT_destroyTextList ((SUIT_textList) list);
    } else
	retval = -1;
    
    return retval;
}



/* gives the appropriate viewport for the logical 'loc'th row */

SUIT_viewport sb_listSubViewport (SUIT_object listWidget, int loc, SUIT_viewport drawingVP, 
			       double siblingScrollerCurrentValue, int textHeight, int margin)
{
    SUIT_viewport retval;

    retval = drawingVP;
    loc -= (int) siblingScrollerCurrentValue;
    retval.top_right.y -= (loc * textHeight );
    retval.bottom_left.y = retval.top_right.y - textHeight;
    retval.bottom_left.x += margin;
    retval.top_right.x -= margin;
    return retval;
}



PRIVATE int fullLinesInViewport (SUIT_object listWidget, SUIT_viewport vp)
{
    return (vp.top_right.y-vp.bottom_left.y) / sb_textHeight(listWidget);
}



void sb_makeListConsistent (SUIT_object listWidget)
{
    SUIT_viewport vp;
    boolean canFree;
    DynArray list = GetList(SUIT_getParent(listWidget), &canFree);
    
    vp = OBJECT_VIEWPORT(listWidget);
    if (LIST_HAS_LABEL(listWidget))
	vp.top_right.y -= sb_textHeight(listWidget);
    
    if (list != NULL) {
	int linesInVP = fullLinesInViewport(listWidget, vp);
	int max = MAX(0, DynSize(list)-linesInVP);

	if (linesInVP > DynSize(list))
	    vp.bottom_left.y = vp.top_right.y - (DynSize(list) * sb_textHeight(listWidget) );
	SUIT_suspendMarkingRedisplay(listWidget);
	SUIT_setViewport(listWidget, DRAWING_VIEWPORT, vp);
	SUIT_resumeMarkingRedisplay(listWidget);
	
	/* here's the trick: scroller thinks maximum possible amount to scroll
	   is an amount that would put our last line at bottom of scroll box */
	
	SUIT_setDouble(SIBLING_SCROLLBAR(listWidget), MAXIMUM_VALUE, (double)max);
	if (max == 0)
	    SUIT_setDouble(SIBLING_SCROLLBAR(listWidget), PERCENT_FULL, 1.0);
	else    
	    SUIT_setDouble(SIBLING_SCROLLBAR(listWidget), PERCENT_FULL, (double)linesInVP / (double)DynSize(list));
    }

    if (canFree)
	SUIT_destroyTextList ((SUIT_textList) list);
}



void sb_listInterestCallback (SUIT_object listWidget, char *name, char *type, void *new, void *old)
{
    if (SUIT_stringsMatch (name, LABEL) ||
	SUIT_stringsMatch (name, VIEWPORT) ||
	SUIT_stringsMatch (name, LINE_SPACING) ||
	SUIT_stringsMatch (name, FONT) ||
	SUIT_stringsMatch (name, LIST))
	sb_makeListConsistent(listWidget);
}



PRIVATE void DrawSimpleText (SUIT_object listWidget, int num, SUIT_viewport vp)
{
    char *entry;
    boolean canFree;
    DynArray list = GetList (SUIT_getParent(listWidget), &canFree);
    int row = SUIT_getInteger (SUIT_getParent(listWidget), CURRENT_ROW);
    
    entry = * (char **) DynGet (list, num);
    if (num == row) {
	int w, a, d;
	int x1=vp.bottom_left.x, y1=vp.bottom_left.y, x2=vp.top_right.x;
	GP_pushGraphicsState();
	GP_setColor (SUIT_getColor (listWidget, FOREGROUND_COLOR));
	SRGP_inquireTextExtent(entry, &w, &a, &d);
	SRGP_fillRectangleCoord (x1,y1-d-1,x2,y1+a+1);
	GP_setColor (SUIT_getColor (listWidget, BACKGROUND_COLOR));
	GP_text(GP_unMapPoint(vp.bottom_left), entry);
	GP_popGraphicsState();
    } else
	GP_text(GP_unMapPoint(vp.bottom_left), entry);

    if (canFree)
	SUIT_destroyTextList ((SUIT_textList) list);
}



PRIVATE void DrawLabelOnScrollableBox (SUIT_object listWidget)
{
    SUIT_viewport vp;
    int	text_height = sb_textHeight(listWidget);
    
    vp = SUIT_mapViewportToScreen(listWidget, OBJECT_VIEWPORT(listWidget));
    SRGP_lineCoord(vp.bottom_left.x, vp.top_right.y-text_height-3,
		   vp.top_right.x, vp.top_right.y-text_height-3);
    
    vp.bottom_left.y = vp.top_right.y - text_height + 5;
    
    GP_pushGraphicsState();
    GP_setViewport(vp);
    GP_setWindow(GP_unMapRectangle(vp) );
    GP_justifyText(SUIT_getText(SUIT_getParent(listWidget), LABEL), JUSTIFY_BOTTOM_CENTER);
    GP_popGraphicsState();
}



void sb_genericPaintScrollableBox (SUIT_object listWidget, void (*DrawSingleItem)(SUIT_object, int, SUIT_viewport))
{
    int scroll, linesInVP;
    double current;
    int tHeight = sb_textHeight(listWidget), margin = SUIT_getInteger(listWidget, MARGIN);
    SUIT_viewport drawVP;
    int	i;

    scroll = (int) SUIT_getDouble (SIBLING_SCROLLBAR(listWidget), CURRENT_VALUE);
    current = SUIT_getDouble (SIBLING_SCROLLBAR(listWidget), CURRENT_VALUE);
    drawVP = SUIT_mapViewportToScreen(listWidget, SUIT_getViewport(listWidget, DRAWING_VIEWPORT));
    linesInVP = fullLinesInViewport(listWidget, drawVP);
    if (LIST_HAS_LABEL(listWidget))
	DrawLabelOnScrollableBox(listWidget);
    
    for (i = 0; i < linesInVP; i++)
	DrawSingleItem (listWidget, scroll + i,
			sb_listSubViewport (listWidget, scroll + i, drawVP, current, tHeight, margin));
}



PRIVATE void PaintScrollableBox (SUIT_object listWidget)
{
    sb_genericPaintScrollableBox(listWidget, DrawSimpleText);
}



PRIVATE void scrollToPosition (SUIT_object o, char c)
{
    SUIT_object parent = SUIT_getParent(o);
    SUIT_object scroller = SIBLING_SCROLLBAR(o);
    SUIT_textList list = SUIT_getTextList (parent, LIST);
    int desiredPosition = -1;
    int initialPosition = (int) SUIT_getDouble (scroller, CURRENT_VALUE);
    int i;
    
    for (i = initialPosition; i < SUIT_sizeOfTextList(list) && desiredPosition == -1; i++) {
	char *text = SUIT_itemInTextList (list, i);
	if (c == text[0])
	    desiredPosition = i;
    }

    if (desiredPosition > -1) {
	SUIT_setDouble (scroller, CURRENT_VALUE, desiredPosition);
	SUIT_setInteger (o, CURRENT_ROW, desiredPosition);
    }
}



PRIVATE void ActuallyHitScroller (SUIT_object listWidget, int row)
{
    DynArray list;
    void (*func)(SUIT_object);
    boolean canFree;
    SUIT_object parent = SUIT_getParent(listWidget);
    
    list = GetList (SUIT_getParent(listWidget), &canFree);

    SUIT_suspendMarkingRedisplay (parent);
    SUIT_setText (parent, CURRENT_VALUE, * (char **)DynGet(list, row));
    SUIT_setInteger (parent, CURRENT_ROW, row);
    SUIT_resumeMarkingRedisplay (parent);

    SUIT_redisplayRequired(listWidget);
    func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (listWidget, CALLBACK_FUNCTION);

    if (func != NULL)
	func (parent);

    if (canFree)
	SUIT_destroyTextList ((SUIT_textList) list);
}

PRIVATE void HitScrollableBox (SUIT_object listWidget, SUIT_event e)
{
    int	row = sb_mapListEvent (listWidget, e.locator.position);
    
    if (e.type == MOUSE_DOWN || e.type == CLICK) {
	if (row != -1)
	    ActuallyHitScroller (listWidget, row);
    }
    else if (e.type == KEYSTROKE && isalpha(e.keyboard))
	scrollToPosition (listWidget, e.keyboard);
}



PRIVATE SUIT_object SUIT_createList (char *name, void (*callback)(SUIT_object))
{
    SUIT_object listWidget;
    boolean firsttime = TRUE;
    
    listWidget = SUIT_createObject (name, LIST);
    SUIT_addDisplayToObject (listWidget, "standard", HitScrollableBox, PaintScrollableBox);
    
    SUIT_setFunctionPointer (listWidget, CALLBACK_FUNCTION, (SUIT_functionPointer)callback);
    if (firsttime) { 
	SUIT_deluxeSetDouble (listWidget, TEXT_SPACING, 1.2, CLASS);
	SUIT_deluxeSetBoolean (listWidget, BORDER_RAISED, FALSE, CLASS);
	firsttime = FALSE;
    }
    SUIT_registerInterest(listWidget, sb_listInterestCallback);
    return listWidget;
}



void sb_listScrollerCallback (SUIT_object scrollbar, char *name, char *type, void *new, void *old)
{
    if (SUIT_stringsMatch (name, CURRENT_VALUE) && 
	SUIT_numberOfChildren(SUIT_getParent(scrollbar)) > 1)
	SUIT_redisplayRequired (SIBLING_LIST(scrollbar));
}



PRIVATE void CoupleCurrentRowAndCurrentValue (SUIT_object bboard, char *name, char *type, void *new, void *old)
{
    static boolean canEnter = TRUE; /* this is just for efficiency, prevents unneeded recursion */
    boolean canFree;
    SUIT_textList list = GetList (bboard, &canFree);
    
    if (SUIT_stringsMatch (name, LIST) && canEnter) {
	int row = SUIT_getInteger (bboard, CURRENT_ROW);
	SUIT_textList arr = * (SUIT_textList *) new;
	if (row > DynHigh(arr) ||
	    (row >= 0 && row <= DynHigh(arr) &&
	     !SUIT_stringsMatch(SUIT_getText (bboard, CURRENT_VALUE),
				SUIT_itemInTextList(arr,row)))) {
	    canEnter = FALSE;
	    SUIT_setInteger (bboard, CURRENT_ROW, -1);
	    SUIT_setText (bboard, CURRENT_VALUE, "");
	    canEnter = TRUE;
	}
    }

    if (SUIT_sizeOfTextList(list) == 0)
	return;

    if (SUIT_stringsMatch (name, CURRENT_VALUE) && canEnter) {
	char *value = (char *) new;
	canEnter = FALSE;
	if (SUIT_stringsMatch (value, ""))
	    SUIT_setInteger (bboard, CURRENT_ROW, -1);
	else {
	    int i;
	    for (i=0; i < DynSize(list); i++)
		if (SUIT_stringsMatch(SUIT_itemInTextList(list, i), value)) {
		    SUIT_setInteger (bboard, CURRENT_ROW, i);
		    break;
		}
	}
	canEnter = TRUE;
    } else if (SUIT_stringsMatch (name, CURRENT_ROW) && canEnter) {
	int row = * (int *) new;
	canEnter = FALSE;
	SUIT_setText (bboard, CURRENT_VALUE, (row == -1)? "" : SUIT_itemInTextList(list, row));
	canEnter = TRUE;
    }

    if (canFree)
	SUIT_destroyTextList ((SUIT_textList) list);
}



SUIT_object SUIT_createScrollableList (char *name, void (*callback)(SUIT_object))
{
    SUIT_object bboard, lister, scroller;
    
    bboard = SUIT_createBulletinBoardWithClass (name, "scrollable list");
      SUIT_deluxeSetBoolean (bboard, HAS_BACKGROUND, FALSE, CLASS);
      SUIT_deluxeSetBoolean (bboard, HAS_BORDER, FALSE, CLASS);
      SUIT_deluxeSetBoolean (bboard, CAN_BE_OPENED, FALSE, CLASS);
      SUIT_makePropertyTemporary (bboard, CAN_BE_OPENED, CLASS);
      SUIT_setText(bboard, LABEL, "");   /* this is so the child can inherit it -- Randy */
      SUIT_setTextList (bboard, LIST, SUIT_defTextList(NULL, 0));
      SUIT_setText (bboard, CURRENT_VALUE, "");
      SUIT_setInteger (bboard, CURRENT_ROW, -1);
      SUIT_changeObjectSize (bboard, 200, 200);
      SUIT_registerInterest (bboard, CoupleCurrentRowAndCurrentValue);
    
    scroller = SUIT_createBoundedValue (SUIT_relativeName(bboard, "scrollbar"), NULL);
      SUIT_addChildToObject (bboard, scroller);
      SUIT_setEnumString (scroller, ACTIVE_DISPLAY, "scroll bar");
      SUIT_setDouble (scroller, GRANULARITY, 1.0);
      SUIT_setDouble (scroller, CURRENT_VALUE, 0.0);
      SUIT_setBoolean (scroller, BORDER_RAISED, FALSE);
      SUIT_setSpringiness (scroller, SPRINGINESS, LEFT_SPRINGINESS|VERTICAL_SPRINGINESS);
      SUIT_setViewport (scroller, VIEWPORT, SUIT_mapToParent(scroller, 0.94, 0.0, 1.0, 1.0));
      SUIT_registerInterest(scroller, sb_listScrollerCallback);

    lister = SUIT_createList (SUIT_relativeName(bboard, "list"), callback);
      SUIT_addChildToObject (bboard, lister);
      SUIT_setViewport (lister, VIEWPORT, SUIT_mapToParent(lister, 0.0, 0.0, 0.89, 1.0));
      SUIT_setSpringiness (lister, SPRINGINESS, HORIZONTAL_SPRINGINESS|VERTICAL_SPRINGINESS);
    
    return bboard;
}


void SUIT_resetScrollableListToTop (SUIT_object scroller)
{
#if defined (TEMPORARY_BACKWARD_COMPATIBILITY_HACK)
    SUIT_object scrollbar = SUIT_name (SUIT_relativeName (scroller, "scrollbar"));
#else
    SUIT_object scrollbar = SUIT_getChild (scroller, 0);
#endif
    SUIT_setDouble (scrollbar, CURRENT_VALUE, 0.0);
}


#undef TEMPORARY_BACKWARD_COMPATIBILITY_HACK
