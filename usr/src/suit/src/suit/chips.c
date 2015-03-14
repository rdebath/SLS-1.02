/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include <math.h>
#include "suit.h"

/********************************************************

  This file handles both color chips and pattern chips.

  ********************************************************/

#define MAXLENGTH    10
#define ISCOLORS(O)  (strcmp(OBJECT_CLASS(O),"color chips")==0)
#define HORIZ(VP)    (VP.top_right.x-VP.bottom_left.x>VP.top_right.y-VP.bottom_left.y)
#define C_STYLE       (((double) j) / ((double) i))


/* Normally having all objects share static data is bad, but here it's
 * exectable, since the number of allocated colors is something all color
 * chips should share anyway. */

PRIVATE NumberOfAllocatedColors = 0;


PRIVATE void ChangeNumColors (void)
{
    NumberOfAllocatedColors = GP_numColorsAllocated ();
    SUIT_allObjectsRequireRedisplay ("color chips");
}



PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer p1, Pointer p2)
{
    if (!(SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	  SUIT_stringsMatch (propName, PREVIOUS_VALUE)))
	OBJECT_OPTIMIZED (o) = FALSE;
}



PRIVATE int GP_numPatternsAllocated (void)
{
    return (40);
}


/* percent of 0.10 means move in by 10 percent */
PRIVATE GP_rectangle IndentRectangle (GP_rectangle r, double percent)
{
    GP_rectangle retval;
    double xTotal, yTotal;

    retval = r;
    xTotal = r.top_right.x - r.bottom_left.x;
    yTotal = r.top_right.y - r.bottom_left.y;

    retval.bottom_left.x += xTotal * percent;
    retval.bottom_left.y += yTotal * percent;
    retval.top_right.x -= xTotal * percent;
    retval.top_right.y -= yTotal * percent;
    return (retval);
}



PRIVATE GP_rectangle GetChipCoords (SUIT_object o, int chipNumber, int numChips)
{
    GP_rectangle retval;
    SUIT_viewport vp;

    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o));
    if (HORIZ (vp)) {
	retval.bottom_left.x = chipNumber * (1.0 / numChips);
	retval.top_right.x = (chipNumber + 1) * (1.0 / numChips);
	retval.bottom_left.y = 0.0;
	retval.top_right.y = 1.0;
    } else {			/* if vertical */
	retval.bottom_left.x = 0.0;
	retval.top_right.x = 1.0;
	retval.bottom_left.y = chipNumber * (1.0 / numChips);
	retval.top_right.y = (chipNumber + 1) * (1.0 / numChips);
    }

    return (retval);
}



PRIVATE void NewSelection (SUIT_object o, int newCurrentValue)
{
    void (*funct) (SUIT_object);

    if (ISCOLORS (o)) {
	GP_color currentColor, fred;
	currentColor = SUIT_getColor (o, CURRENT_VALUE);
	if (newCurrentValue == GP_getColorIndex (currentColor.colorName))
	    return;
	SUIT_setColor (o, PREVIOUS_VALUE, currentColor);
	fred = GP_defColor (GP_getColorName (newCurrentValue), BLACK_ON_MONO);
	SUIT_setColor (o, CURRENT_VALUE, GP_defColor (GP_getColorName (newCurrentValue),
					  (newCurrentValue == SRGP_BLACK)));
    } else {
	if (newCurrentValue == SUIT_getInteger (o, CURRENT_VALUE))
	    return;
	SUIT_setInteger (o, PREVIOUS_VALUE, SUIT_getInteger (o,
							     CURRENT_VALUE));
	SUIT_setInteger (o, CURRENT_VALUE, newCurrentValue);
    }
    if ((funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION)) != NULL)
	funct (o);
}



PRIVATE void PaintIthChip (SUIT_object o, int i, int numAllocated, boolean highlight)
{
    GP_rectangle r;
    boolean colors_match = FALSE;
    r = GetChipCoords (o, i, numAllocated);
    if (highlight) {
	GP_pushGraphicsState ();
	GP_setFillStyle (SOLID);
	GP_setColor (SUIT_getColor (o, HIGHLIGHT_COLOR));
	GP_fillRectangle (r);
	if (ISCOLORS (o)) {
	    if (SUIT_stringsMatch ((SUIT_getColor (o, CURRENT_VALUE)).colorName,
			    (SUIT_getColor (o, HIGHLIGHT_COLOR)).colorName))
		colors_match = TRUE;
	} else if ((i == 1) &&
		   (SUIT_stringsMatch ((SUIT_getColor (o, FOREGROUND_COLOR)).colorName,
			   (SUIT_getColor (o, HIGHLIGHT_COLOR)).colorName)))
	    colors_match = TRUE;
	GP_popGraphicsState ();
    } else {
	GP_pushGraphicsState ();
	GP_setFillStyle (SOLID);
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangle (r);
	GP_popGraphicsState ();
    }

    if (ISCOLORS (o)) {
	SRGP_setColor (i);
	GP_setFillStyle (SOLID);
    } else {
	GP_setFillBitmapPattern (i);
	GP_setFillStyle (BITMAP_PATTERN_OPAQUE);
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    }
    GP_fillRectangle (IndentRectangle (r, SUIT_getDouble (o, CHIP_BORDER)));
    if (colors_match)
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    else
	GP_setColor (SUIT_getColor (o, BORDER_COLOR));
    GP_drawRectangle (IndentRectangle (r, SUIT_getDouble (o, CHIP_BORDER)));
}



PRIVATE void PaintChips (SUIT_object o)
{
    int numAllocated, currentHighlight, i;
    rectangle vp;
    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o));

    if (OBJECT_OPTIMIZED (o)) {
	if (ISCOLORS (o)) {
	    GP_color temp1, temp2;
	    temp1 = SUIT_getColor (o, PREVIOUS_VALUE);
	    temp2 = SUIT_getColor (o, CURRENT_VALUE);
	    numAllocated = NumberOfAllocatedColors;	/* GP_numColorsAllocated(
							 * ); */
	    currentHighlight = GP_getColorIndex (temp1.colorName);
	    PaintIthChip (o, currentHighlight, numAllocated, FALSE);
	    currentHighlight = GP_getColorIndex (temp2.colorName);
	    PaintIthChip (o, currentHighlight, numAllocated, TRUE);
	    GP_setColor (SUIT_getColor (o, BORDER_COLOR));
	    GP_rectangleCoord (0.0, 0.0, 1.0, 1.0);
	} else {
	    numAllocated = GP_numPatternsAllocated ();
	    currentHighlight = SUIT_getInteger (o, PREVIOUS_VALUE);
	    PaintIthChip (o, currentHighlight, numAllocated, FALSE);
	    currentHighlight = SUIT_getInteger (o, CURRENT_VALUE);
	    PaintIthChip (o, currentHighlight, numAllocated, TRUE);
	    GP_setColor (SUIT_getColor (o, BORDER_COLOR));
	    GP_rectangleCoord (0.0, 0.0, 1.0, 1.0);
	}
    } else {

	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangle (GP_defRectangle (0.0, 0.0, 1.0, 1.0));
	SUIT_borderObject (o);

	if (ISCOLORS (o)) {
	    GP_color temp;
	    temp = SUIT_getColor (o, CURRENT_VALUE);
	    currentHighlight = GP_getColorIndex (temp.colorName);
	    numAllocated = NumberOfAllocatedColors;
	    if (ISCOLORS (o) && (GP_inquireCanvasDepth () == 1))
		numAllocated = 2;
	} else {
	    currentHighlight = SUIT_getInteger (o, CURRENT_VALUE);
	    numAllocated = GP_numPatternsAllocated ();

	    /* rob hack to fix ncd problem */
	    SRGP_setColor (0);
	    GP_fillRectangleCoord (0.0, 0.0, 1.0, 1.0);
	    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	}
	if (HORIZ (vp)) {
	    if (numAllocated > vp.top_right.x - vp.bottom_left.x) {
		vp.top_right.x = vp.bottom_left.x + numAllocated;
		SUIT_setViewport (o, VIEWPORT, vp);
	    }
	} else {
	    if (numAllocated > vp.top_right.y - vp.bottom_left.y) {
		vp.top_right.y = vp.bottom_left.y + numAllocated;
		SUIT_setViewport (o, VIEWPORT, vp);
	    }
	}
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangleCoord (0.0, 0.0, 1.0, 1.0);
	for (i = 0; i < numAllocated; i++)
	    PaintIthChip (o, i, numAllocated, i == currentHighlight);
	GP_setColor (SUIT_getColor (o, BORDER_COLOR));
	GP_rectangleCoord (0.0, 0.0, 1.0, 1.0);
    }
    OBJECT_OPTIMIZED (o) = TRUE;
}



PRIVATE void HitChips (SUIT_object o, SUIT_event event)
{
    int numAllocated;
    SUIT_viewport vp;
    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o));
    if (ISCOLORS (o)) {
	numAllocated = NumberOfAllocatedColors;	/* GP_numColorsAllocated(); */
    } else {
	numAllocated = GP_numPatternsAllocated ();
    }

    if (HORIZ (vp)) {
	float xHit;
	/* hack by K.P. to prevent setting colors that don't exist - if
	 * event.worldLocation.x >= 1 then (event.worldLocation.x *
	 * numAllocated) will equal numAllocated which is one greater than
	 * the largest color index */
	if (event.worldLocation.x >= 1.000000)
	    xHit = 0.999999;
	else
	    xHit = event.worldLocation.x;
	NewSelection (o, (int) (xHit * numAllocated));
    } else {
	float yHit;
	if (event.worldLocation.y >= 1.000000)
	    yHit = 0.999999;
	else
	    yHit = event.worldLocation.y;
	NewSelection (o, (int) (yHit * numAllocated));
    }
}



PRIVATE void PaintIJthChip (SUIT_object o, GP_rectangle r, int index, boolean highlight)
{
    boolean colors_match = FALSE;

    GP_setFillStyle (SOLID);
    if (highlight) {
	GP_setColor (SUIT_getColor (o, HIGHLIGHT_COLOR));

	GP_fillRectangle (r);
	if (ISCOLORS (o)) {
	    if (SUIT_stringsMatch ((SUIT_getColor (o, CURRENT_VALUE)).colorName,
			    (SUIT_getColor (o, HIGHLIGHT_COLOR)).colorName))
		colors_match = TRUE;
	} else if ((index == 1) &&
		   (SUIT_stringsMatch ((SUIT_getColor (o, FOREGROUND_COLOR)).colorName,
			   (SUIT_getColor (o, HIGHLIGHT_COLOR)).colorName)))
	    colors_match = TRUE;
    } else {
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangle (r);
    }

    if (ISCOLORS (o)) {
	SRGP_setColor (index);
	GP_setFillStyle (SOLID);
    } else {
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	GP_setFillBitmapPattern (index);
	GP_setFillStyle (BITMAP_PATTERN_OPAQUE);
    }

    GP_fillRectangle (IndentRectangle (r, SUIT_getDouble (o, CHIP_BORDER)));
    if (colors_match)
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    else
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    GP_drawRectangle (IndentRectangle (r, SUIT_getDouble (o, CHIP_BORDER)));
}



PRIVATE void PaintWithDimensions (SUIT_object o, int num_vert_chips, int num_horiz_chips)
{
    double i, j, x, y;
    int chip_num, numColors;

    if (OBJECT_OPTIMIZED (o)) {
	int currentHighlight1, currentHighlight2;

	if (ISCOLORS (o)) {
	    GP_color temp1, temp2;

	    temp1 = SUIT_getColor (o, PREVIOUS_VALUE);
	    temp2 = SUIT_getColor (o, CURRENT_VALUE);
	    currentHighlight1 = GP_getColorIndex (temp1.colorName);
	    currentHighlight2 = GP_getColorIndex (temp2.colorName);
	} else {
	    currentHighlight1 = SUIT_getInteger (o, PREVIOUS_VALUE);
	    currentHighlight2 = SUIT_getInteger (o, CURRENT_VALUE);
	}

	if (!(currentHighlight1 == currentHighlight2)) {
	    x = ((double) (currentHighlight1 % num_horiz_chips)) / num_horiz_chips;
	    y = ((double) (currentHighlight1 / num_horiz_chips)) / num_vert_chips;
	    PaintIJthChip (o, GP_defRectangle (x, y, x + 1.0 / (double) num_horiz_chips,
					 y + 1.0 / (double) num_vert_chips),
			   currentHighlight1, FALSE);
	    x = ((double) (currentHighlight2 % num_horiz_chips)) / num_horiz_chips;
	    y = ((double) (currentHighlight2 / num_horiz_chips)) / num_vert_chips;
	    PaintIJthChip (o, GP_defRectangle (x, y, x + 1.0 / (double) num_horiz_chips,
					 y + 1.0 / (double) num_vert_chips),
			   currentHighlight2, TRUE);
	}
    } else {
	int currentHighlight;

	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangle (GP_defRectangle (0.0, 0.0, 1.0, 1.0));
	SUIT_borderObject (o);

	if (ISCOLORS (o)) {
	    GP_color temp;
	    temp = SUIT_getColor (o, CURRENT_VALUE);
	    currentHighlight = GP_getColorIndex (temp.colorName);
	} else {
	    currentHighlight = SUIT_getInteger (o, CURRENT_VALUE);

	    /* rob hack to fix ncd problem */
	    SRGP_setColor (0);
	    GP_fillRectangleCoord (0.0, 0.0, 1.0, 1.0);
	    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	}

	chip_num = 0;
	for (y = 0; y < num_vert_chips; y++)
	    for (x = 0; x < num_horiz_chips; x++) {
		i = (double) x / (double) num_horiz_chips;
		j = (double) y / (double) num_vert_chips;
		chip_num = y * num_horiz_chips + x;
		numColors = NumberOfAllocatedColors;
		if (ISCOLORS (o)) {
		    if (chip_num < numColors)
			PaintIJthChip (o, GP_defRectangle (i, j, i + 1.0 / num_horiz_chips,
							   j + 1.0 / num_vert_chips),
				       chip_num, chip_num == currentHighlight);
		} else {
		    PaintIJthChip (o, GP_defRectangle (i, j, i + 1.0 / num_horiz_chips,
						       j + 1.0 / num_vert_chips), chip_num,
				   chip_num == currentHighlight);
		}
	    }
    }
    OBJECT_OPTIMIZED (o) = TRUE;
}



PRIVATE void FindDimensions (SUIT_object o, int *vert, int *horiz)
{
    SUIT_viewport vp;
    double p_style1 = 8 / 5;
    double p_style2 = 10 / 4;
    double p_style3 = 20 / 2;
    double p_style4 = 40 / 1;
    double vp_ratio, cc_ratio, diff, leastDiff, numChips;
    int toolong, horizontal, validColors;
    int i, j, bestI, bestJ, big, small, maxFactor;
    int maxLength = 0;
    double c_style, c_stylePrev;

    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT (o));
    if (HORIZ (vp)) {
	*horiz = 2;
	*vert = 1;
	vp_ratio = (double) (vp.top_right.x - vp.bottom_left.x) /
	    (double) (vp.top_right.y - vp.bottom_left.y);
	horizontal = TRUE;
    } else {
	*horiz = 1;
	*vert = 2;
	vp_ratio = (double) (vp.top_right.y - vp.bottom_left.y) /
	    (double) (vp.top_right.x - vp.bottom_left.x);
	horizontal = FALSE;
    }

    if (ISCOLORS (o) && (GP_inquireCanvasDepth () == 1))
	return;

    if (ISCOLORS (o)) {
	validColors = SUIT_getInteger (o, VALID_COLORS);
	numChips = (double) validColors;

	toolong = TRUE;
	while (toolong) {
	    toolong = FALSE;
	    maxFactor = (int) sqrt (numChips);

	    for (i = maxFactor; i >= 1; i--) {
		if ((int) numChips % i == 0) {
		    maxLength = numChips / i;
		    break;
		}
	    }

	    if (maxLength > MAXLENGTH) {
		numChips = numChips + 1;
		toolong = TRUE;
	    } else
		toolong = FALSE;
	}

	leastDiff = 100.0;
	c_style = 100.0;
	bestI = 1;
	bestJ = numChips;

	for (i = 1; i <= maxFactor; i++) {
	    j = numChips / i;
	    c_stylePrev = c_style;
	    c_style = C_STYLE;

	    if ((((int) numChips) % i == 0) && (vp_ratio < c_stylePrev)) {
		cc_ratio = (double) i / (double) j;
		diff = vp_ratio - cc_ratio;

		if (diff < leastDiff) {
		    leastDiff = diff;
		    bestI = i;
		    bestJ = j;
		}
	    }
	}

	if (bestI > bestJ) {
	    big = bestI;
	    small = bestJ;
	} else {
	    big = bestJ;
	    small = bestI;
	}

	if (horizontal) {
	    *vert = small;
	    *horiz = big;
	} else {
	    *vert = big;
	    *horiz = small;
	}
    } else {
	if (vp_ratio > p_style4) {
	    *vert = 1;
	    *horiz = 40;
	} else if (vp_ratio > p_style3)
	    if (vp_ratio - p_style3 < p_style4 - vp_ratio)
		if (HORIZ (vp)) {
		    *vert = 2;
		    *horiz = 20;
		} else {
		    *vert = 20;
		    *horiz = 2;
		}
	    else {
		*vert = 1;
		*horiz = 40;
	    }
	else if (vp_ratio > p_style2)
	    if (vp_ratio - p_style2 < p_style3 - vp_ratio)
		if (HORIZ (vp)) {
		    *vert = 4;
		    *horiz = 10;
		} else {
		    *horiz = 10;
		    *vert = 4;
		}
	    else if (HORIZ (vp)) {
		*vert = 2;
		*horiz = 20;
	    } else {
		*vert = 20;
		*horiz = 2;
	    }
	else if (vp_ratio - p_style1 < p_style2 - vp_ratio)
	    if (HORIZ (vp)) {
		*vert = 4;
		*horiz = 10;
	    } else {
		*vert = 10;
		*horiz = 4;
	    }
	else if (HORIZ (vp)) {
	    *vert = 5;
	    *horiz = 8;
	} else {
	    *vert = 8;
	    *horiz = 5;
	}
    }
}



PRIVATE void HitParquet (SUIT_object o, SUIT_event e)
{
    float x = e.worldLocation.x, y = e.worldLocation.y;
    int chipNmbr, numAllocated;
    int vert = 0, horiz = 0;

    FindDimensions (o, &vert, &horiz);

    chipNmbr = (int) ((x * horiz) + ((int) (y * vert)) * horiz);
    if (ISCOLORS (o)) {
	numAllocated = NumberOfAllocatedColors;
    } else {
	numAllocated = GP_numPatternsAllocated ();
    }
    if (chipNmbr >= numAllocated)
	return;
    NewSelection (o, chipNmbr);
}



PRIVATE void PaintParquet (SUIT_object o)
{
    int vert = 0, horiz = 0;

    SUIT_suspendMarkingRedisplay (o);
    if (ISCOLORS (o))
	SUIT_setInteger (o, VALID_COLORS, NumberOfAllocatedColors);
    SUIT_resumeMarkingRedisplay (o);

    FindDimensions (o, &vert, &horiz);
    PaintWithDimensions (o, vert, horiz);
}



PRIVATE SUIT_object CreateChips (char *name, char *class, void (*callback)(SUIT_object))
{
    SUIT_object newChips;
    boolean firsttime = TRUE;

    newChips = SUIT_createObject (name, class);
    SUIT_addDisplayToObject (newChips, "standard", HitChips, PaintChips);
    SUIT_addDisplayToObject (newChips, "parquet", HitParquet, PaintParquet);
    SUIT_setFunctionPointer (newChips, CALLBACK_FUNCTION, (SUIT_functionPointer) callback);
    if (firsttime) {
	SUIT_deluxeSetDouble (newChips, CHIP_BORDER, 0.08, CLASS);
	SUIT_deluxeSetColor (newChips, HIGHLIGHT_COLOR, GP_defColor ("black", BLACK_ON_MONO), CLASS);
	SUIT_deluxeSetBoolean (newChips, HAS_BACKGROUND, FALSE, CLASS);
	SUIT_registerInterest (newChips, OptimizePaint);
	firsttime = FALSE;
    }
    SUIT_changeObjectSize (newChips, 100, 100);
    return (newChips);

}



SUIT_object SUIT_createColorChips (char *name, void (*callback)(SUIT_object))
{
    NumberOfAllocatedColors = GP_numColorsAllocated ();
    GP_registerForColorAllocation (ChangeNumColors);
    return (CreateChips (name, "color chips", callback));
}



SUIT_object SUIT_createPatternChips (char *name, void (*callback)(SUIT_object))
{
    return (CreateChips (name, "pattern chips", callback));
}
