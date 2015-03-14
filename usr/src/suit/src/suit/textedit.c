/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

/* Text Editor Widget */
/*
  NOTE FOR ALTERING KEY SEQUENCES:  The key sequences used to perform special
  functions in the text editor can be changed by changing the appropriate 
  property to a string that denotes the new key sequence.  'C' or '^X' is used
  to denote the control key, 'M' or 'ESC' is used to denote the escape key. A
  key sequence can be a control or escape, a dash '-', then a printable 
  character, or a sequence of escapes or control-x's or control-c's then a 
  control or escape then a printable character. (Ex: "C-cC-xC-y" is valid, 
  "C-yC-c" is not.  The later will clear the key sequence after trying to match
  "C-y" with its function).  The key sequence is stored in the property 
  INPUT_SEQUENCE.
  NOTE: If a match is found, the key sequence being entered is cleared, i.e. a
  sequence "C-x" will cause another sequence beginning with "C-x" to never be
  recognized.
  
  NOTE FOR INPUT SEQUENCE:  The input sequence string should either contain
  one character to be printed or a command (control or escape key combination).
  */

#include "privsuit.h"
#include <ctype.h>

/* In house properties: */
#define CURSOR_X "cursor x"
#define CURSOR_Y "cursor y"
#define LAST_MARK_INDEX "last mark index"
#define LAST_CLICK_TIME "last click time"
#define APPEND_TO_BUFFER "append to buffer"
#define PAINT_BLOCK_ONLY "paint block only"
#define PRE_CURSOR_INDEX "pre cursor index"
#define CALCULATE_LINES "calculate lines"
#define HAS_A_TAB "has a tab"
#define START_X "start x"
#define START_Y "start y"
#define DOUBLE_CLICK_TIME "double click time"


#define BACKSPACE           '\b'
#define DELETE              127
#define END_OF_LINE         '\n'
#define END_OF_TEXT         '\0'
#define ESCAPE              27
#define TAB                 '\t'
#define CNTL_C              3
#define CNTL_X              24
/*
  LONGEST_LINE is as far as the cursor can go (in characters) on a line.  This 
  limitation rises from storing the current cursor position relative to the 
  beginning of the line.
*/
#define LONGEST_LINE        2200
char *contr = "C-";         /* can only be 2 characters long */
char *contr2 = "^X-";
char *esc = "M-";           /* can only be 2 characters long */
char *esc2 = "ESC-";
/*
  contr2 and esc2 string are provided for convenience.  They can be used in
  the SUIT_sendToEditor command in place of the contr or esc strings.  Ex:
  Both "C-a" and "^X-a" map to the same command (control-'a').
*/

/* This is the difference between a text editor (multiple) and a type-in box
   (one). */
enum linesDisplayed {
    multiple, one
    };

char *CursorStyles[] = { "i-beam", "vertical bar" };

/* These variables are used to prevent unnecessary interest computation while in
   the hit proc. */
static NotInTextEditorHitProc = TRUE;
static CalculateBlock = TRUE;

PRIVATE void TE_OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    int markBegin, markEnd;

    if (CalculateBlock) {
	/* Someone else (other than the hit proc) has changed the current value.
	   A check for CURSOR_INDEX change is hidden until the CalculateBlock
	   flag is set to TRUE in the hit proc. */

	if (SUIT_stringsMatch (propName, CURRENT_VALUE)) {
	    /* 
	      Someone else has altered the CURRENT_VALUE text.  We need to 
	      turn off highlighting, set optimize paint to false, move the
	      cursor to the end of the new text if its position is farther
	      than the string, and we need to check to see if the string has
	      a tab, and set that flag appropriately.
	    */
	    
	    char *newText;
	    int newTextLen;

	    SUIT_deluxeSetBoolean(o, CALCULATE_LINES, TRUE, OBJECT);
	    newText = (char *) new;
	    newTextLen = strlen(newText);
	    SUIT_deluxeSetBoolean(o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
	    o->optimized_paint = FALSE;
	    if (newTextLen < SUIT_getInteger(o, CURSOR_INDEX))
	      SUIT_deluxeSetInteger(o, CURSOR_INDEX, newTextLen, OBJECT);
	    if (strchr(newText, TAB) == NULL)
	      SUIT_deluxeSetBoolean(o, HAS_A_TAB, FALSE, OBJECT);
	} else if (SUIT_stringsMatch(propName, CURSOR_INDEX)) {
	    /*
	      We need to set the PRE_CURSOR_INDEX value so that the paint proc
	      can clear the cursor, and, if there was highlighted text,  we 
	      need to set the MARK_INDEX and LAST_MARK_INDEX values so that the
	      paint proc only has to paint the text that used to be highlighted,
	      and we need to turn highlighting off.
	    */
	    int pos;
	    if (old != NULL)
	      pos = *((int *) old);
	    else pos = 0;
	    SUIT_deluxeSetInteger(o, PRE_CURSOR_INDEX, pos-1, OBJECT);
	    if (SUIT_getBoolean(o, HIGHLIGHT_BLOCK)) {
		markBegin = SUIT_getInteger(o, MARK_INDEX);
		markEnd = SUIT_getInteger(o, MARK_END_INDEX);
		SUIT_deluxeSetInteger(o, LAST_MARK_INDEX, markBegin, OBJECT);
		SUIT_deluxeSetInteger(o, MARK_INDEX, markEnd, OBJECT);
		SUIT_deluxeSetBoolean(o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
	    }
	}
    }
    if (NotInTextEditorHitProc) {
	if (!(SUIT_stringsMatch (propName, CURSOR_X) ||
	      SUIT_stringsMatch (propName, CURSOR_Y) ||
	      SUIT_stringsMatch (propName, CURSOR_INDEX) ||
	      SUIT_stringsMatch (propName, INPUT_SEQUENCE) ||
	      SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	      SUIT_stringsMatch (propName, CALCULATE_LINES) ||
	      SUIT_stringsMatch (propName, MARK_END_INDEX) ||
	      SUIT_stringsMatch (propName, PRE_CURSOR_INDEX) ||
	      SUIT_stringsMatch (propName, LAST_MARK_INDEX) ||
	      SUIT_stringsMatch (propName, MARK_INDEX) ||
	      SUIT_stringsMatch (propName, HIGHLIGHT_BLOCK) ||
	      SUIT_stringsMatch (propName, LAST_CLICK_TIME) ||
	      SUIT_stringsMatch (propName, APPEND_TO_BUFFER) ||
	      SUIT_stringsMatch (propName, PAINT_BLOCK_ONLY) ||
	      SUIT_stringsMatch (propName, NUMBER_OF_LINES) ||
	      SUIT_stringsMatch (propName, ALTERED) ||
	      SUIT_stringsMatch (propName, CUT_BUFFER))) {
	    o->optimized_paint = FALSE;
	    if (SUIT_stringsMatch (propName, VIEWPORT)) {

		/* Align The top line to the top of the text window if necessary
		   so that there is no dead space above the first line. */

		SUIT_viewport vp;
		float height;

		vp = OBJECT_VIEWPORT(o);
		height = vp.top_right.y - vp.bottom_left.y;

		if (height > SUIT_getDouble(o, START_Y))
		  SUIT_deluxeSetDouble (o, START_Y, 
					(float) (height
					      -SUIT_deluxeGetInteger(NULL,MARGIN,
								     GLOBAL)), 
					OBJECT);
	    }
	}
    }
}

PRIVATE void ShrinkToFit (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{

    if (NotInTextEditorHitProc) {
	if ((SUIT_stringsMatch (propName, VIEWPORT) 
	     || SUIT_stringsMatch (propName, SHRINK_TO_FIT) 
	     || SUIT_stringsMatch (propName, FONT)) &&
	    SUIT_getBoolean (o, SHRINK_TO_FIT)) {
	    int w, a, d, border;
	    SUIT_viewport vp;

	    vp = SUIT_getViewport (o, VIEWPORT);
	    GP_setFont (SUIT_getFont (o, FONT));
	    SRGP_inquireTextExtent (SUIT_getText (o, CURRENT_VALUE), &w, &a, &d);
	    w = vp.top_right.x-vp.bottom_left.x;
	    border = SUIT_getInteger (o, MARGIN);
	    SUIT_changeObjectSize (o, w, a+d+2*border);
	}
    }
}


PRIVATE void TE_drawVerticalBarCursor (SUIT_object o, int x, int y, int ascent)
{
    GP_pushGraphicsState();
    GP_setLineWidth (1);
    GP_setColor (SUIT_getColor (o, CURSOR_COLOR));
    SRGP_lineCoord (x, y-2, x, y+ascent);
    GP_popGraphicsState();
}


PRIVATE void TE_drawIBeamCursor (SUIT_object o, int x, int y, int ascent)
{
    GP_pushGraphicsState();
    GP_setLineWidth (1);
    GP_setColor (SUIT_getColor (o, CURSOR_COLOR));
    SRGP_lineCoord (x-3, y-2, x, y); 
    SRGP_lineCoord (x+3, y-2, x, y);
    SRGP_lineCoord (x, y, x, y+ascent-2);
    SRGP_lineCoord (x, y+ascent-2, x-3, y+ascent);
    SRGP_lineCoord (x, y+ascent-2, x+3, y+ascent);
    GP_popGraphicsState();
}


PRIVATE void TE_reverseDisplay (SUIT_object o)
{
    static boolean in_foreground = TRUE;
    
    in_foreground = !in_foreground;
    if (in_foreground)
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    else GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
}


PRIVATE void TE_clearRectangle (SUIT_object o, rectangle rect)
{
    TE_reverseDisplay(o);
    SRGP_fillRectangle (rect);
    TE_reverseDisplay(o);
}


PRIVATE void TE_clearBackground (SUIT_object o)
{
    TE_clearRectangle (o, OBJECT_VIEWPORT(o));
    if (SUIT_getBoolean (o, HAS_BORDER))
        SUIT_borderObject (o);
}


PRIVATE int findLineEnd(char *text, int pos)
{
    int lineEnd;
    
    lineEnd = pos;
    while ((text[lineEnd] != END_OF_LINE) && (text[lineEnd] != END_OF_TEXT))
        lineEnd++;
    return lineEnd;
}


PRIVATE int findLinePos(char *text, int pos)
{
    int linePos;
    
    if (pos > 0) {
	linePos = pos -1;
	while ((text[linePos] != END_OF_LINE) && (linePos > 0))
	    linePos--;
	if (text[linePos] == END_OF_LINE)
	    linePos++;
    }
    else linePos = 0;
    return linePos;
}


PRIVATE int getNumLines(register char *text)
{
    register int i;
    register int retval = 1;
    
    for (i=0; text[i] != '\0'; i++)
        if (text[i] == '\n')
	    retval++;
    
    return retval;
}


PRIVATE char *fillTabs(SUIT_object o, char *inText, int pos, int numChars)
{
    char *temp, *retTemp;
    int numTabs, i, j, k, tempI;

    numTabs = 0;
    retTemp = SUIT_malloc(numChars+1);
    temp = NULL;
    for (i = 0; i < numChars; i++)
      if ((retTemp[i] = inText[i+pos]) == TAB)
	temp = retTemp;
    retTemp[numChars] = END_OF_TEXT;

    if (temp != NULL) {
	int tabLen = SUIT_getInteger(o, TAB_LENGTH);
	int numSpaces;

	SUIT_deluxeSetBoolean(o, HAS_A_TAB, TRUE, OBJECT);
	if (tabLen < 0)
	  tabLen = 0;
	temp = strchr(temp, TAB);
	do {
	    numTabs++;
	    temp = strchr (temp+1,TAB);
	} while (temp != NULL);
	SUIT_free(retTemp);
	retTemp = SUIT_malloc(numChars + 
			      (numTabs * tabLen) + 1);
	i = 0;
	j = 0;
	tempI = 0;
	while ((i < (pos + numChars)) && (inText[i] != END_OF_TEXT)) {
	    if (inText[i] == TAB) {
		if (tabLen > 0)
		  numSpaces = (tabLen - (j % tabLen));
		else numSpaces = 0;
		j+=numSpaces;
		if (i>=pos) 
		  for (k=0; k < numSpaces ; k++) {
		      retTemp[tempI] = ' ';
		      tempI++;
		  }
	    }
	    else {
		j++;
		if (i>=pos) {
		    retTemp[tempI] = inText[i];
		    tempI++;
		}
	    }
	    if (inText[i] == END_OF_LINE)
	      j = 0;
	    i++;
	}
	retTemp[tempI] = END_OF_TEXT;
    }
    return retTemp;
}


PRIVATE void DrawTextEditor (SUIT_object o, enum linesDisplayed lines)
{
    double y, cursor_y, cursor_h;
    int x, cursor_pos, gap, border;
    int width, ascent, descent, cursor_x, pos, indent;
    char *text, *line, *temp;
    int w, a, d, linePos, lineEnd, numChars, realLineEnd;
    int lineClearEnd;
    boolean screenChanged;
    int numLines = 0;
    int markBegin = 0;
    int markEnd = 0;
    int markLast = 0;
    int preCursor;
    boolean printCursor = FALSE;
    boolean printScreen = FALSE;
    int printing = 0;
    boolean highlighting = FALSE;
    rectangle screen;
    int X1, Y1, X2, Y2, objWidth, objHeight;
    char savedChar;
    boolean highlightBlock = SUIT_getBoolean(o, HIGHLIGHT_BLOCK);
    
    screen = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT(o));
    X1 = screen.bottom_left.x;
    Y1 = screen.bottom_left.y;
    X2 = screen.top_right.x;
    Y2 = screen.top_right.y;
    objWidth = screen.top_right.x - screen.bottom_left.x;
    objHeight = screen.top_right.y - screen.bottom_left.y;

    GP_pushGraphicsState ();
    
    SUIT_suspendMarkingRedisplay(o);

    text = SUIT_getText (o, CURRENT_VALUE);
    SRGP_inquireTextExtent ("Ay", &w, &a, &d);
    gap = SUIT_getInteger (o, SPACING_GAP);
    border = SUIT_getInteger (o, MARGIN);
    if (objHeight < (2 * border))
	SUIT_changeObjectSize (o, objWidth, 2*border);
    if (objWidth < (2 * border))
        SUIT_changeObjectSize (o, 2*border, objHeight);
    
    cursor_pos = SUIT_getInteger (o, CURSOR_INDEX);
    if (cursor_pos < 0)
        cursor_pos = 0;
    preCursor = SUIT_getInteger(o, PRE_CURSOR_INDEX);
    
    if (lines == multiple)
        linePos = findLinePos (text, cursor_pos);
    else linePos = 0;
    
    if (!SUIT_getBoolean(o, HAS_A_TAB)) {
	savedChar = text[cursor_pos];
	text[cursor_pos] = END_OF_TEXT;
	SRGP_inquireTextExtent (text+linePos, &width, &ascent, &descent);
	text[cursor_pos] = savedChar;
    } else {
	line = fillTabs (o, text, linePos, (cursor_pos - linePos));
	SRGP_inquireTextExtent (line, &width, &ascent, &descent);
	SUIT_free (line);
    }
    x = SUIT_getInteger (o, START_X);
    cursor_x = x + width;
    
    if (cursor_x < border) { /* fell off the left edge */
	x += border-cursor_x;
	SUIT_deluxeSetInteger (o, START_X, x, OBJECT);
    }
    else if (cursor_x > objWidth-border) { /* fell off the right edge */
	x -= cursor_x-(objWidth-border);
	SUIT_deluxeSetInteger (o, START_X, x, OBJECT);
    }
    cursor_x = x + width;
    
    if (lines == multiple) {
        y = SUIT_getDouble (o, START_Y);
	pos = findLinePos(text,cursor_pos);
	cursor_h = (double) (a + d);
	while (pos > 0) {
	    cursor_h = cursor_h + (double) (a + d + gap);
	    pos = findLinePos (text, pos-1);
	}
	cursor_y = y - cursor_h;
	if (cursor_y < (double) (border + d)) { /*fell off the bottom*/
	    y+= (double) (border + d) - cursor_y;
	    SUIT_deluxeSetDouble (o, START_Y, y, OBJECT);
	}
	cursor_y = y - cursor_h;
	if (cursor_y > (double) (objHeight - border - (a + d))) {/*fell off the top*/
	    y-= cursor_y - (double) (objHeight - border - (a + d));
	    SUIT_deluxeSetDouble (o, START_Y, y, OBJECT);
	}
	cursor_y = y - cursor_h;
	y = y - (double) (a + d);
    } else {
	y = (double) (objHeight - ascent - border);
	cursor_y = y;
    }
    
    screenChanged = (!o->optimized_paint 
		     || !SUIT_getBoolean(o,PAINT_BLOCK_ONLY));
    if (!SUIT_getBoolean(o, PAINT_BLOCK_ONLY)
	&& SUIT_getBoolean(o, HAS_BACKGROUND))
	TE_clearBackground(o);
    
    if ((objWidth < (2*border))
	|| (objHeight < (2*border)))
	GP_setClipRectangle (SRGP_defRectangle (X1+border, Y1+border,
						X1+border,
						Y1+border));
    else GP_setClipRectangle (SRGP_defRectangle (X1+border, Y1+border,
						 X1+objWidth-border,
						 Y1+objHeight-border));
    
    if ((lines == multiple) && (SUIT_getBoolean(o, HAS_BACKGROUND)))
	TE_clearRectangle (o, SRGP_defRectangle(X1, (int) (Y1 + y + a), X2,
						(int) (Y1 + y + a + a + d)));
    
    temp = SUIT_createSafeString(text);
    pos = 0;
    
    markLast = SUIT_getInteger(o, LAST_MARK_INDEX);
    markBegin = SUIT_getInteger(o, MARK_INDEX);
    markEnd = SUIT_getInteger(o,MARK_END_INDEX);
    
    if (lines == multiple) {
	lineEnd = findLineEnd(temp, pos);
	while ((temp[lineEnd] != END_OF_TEXT) && (y > objHeight)) {
	    pos = lineEnd + 1;
	    lineEnd = findLineEnd(temp, pos);
	    y=y - (double) (a+d+gap);
	    numLines++;
	}
	
	if (pos > 0) {
	    if (pos >= markLast)
		printing++;
	    if (pos >= markBegin)
		printing++;
	    if (pos >= markEnd)
		printing++;
	    if (printing == 3)
		printing = 0;
	    
	    if (highlightBlock) {
		if (pos >= markEnd) {
		    highlighting = !highlighting;
		    TE_reverseDisplay(o);
		}
		if (pos >= markBegin) {
		    highlighting = !highlighting;
		    TE_reverseDisplay(o);
		}
	    }
	}
    }    
    
    /* begin displaying lines */
    indent = 0;
    do {
	if (lines == multiple) {
	    if ((!printScreen && y <= (double) objHeight))
		printScreen = TRUE;
	    realLineEnd = findLineEnd(temp, pos);
	} else {
	    printScreen = TRUE;
	    realLineEnd = strlen(temp);
	}
	lineEnd = realLineEnd;

	if (!((markBegin == markEnd) && (markLast == markEnd))) {
	    /* if a block has been defined and needs to be updated */
	    if ((markLast > pos) && (markLast <= lineEnd))
	      lineEnd = markLast-1;
	    if ((markBegin > pos) && (markBegin <= lineEnd)
		&& (markLast != markBegin))
	      lineEnd = markBegin-1;
	    if ((markEnd > pos) && (markEnd <= lineEnd))
	      lineEnd = markEnd-1;
	}
	  
	if (pos >= (preCursor+2))
	    printCursor = FALSE;
	else if (pos >= preCursor) {
	    printCursor = TRUE;
	    if (lineEnd > (preCursor+2)) {
		lineEnd = preCursor + 2;
	    } else if ((markBegin == (lineEnd+1)) && (markLast == (lineEnd+1))
			&& (markEnd > markBegin) && (markEnd <= (realLineEnd+1))
		       && (!highlightBlock)) {
		/*
		   This is the special case that the cursor has only moved
		   forward one character without highlighting anything.
		   If this wasn't here, the character before and after the 
		   cursor would be printed separately.
		*/
		lineEnd = markEnd - 1;
		printing += 2;  /* We have passed markBegin and markLast without
				   noting them in the usual way, below */
	    }
	} else if (lineEnd >= preCursor) {
	    lineEnd = preCursor - 1;	
	}
	
	if (pos == markLast)
	    printing++;
	if (pos == markBegin)
	    printing++;
	if (pos == markEnd)
	    printing++;
	if (printing == 3)
	    printing = 0;
	
	if (highlightBlock) {
	    if (pos == markEnd) {
		highlighting = !highlighting;
		TE_reverseDisplay(o);
	    }
	    if (pos == markBegin) {
		highlighting = !highlighting;
		TE_reverseDisplay(o);
	    }
	}
	if (printScreen && (screenChanged || (printing>0) || (printCursor) ||
			    !(((temp[lineEnd] == END_OF_LINE)
			       && (lines == multiple)) ||
			      (temp[lineEnd] == END_OF_TEXT)))) {
	    int lineLen;

	    if (lines == multiple)
		linePos = findLinePos (temp,pos);
	    else linePos = 0;
	    numChars = lineEnd-linePos + 1;
	    if (numChars > strlen(temp+linePos))
	      numChars = strlen(temp+linePos);
	    lineLen = (numChars-(pos-linePos));
	    line = fillTabs (o, temp, pos, lineLen);
	    if (line[lineLen - 1] == END_OF_LINE) 
	      line[lineLen - 1] = END_OF_TEXT;
	    SRGP_inquireTextExtent (line, &width, &ascent, &descent);
	    if (screenChanged || (printing>0) || (printCursor)) {
		if (((temp[lineEnd] == END_OF_LINE) &&
		     (lines == multiple)) ||
		    (temp[lineEnd] == END_OF_TEXT))
		  lineClearEnd = X2;
		else lineClearEnd = X1 + x + indent + width - 1;
		if ((temp[lineEnd] == END_OF_TEXT)
		    && SUIT_getBoolean(o, HAS_BACKGROUND)
		    && (lines == multiple))
		  TE_clearRectangle (o, SRGP_defRectangle(X1, Y1, X2,
						   (int) (Y1 + y-d-gap)));
		
		if (SUIT_getBoolean(o, HAS_BACKGROUND))
		  if (lines == multiple)
		    TE_clearRectangle (o, SRGP_defRectangle(X1 + x + indent,
						     (int) (Y1 + y-d-gap+1),
							    lineClearEnd,
						     (int) (Y1 + y + a)));
		  else TE_clearRectangle (o, SRGP_defRectangle(X1 + x + indent,
							       Y1, lineClearEnd,
							       Y2));
		SRGP_text (SRGP_defPoint(X1 + x + indent, (int) (Y1 + y)),
			   line);
	    }
	    SUIT_free(line);
	}
	pos = lineEnd + 1;
	if ((lines == multiple) && 
	    ((temp[lineEnd] == END_OF_LINE) ||
	     (temp[lineEnd] == END_OF_TEXT))) {
	    y=y - (double) (a+d+gap);
	    numLines++;
	    indent = 0;
	}
	else indent = indent + width;
    } while ((temp[lineEnd] != END_OF_TEXT) && 
	     ((lines != multiple) || (y >= (double) (-(a+d)))));
    
    if (SUIT_getBoolean(o, CALCULATE_LINES)) {
	if ((temp[lineEnd] != END_OF_TEXT) && (lines == multiple))
	    numLines += getNumLines(temp+pos);
	else if (lines != multiple)
	    numLines = 1;
	SUIT_deluxeSetInteger(o, NUMBER_OF_LINES, numLines, OBJECT);
	SUIT_deluxeSetBoolean(o, CALCULATE_LINES, FALSE, OBJECT);
    }
    SUIT_free(temp);
    if (highlighting)
	TE_reverseDisplay(o);
    
    if (SUIT_stringsMatch (SUIT_getEnumString (o, CURSOR_STYLE), "i-beam"))
	TE_drawIBeamCursor (o, X1+cursor_x, (int) (Y1+cursor_y) , a);
    else
	TE_drawVerticalBarCursor (o, X1+cursor_x, (int) (Y1+cursor_y) , a);
    
    SUIT_deluxeSetInteger(o,CURSOR_X,cursor_x, OBJECT);
    SUIT_deluxeSetInteger(o,CURSOR_Y,(int) cursor_y, OBJECT);
    
    SUIT_deluxeSetInteger(o,LAST_MARK_INDEX, markBegin, OBJECT);
    
    o->optimized_paint = TRUE;
    SUIT_resumeMarkingRedisplay(o);
    
    GP_popGraphicsState ();
}


PRIVATE int TE_clickPosition (SUIT_object o, int where_x, int where_y, enum linesDisplayed lines)
{
    char *text = SUIT_getText (o, CURRENT_VALUE), *temp, *line;
    int guess_x, guess_y, x = SUIT_getInteger (o, START_X);
    double y = SUIT_getDouble (o, START_Y);
    int width, ascent, descent, oldwidth, linePos, lineEnd, gap;
    int w, a, d;
    GP_font fontVal;

    if (strlen(text) == 0)
        return 0;
    /* else */
    fontVal = SUIT_getFont (o, FONT);
    GP_setFont(fontVal);
    SRGP_inquireTextExtent("Ay", &w, &a, &d);
    if (lines == multiple) {
        gap = SUIT_getInteger (o, SPACING_GAP);
	guess_y = (y - where_y)/(a+d+gap);
	lineEnd = findLineEnd (text, 0);
	while ((guess_y > 0) && (text[lineEnd] != END_OF_TEXT)) {
	    guess_y--;
	    lineEnd = findLineEnd (text, lineEnd + 1);
	}
	linePos = findLinePos (text, lineEnd);

	temp = SUIT_createSafeString (text+linePos);
	temp[lineEnd-linePos] = END_OF_TEXT;
	line = fillTabs (o, temp, 0, lineEnd-linePos);
	SRGP_inquireTextExtent (line, &width, &ascent, &descent);
	SUIT_free(line);
	if (where_x >= width+x) {
	    SUIT_free(temp);
	    return lineEnd;
	}
	if (where_x < x) {
	    SUIT_free(temp);
	    return linePos;
	}	    
	
        /* else */
    }
    else {
        linePos = 0;
        lineEnd = strlen (text);
	temp = SUIT_createSafeString (text);
    }
    
    guess_x = 2 * ((where_x - x)/w);
    if (guess_x < 0)
	return linePos;
    else if((guess_x + linePos) > lineEnd)
        guess_x = lineEnd;
    temp[guess_x] = END_OF_TEXT;
    line = fillTabs (o, temp, 0, guess_x);
    if (strlen(line) > LONGEST_LINE) {
        SUIT_inform("Sorry, you cannot move the cursor to that position");
	guess_x = LONGEST_LINE;
	SUIT_free(line);
    }
    else {
        SRGP_inquireTextExtent (line, &width, &ascent, &descent);
	SUIT_free(line);
	oldwidth = width;
	if (x+width < where_x) {
            while (x+width < where_x && guess_x < (lineEnd-linePos)) {
	        oldwidth = width;
		strcpy (temp, text+linePos);
		temp[++guess_x] = END_OF_TEXT;
		line = fillTabs (o, temp, 0, guess_x);
		SRGP_inquireTextExtent (line, &width, &ascent, &descent);
		SUIT_free(line);
	    }
	    if (ABS(x+oldwidth-where_x) < ABS(x+width-where_x))
		guess_x--;
	}
	else if (x+width > where_x) {
	    while (x+width > where_x && guess_x != 0) {
	        oldwidth = width;
		temp[--guess_x] = END_OF_TEXT;
		line = fillTabs (o, temp, 0, guess_x);
		SRGP_inquireTextExtent (line, &width, &ascent, &descent);
		SUIT_free(line);
	    }
	    if (ABS(x+oldwidth-where_x) < ABS(x+width-where_x))
		guess_x++;
	}
    }
    SUIT_free (temp);
    return linePos + guess_x;
}


PRIVATE int scrollUp(SUIT_object o)
{
    SUIT_viewport vp;
    int height;
    int pos, y, w, a, d;
    
    vp = OBJECT_VIEWPORT(o);
    height = vp.top_right.y - vp.bottom_left.y;
    pos = TE_clickPosition (o, SUIT_getInteger (o, MARGIN), SUIT_getInteger (o, MARGIN), multiple);
    y = (int) SUIT_getDouble (o, START_Y);
    
    SRGP_inquireTextExtent ("Ay", &w, &a, &d);
    y = y + (height - (2*SUIT_getInteger (o, MARGIN))) - (a + d);
    SUIT_deluxeSetDouble (o, START_Y, (double)y, OBJECT);
    
    return pos;
}


PRIVATE int scrollDown(SUIT_object o)
{
    SUIT_viewport vp;
    int height, pos, y, w, a, d;
    
    vp = OBJECT_VIEWPORT(o);
    height = vp.top_right.y - vp.bottom_left.y;
    pos = TE_clickPosition (o, SUIT_getInteger (o, MARGIN),
			    height - SUIT_getInteger (o, MARGIN),
			    multiple);
    y = (int) SUIT_getDouble (o, START_Y);
    
    SRGP_inquireTextExtent("Ay", &w, &a, &d);
    y = y - (height - (2*SUIT_getInteger (o, MARGIN))) + (a + d);
    if (y < (double) (height - SUIT_getInteger (o, MARGIN)))
        y = (height - SUIT_getInteger (o, MARGIN));
    SUIT_deluxeSetDouble (o, START_Y, (double)y, OBJECT);
    return pos;
}


PRIVATE void translateMacro(char *macro)
{
    int i, j;
    char *next;
    
    i = 0;
    j = 0;
    next = macro;
    while (next = strstr(next,esc2)) {
	for (; j < (next - macro); i++, j++)
	    macro[i] = macro[j];	
	macro[i++] = esc[0];
	macro[i++] = esc[1];
	j = j + strlen(esc2);
	next = macro + j;
    }
    for (; macro[j] != '\0'; i++, j++)
	macro[i] = macro[j];
    macro[i] = macro[j];
    i = 0;
    j = 0;
    next = macro;
    while (next = strstr(next,contr2)) {
	for (; j < (next - macro); i++, j++)
	    macro[i] = macro[j];
	macro[i++] = contr[0];
	macro[i++] = contr[1];
	j = j + strlen(contr2);
	next = macro + j;
    }
    for (; macro[j] != '\0'; i++, j++)
	macro[i] = macro[j];
    macro[i] = macro[j];
}


PRIVATE boolean inputSequenceMatch(SUIT_object o, char *command)
{
    boolean returnValue;
    char *inputStr = SUIT_createSafeString(command);
    
    translateMacro(inputStr);
    returnValue = SUIT_stringsMatch(inputStr, SUIT_getText (o, INPUT_SEQUENCE));
    SUIT_free(inputStr);
    return returnValue;
}


void SUIT_sendToEditor(SUIT_object o, char *command)
{
    int i;
    SUIT_event ev;
    char *inputStr = SUIT_createSafeString(command);
    
    translateMacro(inputStr);
    ev.type = KEYSTROKE;
    for (i=0; i<strlen(inputStr); i++) {
	ev.keyboard = inputStr[i];
	if ((inputStr[i] == contr[0])
	    || (inputStr[i] == esc[0]))
	    if (inputStr[i+1] == contr[1])
		if (inputStr[i] == esc[0]) {
		    i += 1;
		    ev.keyboard = ESCAPE;
		} else {
		    if (isupper(inputStr[i+2]))
			ev.keyboard = inputStr[i+2] - 'A' + 1;
		    else ev.keyboard = inputStr[i+2] - 'a' + 1;
		    i += 2;
		}
	SUIT_hitObject(o, ev);
    }
    SUIT_free(inputStr);
    o->optimized_paint = FALSE;
}


PRIVATE char *TE_getBuffer (SUIT_object o)
{
    char *bufferText;
    
    bufferText = SUIT_createSafeString (SUIT_getText(o, CUT_BUFFER));
    
    return bufferText;
}


PRIVATE void TE_setBuffer (SUIT_object o, char *buffer)
{
    SUIT_deluxeSetText (o, CUT_BUFFER, buffer, OBJECT);
}


PRIVATE void TE_clearBuffer (SUIT_object o)
{
    SUIT_deluxeSetText (o, CUT_BUFFER, "", OBJECT);
}

PRIVATE void highlightText(SUIT_object o, int beginPos, int endPos)
{
    int lastMarkIndex = SUIT_getInteger(o, LAST_MARK_INDEX);
    int temp;
    int assigns = 0;
    int startBlock = SUIT_getInteger(o, MARK_INDEX);
    int endBlock = SUIT_getInteger(o, MARK_END_INDEX);

    if (beginPos > endPos) {
	temp = endPos;
	endPos = beginPos;
	beginPos = temp;
    }
    if (SUIT_getBoolean(o, HIGHLIGHT_BLOCK)) {
	if (startBlock > endBlock) {
	    temp = endBlock;
	    endBlock = startBlock;
	    startBlock = temp;
	}
	if (endPos < endBlock) {
	    lastMarkIndex = endBlock;
	    assigns++;
	}
	if (beginPos > startBlock) {
	    lastMarkIndex = startBlock;
	    assigns++;
	}
	if ((assigns == 2) && (SUIT_getBoolean(o, HIGHLIGHT_BLOCK)))
	  SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, FALSE, OBJECT);
    } else if ((beginPos > lastMarkIndex) && (startBlock < lastMarkIndex))
      lastMarkIndex = startBlock;
    else if ((beginPos < lastMarkIndex) && (endBlock > lastMarkIndex))
      lastMarkIndex = endBlock;
    
    SUIT_deluxeSetInteger(o, MARK_INDEX, beginPos, OBJECT);
    SUIT_deluxeSetInteger(o, MARK_END_INDEX, endPos, OBJECT);
    SUIT_deluxeSetInteger(o, LAST_MARK_INDEX, lastMarkIndex, OBJECT);
    SUIT_deluxeSetBoolean(o, HIGHLIGHT_BLOCK, TRUE, OBJECT);
}    


void SUIT_highlightBlockInTextEditor(SUIT_object o, int beginPos, int endPos)
{
    char *text = SUIT_getText(o, CURRENT_VALUE);
    int textLen = strlen(text);
    char *insertText;
    
    if (beginPos > textLen)
	beginPos = textLen;
    if (endPos > textLen)
	endPos = textLen;
    if (beginPos < 0)
	beginPos = 0;
    if (endPos < 0)
	endPos = 0;
    
    highlightText(o, beginPos, endPos);
    beginPos = SUIT_getInteger(o, MARK_INDEX);
    endPos = SUIT_getInteger(o, MARK_END_INDEX);
    insertText = SUIT_createSafeString(text+beginPos);
    insertText[endPos-beginPos] = END_OF_TEXT;
    TE_setBuffer(o, insertText);
    SUIT_free(insertText);
}


PRIVATE boolean getWordIndexes(char *text, int pos, int *begin, int *end)
{
    if (pos > strlen(text))
        pos = strlen(text);
    else if (pos < 0)
        pos = 0;
    
    *end = pos;
    while (isalnum(text[*end]))
        (*end)++;
    if ((text[*end] == TAB) || (text[*end] == ' '))
        (*end)++;
    *begin = pos - 1;
    while ((*begin > 0) &&
	   (isalnum(text[*begin])))
        (*begin)--;
    if (*begin <= 0)
        *begin = 0;
    else (*begin)++;
    if (text[*begin] == ' ')
        return FALSE;
    if ((text[(*end)-1] != TAB) &&
	(text[(*end)-1] != ' ') &&
	(*begin > 0))
        if ((text[(*begin)-1] == TAB) || (text[(*begin)-1] == ' '))
	    (*begin)--;
    return TRUE;
}    


PRIVATE void addCharToInputString(SUIT_object o, char c)
{
    char *newStr;
    char *inputStr = SUIT_getText(o, INPUT_SEQUENCE);
    int inputStrLen = strlen(inputStr);
    
    newStr = SUIT_malloc(inputStrLen + 2);
    strcpy (newStr, inputStr);
    newStr[inputStrLen++] = c;
    newStr[inputStrLen] = END_OF_TEXT;
    SUIT_deluxeSetText(o, INPUT_SEQUENCE, newStr, OBJECT);
    SUIT_free (newStr);
}


PRIVATE char *yankText(SUIT_object o, char *text, int *pos, int *markBegin)
{
    char *oldText = SUIT_getText(o, CURRENT_VALUE);
    char *newText;
    char *insertText;
    int insertLen;
    int i;
    
    if (SUIT_getBoolean (o, READ_ONLY))
	SUIT_inform ("Sorry, you may only read this text");
    else {
	insertText = TE_getBuffer(o);
	if (strchr(insertText, TAB) != NULL)
	  SUIT_deluxeSetBoolean(o, HAS_A_TAB, TRUE, OBJECT);
	insertLen = strlen(insertText);
	newText = SUIT_malloc(strlen(oldText)+insertLen+1);
	for (i=0; i < (*pos); i++)
	    newText[i] = oldText[i];
	for (i = strlen(oldText);  i >= (*pos);  i--)
	    newText[i+insertLen] = oldText[i];
	for (i = 0; i < insertLen; i++)
	    newText[i+(*pos)] = insertText[i];
	if ((*pos) <= SUIT_getInteger(o, MARK_INDEX)) 
	    (*markBegin) = (*pos);
	else (*markBegin) = SUIT_getInteger(o, MARK_INDEX);
	(*pos) += insertLen;
	SUIT_free(text);
	SUIT_free(insertText);
	return newText;
    }
    return text;
}

PRIVATE
void deleteBlockAndInsertChar(SUIT_object o, char *text, int *pos, int numChars,
			      char insertThis, int *currlen, int *markBegin,
			      boolean *lineOnlyAltered, 
			      boolean cutUnHighlightToBuffer)
{
    char *insertText;
    int markEnd;
    int i;

    if (SUIT_getBoolean (o, READ_ONLY))
      SUIT_inform ("Sorry, you may only read this text");
    else {
	if (SUIT_getBoolean (o, HIGHLIGHT_BLOCK)) {
	    *markBegin = SUIT_getInteger(o, MARK_INDEX);
	    if (SUIT_getInteger(o, MARK_END_INDEX) < (*markBegin)) {
		markEnd = *markBegin;
		*markBegin = SUIT_getInteger (o, MARK_END_INDEX);
	    }
	    else markEnd = SUIT_getInteger (o, MARK_END_INDEX);
	    insertText = SUIT_createSafeString(text+(*markBegin));
	    insertText[markEnd-(*markBegin)] = END_OF_TEXT;
	    TE_setBuffer (o, insertText);
	    SUIT_free(insertText);
	    strcpy (text+(*markBegin), text+markEnd);
	    *pos = *markBegin;
	    *currlen = strlen (text);
	}
	else {	
	    *lineOnlyAltered = TRUE;
	    if (cutUnHighlightToBuffer) {
		insertText = SUIT_createSafeString(text+(*pos));
		insertText[numChars] = END_OF_TEXT;
		TE_setBuffer (o, insertText);
		SUIT_free(insertText);
	    }		
	    if (numChars > 0) {
		for (i=0; (i<numChars) && (text[i+(*pos)] != END_OF_TEXT); i++)
		  if (text[(*pos)+i] == END_OF_LINE)
		    *lineOnlyAltered = FALSE;
		strcpy (text+(*pos), text+(*pos)+numChars);
	    }
	    *markBegin = (*pos);
	}
	if (insertThis != '\0') {
	    for (i = *currlen;  i >= *pos;  i--)
	      text[i+1] = text[i];
	    text[(*pos)++] = insertThis;
	    if (insertThis == TAB)
		SUIT_deluxeSetBoolean(o, HAS_A_TAB, TRUE, OBJECT);
	    else if (insertThis == END_OF_LINE)
	        *lineOnlyAltered = FALSE;
	}
    }
}



PRIVATE void HitTextEditor (SUIT_object o, SUIT_event input, enum linesDisplayed lines)
{
    char *inputStr, *contrStr;
    
    char *oldtext, *text, *insertText, *oldbuf, *newbuf, *line;
    void (*funct)(SUIT_object);
    int currlen, inputStrLen, pos, i;
    int hour, min, second, micro;
    double theTime;
    
    int cursorXPos, linePos, lineEnd, newPos, markEnd, markLast;
    int markBegin = SUIT_getInteger(o, MARK_INDEX);
    boolean commandFound = FALSE;
    boolean clearHighlight = FALSE;
    boolean doCallback, textAltered, lineOnlyAltered;
    boolean appendToBuffer = FALSE;
    
    doCallback = FALSE;
    textAltered = FALSE;
    lineOnlyAltered = FALSE;
    oldtext =  SUIT_getText (o, CURRENT_VALUE);
    text = SUIT_malloc (strlen(oldtext)+4);
    strcpy (text, oldtext);
    pos = SUIT_getInteger (o, CURSOR_INDEX);
    
    cursorXPos = pos - findLinePos (text, pos) + 1;
    currlen = strlen (oldtext);
    

    if (pos > currlen)
        pos = currlen;

    NotInTextEditorHitProc = FALSE;

    SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, TRUE, OBJECT);
    
    if (input.type == KEYSTROKE) {
        commandFound = TRUE;
	inputStr = SUIT_getText (o, INPUT_SEQUENCE);
	inputStrLen = strlen(inputStr);
	
	if (isprint(input.keyboard)) {
	    addCharToInputString(o, input.keyboard);
	}
	
	else if (input.keyboard == BACKSPACE || input.keyboard == DELETE) {
	    addCharToInputString(o, BACKSPACE);
	    if (pos > 0) {
		pos--;
		deleteBlockAndInsertChar(o, text, &pos, 1, '\0', &currlen, 
					 &markBegin, &lineOnlyAltered, FALSE);
		textAltered = TRUE;
	    }
	}

	else if (input.keyboard == ESCAPE) {
	    if (inputStrLen == 0) {
		SUIT_deluxeSetText (o, INPUT_SEQUENCE, esc, OBJECT);
	    }
	    else {
		contrStr = SUIT_malloc (inputStrLen+1+strlen(esc));
		strcpy (contrStr, inputStr);
		strcat (contrStr, esc);
		SUIT_deluxeSetText (o, INPUT_SEQUENCE, contrStr, OBJECT);
		SUIT_free(contrStr);
	    }
	}
	
	else if ((input.keyboard >= 0) && (input.keyboard < ESCAPE)) {
	    contrStr = SUIT_malloc (inputStrLen + strlen(contr) + 2);
	    strcpy (contrStr, inputStr);
	    strcat (contrStr, contr);
	    contrStr[strlen(contrStr)+1] = '\0';
	    contrStr[strlen(contrStr)] = input.keyboard + 'a' - 1;
	    SUIT_deluxeSetText (o, INPUT_SEQUENCE, contrStr, OBJECT);
	    SUIT_free(contrStr);
	}
	
	if ((strlen(SUIT_getText(o, INPUT_SEQUENCE)) == 1)
	    && isprint(input.keyboard)) {
	    deleteBlockAndInsertChar(o, text, &pos, 0, input.keyboard,
				     &currlen, &markBegin, &lineOnlyAltered,
				     FALSE);
	    textAltered = TRUE;
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, NEWLINE_KEY))) {
	    if (lines == multiple) {
		deleteBlockAndInsertChar(o, text, &pos, 0, END_OF_LINE,
					 &currlen, &markBegin, &lineOnlyAltered,
					 FALSE);
		textAltered = TRUE;
	    }
	    else doCallback = TRUE;
	}

	else if (inputSequenceMatch (o, SUIT_getText (o, TAB_KEY))) {
	    deleteBlockAndInsertChar(o, text, &pos, 0, TAB,
				     &currlen, &markBegin, &lineOnlyAltered,
				     FALSE);
	    textAltered = TRUE;
	}

	else if (inputSequenceMatch (o, SUIT_getText (o, DONE_EDITING_KEY)))
	    doCallback = TRUE;
	
	else if (inputSequenceMatch (o, SUIT_getText (o, FORWARD_CHAR_KEY)) &&
		 pos < strlen(text))
	    pos++;
	
	else if (inputSequenceMatch (o, SUIT_getText (o, BACKWARD_CHAR_KEY)) && pos > 0)
	    pos--;
	
	else if (inputSequenceMatch (o, SUIT_getText (o, NEXT_LINE_KEY))) {
	    lineEnd = findLineEnd (text, pos);
	    if (text[lineEnd] != END_OF_TEXT) {
		newPos = lineEnd + cursorXPos;
		pos = lineEnd + 1;
		while ((text[pos] != END_OF_LINE) 
		       && (text[pos] != END_OF_TEXT)
		       && (pos < newPos))
		    pos++;
		cursorXPos = pos - lineEnd;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, PREVIOUS_LINE_KEY))) {
	    linePos = findLinePos (text, pos);
	    if (linePos > 0) {
		linePos = findLinePos(text,linePos-1);
		newPos = linePos + cursorXPos-1;
		pos = linePos;
		while ((text[pos] != END_OF_LINE)
		       && (pos < newPos))
		    pos++;
		cursorXPos = pos - linePos+1;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, DELETE_CHAR_KEY))) {
	    if ((text[pos] != END_OF_TEXT) 
		|| (SUIT_getBoolean(o, HIGHLIGHT_BLOCK))) {
		deleteBlockAndInsertChar(o, text, &pos, 1, '\0', &currlen, 
					 &markBegin, &lineOnlyAltered, FALSE);
		textAltered = TRUE;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, DELETE_ENTIRE_LINE_KEY))) {
	    if (SUIT_getBoolean (o, READ_ONLY))
		SUIT_inform ("Sorry, you may only read this text");
	    else {
		if (lines == multiple) {
		    if (text[pos] == END_OF_LINE) {
			strcpy (text+pos, text+pos + 1);
			insertText = SUIT_malloc(2);
			insertText[0] = END_OF_LINE;
			insertText[1] = END_OF_TEXT;
		    }
		    else {
			linePos = findLinePos (text, pos);
			insertText = SUIT_createSafeString(text+linePos);
			insertText[findLineEnd(text,pos)-linePos] = END_OF_TEXT;
			strcpy (text+linePos, text+findLineEnd (text, pos));
			pos = linePos; 
			lineOnlyAltered = TRUE;
		    }
		}
		else {
		    insertText = SUIT_createSafeString(text);
		    strcpy (text, "");
		    pos = 0;
		}
		markBegin = pos;
		textAltered = TRUE;
		if (SUIT_getBoolean (o, APPEND_TO_BUFFER)) {
		    oldbuf = SUIT_createSafeString(SUIT_getText(o,CUT_BUFFER));
		    newbuf = SUIT_malloc(strlen(oldbuf)+strlen(insertText));
		    strcpy (newbuf,oldbuf);
		    strcat (newbuf,insertText);
		    TE_setBuffer (o, newbuf);
		    SUIT_free(oldbuf);
		    SUIT_free(newbuf);
		}
		else TE_setBuffer (o, insertText);
		SUIT_free(insertText);
		appendToBuffer = TRUE;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, KILL_LINE_KEY))) {
	    if (SUIT_getBoolean (o, READ_ONLY))
		SUIT_inform ("Sorry, you may only read this text");
	    else {
		if (text[pos] == END_OF_LINE) {
		    strcpy (text+pos, text+pos + 1);
		    insertText = SUIT_malloc(2);
		    insertText[0] = END_OF_LINE;
		    insertText[1] = END_OF_TEXT;
		}
		else {
		    insertText = SUIT_createSafeString(text+pos);
		    insertText[findLineEnd(text,pos)-pos] = END_OF_TEXT;
		    strcpy (text+pos, text+findLineEnd(text,pos));
		    lineOnlyAltered = TRUE;
		}
		markBegin = pos;
		textAltered = TRUE;
		if (SUIT_getBoolean (o, APPEND_TO_BUFFER)) {
		    oldbuf = SUIT_createSafeString(SUIT_getText(o,CUT_BUFFER));
		    newbuf = SUIT_malloc(strlen(oldbuf)+strlen(insertText)+1);
		    if (strlen(oldbuf) > 0) {
			strcpy (newbuf,oldbuf);
			strcat (newbuf,insertText);
		    }
		    else strcpy (newbuf,"");
		    TE_setBuffer (o, newbuf);
		    SUIT_free(oldbuf);
		    SUIT_free(newbuf);
		}
		else TE_setBuffer (o, insertText);
		SUIT_free(insertText);
		appendToBuffer = TRUE;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, YANK_KEY))) {
	    text = yankText(o, text, &pos, &markBegin);
	    textAltered = TRUE;
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, OPEN_LINE_KEY))) {
	    if (SUIT_getBoolean (o, READ_ONLY))
		SUIT_inform ("Sorry, you may only read this text");
	    else {
		markBegin = pos;
		for (i = strlen(text); i >= pos; i--)
		    text[i+1] = text[i];
		text[pos] = END_OF_LINE;
		textAltered = TRUE;
	    }
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, SET_MARK_KEY))) {
	    SUIT_deluxeSetInteger(o, LAST_MARK_INDEX, SUIT_getInteger(o, MARK_INDEX), OBJECT);
	    markBegin = pos;
	    TE_clearBuffer (o);
	    SUIT_deluxeSetBoolean (o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, WIPE_BLOCK_KEY))) {
	    int blockLen;

	    markBegin = SUIT_getInteger(o, MARK_INDEX);
	    if (pos > markBegin) {
		blockLen = pos - markBegin;
		pos = markBegin;
	    }
	    else blockLen = markBegin-pos;
	    deleteBlockAndInsertChar(o, text, &pos, blockLen, '\0', &currlen,
				     &markBegin, &lineOnlyAltered, TRUE);
	    textAltered = TRUE;
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, REPAINT_KEY))) {
	    SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, FALSE, OBJECT);
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, BEGINNING_OF_LINE_KEY))) {
	    if (lines == multiple)
		pos = findLinePos (text, pos);
	    else pos = 0;
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, END_OF_LINE_KEY))) {
	    if (lines == multiple)
		pos = findLineEnd (text, pos);
	    else pos = strlen (text);
	}
	
	else if (inputSequenceMatch (o, SUIT_getText (o, BEGINNING_OF_TEXT_KEY)))
	    pos = 0;
	
	else if (inputSequenceMatch (o, SUIT_getText (o, END_OF_TEXT_KEY)))
	    pos = strlen (text);
	
	else if ((inputSequenceMatch (o, SUIT_getText (o, SCROLL_UP_KEY)))
		 && (lines == multiple)) {
	    pos = scrollUp(o);
	    SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, FALSE, OBJECT);
	}
	
	else if ((inputSequenceMatch (o, SUIT_getText (o, SCROLL_DOWN_KEY)))
		 && (lines == multiple)) {
	    pos = scrollDown(o);
	    SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, FALSE, OBJECT);
	}
	
	else
	    commandFound = FALSE;
	
	
	if (pos > 0)
	    if (text[pos-1] == END_OF_LINE)
	        cursorXPos = 0;
	if ((commandFound) 
            || ((input.keyboard != ESCAPE) && (input.keyboard != CNTL_X) && (input.keyboard != CNTL_C))) {
	    SUIT_deluxeSetText (o, INPUT_SEQUENCE, "", OBJECT);
	}
    }

    
    else if (input.type == CLICK)  {
	GP_convertTime(GP_getCurrentTime(), &hour, &min, &second, &micro);
	markLast = pos;
	pos = TE_clickPosition (o, input.relativePixelLocation.x, input.relativePixelLocation.y, lines);
	theTime = (((((((double) hour * 60) + (double) min) * 60) + (double) second) * 1000000) + (double) micro);
	if (input.locator.modifier_chord[SHIFT]) {
	    if (!SUIT_getBoolean(o, HIGHLIGHT_BLOCK)) {
		SUIT_highlightBlockInTextEditor(o, pos, markLast);
	    }
	    else {
		markBegin = SUIT_getInteger(o, MARK_INDEX);
		markEnd = SUIT_getInteger(o, MARK_END_INDEX);
		if (((pos < markBegin) && (markBegin < markEnd)) ||
		    ((pos > markBegin) && (markBegin > markEnd)))
		    markBegin = pos;
		else markEnd = pos;
		SUIT_highlightBlockInTextEditor(o, markBegin, markEnd);
	    }
	}
	else if (input.button == MIDDLE_BUTTON) {
	    text = yankText(o, text, &pos, &markBegin);
	    textAltered = TRUE;
	}
	else if ((SUIT_getDouble(o, LAST_CLICK_TIME) + SUIT_getDouble(o, DOUBLE_CLICK_TIME)) >= theTime) {
	    if (getWordIndexes(text, pos, &markBegin, &markEnd)) {
		SUIT_highlightBlockInTextEditor(o, markBegin, markEnd);
		pos = markBegin;
	    }
	    SUIT_deluxeSetDouble (o, LAST_CLICK_TIME, 0.0, OBJECT);
	} 
	else {
	    clearHighlight = TRUE;
	    SUIT_deluxeSetDouble (o, LAST_CLICK_TIME, theTime, OBJECT);
	}
    }

    
    else if (input.type == MOUSE_DOWN) {
	pos = TE_clickPosition (o, input.relativePixelLocation.x, input.relativePixelLocation.y, lines);
	highlightText(o, pos, pos);
	SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
    }

    
    else if (input.type == MOUSE_MOTION) {
	SUIT_deluxeSetInteger (o, LAST_MARK_INDEX, pos, OBJECT);
	pos = TE_clickPosition (o, input.relativePixelLocation.x, input.relativePixelLocation.y, lines);
	SUIT_deluxeSetInteger (o, MARK_END_INDEX, pos, OBJECT);
    }

    
    else if (input.type == MOUSE_UP) {
	pos = TE_clickPosition (o, input.relativePixelLocation.x, input.relativePixelLocation.y, lines);
	markBegin = SUIT_getInteger(o, MARK_INDEX);
	SUIT_highlightBlockInTextEditor(o, markBegin, pos);
    }


    
    if (textAltered) {
	CalculateBlock = FALSE;
        SUIT_setText (o, CURRENT_VALUE, text);
	CalculateBlock = TRUE;
	SUIT_setBoolean (o, ALTERED, TRUE);
	SUIT_deluxeSetBoolean (o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
	if (!lineOnlyAltered) {
	    if (markBegin < 0)
	        markBegin = 0;
	    SUIT_deluxeSetInteger(o, MARK_INDEX, markBegin, OBJECT);
	    SUIT_deluxeSetInteger(o, LAST_MARK_INDEX, markBegin, OBJECT);
	    SUIT_deluxeSetInteger(o, MARK_END_INDEX, strlen(text)+1, OBJECT);
	    SUIT_deluxeSetBoolean(o, CALCULATE_LINES, TRUE, OBJECT);
   	} else {
	    markBegin = pos-1; 
	    if (markBegin < 0)
	      markBegin = 0;
	    SUIT_deluxeSetInteger(o, MARK_INDEX, markBegin, OBJECT);
	    SUIT_deluxeSetInteger(o, LAST_MARK_INDEX, markBegin, OBJECT);
	    if (lines == multiple) {
		SUIT_deluxeSetInteger(o, MARK_END_INDEX, findLineEnd(text,pos)+1, OBJECT);
	    }
	    else {
		SUIT_deluxeSetInteger(o, MARK_END_INDEX, strlen(text)+1, OBJECT);
	    }
	}
    } else if (!SUIT_getBoolean(o, HIGHLIGHT_BLOCK)) {
	SUIT_deluxeSetInteger(o, MARK_INDEX, markBegin, OBJECT);
	SUIT_deluxeSetInteger(o, MARK_END_INDEX, markBegin, OBJECT);
    }
    
    SUIT_deluxeSetBoolean (o, APPEND_TO_BUFFER, appendToBuffer, OBJECT);
    
    text[pos] = END_OF_TEXT;
    if (lines == multiple) {
	linePos = findLinePos(text, pos);
	line = fillTabs(o, text+linePos, 0, pos - linePos);
	if ((pos - linePos) > LONGEST_LINE) {
	    SUIT_inform("Sorry, you cannot move the cursor past this point");
	    pos = findLinePos(text, pos) + LONGEST_LINE;
	} 
    }
    else {
	line = fillTabs(o, text, 0, pos);
	if (pos > LONGEST_LINE) {
	    SUIT_inform("Sorry, you cannot move the cursor past this point");
	    pos = LONGEST_LINE;
	} 
    }
    SUIT_free(text);
    SUIT_free(line);
    SUIT_deluxeSetInteger (o, PRE_CURSOR_INDEX, 
			   SUIT_getInteger(o, CURSOR_INDEX) - 1, OBJECT);
    if (clearHighlight)
	NotInTextEditorHitProc = TRUE;
    else CalculateBlock = FALSE;
    SUIT_deluxeSetInteger (o, CURSOR_INDEX, pos, OBJECT);
    CalculateBlock = TRUE;
    NotInTextEditorHitProc = FALSE;
    if (commandFound)	
        SUIT_deluxeSetBoolean (o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
    NotInTextEditorHitProc = TRUE;
    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
    if (funct != NULL && (doCallback || SUIT_getBoolean (o, ANY_KEYSTROKE_TRIGGERS)))
        funct (o);
    SUIT_redisplayRequired (o);
}


PRIVATE void DrawOneLine (SUIT_object o)
{
    DrawTextEditor (o, one);
}

PRIVATE void DrawMultipleLines (SUIT_object o)
{
    DrawTextEditor (o, multiple);
}

PRIVATE void HitOneLine (SUIT_object o, SUIT_event input)
{
    HitTextEditor (o, input, one);
}

PRIVATE void HitMultipleLines (SUIT_object o, SUIT_event input)
{
    HitTextEditor (o, input, multiple);
}

PRIVATE void TE_setUpTextEditor (SUIT_object o, void (*callback)(SUIT_object))
{
    SUIT_deluxeSetFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer) (SUIT_functionPointer) callback, OBJECT);
    SUIT_setText (o, CURRENT_VALUE, "");
    SUIT_deluxeSetBoolean (o, PAINT_BLOCK_ONLY, FALSE, OBJECT); 
    SUIT_setBoolean (o, READ_ONLY, FALSE);
    
    SUIT_deluxeSetBoolean (o, ALTERED, FALSE, OBJECT);
    
    SUIT_registerInterest (o, TE_OptimizePaint);
}


PRIVATE void TE_setUpClassValues (SUIT_object o)
{
    SUIT_viewport vp;
    int height;
    
    vp = OBJECT_VIEWPORT(o);
    height = vp.top_right.y - vp.bottom_left.y;
    
    SUIT_deluxeSetInteger (o, CURSOR_INDEX, 0, OBJECT);
    SUIT_makePropertyTemporary(o, CURSOR_INDEX, OBJECT);
    SUIT_deluxeSetInteger (o, PRE_CURSOR_INDEX, -1, OBJECT);
    SUIT_makePropertyTemporary(o, PRE_CURSOR_INDEX, OBJECT);
    SUIT_deluxeSetInteger (o, START_X, 0, OBJECT);
    SUIT_makePropertyTemporary(o, START_X, OBJECT);
    SUIT_deluxeSetDouble (o, START_Y, (double) (height-SUIT_getInteger (o, MARGIN)), OBJECT);
    SUIT_makePropertyTemporary(o, START_Y, OBJECT);
    SUIT_deluxeSetInteger (o, CURSOR_X, -1, OBJECT);
    SUIT_makePropertyTemporary(o, CURSOR_X, OBJECT);
    SUIT_deluxeSetInteger (o, CURSOR_Y, -1, OBJECT);
    SUIT_makePropertyTemporary(o, CURSOR_Y, OBJECT);
    SUIT_deluxeSetText (o, INPUT_SEQUENCE, "", OBJECT); 
    SUIT_makePropertyTemporary(o, INPUT_SEQUENCE, OBJECT);
    SUIT_deluxeSetDouble (o, LAST_CLICK_TIME, 0.0, OBJECT);
    SUIT_makePropertyTemporary(o, LAST_CLICK_TIME, OBJECT);
    SUIT_deluxeSetInteger (o, MARK_INDEX, 0, OBJECT);
    SUIT_makePropertyTemporary(o, MARK_INDEX, OBJECT);
    SUIT_deluxeSetInteger (o, MARK_END_INDEX, 0, OBJECT);
    SUIT_makePropertyTemporary(o, MARK_END_INDEX, OBJECT);
    SUIT_deluxeSetInteger (o, LAST_MARK_INDEX, 0, OBJECT);
    SUIT_makePropertyTemporary(o, LAST_MARK_INDEX, OBJECT);
    SUIT_deluxeSetBoolean (o, APPEND_TO_BUFFER, FALSE, OBJECT);
    SUIT_makePropertyTemporary(o, APPEND_TO_BUFFER, OBJECT);
    SUIT_deluxeSetBoolean (o, HIGHLIGHT_BLOCK, FALSE, OBJECT);
    SUIT_makePropertyTemporary(o, HIGHLIGHT_BLOCK, OBJECT);
    SUIT_deluxeSetBoolean (o, CALCULATE_LINES, FALSE, OBJECT);
    SUIT_makePropertyTemporary(o, CALCULATE_LINES, OBJECT);
    
    SUIT_deluxeSetInteger (o, SPACING_GAP, 3, CLASS);
    SUIT_deluxeSetDouble (o, DOUBLE_CLICK_TIME, 400000.0, CLASS);
    SUIT_deluxeSetText (o, DONE_EDITING_KEY, "C-x", CLASS);
    SUIT_deluxeSetText (o, NEWLINE_KEY, "C-m", CLASS);
    SUIT_deluxeSetText (o, TAB_KEY, "C-i", CLASS);
    SUIT_deluxeSetText (o, FORWARD_CHAR_KEY, "C-f", CLASS);
    SUIT_deluxeSetText (o, BACKWARD_CHAR_KEY, "C-b", CLASS);
    SUIT_deluxeSetText (o, NEXT_LINE_KEY, "C-n", CLASS);
    SUIT_deluxeSetText (o, PREVIOUS_LINE_KEY, "C-p", CLASS);
    SUIT_deluxeSetText (o, BEGINNING_OF_TEXT_KEY, "M-<", CLASS);
    SUIT_deluxeSetText (o, END_OF_TEXT_KEY, "M->", CLASS);
    SUIT_deluxeSetText (o, SCROLL_UP_KEY, "C-v", CLASS);
    SUIT_deluxeSetText (o, SCROLL_DOWN_KEY, "M-v", CLASS);
    SUIT_deluxeSetText (o, DELETE_ENTIRE_LINE_KEY, "C-u", CLASS);
    SUIT_deluxeSetText (o, BEGINNING_OF_LINE_KEY, "C-a", CLASS);
    SUIT_deluxeSetText (o, REPAINT_KEY, "C-l", CLASS);
    SUIT_deluxeSetText (o, END_OF_LINE_KEY, "C-e", CLASS);
    SUIT_deluxeSetText (o, KILL_LINE_KEY, "C-k", CLASS);
    SUIT_deluxeSetText (o, DELETE_CHAR_KEY, "C-d", CLASS);
    SUIT_deluxeSetText (o, YANK_KEY, "C-y", CLASS);
    SUIT_deluxeSetText (o, OPEN_LINE_KEY, "C-o", CLASS);
    SUIT_deluxeSetText (o, SET_MARK_KEY, "C-`", CLASS);
    SUIT_deluxeSetText (o, WIPE_BLOCK_KEY, "C-w", CLASS);
    
    SUIT_deluxeSetColor (o, CURSOR_COLOR, GP_defColor ("black", TRUE), CLASS);
    SUIT_deluxeSetEnum (o, CURSOR_STYLE, SUIT_defEnum("vertical bar", 2, CursorStyles), CLASS);
    SUIT_deluxeSetInteger (o, TAB_LENGTH, 5, CLASS);
    SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
    SUIT_deluxeSetBoolean (o, ANY_KEYSTROKE_TRIGGERS, FALSE, CLASS);
    SUIT_deluxeSetBoolean (o, HAS_A_TAB, FALSE, CLASS);
    SUIT_deluxeSetText (o, CUT_BUFFER, "", OBJECT); 
    SUIT_deluxeSetInteger (o, NUMBER_OF_LINES, 1, OBJECT);
}


SUIT_object SUIT_createTextEditor (char *name, void (*callback)(SUIT_object))
{
    static boolean textfirsttime = TRUE;
    
    SUIT_object o;
    
    o = SUIT_createObject (name, "text editor");    
    SUIT_addDisplayToObject (o, "standard", HitMultipleLines, DrawMultipleLines);
    if (textfirsttime) {
        TE_setUpClassValues (o);
	textfirsttime = FALSE;
    }
    TE_setUpTextEditor (o, callback);
    
    return (o);
}

SUIT_object SUIT_createTypeInBox (char *name, void (*callback)(SUIT_object))
{
    static boolean typeinfirsttime = TRUE;
    
    SUIT_object o;
    
    o = SUIT_createObject (name, "type in box");
    SUIT_addDisplayToObject (o, "standard", HitOneLine, DrawOneLine);
    if (typeinfirsttime) {
        TE_setUpClassValues (o);
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	typeinfirsttime = FALSE;
    }
    SUIT_registerInterest (o, ShrinkToFit);  /* It is important that this call
						be made before registering the
						TE_optimizePaint interest routine
						*/
    TE_setUpTextEditor (o, callback);
    ShrinkToFit (o, VIEWPORT, "", NULL, NULL);
    
    return (o);
}

/* Text editor with a scroll bar functions: */

PRIVATE int TE_stringPos(char *text, int curPos)
{
    int i;
    int lineNum;
    
    for (i = 0, lineNum = 1; (text[i] != '\0') && (i < curPos); i++)
        if (text[i] == '\n')
	    lineNum++;
    return lineNum;
}

PRIVATE void TE_updateScrollBar(SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    SUIT_object scrollBar;

    if (SUIT_stringsMatch (propName, NUMBER_OF_LINES)) {
	scrollBar = SUIT_objectNamed(SUIT_relativeName(SUIT_getParent(o), 
						       "scroll bar"));
	SUIT_setDouble(scrollBar, MAXIMUM_VALUE, (double)(*(int *)new));
    }
    else if (SUIT_stringsMatch (propName, CURSOR_INDEX)) {
	scrollBar = SUIT_objectNamed(SUIT_relativeName(SUIT_getParent(o), 
						       "scroll bar"));
	SUIT_setDouble(scrollBar, CURRENT_VALUE, 
		       (double)TE_stringPos(SUIT_getText(o, CURRENT_VALUE),
					    * (int *) new));
    }
}

PRIVATE int TE_curPos(char *text, register int lineNum)
{
    register int i;
    register int atLine;
    
    for (i = 0, atLine = 1; (text[i] != '\0') && (atLine < lineNum); i++)
	if (text[i] == '\n')
	    atLine++;
    return i;
}

PRIVATE void TE_scrollBarCallback(SUIT_object o)
{
    SUIT_object textEditor;
    
    textEditor = SUIT_objectNamed(SUIT_relativeName(SUIT_getParent(o), "text editor"));
    SUIT_setInteger(textEditor, CURSOR_INDEX, 
		    TE_curPos(SUIT_getText(textEditor, CURRENT_VALUE), 
			      (int) SUIT_getDouble(o, CURRENT_VALUE)));
}

SUIT_object SUIT_createTextEditorWithScrollBar(char *name, void (*callback)(SUIT_object))
{
    SUIT_object o;
    SUIT_object scrollBar;
    SUIT_object textEditor;
    
    o = SUIT_createBulletinBoardWithClass (name, "text editor with scrollbar");
    SUIT_setBoolean(o, HAS_BORDER, FALSE);
    textEditor = SUIT_createTextEditor(SUIT_relativeName(o, "text editor"), 
				       callback);
    SUIT_setBoolean(textEditor, HAS_BORDER, TRUE);
    SUIT_addChildToObject(o, textEditor);
    SUIT_setViewport (textEditor, VIEWPORT, 
		      SUIT_mapToParent(textEditor, 0.0, 0.0, 0.93, 1.0));
    scrollBar = SUIT_createBoundedValue(SUIT_relativeName(o, "scroll bar"),
					TE_scrollBarCallback);
    SUIT_setBoolean(scrollBar, HAS_BORDER, TRUE);
    SUIT_setEnumString (scrollBar, ACTIVE_DISPLAY, "scroll bar");
    SUIT_setDouble(scrollBar, GRANULARITY, 1.0);
    SUIT_setDouble(scrollBar, MINIMUM_VALUE, 1.0);
    SUIT_addChildToObject(o, scrollBar);
    SUIT_setViewport (scrollBar, VIEWPORT, 
		      SUIT_mapToParent(scrollBar, 0.95, 0.0, 1.0, 1.0));

    SUIT_registerInterest (textEditor, TE_updateScrollBar);
    
    return (textEditor);
}


