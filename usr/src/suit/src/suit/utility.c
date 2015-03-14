/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#ifdef SUN
#include <memory.h>
#endif



#include <ctype.h>
#include "privsuit.h"


PRIVATE int dumpCore = 0;



/* This routine allows you to set a debugger breakpoint when an assertion has
 * failed */

void si_assertionHasFailed (char *message, int lineNumber, char *fileName)
{
    if (message[strlen(message)-1] == '\n')  /* if there's a CR at the end of the message, nuke it */
	message[strlen(message)-1] = '\0';

    fprintf (stderr, "\n* SUIT has detected an error at line %d in the SUIT source file %s:\n\
*\n\
*     %s\n\
*\n\
* In most cases, this type of error indicates that you have\n\
* made a mistake when calling the SUIT library routines.\n\n",
	     lineNumber, fileName, message);
#ifdef IBM_PC
    {
	FILE *file;
	if ((file = fopen ("assert.err", "a")) != NULL) {
	    fprintf (stderr, "\n* SUIT has detected an error at line %d in the SUIT source file %s:\n"
		     "*\n"
		     "*     %s\n"
		     "*\n"
		     "* In most cases, this type of error indicates that you have\n"
		     "* made a mistake when calling the SUIT library routines.\n\n",
		     lineNumber, fileName, message);
	    fclose (file);
	}
    }
#endif

    if (dumpCore)
	abort();
}



void SUIT_makeAssertionsFatal (boolean fatal)
{
    dumpCore = fatal;
}



/* pausch addition: */
/* only useful for SUIT-internal animations that "take over" the screen:
   as often as possible, the given function is called, with the given
   object, current time and total time for the animation (both time
   are in milliseconds - the first call will be with zero milliseconds),
   and the last call with be at 'totalTime,' but no guarantees about
   the number of calls, if any, that occur in-between.
   */
    
void si_animateOverTime (SUIT_animationFunction func, SUIT_object o, long totalTime)
{
    GP_time	current, start;
    long	diff;
    
    
    start = GP_getCurrentTime();
    current = GP_getCurrentTime();
    func(o, 0, totalTime);
    
    diff = GP_timeDifference(current, start);
    
    while ( diff < totalTime )
    {
	func(o, diff, totalTime);
	current = GP_getCurrentTime();
	diff = GP_timeDifference(current, start);
    }
    
    func(o, totalTime, totalTime);
    
}


void SUIT_getObjectSize (SUIT_object o, int *width, int *height)
{
    SUIT_viewport vp;
    vp = OBJECT_VIEWPORT(o);
    *width = vp.top_right.x - vp.bottom_left.x + 1;
    *height = vp.top_right.y - vp.bottom_left.y + 1;
}




void si_clearArea (rectangle r)
{
    GP_pushGraphicsState ();

    GP_setColor (SUIT_deluxeGetColor (NULL, BACKGROUND_COLOR, GLOBAL));
    SRGP_setFillStyle (SOLID);
    SRGP_setWriteMode (WRITE_REPLACE);
    SRGP_fillRectangle (r);
    
    GP_popGraphicsState ();
}



void SUIT_backgroundAndBorderObject(SUIT_object o)
{
    GP_pushGraphicsState();
    GP_setColor(GP_defColor("blue",FALSE));
    SRGP_rectangle(CalculateVisiblePortion(o));
    GP_popGraphicsState();

    GP_setColor(SUIT_getColor(o,BACKGROUND_COLOR));
    SRGP_fillRectangle(SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
    SUIT_borderObject (o);
    GP_setColor(SUIT_getColor(o,FOREGROUND_COLOR));
}


/* Given a DynArray of objects and an object, this routine delete the given object
   from the list.  */

void DeleteObjectFromList (DynArray a, SUIT_object o)
{
    int i;
    for (i=DynLow(a); i <= DynHigh(a); i++) {
	SUIT_object element = * (SUIT_object *) DynGet(a, i);
	if (o == element) {
	    DynDelete (a, i);
	    return;
	}
    }
}



/* SUIT_stringsMatch This routine is used to provide suit with its own string
 * comparison routine. */

int SUIT_stringsMatch (char *a, char *b)
{
    if (a == NULL) {
	if (b == NULL)
	    return TRUE;
	else
	    return FALSE;
    } else if (b == NULL)
	return FALSE;
    else
	return (strcmp (a, b) == 0);
}




int CaseInsensitiveMatch (char *, char *);

int SUIT_caseInsensitiveCompare (char *a, char *b)
{
    return CaseInsensitiveMatch (a, b);
}


boolean SUIT_caseInsensitiveMatch (char *a, char *b)
{
    return (CaseInsensitiveMatch (a, b) == 0);
}


/* This routine is used to put numbers in ascending order, the smaller value
 * is returned in left, the larger in right. */

void si_lessAndGreater (int x, int x2, int *left, int *right)
{
    if (x < x2) {
	*left = x;
	*right = x2;
    } else {
	*left = x2;
	*right = x;
    }
}





/* SUIT_copyData makes a copy of the passed data in a safe place */

Pointer SUIT_copyData (Pointer p, int len)
{
    Pointer retval;
    
    ENTER (10, (buf, "SUIT_copyData (ptr,%d)\n", len));
    retval = (Pointer) SUIT_malloc (len);
    
    memcpy (retval, p, len);
    LEAVE (10, (buf, "SUIT_copyData (ptr,%d) ==> ptr\n", len));
    return retval;
}


/* ---------------------------------------- */
/* Some utility routine for SUIT_textList's */
/* ---------------------------------------- */


SUIT_textList SUIT_defTextList (char *list[], int n)
{
    SUIT_textList retval = DynCreate (sizeof(char*), 1);
    int i;

    for (i=0; i < n; i++) {
	char *copy = SUIT_copyString (list[i]);
	DynAdd (retval, (void *)&copy);
    }
    return retval;
}



int SUIT_sizeOfTextList (SUIT_textList list)
{
    return DynSize(list);
}



void SUIT_sortTextList (SUIT_textList list)
{
    DynQsort (list, DynLow(list), DynHigh(list), strcmp);
}



char *SUIT_itemInTextList (SUIT_textList list, int index)
{
    ASSERT ((index >= DynLow(list)), (mes, "SUIT_itemInTextList too small an index (%d)\n",index));
    ASSERT ((index <= DynHigh(list)), (mes, "SUIT_itemInTextList given too large an index (%d)\n",index));
    return * (char **) DynGet (list, index);
}



void SUIT_appendToTextList (SUIT_textList list, char *aString)
{
    char *copy;
    ASSERT ((list != NULL), (mes, "SUIT_appendToTextList was given a NULL list of text.\n"));
    ASSERT ((aString != NULL), (mes, "SUIT_appendToTextList was given a NULL string.\n"));
    copy = SUIT_copyString(aString);
    DynAdd (list, (void *)&copy);
}



void SUIT_addToTextList (SUIT_textList list, int beforeIndex, char *aString)
{
    DynArray hold;
    char *copy;

    ASSERT ((list != NULL), (mes, "SUIT_addToTextList was given a NULL list of text.\n"));
    ASSERT ((aString != NULL), (mes, "SUIT_addToTextList was given a NULL string.\n"));
    ASSERT ((beforeIndex >= DynLow(list)), 
	    (mes, "SUIT_addToTextList was given a bad index (%d).\n",beforeIndex));
    ASSERT ((beforeIndex <= DynHigh(list)), 
	    (mes, "SUIT_addToTextList was given a bad index (%d).\n",beforeIndex));
    hold = DynCreate(sizeof(char *), DynHigh(list)-beforeIndex);
    copy = SUIT_copyString(aString);
    while (DynHigh(list) >= beforeIndex) {
	char *temp = * (char **) DynGet(list, DynHigh(list));
	DynAdd (hold, (void *)&temp);
	DynDelete (list, DynHigh(list));
    }
    DynAdd (list, (void *)&copy);
    while (DynSize(hold) > 0) {
	char *temp = * (char **) DynGet(hold, DynHigh(hold));
	DynAdd (list, (void *)&temp);
	DynDelete (hold, DynHigh(hold));
    }
    DynDestroy (hold);
}



void SUIT_deleteFromTextList (SUIT_textList list, int index)
{
    ASSERT ((list != NULL), (mes, "SUIT_deleteFromTextList was given a NULL list of text.\n"));
    ASSERT ((index >= DynLow(list)), 
	    (mes, "SUIT_deleteFromTextList was given a bad index (%d).\n",index));
    ASSERT ((index <= DynHigh(list)), 
	    (mes, "SUIT_deleteFromTextList was given a bad index (%d).\n",index));
    DynDelete (list, index);
}



SUIT_textList SUIT_copyTextList (SUIT_textList list)
{
    SUIT_textList newList;
    int size, i;
    
    newList = SUIT_defTextList ((void*)NULL, 0);
    size = SUIT_sizeOfTextList(list);
    for (i=0; i < size; i++)
	SUIT_appendToTextList(newList, SUIT_itemInTextList(list, i));
    return newList;
}


void SUIT_destroyTextList (SUIT_textList list)
{
    int i;
    for (i=0; i < DynSize(list); i++) {
	char *entry = SUIT_itemInTextList (list, i);
	SUIT_free((void *)entry);
    }
    DynDestroy (list);
}



/* ------------------------------------ */
/* Some utility routine for SUIT_enum's */
/* ------------------------------------ */


char *SUIT_getEnumSelection (SUIT_enum e)
{
    ASSERT ((e.currentChoice >= 0), (mes, "SUIT_getEnumSelection was given an enum with a bad current choice (%d)\n",e.currentChoice));
    ASSERT ((e.choices != NULL), (mes, "SUIT_getEnumSelection was given an enum with no choices\n"));
    ASSERT ((DynSize(e.choices) > 0), (mes, "SUIT_getEnumSelection was given an enum with no choices\n"));
    return (* (char **) DynGet(e.choices, e.currentChoice));
}



void SUIT_setEnumSelection (SUIT_enum *e, char *choice)
{
    int i;
    ASSERT ((DynSize(e->choices) > 0), (mes, "SUIT_setEnumSelection was given an enum with no choices\n"));
    e->currentChoice = -1;
    for (i=DynLow(e->choices); i <= DynHigh(e->choices); i++) {
	char *temp = * (char **) DynGet(e->choices, i);
	if (SUIT_caseInsensitiveMatch(temp, choice))
	    e->currentChoice = i;
    }
    ASSERT ((e->currentChoice >= 0), (mes, "SUIT_setEnumSelection was given a choice that is not part of the enum (%s)\n",choice));
}



SUIT_enum SUIT_defEnum (char *current, int numChoices, char *choices[])
{
    int i;
    SUIT_enum retval;
    DynArray new = DynCreate (sizeof(char *), 1);

    for (i=0; i < numChoices; i++) {
	char *copy = SUIT_createRCString(choices[i]);
	DynAdd (new, (void *)&copy);
    }
    retval.choices = new;

    SUIT_setEnumSelection (&retval, current);
    return retval;
}



char *SUIT_textOfFile (char *filename)
{
#define BUFFER_SIZE  50
    FILE *fp;
    char *characters;
    int c, i;
    
    if ((fp = fopen(filename, "r")) == NULL) {
	char buf[100];
	sprintf(buf, "Sorry, the file '%s' was not found", filename);
	SUIT_inform(buf);
	return "";
    }

    characters = (char *) SUIT_malloc(BUFFER_SIZE);
    i=0;
    while ((c = fgetc(fp)) != EOF) {
	if (i > 0 && i % BUFFER_SIZE == 0)
	    characters = (char *) SUIT_realloc ((void *)characters, i+BUFFER_SIZE);
	characters[i++] = (char) c;
    }
    if (i > 0 && i % BUFFER_SIZE == 0)
	characters = (char *) SUIT_realloc ((void *)characters, i+1);
    characters[i] = '\0';
    fclose (fp);
    return characters;
} 


char *SUIT_levelName (SUIT_level level)
{
	static char errormsg[30];
    switch (level) {
      case GLOBAL: return "GLOBAL (or parent's OBJECT)"; break;
      case CLASS: return "CLASS"; break;
      case OBJECT: return "OBJECT"; break;
    }
    sprintf (errormsg, "** BAD LEVEL %d **",level);
    return errormsg; /* should never get here */
}
