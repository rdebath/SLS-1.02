/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "privsuit.h"


PRIVATE char *retvalBuffer;
PRIVATE int retvalBufferSize = 5000;


/* This routine converts any registered type into any other registered type.
   The pointer you get back points to storage that is not "safe" - you
   must copy it yourself with SUIT_copyData if you want a safe copy.
   
   If the types are the same, you get back the pointer you sent in.
   */

Pointer	SUIT_convertType (Pointer value, char *fromType, char *toType)
{
    
    if ( SUIT_caseInsensitiveMatch(fromType, toType) )
	return value;
    
    else
    {
	SUIT_type	*from = si_getType(fromType);	
	SUIT_type	*to = si_getType(toType);	
	
	char *temp = from->convertToAscii(value);
	boolean	error;
	Pointer	retval = to->convertFromAscii(temp, &error);
	/* if read fails, return NULL */
	if ( error )
	    retval = NULL;
	return retval;
    }
}


/*
   NOTE:  all the "compare" procedures work like strcmp.  Returning zero indicates that
   the two are equal.  Returning >0 indicates the first is "bigger".  Returning <0 indicates
   that the first is "smaller".  (In the cases where "bigger" and "smaller" don't make sense,
   e.g. color, we just return non-zero to indicate "different.")

   These routine do **not** return boolean values, i.e. where zero means false and non-zero
   true.  In fact, this is quite the opposite.  Forgetting this fact will make the code
   incomprehensible!
*/


PRIVATE int si_compareIntegers (Pointer firstPtr, Pointer secondPtr)
{
    return (* (int *) firstPtr) - (* (int *) secondPtr);
}



PRIVATE char *si_writeInteger (Pointer val)
{
    ASSERT ((val != VOIDNULL), (mes, "WriteInteger was called with void or null pointer (val).\n"));
    sprintf (retvalBuffer, "%d", (*(int *) val));
    return retvalBuffer;
}



PRIVATE Pointer si_readInteger (char *buffer, boolean *error)
{
    int value;

    ENTER (3, (buf, "ReadInteger()\n"));
    ASSERT ((buffer != CHARNULL), (mes, "ReadInteger was called with null character string (buffer).\n"));
    *error = (sscanf (buffer, "%d", &value) != 1);
    LEAVE (3, (buf, "ReadInteger()\n"));
    return SUIT_copyData (&value, sizeof (int));
}



PRIVATE Pointer si_copyInteger (Pointer val)
{
    return SUIT_copyData (val, sizeof (int));
}



PRIVATE int si_compareBooleans (Pointer firstPtr, Pointer secondPtr)
{
    return (* (boolean *) firstPtr) - (* (boolean *) secondPtr);
}



PRIVATE char *si_writeBoolean (Pointer val)
{
    ASSERT ((val != VOIDNULL), (mes, "WriteBoolean was called with null or void pointer (val).\n"));
    return (* (boolean *) val)? "yes" : "no";
}



PRIVATE Pointer si_readBoolean (char *buffer, boolean *error)
{
    boolean value = TRUE;

    ENTER (3, (buf, "ReadBoolean()\n"));
    ASSERT ((buffer != CHARNULL), (mes, "ReadBoolean was called with null character string (buffer).\n"));
    if (SUIT_stringsMatch (buffer, "0") ||
	SUIT_caseInsensitiveMatch (buffer, "no") ||
	SUIT_caseInsensitiveMatch (buffer, "false"))
	value = FALSE;
    *error = FALSE; /* this never returns an error */
    LEAVE (3, (buf, "ReadBoolean()\n"));
    return SUIT_copyData (&value, sizeof (boolean));
}



PRIVATE Pointer si_copyBoolean (Pointer val)
{
    return SUIT_copyData (val, sizeof (boolean));
}



#define DOUBLE_COMPARE_THRESHOLD 0.000001

PRIVATE int si_compareDoubles (Pointer firstPtr, Pointer secondPtr)
{

    double first = *((double *) firstPtr);
    double second = *((double *) secondPtr);
    double difference = first - second;

    if (ABS (difference) < DOUBLE_COMPARE_THRESHOLD)
	return 0;
    else if (difference > 0)
	return 1;
    else
	return -1;
}



PRIVATE char *si_writeDouble (Pointer val)
{
    sprintf (retvalBuffer, "%f", (*(double *) val));
    return retvalBuffer;
}



#ifdef MACINTOSH
PRIVATE double ATOF (char *str)
{
    char digit;
    int i;
    double retval = 0.0, factor = 1.0;
    
    for (i=0; i < strlen(str) && str[i] != '.'; i++) {
	digit = str[i] - '0';
	retval = 10.0 * retval + digit;
    }
    for (i++; i < strlen(str); i++) {
	factor /= 10.0;
	digit = str[i] - '0';
	retval += factor * digit;
    }
    return retval;
}
#endif



PRIVATE Pointer si_readDouble (char *buffer, boolean *error)
{
    double value;

#ifdef MACINTOSH
    value = ATOF(buffer);
    *error = FALSE;
#else
    *error = (sscanf (buffer, "%lf", (double *) &value) != 1);
#endif
    return SUIT_copyData ((void *) &value, sizeof (double));
}



PRIVATE Pointer si_copyDouble (Pointer val)
{
    return SUIT_copyData (val, sizeof (double));
}



PRIVATE int si_compareStrings (Pointer firstPtr, Pointer secondPtr)
{
    return !SUIT_stringsMatch ((char *)firstPtr, (char *)secondPtr);
}



PRIVATE int si_compareColors (Pointer firstPtr, Pointer secondPtr)
{
    GP_color first, second;
    first = *((GP_color *) firstPtr);
    second = *((GP_color *) secondPtr);

    if (first.blackOnMonochrome == second.blackOnMonochrome)
	return !SUIT_stringsMatch (first.colorName, second.colorName);
    else
	return 1;
}



PRIVATE char *si_writeColor (Pointer val)
{
    GP_color *c = (GP_color *) val;
    sprintf (retvalBuffer, "%s, %s", c->colorName, c->blackOnMonochrome? "black" : "white");
    return retvalBuffer;
}



PRIVATE Pointer si_readColor (char *buffer, boolean *error)
{
    GP_color val;
    char *cname, *bw, *copy;

    ENTER (3, (buf, "entering ReadColor(%s, %x)\n", buffer, *error));
    copy = SUIT_copyString(buffer);
    cname = strtok (copy, ",");  /* get everything before the comma */
    bw = strtok (NULL, ", ");     /* get everything after the comma, ignoring spaces too */
    *error = ((cname == NULL) || (bw == NULL));
    val.colorName = SUIT_copyString (cname);
    val.blackOnMonochrome = SUIT_stringsMatch (bw, "black");
    LEAVE (3, (buf, "leaving ReadColor(%s, %x)\n", buffer, *error));
    SUIT_free (copy);
    return SUIT_copyData ((Pointer) &val, sizeof (GP_color));
}



PRIVATE void si_destroyColor (Pointer value)
{
    GP_color *c = (GP_color *) value;
    /* SUIT_free (c->colorName); */
    SUIT_free (c);
}



PRIVATE Pointer si_copyColor (Pointer value)
{
    GP_color *c = (GP_color *) value;
    GP_color *newc = (GP_color *) SUIT_copyData (value, sizeof (GP_color));
    newc->colorName = SUIT_createRCString (c->colorName);
    return (Pointer) newc;
}


PRIVATE char *si_writeText (Pointer val)
{
    return (char *) val;
}



PRIVATE Pointer si_readText (char *buffer, boolean *error)
{
    *error = FALSE;
    return (Pointer) SUIT_createRCString (buffer);
}




/* pausch: this is not done with a reference counted string
   because we're gambling that people will be setting text properties
   and then setting them to other values (think type in box), and we
   want to be able to reclaim the storage - reference counted strings
   never get freed up 
*/

PRIVATE Pointer si_copyText (Pointer val)
{
    return (Pointer) SUIT_copyString( (char *) val);
}



PRIVATE int si_compareViewports (Pointer firstPtr, Pointer secondPtr)
{
    SUIT_viewport *first = (SUIT_viewport *) firstPtr;
    SUIT_viewport *second = (SUIT_viewport *) secondPtr;

    return ((first->bottom_left.x != second->bottom_left.x) ||
	    (first->bottom_left.y != second->bottom_left.y) ||
	    (first->top_right.x != second->top_right.x) ||
	    (first->top_right.y != second->top_right.y));
}



boolean SUIT_viewportsEqual (SUIT_viewport first, SUIT_viewport second)
{
    return (si_compareViewports (&first, &second) == 0);
}



PRIVATE Pointer si_readViewport (char *buffer, boolean *error)
{
    SUIT_viewport *val;

    val = (SUIT_viewport *) SUIT_malloc (sizeof (SUIT_viewport));
    *error = (sscanf (buffer, "%d %d %d %d", &(val->bottom_left.x),
	   &(val->bottom_left.y), &(val->top_right.x), &(val->top_right.y)) != 4);
	    
    if ((val->bottom_left.x > val->top_right.x) ||
	(val->bottom_left.y > val->top_right.y)) {
        SUIT_type *viewportType = si_getType("viewport");
	val = (SUIT_viewport *) SUIT_copyData ((SUIT_viewport *) (viewportType->defaultValue()), sizeof (SUIT_viewport));
	*error = TRUE;
    }
    return (Pointer) val;
}



PRIVATE char *si_writeViewport (Pointer val)
{
    int i1 = ((SUIT_viewport *) val)->bottom_left.x;
    int i2 = ((SUIT_viewport *) val)->bottom_left.y;
    int i3 = ((SUIT_viewport *) val)->top_right.x;
    int i4 = ((SUIT_viewport *) val)->top_right.y;

    sprintf (retvalBuffer, "%d %d %d %d", i1, i2, i3, i4);

    return retvalBuffer;
}



PRIVATE Pointer si_copyViewport (Pointer val)
{
    return SUIT_copyData (val, sizeof (SUIT_viewport));
}



PRIVATE Pointer si_readWindow (char *buffer, boolean *error)
{
    GP_rectangle *val;

    val = (GP_rectangle *) SUIT_malloc (sizeof (GP_rectangle));
    *error = (sscanf (buffer, "%lf %lf %lf %lf", &(val->bottom_left.x),
	   &(val->bottom_left.y), &(val->top_right.x), &(val->top_right.y)) != 4);
    if ((val->bottom_left.x > val->top_right.x) ||
	(val->bottom_left.y > val->top_right.y)) {
        SUIT_type *windowType = si_getType (WINDOW);
	val = (GP_rectangle *) SUIT_copyData ((GP_rectangle *) (windowType->defaultValue()), sizeof (GP_rectangle));
    }
    return (Pointer) val;
}



PRIVATE char *si_writeWindow (Pointer val)
{
    double i1 = ((GP_rectangle *) val)->bottom_left.x;
    double i2 = ((GP_rectangle *) val)->bottom_left.y;
    double i3 = ((GP_rectangle *) val)->top_right.x;
    double i4 = ((GP_rectangle *) val)->top_right.y;

    sprintf (retvalBuffer, "%f %f %f %f", i1, i2, i3, i4);
    return retvalBuffer;
}



PRIVATE int si_compareWindows (Pointer firstPtr, Pointer secondPtr)
{
    GP_rectangle *first = (GP_rectangle *) firstPtr;
    GP_rectangle *second = (GP_rectangle *) secondPtr;

    return ((first->bottom_left.x != second->bottom_left.x) ||
	    (first->bottom_left.y != second->bottom_left.y) ||
	    (first->top_right.x != second->top_right.x) ||
	    (first->top_right.y != second->top_right.y));
}



PRIVATE Pointer si_copyWindow (Pointer val)
{
    return SUIT_copyData (val, sizeof (GP_rectangle));
}



PRIVATE char *si_writeSUITobject (Pointer val)
{
    sprintf (retvalBuffer, "SUIT_object (%s)", OBJECT_NAME (*(SUIT_object *) val));
    return retvalBuffer;
}



PRIVATE Pointer si_readSUITobject (char *unused, boolean *error)
{
    ASSERT (FALSE, (mes, "Cannot read this application's .sui file as it contains a reference to a\n\
*     property of type SUIT_object.  The last time this application was executed this\n\
*     property must have been made permanent, thereby causing all the data associated\n\
*     with it to be saved to the .sui file upon exit.  Properties of type SUIT_object\n\
*     MUST be temporary."));

    *error = TRUE;
    return NULL;
}



PRIVATE int si_compareSUITobjects (Pointer firstPtr, Pointer secondPtr)
{
    return (* (SUIT_object *) firstPtr) - (* (SUIT_object *) secondPtr);
}



PRIVATE Pointer si_copySUITObject (Pointer val)
{
    return SUIT_copyData (val, sizeof (SUIT_object));
}



char *si_writeFunctionPtr (Pointer val)
{
    char *fname = SUIT_getCallbackByPtr (*((SUIT_callbackFunctionPtr *) val));
    return (fname == NULL)? "function ptr" : fname;
}



Pointer si_readFunctionPtr (char *buffer, boolean *error)
{
    SUIT_callbackFunctionPtr fptr;
    fptr = SUIT_getCallbackByName (buffer);
    *error = (fptr == NULL);
    return SUIT_copyData (&fptr, sizeof (SUIT_callbackFunctionPtr));
}



PRIVATE int si_compareFunctionPtrs (Pointer firstPtr, Pointer secondPtr)
{
    SUIT_functionPointer one = * (SUIT_functionPointer*)firstPtr;
    SUIT_functionPointer two = * (SUIT_functionPointer*)secondPtr;
    return (unsigned long)one - (unsigned long)two;
}



PRIVATE Pointer si_copyFunctionPointer (Pointer val)
{
    return SUIT_copyData (val, sizeof (SUIT_functionPointer));
}



PRIVATE char *si_writeDynArrayPtr (Pointer unused)
{
    return "dynarray ptr";
}



PRIVATE Pointer si_readDynArrayPtr (char *unused, boolean *error)
{
    ASSERT (FALSE, (mes, "Cannot read this application's .sui file as it contains a reference to a\n\
*     property of type DynArray.  The last time this application was executed this\n\
*     property must have been made permanent, thereby causing all the data associated\n\
*     with it to be saved to the .sui file upon exit.  Properties of type Dynarray\n\
*     MUST be temporary."));

    *error = TRUE;
    return NULL;
}



PRIVATE int si_compareDynArrayPtrs (Pointer firstPtr, Pointer secondPtr)
{
    DynArray *firstDP = (DynArray *) firstPtr;
    DynArray *secondDP = (DynArray *) secondPtr;
    DynArray first;
    DynArray second;

    /* if either is null, then return that they're different if one is
     * non-null */
    if ((firstDP == NULL) || (secondDP == NULL)) {
	if ((firstDP == NULL) && (secondDP == NULL))
	    return 0;
	else
	    return 1;
    }
    first = *firstDP;
    second = *secondDP;

    if ((first == NULL) || (second == NULL)) {
	if ((first == NULL) && (second == NULL))
	    return 0;
	else
	    return 1;
    }
    if (first == second)
	return 0;
    else {

	/* I believe this is not doing what we want it to.  This tells us if
	 * two different dydnarrays have changed.  We want to replace a
	 * dynarray with another if it is not the same array. */

	if ((second->changed) || (first->changed)) {
	    second->changed = FALSE;
	    first->changed = FALSE;
	    return (1);
	} else
	    return (0);
    }

    /* I changed this so that we could check the status bit also - Nat */
    /* return !DynEqual (first, second);  */
}



PRIVATE Pointer si_copyDynArray (Pointer val)
{
    return SUIT_copyData (val, sizeof (DynArray));
}



PRIVATE int si_compareSpringinesses (Pointer firstPtr, Pointer secondPtr)
{
    return (* (SUIT_springiness *) firstPtr) - (* (SUIT_springiness *) secondPtr);
}



PRIVATE char *si_writeSpringiness (Pointer val)
{
    ASSERT ((val != VOIDNULL), (mes, "WriteSpringiness was called with void or null pointer (val).\n"));
    sprintf (retvalBuffer, "%d", (*(int *) val));
    return (retvalBuffer);
}



PRIVATE Pointer si_readSpringiness (char *buffer, boolean *error)
{
    int value;
    ASSERT ((buffer != CHARNULL),
	    (mes, "ReadSpringiness was called with null character string (buffer).\n"));
    *error = (sscanf (buffer, "%d", &value) != 1);
    return (SUIT_copyData (&value, sizeof (int)));
}



PRIVATE Pointer si_copySpringiness (Pointer val)
{
    return SUIT_copyData (val, sizeof (SUIT_springiness));
}



PRIVATE Pointer si_readFont (char *buffer, boolean *error)
{
    int matches;
    char family[75], style[75];
    double pnt;
    GP_font thefont;

    ENTER (3, (buf, "entering ReadFont(%s, %x)\n", buffer, *error));
    matches = sscanf (buffer, "%[^,],%[^,],%lf", family, style, &pnt);
    if (matches == 1) {
	matches = 1 + sscanf (buffer, "%[^,],,%lf", family, &pnt);
	strcpy (style, "");
    }
    if (matches == 3) {
	thefont.family = SUIT_createRCString (family);
	thefont.style = SUIT_createRCString (style);
	thefont.pointSize = pnt;
	*error = FALSE;
    } else
	*error = TRUE;
    LEAVE (3, (buf, "leaving ReadFont(%s, %x)\n", buffer, *error));
    return (SUIT_copyData ((Pointer) & thefont, sizeof (GP_font)));
}



PRIVATE char *si_writeFont (Pointer val)
{
    GP_font thefont;

    ASSERT ((val != NULL), (mes, "si_writeFont was called with null pointer.\n"));
    thefont = *(GP_font *) val;
    sprintf (retvalBuffer, "%s,%s,%f", thefont.family, thefont.style, thefont.pointSize);
    return retvalBuffer;
}



PRIVATE void si_destroyFont (Pointer val)
{
    GP_font thefont;
    
    ASSERT ((val != NULL), (mes, "si_destroyFont was called with null pointer.\n"));
    thefont = *(GP_font *) val;
    /* SUIT_free (thefont.family); */
    /* SUIT_free (thefont.style); */
}



PRIVATE int si_compareFonts (Pointer ptr1, Pointer ptr2)
{
    GP_font font1, font2;
    int retval;
    
    ASSERT ((ptr1 != NULL), (mes, "si_compareFonts was called with null pointer.\n"));
    ASSERT ((ptr2 != NULL), (mes, "si_compareFonts was called with null pointer.\n"));
    font1 = *(GP_font *) ptr1;
    font2 = *(GP_font *) ptr2;
    if ((retval = SUIT_caseInsensitiveCompare (font1.family, font2.family)) == 0)
	if ((retval = SUIT_caseInsensitiveCompare (font1.style, font2.style)) == 0)
	    retval = font1.pointSize - font2.pointSize;
    return retval;
}



PRIVATE Pointer si_copyFont (Pointer val)
{
    GP_font thefont, *retval;
    
    retval = (GP_font *) SUIT_copyData (val, sizeof (GP_font));
    ASSERT ((val != NULL), (mes, "si_copyFont was called with null pointer.\n"));
    thefont = *(GP_font *) val;
    retval->family = SUIT_createRCString (thefont.family);
    retval->style = SUIT_createRCString (thefont.style);
    return (Pointer) retval;
}



PRIVATE Pointer si_readEnum (char *buffer, boolean *error)
{
    char *delim = "\"{";
    char *str, *copy;
    char *current;
    SUIT_enum new;
    int i;
    
    copy = SUIT_copyString(buffer);
    *error = FALSE;
    current = strtok (copy, delim);
    str = strtok (NULL, delim);  /* get rid of the 'of' noise word */
    
    new.choices = DynCreate (sizeof(char *), 1);
    for (i=0; (str = strtok(NULL, delim)) != NULL; i++) {
	char *mycopy = SUIT_createRCString(str);
	DynAdd (new.choices, (void *) &mycopy);
	if (SUIT_stringsMatch(mycopy,current))
	    new.currentChoice = i;
	strtok (NULL, delim);   /* throw away the spaces in between */
    }
    
    *error = (DynSize(new.choices) == 0 ||
	      new.currentChoice < 0 ||
	      new.currentChoice > DynHigh(new.choices));
    
    SUIT_free (copy);
    return SUIT_copyData ((Pointer)&new, sizeof(SUIT_enum));
}



PRIVATE char *si_writeEnum (Pointer val)
{
    SUIT_enum e;
    char buf[300];
    int i;
    
    ASSERT ((val != NULL), (mes, "si_writeFont was called with null pointer.\n"));
    e = * (SUIT_enum *) val;
    ASSERT ((e.currentChoice >= 0), (mes, "SUIT_writeEnum was given an enum with a bad current choice (%d)\n",e.currentChoice));
    ASSERT ((DynSize(e.choices) > 0), (mes, "SUIT_writeEnum was given an enum with no choices\n"));
    if (e.currentChoice == -1)
	sprintf (retvalBuffer, "{}");
    else {
	sprintf (buf, "\"%s\" of {", *(char **)DynGet(e.choices,e.currentChoice));
	strcpy (retvalBuffer, buf);
	for (i=DynLow(e.choices); i <= DynHigh(e.choices); i++) {
	    sprintf (buf, "\"%s\" ", * (char **) DynGet(e.choices, i));
	    strcat (retvalBuffer, buf);
	}
	retvalBuffer[strlen(retvalBuffer)-1] = '}';
    }
    return retvalBuffer;
}



PRIVATE int CompareStringLists (DynArray arr1, DynArray arr2)
{
    int retval;

    /* pausch hack */
    if ( (arr1 == NULL) && (arr2 != NULL) )
	return(-1);

    if ( (arr1 != NULL) && (arr2 == NULL) )
	return(1);

    if (arr1 == arr2 && (arr1->changed || arr2->changed)) {
	arr1->changed = FALSE;
	arr2->changed = FALSE;
	return 1;
    }
    if ((retval = DynSize(arr1) - DynSize(arr2)) == 0) {
	int i;
	for (i=DynLow(arr1); retval == 0 && i <= DynHigh(arr1); i++) {
	    char *choice1 = * (char **) DynGet(arr1, i);
	    char *choice2 = * (char **) DynGet(arr2, i);
	    retval = SUIT_caseInsensitiveCompare (choice1, choice2);
	}
    }
    /*
    if (retval == 0 && (arr1->changed || arr2->changed)) {
	arr1->changed = FALSE;
	arr2->changed = FALSE;
	return 1;
    }
    */
    return retval;
}



PRIVATE int si_compareEnums (Pointer ptr1, Pointer ptr2)
{
    SUIT_enum e1, e2;
    
    ASSERT ((ptr1 != NULL), (mes, "si_compareEnums was called with null pointer.\n"));
    ASSERT ((ptr2 != NULL), (mes, "si_compareEnums was called with null pointer.\n"));
    e1 = * (SUIT_enum *) ptr1;
    e2 = * (SUIT_enum *) ptr2;
    if (e1.currentChoice != e2.currentChoice)
	return e1.currentChoice - e2.currentChoice;
    return CompareStringLists (e1.choices, e2.choices);
}



PRIVATE void si_destroyEnum (Pointer val)
{
    SUIT_enum e;
    ASSERT ((val != NULL), (mes, "si_destroyEnum was called with null pointer.\n"));
    e = * (SUIT_enum *) val;


 /* pausch hack: can't destroy it since some other enum might point to this */
/*
    if ( e.choices != NULL )
	DynDestroy (e.choices); 
*/
    e.currentChoice = -1;
    SUIT_free (val);
}



PRIVATE Pointer si_copyEnum (Pointer val)
{
    SUIT_enum e, *retval;
    int i;
    
    retval = (SUIT_enum *) SUIT_malloc (sizeof(SUIT_enum));
    ASSERT ((val != NULL), (mes, "si_copyEnum was called with null pointer.\n"));
    e = * (SUIT_enum *) val;
    retval->currentChoice = e.currentChoice;
    retval->choices = DynCreate (sizeof(char *), 1);
    for (i=DynLow(e.choices); i <= DynHigh(e.choices); i++) {
	char *temp = SUIT_createRCString (*(char **)DynGet(e.choices, i));
	DynAdd (retval->choices, (void *) &temp);
    }
    return (Pointer)retval;
}



Pointer si_readTextList (char *buffer, boolean *error)
{
    char *copy = SUIT_copyString(buffer), *ptr, *line, *temp;
    SUIT_textList new = SUIT_defTextList (NULL, 0);

    for (line = copy; (ptr = strchr (line, '\n')) != NULL; line = ptr+1) {
	ptr[0] = '\0';
	temp = SUIT_copyString(line);
	DynAdd (new, (void *)&temp);
    }
    if (line[0] != '\0') {
	temp = SUIT_copyString(line);
	DynAdd (new, (void *)&temp);
    }
    SUIT_free(copy);
    *error = FALSE;
    return SUIT_copyData ((Pointer)&new, sizeof(SUIT_textList));
}



char *si_writeTextList (Pointer val)
{
    SUIT_textList list;
    int i, total=0;

    ASSERT ((val != NULL), (mes, "si_writeTextList was called with null pointer.\n"));
    list = * (SUIT_textList *) val;
    strcpy (retvalBuffer, "");
    for (i=0; i < DynSize(list); i++) {
	char *item = * (char **) DynGet(list, i);
	total += strlen(item)+1;
	if (total > retvalBufferSize-10)
	    retvalBuffer = SUIT_realloc (retvalBuffer, retvalBufferSize += 1000);
	strcat (retvalBuffer, item);
	strcat (retvalBuffer, "\n");
    }
    return retvalBuffer;
}



PRIVATE int si_compareTextLists (Pointer ptr1, Pointer ptr2)
{
    ASSERT ((ptr1 != NULL), (mes, "si_compareTextLists was called with null pointer (arg1).\n"));
    ASSERT ((ptr2 != NULL), (mes, "si_compareTextLists was called with null pointer (arg2).\n"));
    return CompareStringLists (*(SUIT_textList*)ptr1, *(SUIT_textList*)ptr2);
}



PRIVATE void si_destroyTextList (Pointer val)
{
    SUIT_textList list;
    int i;
    ASSERT ((val != NULL), (mes, "si_destroyTextList was called with null pointer.\n"));
    list = * (SUIT_textList *) val;
    for (i=0; i < DynSize(list); i++) {
	char *entry = SUIT_itemInTextList (list, i);
	SUIT_free (entry);
    }
    DynDestroy (list);
    SUIT_free (val);
}



PRIVATE Pointer si_copyTextList (Pointer val)
{
    SUIT_textList given, new;
    ASSERT ((val != NULL), (mes, "si_copyTextList was called with null pointer.\n"));
    given = * (SUIT_textList *) val;
    new = SUIT_copyTextList (given);
    return SUIT_copyData ((Pointer)&new, sizeof(SUIT_textList));
}



PRIVATE void destroySimpleType (Pointer val)
{
    SUIT_free (val);
}



void SUIT_registerType (char *name,
			Pointer (*readproc) (char *, boolean *),
			char *(*writeproc) (Pointer),
			int (*compareproc) (Pointer, Pointer),
			void (*destroyproc) (Pointer),
			Pointer (*copyproc) (Pointer),
			Pointer (*defaultproc) (void),
			char *widgetClass)
{
    SUIT_type temp;

    ENTER (1, (buf, "SUIT_registerType(%s,...)\n", name));
    ASSERT ((name != NULL && !SUIT_stringsMatch (name, "")),
	    (mes, "SUIT_registerType was called with null non-existent string (name).\n"));

    temp.name = SUIT_createRCString (name);
    temp.convertFromAscii = readproc;
    temp.convertToAscii = writeproc;
    temp.compare = compareproc;
    temp.destroy = destroyproc;
    temp.copy = copyproc;
    temp.defaultValue = defaultproc;
    temp.widgetClass = widgetClass;
    DynAdd (global.types, (void *) &temp);
    LEAVE (1, (buf, "SUIT_registerType(%s,...)\n", name));
}


PRIVATE Pointer defaultInt(void) { static int t = 0 ; return((Pointer)&t); }

PRIVATE Pointer defaultDouble(void) { static double t = 0.0 ; return((Pointer)&t); }

PRIVATE Pointer defaultBoolean(void) { static boolean t = FALSE ; return((Pointer)&t); }


PRIVATE Pointer defaultColor(void) {
    static GP_color t;
    t = GP_defColor("black", BLACK_ON_MONO);
    return((Pointer)&t);
}


PRIVATE Pointer defaultViewport(void) {
    static SUIT_viewport t;
    t = SRGP_defRectangle (50, 50, 212, 150);
    return((Pointer)&t);
}


PRIVATE Pointer defaultWindow(void) {
    static GP_rectangle t;
    t = GP_defRectangle (0.0, 0.0, 1.0, 1.0);
    return((Pointer)&t);
}


PRIVATE Pointer defaultNullPointer (void) { static Pointer ptr = (Pointer) NULL; return &ptr; }

PRIVATE Pointer defaultSpringiness (void) { static SUIT_springiness t = ALL_SPRINGINESS ; return &t; }


PRIVATE Pointer defaultText (void)
{
    static char	*t = "";
    return( (Pointer)t );	/* this shouldn't be &t --Rob */
}


PRIVATE Pointer defaultFont (void)
{ 
    static GP_font t; 
    t = GP_defFont("times", "", 12.0);
    return((Pointer)&t);
}


PRIVATE Pointer defaultEnum (void)
{
    char *s = "default value";
    static SUIT_enum t;
    t.choices = DynCreate(sizeof(char *), 1);
    DynAdd (t.choices, (void *) &s);
    t.currentChoice = 0;
    return((Pointer)&t);
}


PRIVATE Pointer defaultTextList (void)
{
    static SUIT_textList retval;
    retval = SUIT_defTextList (NULL, 0);	
    return (Pointer) &retval;
}


void si_registerInitialTypes (void)
{
    ENTER (2, (buf, "si_registerInitialTypes()\n"));
    
    retvalBuffer = (char *) SUIT_malloc(retvalBufferSize);

    SUIT_registerType ("int", si_readInteger, si_writeInteger, si_compareIntegers, destroySimpleType, si_copyInteger, defaultInt, "bounded value");

    SUIT_registerType ("GP_color", si_readColor, si_writeColor, si_compareColors, si_destroyColor, si_copyColor, defaultColor, "color chips");

    SUIT_registerType ("double", si_readDouble, si_writeDouble, si_compareDoubles, destroySimpleType, si_copyDouble, defaultDouble, "bounded value");

    SUIT_registerType ("text", si_readText, si_writeText, si_compareStrings, destroySimpleType, si_copyText, defaultText, "type in box");

    SUIT_registerType (VIEWPORT, si_readViewport, si_writeViewport, si_compareViewports, destroySimpleType, si_copyViewport, defaultViewport, NULL);

    SUIT_registerType (WINDOW, si_readWindow, si_writeWindow, si_compareWindows, destroySimpleType, si_copyWindow, defaultWindow, NULL);

    SUIT_registerType ("boolean", si_readBoolean, si_writeBoolean, si_compareBooleans, destroySimpleType, si_copyBoolean, defaultBoolean, "on/off switch");

    SUIT_registerType ("SUIT_object", si_readSUITobject, si_writeSUITobject, si_compareSUITobjects, destroySimpleType, si_copySUITObject, defaultNullPointer, NULL);

    SUIT_registerType ("SUIT_functionPointer", si_readFunctionPtr, si_writeFunctionPtr, si_compareFunctionPtrs, destroySimpleType, si_copyFunctionPointer, defaultNullPointer, "callback function panel");

    SUIT_registerType ("DynArray", si_readDynArrayPtr, si_writeDynArrayPtr, si_compareDynArrayPtrs, destroySimpleType, si_copyDynArray, defaultNullPointer, NULL);

    SUIT_registerType ("SUIT_springiness", si_readSpringiness, si_writeSpringiness, si_compareSpringinesses, destroySimpleType, si_copySpringiness, defaultSpringiness, "spring panel");

    SUIT_registerType ("GP_font", si_readFont, si_writeFont, si_compareFonts, si_destroyFont, si_copyFont, defaultFont, "font panel");

    SUIT_registerType ("SUIT_enum", si_readEnum, si_writeEnum, si_compareEnums, si_destroyEnum, si_copyEnum, defaultEnum, "radio buttons");

    SUIT_registerType ("SUIT_textList", si_readTextList, si_writeTextList, si_compareTextLists, si_destroyTextList, si_copyTextList, defaultTextList, "text editor");

    LEAVE (2, (buf, "si_registerInitialTypes()\n"));
}



PRIVATE callbackPair *DummyCallbackWithName (char *name)
{
    static callbackPair dummy;
    dummy.name = name;
    return (&dummy);
}




PRIVATE int CompareCallbackPairByName (void *first, void *second)
{
    callbackPair *f = (callbackPair *) first;
    callbackPair *s = (callbackPair *) second;
    return (CaseInsensitiveMatch (f->name, s->name));
}



SUIT_object SUIT_createCallbackFunctionPanel (char *name)
{
    SUIT_object retval;
    SUIT_textList list = SUIT_defTextList(NULL, 0);
    int i;

    retval = SUIT_createScrollableList (name, NULL);
    for (i=0; i < DynSize(global.callbackPtrs); i++) {
	callbackPair *pair = (callbackPair *) DynGet (global.callbackPtrs, i);
	SUIT_appendToTextList (list, pair->name);
    }
    SUIT_setTextList (retval, LIST, list);
    return retval;
}



void SUIT_registerCallbackPtr (char *functionName, SUIT_callbackFunctionPtr func)
{
    callbackPair f;

    f.name = SUIT_createRCString (functionName);
    f.ptr = func;
    DynAdd (global.callbackPtrs, (Pointer) & f);
}



SUIT_callbackFunctionPtr SUIT_getCallbackByName (char *functionName)
{
    SUIT_callbackFunctionPtr f;
    int slot = DynFindIndex (global.callbackPtrs, DummyCallbackWithName (functionName), CompareCallbackPairByName);
    if (slot != DYN_NOT_FOUND) {
	f = ((callbackPair *) DynGet (global.callbackPtrs, slot))->ptr;
	return f;
    } else
	return NULL;
}



char *SUIT_getCallbackByPtr (SUIT_callbackFunctionPtr functionPtr)
{
    int slot;

    for (slot = 0; slot < DynSize(global.callbackPtrs); slot++) {
	callbackPair * cb = ((callbackPair *) DynGet (global.callbackPtrs, slot));
	if (cb->ptr == functionPtr)
	    return cb->name;
    }
    return NULL;
}


GP_rectangle SUIT_defWindow (double x1, double y1, double x2, double y2)
{
    return GP_defRectangle (x1, y1, x2, y2);
}


SUIT_viewport SUIT_defViewport (int x1, int y1, int x2, int y2)
{
    return SRGP_defRectangle (x1, y1, x2, y2);
}
