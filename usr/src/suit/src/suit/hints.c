/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "privsuit.h"

#define CHANGED_CLASS    "changed class"


#if defined(SUN) || defined(SGI_X)
extern int fputc (int, FILE *);
#endif

/* pausch: this is what he wanted.  He may running on a B/W screen now, so we
 * need to remember what he wanted so we can put it in the .sui file. */

PRIVATE int originallyRequestedDepth;
PRIVATE int SUITversion = 0;
PRIVATE int SUITrevision = 0;

typedef struct hint_prop_str {
    char *className;
    char *objectName;
    char *displayName;
    property prop;
} hintFileEntry;


PRIVATE char *BigString = NULL;


PRIVATE void FlushQuotedString (void)
{
    SUIT_free ((void *)BigString);
}



PRIVATE char *ReadQuotedString (FILE * f)
{
#define QUOTED_STRING_SIZE  500
    int i, c;
    char ch;

    c = fgetc (f);
    ASSERT ((c != EOF), (mes, "ReadQuotedString: EOF was true!\n"));
    ch = (char) c;
    ASSERT ((ch == '"'), (mes, "ReadQuotedString: string doesn't start with a quote\n"));

    if (BigString != NULL)
	FlushQuotedString();	/* get rid of whatever was there before */

    BigString = (char *) SUIT_malloc(QUOTED_STRING_SIZE );
    i = 0;

    while ((c = fgetc (f)) != '"') {
	ASSERT ((c != EOF), (mes, "ReadQuotedString: EOF was true!\n"));
	ch = (char) c;
	if (ch == '\\') {
	    c = fgetc (f);
	    ch = (char) c;
	}
	if (i > 0 && i % QUOTED_STRING_SIZE == 0) /* enough room left? */
	    BigString = (char *) SUIT_realloc ((void *)BigString, i+QUOTED_STRING_SIZE); /* make more room */
	BigString[i++] = ch;
    }
    if (i > 0 && i % QUOTED_STRING_SIZE == 0) /* enough room for the '\0'? */
	BigString = (char *) SUIT_realloc ((void *)BigString, i+1); /* make more room */
    BigString[i] = '\0';
    return BigString;
}



PRIVATE void FlushUntil (FILE * f, char ch)
{
    int c;
    while ((c = fgetc (f)) != ch)
	if (c == EOF)
	    fprintf (stderr, "error reading .sui file\n");
}


PRIVATE void readMakeFromFile (FILE * f)
{
    char name[100];
    char class[100];
    char parent[100];
    int ch;
    boolean interactive = (SUITversion >= 2 && SUITrevision >= 3)? FALSE : TRUE;
    SUIT_object obj, parentObj;

    FlushUntil (f, '(');
    strcpy (name, ReadQuotedString (f));
    fgetc (f);			/* skip over the comma */
    strcpy (class, ReadQuotedString (f));
    fgetc (f);			/* skip over the comma */
    strcpy (parent, ReadQuotedString (f));
    if (SUITversion >= 2 && SUITrevision >= 3) {
	fgetc (f);			/* skip over the comma */
	ch = fgetc (f);
	interactive = (ch == '1');
    }
    FlushUntil (f, '\n');

    obj = SUIT_name (name);
    parentObj = SUIT_name (parent);
    if (parentObj) {
	if (obj != NULL && !SUIT_stringsMatch(class, OBJECT_CLASS(obj))) {
	    fprintf (stderr, "**  Warning: SUIT object \"%s\" was of class \"%s\"\n", OBJECT_NAME(obj), class);
	    fprintf (stderr, "**  and now is of class \"%s\". SUIT will therefore ignore\n", OBJECT_CLASS(obj));
	    fprintf (stderr, "**  its properties stored in the .sui file, since they are\n");
	    fprintf (stderr, "**  likely to be inappropriate for this new class.\n");
	    SUIT_setBoolean (obj, CHANGED_CLASS, TRUE);
	    SUIT_makePropertyTemporary (obj, CHANGED_CLASS, OBJECT);
	}
	if (obj == NULL && interactive) /* object doesn't exist and was interactively created */
	    obj = SUIT_createObjectByClass (name, class);	/* create it again */
	if (interactive) {
	    SUIT_setBoolean (obj, INTERACTIVELY_CREATED, TRUE);
	    SUIT_makePropertyTemporary (obj, INTERACTIVELY_CREATED, OBJECT);
	}
	if (obj)
	    si_addChildToObject (parentObj, obj, FALSE);
    }
}



PRIVATE void readSetFromFile (FILE * f)
{
    char objOrClass[100];
    char propName[100];
    char propType[100];
    SUIT_object obj;
    SUIT_type *type;
    boolean locked;
    char *retval;
    SUIT_level level;
    int ch;
    char *value;
    boolean errorStatus;

    FlushUntil (f, '(');
    strcpy (objOrClass, ReadQuotedString (f));
    fgetc (f);			/* skip over the comma */
    strcpy (propName, ReadQuotedString (f));
    fgetc (f);			/* skip over the comma */
    strcpy (propType, ReadQuotedString (f));
    fgetc (f);			/* skip over the comma */

    ch = fgetc (f);
    if (ch == '1')
	level = CLASS;
    else
	level = OBJECT;

    fgetc (f);			/* skip over the comma */
    ch = fgetc (f);
    locked = (ch == '1');
    fgetc (f);			/* skip over the comma */
    value = ReadQuotedString (f);
    FlushUntil (f, '\n');

    if (level == CLASS)
	obj = SUIT_dummyObjectInClass (objOrClass);
    else
	obj = SUIT_name (objOrClass);

    if (obj == NULL)		/* if this object doesn't exist, */
	return;			/* toss the property on the floor */

    if (SUIT_getBoolean (obj, CHANGED_CLASS)) /* the object has changed class */
	return;			/* toss the property on the floor */

    type = si_getType (propType);
    retval = type->convertFromAscii (value, &errorStatus);

    if (errorStatus == FALSE)
	SUIT_setProperty (obj, propName, propType, retval, level);

    if (locked)
	SUIT_lockProperty (obj, propName, level);
}



PRIVATE void DoReadHints (FILE * f)
{
    int c;

    FlushUntil (f, '@');	/* put in there just so we could parse it */
    FlushUntil (f, '\n');

    while ((c = fgetc (f)) != '\n') {
	switch (c) {
	  case 'M':
	    readMakeFromFile (f);
	    break;
	  case 'S':
	    readSetFromFile (f);
	    break;
	  default:
	    ASSERT (FALSE, (mes, "error parsing .sui file: line begins with: '%c'\n", (char) c));
	    break;
	}
    }
}



void si_readAndProcessHints (char *filename)
{
    FILE *infile;

    infile = fopen (filename, "r");

    if (infile != ((FILE *) NULL)) {
	DoReadHints (infile);
	FlushQuotedString();	/* flush a big buffer we've been keeping */
	fclose (infile);
    }
}



/*------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------*/

/* put backslashes before the double quotes and backslashes: */
PRIVATE void OutputStringQuotingSpecialChars (FILE * file, char *s)
{
    while (*s != '\0') {
	if ((*s == '"') || (*s == '\\'))
	    fputc ('\\', file);
	fputc ((int) *s, file);
	s++;
    }

}

PRIVATE void DumpSingleProperty (FILE * file, property * p, char *level, SUIT_object o, char *class)
{
    char *objName;

    if (p->permanent != PERMANENT)
	return;

    ASSERT (!SUIT_stringsMatch (level, "GLOBAL"), (mes, "DumpSingleProperty got a global property"));

    if (SUIT_stringsMatch (level, "CLASS"))
	objName = class;
    else			/* object level */
	objName = OBJECT_NAME (o);

    fprintf (file, "SET(\"");
    OutputStringQuotingSpecialChars (file, objName);
    fprintf (file, "\",\"");
    OutputStringQuotingSpecialChars (file, p->name);
    fprintf (file, "\",\"");
    OutputStringQuotingSpecialChars (file, p->type);
    fprintf (file, "\",%d,%d,\"", SUIT_stringsMatch ("CLASS", level), p->locked);
    {
	char *s = (si_getType (p->type))->convertToAscii (p->value);
	OutputStringQuotingSpecialChars (file, s);
    }
    fprintf (file, "\");\n");

}


PRIVATE void DumpCreationAndParentCodeRecursively (FILE * file, SUIT_object o)
{
    int i;

    if (OBJECT_PERMANENT (o)) {
	SUIT_object p = SUIT_getParent (o);

	if (p != NULL) {
	    fprintf (file, "MAKE(\"");
	    OutputStringQuotingSpecialChars (file, OBJECT_NAME (o));
	    fprintf (file, "\",\"");
	    OutputStringQuotingSpecialChars (file, OBJECT_CLASS (o));
	    fprintf (file, "\",\"");
	    OutputStringQuotingSpecialChars (file, OBJECT_NAME (p));
	    fprintf (file, "\",%d);\n", (int) SUIT_getBoolean (o, INTERACTIVELY_CREATED));
	}
	for (i = 0; i < SUIT_numberOfChildren(o); i++) {
	    SUIT_object child = SUIT_getChild (o, i);
	    DumpCreationAndParentCodeRecursively (file, child);
	}

    }
}


PRIVATE void DumpCreationAndParentCode (FILE * file)
{
    DumpCreationAndParentCodeRecursively (file, global.root);
}



PRIVATE void DumpClassCode (FILE * file, DynArray propertyList, char *class)
{
    property *p;
    int i;

    for (i = DynLow (propertyList); i <= DynHigh (propertyList); i++) {
	p = (property *) DynGet (propertyList, i);
	if (p->permanent != TEMPORARY)
	    DumpSingleProperty (file, p, "CLASS", NULL, class);
    }

}


PRIVATE void DumpPropertyCode (FILE * file, DynArray list)
{
    int i, j;


    for (i = DynLow (global.classProperties); i <= DynHigh (global.classProperties); i++) {
	SUIT_class *class = *(SUIT_class **) DynGet (global.classProperties, i);
	DumpClassCode (file, class->props, class->name);
    }

    for (i = DynLow (list); i <= DynHigh (list); i++) {
	SUIT_object o = *((SUIT_object *) DynGet (list, i));

	for (j = 0; j < DynSize (o->props); j++) {
	    property *p = (property *) DynGet (o->props, j);
	    DumpSingleProperty (file, p, "OBJECT", o, OBJECT_CLASS (o));
	}

    }
}



void si_dumpInitializationCode (FILE * file)
{

    DynArray list;

    fprintf (file, "/* SUIT version %d.%d */\n\n", SUIT_VERSION, SUIT_REVISION);
    fprintf (file, "#define THE_SCREEN_WIDTH %d\n",
	     SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL));
    fprintf (file, "#define THE_SCREEN_HEIGHT %d\n",
	     SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL));
    fprintf (file, "#define THE_SCREEN_DEPTH %d\n",
	     originallyRequestedDepth);

    fprintf (file, "\n#include \"suit.h\"\n\n\n");

    fprintf (file, "/*--------------------------------------------------------------------------------*/\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*                                  NOTE:                                         */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*    This file contains all the permanent properties of this application's       */\n");
    fprintf (file, "/*    objects and is read in as a data file.  Compiling this file as part of      */\n");
    fprintf (file, "/*    your application \"hard codes\" your interface.  Please see \"shipping\"        */\n");
    fprintf (file, "/*    in the SUIT reference manual for further information.                       */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*                              !! DANGER !!                                      */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*      This file is machine-generated, and its contents are very important.      */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*          If you must edit this file, do so with extreme caution!               */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*                              !! DANGER !!                                      */\n");
    fprintf (file, "/*                                                                                */\n");
    fprintf (file, "/*--------------------------------------------------------------------------------*/\n\n\n");

    fprintf (file, "extern void SUIT_interiorInitFromCode (char *programName, SUIT_functionPointer suiRoutine,\n");
    fprintf (file, "                                       int width, int height, int depth);\n\n");

    fprintf (file, "extern SUIT_type *si_getType(char *typeName);\n\n");

    fprintf (file, "extern void si_addChildToObject(SUIT_object, SUIT_object, boolean);\n\n");

    /* pausch: putting an fprintf on every line is ugly, and in theory I
     * could do this better, but I'm not risking the chance that some C
     * compiler will support the fancier ways. */

    fprintf (file, "static void MAKE (char *name, char *class, char *parent, boolean interactive) \n");
    fprintf (file, "{ \n");
    fprintf (file, "    SUIT_object o = SUIT_name(name); \n");
    fprintf (file, "    SUIT_object p = SUIT_name(parent); \n");
    fprintf (file, "    if (p != NULL) {\n");
    fprintf (file, "        if (o == NULL) \n");
    fprintf (file, "            o = SUIT_createObjectByClass(name, class); \n");
    fprintf (file, "    	if (interactive) {\n");
    fprintf (file, "    	    SUIT_setBoolean (o, INTERACTIVELY_CREATED, TRUE);\n");
    fprintf (file, "    	    SUIT_makePropertyTemporary (o, INTERACTIVELY_CREATED, OBJECT);\n");
    fprintf (file, "    	}\n");
    fprintf (file, "        si_addChildToObject(SUIT_name(parent), o, FALSE); \n");
    fprintf (file, "    }\n");
    fprintf (file, "} \n");
    fprintf (file, " \n\n\n");

    fprintf (file, "static void SET (char *objOrClass, char *propertyName, char *propertyType, boolean atClass,\n");
    fprintf (file, "                 boolean locked, char *stringValue) \n");
    fprintf (file, "{ \n");
    fprintf (file, "    SUIT_type	*type; \n");
    fprintf (file, "    boolean	errorStatus; \n");
    fprintf (file, "    Pointer	retval; \n");
    fprintf (file, "    SUIT_level level = OBJECT; \n");
    fprintf (file, "    SUIT_object o; \n");
    fprintf (file, "     \n");
    fprintf (file, "    if (atClass) \n");
    fprintf (file, "        level = CLASS; \n");

    fprintf (file, "    if (level == CLASS) \n");
    fprintf (file, "        o = SUIT_dummyObjectInClass(objOrClass); \n");
    fprintf (file, "    else \n");
    fprintf (file, "        o = SUIT_name(objOrClass); \n");
    fprintf (file, "    type = si_getType(propertyType); \n");
    fprintf (file, "    retval = type->convertFromAscii(stringValue, &errorStatus); \n");
    fprintf (file, "    if (errorStatus == FALSE) \n");
    fprintf (file, "	SUIT_setProperty(o, propertyName, propertyType, retval, level); \n");
    fprintf (file, "\n");
    fprintf (file, "    if (locked) \n");
    fprintf (file, "        SUIT_lockProperty(o, propertyName, level); \n");
    fprintf (file, "}\n\n\n");

    fprintf (file, "static void INIT_suiRoutine(void)\n{\n");

    list = si_createObjectList ();	/* creates list of all perm objects */


    fprintf (file, "/* This line is for parsing simplicity -- do NOT remove it!  @ */\n");
    DumpCreationAndParentCode (file);
    DumpPropertyCode (file, list);

    fprintf (file, "\n\n} /* end of INIT_suiRoutine */\n\n\n\n");

    fprintf (file, "void SUIT_initFromCode (char *programName)\n{\n");

    fprintf (file, "    SUIT_interiorInitFromCode (programName, INIT_suiRoutine, THE_SCREEN_WIDTH,\n");
    fprintf (file, "                               THE_SCREEN_HEIGHT, THE_SCREEN_DEPTH);\n");

    fprintf (file, "}\n");
}



void WriteHintsToFile (FILE * file)
{
    ENTER (2, (buf, "WriteHintsToFile\n"));
    si_dumpInitializationCode (file);
    LEAVE (2, (buf, "WriteHintsToFile\n"));
}



PRIVATE void CreateObjectListRecursively (DynArray objectList, SUIT_object o)
{
    int i;
    SUIT_display *display;
    SUIT_object child, employee;


    if (OBJECT_PERMANENT (o)) {
	DynAdd (objectList, (void *) &o);
	for (i = 0; i < SUIT_numberOfChildren (o); i++) {
	    child = SUIT_getChild (o, i);
	    CreateObjectListRecursively (objectList, child);
	}
	display = si_getDisplay (o, SUIT_getEnumString (o, ACTIVE_DISPLAY));
	if (display != NULL) {
	    for (i = 0; i < SUIT_numberOfEmployees (o, display->name); i++) {
		employee = SUIT_getEmployee (o, display->name, i);
		CreateObjectListRecursively (objectList, employee);
	    }
	}
    }
}



DynArray si_createObjectList (void)
{
    DynArray objectList;

    objectList = DynCreate (sizeof (SUIT_object), 1);

    CreateObjectListRecursively (objectList, global.root);

    return objectList;
}



/*------------------------------------------------------------------------------------------*/


char *si_makeFileName (char *str1, char *original)
{
    char *end;

    ASSERT ((original != NULL), (mes, "si_makeFileName was called with an invalid string (original).\n"));
    ENTER (2, (buf, "si_makeFileName('%s', '%s')\n", str1, original));

    end = strrchr (original, (int) '.');
    if (SUIT_stringsMatch (end, ".EXE"))	/* remove the .EXE extension
						 * under DOS */
	end[0] = '\0';
    strcpy (str1, original);
    strcat (str1, ".sui");
    LEAVE (2, (buf, "si_makeFileName('%s', '%s')\n", str1, original));
    return (str1);
}



PRIVATE void DoPeek (int *width, int *height, int *depth)
{
    FILE *f;
    char s[100], s2[100];

    if ((f = fopen (global.hintsfile, "r")) == NULL) {
	*width = DEFAULT_SCREEN_WIDTH;
	*height = DEFAULT_SCREEN_HEIGHT;
	*depth = DEFAULT_SCREEN_DEPTH;
    } else {
	char buffer[100];
	int ver, rev;
	fgets (buffer, 100, f);
	if (sscanf (buffer, "/* %s %s %d.%d */", s, s2, &ver, &rev) == 4) {
	    SUITversion = ver;
	    SUITrevision = rev;
	    fgets (buffer, 100, f); /* skip the blank line */
	    fgets (buffer, 100, f); /* get the screen width line */
	}
	if ((sscanf (buffer, "%s %s %d\n", s, s2, width) != 3))
	    fprintf (stderr, "trouble reading hintsfile: couldn't read screen width\n");
	if ((fscanf (f, "%s %s %d\n", s, s2, height) != 3))
	    fprintf (stderr, "trouble reading hintsfile: couldn't read screen height\n");
	if ((fscanf (f, "%s %s %d\n", s, s2, depth) != 3))
	    fprintf (stderr, "trouble reading hintsfile: couldn't read screen depth\n");
	fclose (f);
    }
    originallyRequestedDepth = *depth;
}



void PeekAtHintsFile (int *width, int *height, int *depth)
{
    DoPeek (width, height, depth);
}
