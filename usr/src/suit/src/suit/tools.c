
/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "privsuit.h"
#if defined(SUN) || defined(SGI_X)
extern int  fputc(int, FILE *);
#endif


#define SUIT_COLOR  GP_defColor("grey",WHITE_ON_MONO)

typedef enum { NO_CHANGE, LEFT, BOTTOM, RIGHT, TOP, CENTER_H, CENTER_V } ARROW_DIRECTION;

static SUIT_object SUITMenu = NULL;

PRIVATE void DoTheIteration(SUIT_object o, void (*callback) (SUIT_object))
{
    int i;
    callback(o);
    for (i = 0; i < SUIT_numberOfChildren(o); i++)
	    DoTheIteration(SUIT_getChild (o, i), callback);
}

void SUIT_iterateOverObjects(void (*callback) (SUIT_object))
{
    DoTheIteration(global.root, callback);
}


PRIVATE void SUIT_changeObjectPos (SUIT_object o, int newpoint, ARROW_DIRECTION ptOfChange)
{
    rectangle vp;
    int height, width;

    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT(o));
    height = vp.top_right.y - vp.bottom_left.y;
    width = vp.top_right.x - vp.bottom_left.x;

    if (ptOfChange == NO_CHANGE)
	return;

    switch (ptOfChange) {
      case LEFT:
	vp.top_right.x = newpoint + width;
	vp.bottom_left.x = newpoint;
	break;
      case BOTTOM:
	vp.top_right.y = newpoint + height;
	vp.bottom_left.y = newpoint;
	break;
      case RIGHT:
	vp.bottom_left.x = newpoint - width;
	vp.top_right.x = newpoint;
	break;
      case TOP:
	vp.bottom_left.y = newpoint - height;
	vp.top_right.y = newpoint;
	break;
      case CENTER_H:
	vp.bottom_left.x = newpoint - width/2;
	vp.top_right.x = newpoint + width/2;
	break;
      case CENTER_V:
	vp.bottom_left.y = newpoint - height/2;
	vp.top_right.y = newpoint + height/2;
	break;
      default:
	ASSERT (FALSE, (mes, "SUIT_changeObjectPos was called with an invalid PTOFCHANGE.\n"));
	break;
    }
    SUIT_setViewport (o, VIEWPORT, SUIT_mapScreenToViewport (o, vp));
}



PRIVATE SUIT_object si_promptForObject (int cursor)
{
    deluxe_locator_measure current;
    SUIT_object openobj;

    GP_setCursor (cursor);
    GP_setInputMode (LOCATOR, EVENT);
    while (SRGP_waitEvent (INDEFINITE) != LOCATOR) ;  /* mouse down */
    while (SRGP_waitEvent (INDEFINITE) != LOCATOR) ;  /* mouse up */
    current = si_getLocatorMeasure ();
    if (global.sampleMode)
	si_inSample();
    GP_setCursor (STANDARD_CURSOR);
    openobj = si_mapPointToObject (OpenObjects, current.position.x, current.position.y);
    if ( openobj == NULL )
	return NULL; /* pausch bug fix */
    else
	return si_mapPointToObject (SUIT_getChildren (openobj), current.position.x, current.position.y);
}



PRIVATE void MakeSystemFontRec (SUIT_object obj)
{
    int i;
    SUIT_setFont (obj, FONT, SUIT_getFont (obj, SUIT_SYSTEM_FONT));
    for (i=0; i < SUIT_numberOfChildren(obj); i++)
	MakeSystemFontRec (SUIT_getChild(obj, i));
}



void si_commandAlign (SUIT_object unused) 
{
    static boolean canEnter = TRUE;
    SUIT_object Insides, horizRB, vertRB, horizL, vertL, alignL, Dbox;
    int w, a, d;
    Reply rep;
    char *h = NULL;
    char *v = NULL;

    if (!canEnter) {
	SUIT_inform ("Sorry, but the alignment tool is already active.");
	return;
    }

    if (DynSize (SelectedObjects) < 2) {
	SUIT_inform ("Align:\n\n\
Sorry, but you must select two or more objects to use the alignment tool.\n\nThere are two ways to select multiple objects; you can either\n\n Use SUIT-s to select each of the objects in turn;\n\n OR \n\n Use SUIT-click over an empty area. With the mouse button down, you can drag out a bounding rectangle. Objects that fall entirely inside the box will be selected.");
	return;
    }

    canEnter = FALSE;
    Insides = SUIT_createBulletinBoardWithClass ("align d-box", "borderless bulletin board");
    SUIT_deluxeSetBoolean (Insides, HAS_BORDER, FALSE, CLASS);

    alignL = SUIT_createLabel ("Align");
      SUIT_addChildToObject (Insides, alignL);
      SUIT_setViewport (alignL, VIEWPORT, SUIT_mapToParent (alignL, 0.05, 0.8, 0.15, 0.9));
    horizL = SUIT_createLabel ("Horizontal Alignment");
      SUIT_addChildToObject (Insides, horizL);
      SUIT_setViewport (horizL, VIEWPORT, SUIT_mapToParent (horizL, 0.05, 0.6, 0.45, 0.7));
    horizRB = SUIT_createRadioButtons ("horiz alignment", NULL);
      SUIT_addChildToObject (Insides, horizRB);
      SUIT_setViewport (horizRB, VIEWPORT, SUIT_mapToParent (horizRB, 0.05, 0.05, 0.45, 0.55));
      SUIT_addButtonToRadioButtons (horizRB, "No change");
      SUIT_addButtonToRadioButtons (horizRB, "Left sides");
      SUIT_addButtonToRadioButtons (horizRB, "L/R centers");
      SUIT_addButtonToRadioButtons (horizRB, "Right sides");
      SUIT_pressThisRadioButton (horizRB, "No change");
    vertL = SUIT_createLabel ("Vertical Alignment");
      SUIT_addChildToObject (Insides, vertL);
      SUIT_setViewport (vertL, VIEWPORT, SUIT_mapToParent (vertL, 0.55, 0.6, 0.95, 0.7));
    vertRB = SUIT_createRadioButtons ("vert alignment", NULL);
      SUIT_addChildToObject (Insides, vertRB);
      SUIT_setViewport (vertRB, VIEWPORT, SUIT_mapToParent (vertRB, 0.55, 0.05, 0.95, 0.55));
      SUIT_addButtonToRadioButtons (vertRB, "No change");
      SUIT_addButtonToRadioButtons (vertRB, "Tops");
      SUIT_addButtonToRadioButtons (vertRB, "T/B centers");
      SUIT_addButtonToRadioButtons (vertRB, "Bottoms");
      SUIT_pressThisRadioButton (vertRB, "No change");
    GP_setFont (SUIT_getFont (Insides, SUIT_SYSTEM_FONT));
    SRGP_inquireTextExtent ("Horizontal Alignment", &w, &a, &d);
    SUIT_changeObjectSize (Insides, (int)(2.2 * w), (int)(14 * (a + d)));

    Dbox = SUIT_createOKCancelDialogBox ("SUIT alignment dialog box", Insides, NULL);
    SUIT_setColor (Dbox, BACKGROUND_COLOR, SUIT_COLOR);
    SUIT_setColor (Dbox, BORDER_COLOR, SUIT_COLOR);
    MakeSystemFontRec (Dbox);

    if ((rep = SUIT_activateDialogBox (Dbox)) == REPLY_OK) {
	h = SUIT_getEnumString (horizRB, CURRENT_VALUE);
	v = SUIT_getEnumString (vertRB, CURRENT_VALUE);
    }

    SUIT_destroyObject (Dbox);
    if (rep == REPLY_OK) {
	SUIT_object last = * (SUIT_object *) DynGet (SelectedObjects, DynHigh(SelectedObjects));
	int i, HnewPt=0, VnewPt=0;
	ARROW_DIRECTION hdir, vdir;

	if (SUIT_stringsMatch (h, "Left sides")) {
	    hdir = LEFT;
	    HnewPt = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last)).bottom_left.x;
	} else if (SUIT_stringsMatch (h, "Right sides")) {
	    hdir = RIGHT;
	    HnewPt = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last)).top_right.x;
	} else if (SUIT_stringsMatch (h, "L/R centers")) {
	    SUIT_viewport vp;
	    vp = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last));
	    hdir = CENTER_H;
	    HnewPt = (vp.bottom_left.x + vp.top_right.x)/2;
	} else
	    hdir = NO_CHANGE;
	if (SUIT_stringsMatch (v, "Tops")) {
	    vdir = TOP;
	    VnewPt = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last)).top_right.y;
	} else if (SUIT_stringsMatch (v, "Bottoms")) {
	    vdir = BOTTOM;
	    VnewPt = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last)).bottom_left.y;
	} else if (SUIT_stringsMatch (v, "T/B centers")) {
	    SUIT_viewport vp;
	    vp = SUIT_mapViewportToScreen(last, OBJECT_VIEWPORT(last));
	    vdir = CENTER_V;
	    VnewPt = (vp.bottom_left.y + vp.top_right.y)/2;
	} else
	    vdir = NO_CHANGE;

	for (i=DynLow(SelectedObjects); i < DynHigh(SelectedObjects); i++) {
	    SUIT_object chase = * (SUIT_object *) DynGet(SelectedObjects, i);
	    SUIT_changeObjectPos (chase, HnewPt, hdir);
	    SUIT_changeObjectPos (chase, VnewPt, vdir);
	}
    }
    canEnter = TRUE;
}



PRIVATE void PerformCommand (void (*command)(SUIT_object))
{
    SUIT_object o;
    int i;

    if (DynSize (SelectedObjects) == 0) {
	if ((o = si_promptForObject (PROMPT_CURSOR)) != NULL)
	    command(o);
    } else
	for (i = DynLow (SelectedObjects); i <= DynHigh (SelectedObjects); i++) {
	    o = *(SUIT_object *) DynGet (SelectedObjects, i);
	    command(o);
	}
}


PRIVATE boolean FileExists (char *fileName)
{
    FILE *outFile;
    
    if ((outFile = fopen (fileName, "r")) != NULL) {
	fclose(outFile);
	return TRUE;
    }
    return FALSE;
}


PRIVATE void informUserOfFileCreationError (char *backup_or_hints) {
    
    fprintf (stderr, "Sorry, SUIT was unable to create %s file for this application.\n", backup_or_hints);
    fprintf (stderr, "   The most likely reasons being: \n");
    fprintf (stderr, "      there was not enough disk space available.\n"); 
    fprintf (stderr, "      there was not the appropriate write permission.\n");
}

PRIVATE void informUserOfNoChangeToInterface (void) {
    
    fprintf (stderr, "When you next execute this application it will appear as it did at the\n");
    fprintf (stderr, "beginning of the invocation just terminated.\n\n");
}

PRIVATE void informUserOfHowToRestoreOldHintsFile (char *fileName, char *backupFileName) {
    
    fprintf (stderr, "Your old .sui file may be found in %s.  If you copy this file to\n", backupFileName);
    fprintf (stderr, "%s, when you next execute this application it will appear as it\n", fileName);
    fprintf (stderr, "did at the beginning of the invocation just terminated.\n\n");
}

PRIVATE boolean CreateBackupFile (char *fileName, char *backupFileName)
{
   char *period;
   boolean retval = FALSE;

    period = strchr (backupFileName, '.');
    period[0] = '\0';
    strcat (backupFileName, ".bak");
    if (rename (fileName, backupFileName) == 0)  /* backup didn't exist */
	retval = TRUE;
    else {
    	remove (backupFileName);  /* backup did exist, erase it first */
    	if (rename (fileName, backupFileName) == 0)
    	    retval = TRUE;
    	else {
	    informUserOfFileCreationError ("a backup");
	}
    }
    return retval;
}


void si_writeHints (char *fileName)
{
    char *backupFileName = SUIT_createSafeString(fileName);
    FILE *outFile;

    ENTER (2, (buf, "si_writeHints(%s)\n", fileName));

    if (FileExists (fileName) && CreateBackupFile (fileName, backupFileName)) { /* make a backup copy */
	if ((outFile = fopen (fileName, "w")) == NULL) {
	    informUserOfFileCreationError ("the .sui");
	    informUserOfHowToRestoreOldHintsFile (fileName, backupFileName);
	} else {
	    WriteHintsToFile (outFile);
	    fclose (outFile);
	}
    } else if ((outFile = fopen (fileName, "w")) == NULL) {
	informUserOfFileCreationError ("the .sui");
	informUserOfNoChangeToInterface ();
    } else {
	WriteHintsToFile (outFile);
	fclose (outFile);
    }
    
    SUIT_free (backupFileName);
    LEAVE (2, (buf, "si_writeHints(%s)\n", fileName));
}


PRIVATE void PrepareToShip(SUIT_object unused)
{
/* pausch hack: 8/16/92: mac can't string constants over 512 chars long */
#ifdef MACINTOSH
    SUIT_inform("Prepare to ship:\n\nIf you are now ready to ship your application,"
"having a .sui file may be a nuisance. To build an application that needs no .sui file:\n\n"
"1) exit your program;\n"
"2) rename your .sui file to @b(suitinit.c);\n"
"3) change your call from\n"
"         @b(SUIT_init)\n"
"            to\n"
"         @b(SUIT_initFromCode);\n"
"4) compile suitinit.c and link it in with your application.\n\n"
"This \"@u(hard codes)\" the contents of your .sui file into your application - "
"a side effect is that SUIT's interactive tools will no longer be available to you. "
"If you ever want to develop your interface further, just rename the file back, "
"and change your init call back to @b(SUIT_init).");
#else
    SUIT_inform("Prepare to ship:\n\nIf you are now ready to ship your application, \
having a .sui file may be a nuisance. To build an application that needs no .sui file:\n\n\
1) exit your program;\n\
2) rename your .sui file to @b(suitinit.c);\n\
3) change your call from\n\
         @b(SUIT_init)\n\
            to\n\
         @b(SUIT_initFromCode);\n\
4) compile suitinit.c and link it in with your application.\n\n\
This \"@u(hard codes)\" the contents of your .sui file into your application - \
a side effect is that SUIT's interactive tools will no longer be available to you. \
If you ever want to develop your interface further, just rename the file back, \
and change your init call back to @b(SUIT_init).");
#endif
}


PRIVATE void CommandPropEd (SUIT_object unused)          { PerformCommand(si_editObject); }
PRIVATE void CommandInfo (SUIT_object unused)            { PerformCommand (si_giveInfo); }
PRIVATE void CommandCycle (SUIT_object unused)           { PerformCommand (SUIT_cycleObject); }
PRIVATE void CommandMoveToTop (SUIT_object unused)       { PerformCommand (SUIT_bringToFront); }
PRIVATE void CommandMoveToBottom (SUIT_object unused)    { PerformCommand (SUIT_sendToBack); }
PRIVATE void CommandDuplicateObject (SUIT_object unused) { PerformCommand (si_duplicateObject); }


void si_commandDestroyObject (SUIT_object unused)
{
    SUIT_object o;

    o = si_promptForObject (PIRATE_CURSOR);
    if (SUIT_stringsMatch (OBJECT_CLASS(o), "dialog box")) {
	GP_setCursor (STANDARD_CURSOR);
	SUIT_inform ("Please do not destroy the dialog box.");
	return;
    } else if ((global.propertyEditorIsActive == TRUE) && (o == EditorActiveWidget())) {
	GP_setCursor (STANDARD_CURSOR);
	SUIT_inform ("Please do @u(not) destroy the SUIT property editor.");
	return;
    } else if (o != NULL)
	SUIT_destroyObject (o);
    else GP_setCursor (STANDARD_CURSOR);
}



PRIVATE char *ProduceNewName (void)     /* Get unique name */
{
    static char newName[100];
    static int count;

    count = 0;
    sprintf (newName, "unnamed object %d", count);
    while (SUIT_name (newName) != NULL) {
	count++;
	sprintf (newName, "unnamed object %d", count);
    }
    return newName;
}



PRIVATE SUIT_object ClassesL;

/*
PRIVATE void SetClass (SUIT_object o)
{
    char buf[80];
    sprintf (buf, "Object class: @b(%s)", SUIT_getText (o, CURRENT_VALUE));
    SUIT_setText (ClassesL, LABEL, buf);
}
*/


PRIVATE boolean ClassIsNotValidForInteractiveCreation(char *classname)
{
    return (SUIT_stringsMatch(classname, "ROOT") ||
	    SUIT_stringsMatch(classname, "borderless bulletin board") ||
	    SUIT_stringsMatch(classname, "dialog box") ||
	    SUIT_stringsMatch(classname, "list") ||
	    SUIT_stringsMatch(classname, "callback function panel") ||
	    SUIT_stringsMatch(classname, "elevator") ||
	    SUIT_stringsMatch(classname, "billboard") ||
	    SUIT_stringsMatch(classname, "menu") ||
	    SUIT_stringsMatch(classname, "font panel") ||
	    SUIT_stringsMatch(classname, "creation tool") ||
	    SUIT_stringsMatch(classname, "property editor") ||
	    SUIT_stringsMatch(classname, "property list") ||
	    SUIT_stringsMatch(classname, "p.e.collection") ||
	    SUIT_stringsMatch(classname, "scrollable property list") ||
	    SUIT_stringsMatch(classname, "property editor collection") ||
	    SUIT_stringsMatch(classname, "radio buttons"));
}



void SUIT_centerObjectOnScreen(SUIT_object o)
{
    /* put it in the middle of the screen, just to be consistent */
    SUIT_changeObjectPos(o, SUIT_deluxeGetInteger(NULL, SCREEN_WIDTH, GLOBAL) / 2, CENTER_H);
    SUIT_changeObjectPos(o, SUIT_deluxeGetInteger(NULL, SCREEN_HEIGHT, GLOBAL) / 2, CENTER_V);
}



void SUIT_forceOnScreen (SUIT_object o)
{
    SUIT_viewport vp;
    int width, height;
    int swidth = SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL);
    int sheight = SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL);

    vp = OBJECT_VIEWPORT(o);
    width = vp.top_right.x - vp.bottom_left.x;
    height = vp.top_right.y - vp.bottom_left.y;

    if (width > swidth)
	width = 0.95*swidth;
    if (height > sheight)
	height = 0.95*sheight;
    SUIT_changeObjectSize (o, width, height);
}



void si_commandCreateObject (SUIT_object unused)
{
    static boolean canEnter = TRUE;
    static boolean firstTime = TRUE;
    SUIT_object Name, Classes, Dbox, NameL, Label, Insides;
    SUIT_object newObj = NULL;
    DynArray ClassNames = DynCreate (sizeof (char *), DynSize (global.classProperties));
    int i, w, a, d;
    char *newname, *newclass=NULL;
    boolean badname, badclass, cancel;

    if (!canEnter) {
	SUIT_inform ("Sorry, but the Create object tool is already active.");
	return;
    }
    canEnter = FALSE;
    Insides = SUIT_createBulletinBoardWithClass ("new object d-box", "borderless bulletin board");
    SUIT_deluxeSetBoolean (Insides, HAS_BORDER, FALSE, CLASS);

    for (i = DynLow (global.classProperties); i <= DynHigh (global.classProperties); i++) {
	char *classname = (* (SUIT_class **) DynGet (global.classProperties, i))->name;
	if ( ClassIsNotValidForInteractiveCreation(classname) )
	    continue;
	DynAdd (ClassNames, (void *) &classname);
    }

    Label = SUIT_createLabel ("Create");
      SUIT_addChildToObject (Insides, Label);
      SUIT_setBoolean (Label, SHRINK_TO_FIT, FALSE);
      SUIT_setEnumString (Label, JUSTIFICATION, "left");
      SUIT_setViewport (Label, VIEWPORT, SUIT_mapToParent (Label, 0.05, 0.90, 0.95, 0.95));
    Classes = SUIT_createScrollableList ("classes scrollbox", NULL);
      SUIT_setTextList (Classes, LIST, ClassNames);
      SUIT_addChildToObject (Insides, Classes);
      SUIT_setViewport (Classes, VIEWPORT, SUIT_mapToParent (Classes, 0.05, 0.30, 0.95, 0.75));
    ClassesL = SUIT_createLabel ("Object class");
      SUIT_addChildToObject (Insides, ClassesL);
      SUIT_setViewport (ClassesL, VIEWPORT, SUIT_mapToParent (ClassesL, 0.05, 0.80, 0.95, 0.85));
    Name = SUIT_createTypeInBox ("new name text box", NULL);
      SUIT_addChildToObject (Insides, Name);
      SUIT_setViewport (Name, VIEWPORT, SUIT_mapToParent (Name, 0.05, 0.05, 0.95, 0.15));
      newname = ProduceNewName();
      SUIT_setText (Name, CURRENT_VALUE, newname);
    NameL = SUIT_createLabel ("Object name");
      SUIT_addChildToObject (Insides, NameL);
      SUIT_setViewport (NameL, VIEWPORT, SUIT_mapToParent (NameL, 0.015, 0.14, 0.95, 0.25));
    GP_setFont (SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
    SRGP_inquireTextExtent ("Object class:", &w, &a, &d);
    SUIT_changeObjectSize (Insides, 2*w, 16*(a+d));

    Dbox = SUIT_createOKCancelDialogBox ("SUIT object creation dialog box", Insides, NULL);
    SUIT_forceOnScreen (Dbox);
    MakeSystemFontRec (Dbox);
    SUIT_setColor (Dbox, BACKGROUND_COLOR, SUIT_COLOR);
    SUIT_setColor (Dbox, BORDER_COLOR, SUIT_COLOR);

    do {
	if (cancel = (SUIT_activateDialogBox (Dbox) == REPLY_CANCEL))
	    break;
	newname = SUIT_getText (Name, CURRENT_VALUE);
	newclass = SUIT_getText (Classes, CURRENT_VALUE);
	if (badclass = SUIT_stringsMatch (newclass, ""))
	    SUIT_inform ("Sorry, but you did not select a class.");
	if (badname = SUIT_stringsMatch(newname, "")) {
	    char buf[80];
	    sprintf (buf, "Sorry, but you must provide a name.");
	    SUIT_inform (buf);
	}
	if (!badname && (badname = (SUIT_name(newname) != NULL))) {
	    char buf[80];
	    sprintf (buf, "Sorry, but an object named '%s' already exists.", newname);
	    SUIT_inform (buf);
	}
    } while (badclass || badname);

    if (!cancel)
    {
	newObj = SUIT_createObjectByClass(newname, newclass);
	SUIT_setBoolean (newObj, INTERACTIVELY_CREATED, TRUE);
	SUIT_makePropertyTemporary (newObj, INTERACTIVELY_CREATED, OBJECT);
	SUIT_centerObjectOnScreen(newObj);
    }
    SUIT_destroyObject (Dbox);
    if ( !cancel )
    {
	/* pausch addition */
	GP_pushGraphicsState();
	SRGP_setColor(1);
	SRGP_setWriteMode (WRITE_XOR);
	si_animateOverTime(si_animateScreenBorderToObject, newObj, firstTime ? 1000 : 500);
	GP_popGraphicsState();		       
	firstTime = FALSE;
    }
    canEnter = TRUE;
}



PRIVATE void HideSUITMenu (SUIT_object unused)
{
    if (SUITMenu != NULL)
	SUIT_setBoolean (SUITMenu, VISIBLE, FALSE);
}





SUIT_object SUIT_createPropEd (int level, char *objectName);


void si_quitLikeDoneButton (SUIT_object unused)
{ 
    switch (SUIT_askWithCancel ("Quit this application:\n\n\
Any information about the state of your application \
(e.g. widget locations, colors, fonts, etc.) that has been changed \
during this invocation will be lost if the .sui file is not saved \
before quitting.\n\nSave .sui file before quitting?", "Yes", "No")) {
      case REPLY_YES: SUIT_done(SAVE_SUI_FILE, EXIT_APPLICATION);  break;
      case REPLY_NO:  SUIT_done(DO_NOT_SAVE_SUI_FILE, EXIT_APPLICATION); break;
      default: break;
    }
}



PRIVATE void STRIP_si_repaintScreen(SUIT_object unused) { si_repaintScreen(); } 

PRIVATE void InitSUITMenu (void)
{
    SUITMenu = SUIT_createVerticalMenu ("SUIT system menu");

    SUIT_addToMenuWithHotKey (SUITMenu, "Align...", si_commandAlign, "SUIT-a");
    SUIT_addToMenuWithHotKey (SUITMenu, "Repaint all", STRIP_si_repaintScreen, "SUIT-r");
    SUIT_addToMenuWithHotKey (SUITMenu, "Bring to front", CommandMoveToTop, "SUIT-f");
    SUIT_addToMenuWithHotKey (SUITMenu, "Send to back", CommandMoveToBottom, "SUIT-b");
    SUIT_addToMenuWithHotKey (SUITMenu, "Create...", si_commandCreateObject, "SUIT-n");
    SUIT_addToMenuWithHotKey (SUITMenu, "Duplicate...", CommandDuplicateObject, "SUIT-d");
    SUIT_addToMenuWithHotKey (SUITMenu, "Cycle", CommandCycle, "SUIT-c");
    SUIT_addToMenuWithHotKey (SUITMenu, "Destroy", si_commandDestroyObject, "SUIT-x");
    SUIT_addToMenuWithHotKey (SUITMenu, "Get info...", CommandInfo, "SUIT-i");
    SUIT_addToMenuWithHotKey (SUITMenu, "Edit...", CommandPropEd, "SUIT-e");
    SUIT_addToMenuWithHotKey (SUITMenu, "Prepare to Ship...", PrepareToShip, "");
    SUIT_addToMenuWithHotKey (SUITMenu, "Quit this application...", si_quitLikeDoneButton, "SUIT-q");
    SUIT_addToMenuWithHotKey (SUITMenu, "Remove this menu", HideSUITMenu, "");

    SUIT_setColor (SUITMenu, BACKGROUND_COLOR, SUIT_COLOR);
    SUIT_setColor (SUITMenu, BORDER_COLOR, SUIT_COLOR);
    SUIT_setBoolean (SUITMenu, BORDER_RAISED, TRUE);
    SUIT_setSpringiness (SUITMenu, SPRINGINESS, ABOVE_SPRINGINESS|BELOW_SPRINGINESS|RIGHT_SPRINGINESS|LEFT_SPRINGINESS);
    MakeSystemFontRec (SUITMenu);
    OBJECT_PERMANENT(SUITMenu) = FALSE;

    /*
      the following is performed to trigger the interest function TileVertically()
      in menu.c

      dennis hack... july 9, 1992  5:17 am
    */
    if (TRUE) {
	/* the followed was changed to make the dec compiler happy... pausch hack */
	/* we couldn't say "SUIT_viewport orginial = OBJECT_VIEWPORT(SUITMenu) */
	SUIT_viewport original;
	SUIT_viewport temporary;
	original = OBJECT_VIEWPORT(SUITMenu);
	temporary.bottom_left.x = original.bottom_left.x - 1;
	temporary.bottom_left.y = original.bottom_left.x - 1;
	temporary.top_right.x = original.top_right.x - 1;
	temporary.top_right.y = original.top_right.y - 1;
	
	SUIT_setViewport (SUITMenu, VIEWPORT, temporary);
	SUIT_setViewport (SUITMenu, VIEWPORT, original);
    }
}



void si_showSUITMenu (point where)
{
    SUIT_viewport vp;
    int midx, midy, diffx, diffy;
    int width2, height2, screenwidth, screenheight;
    static boolean toolsShownYet = FALSE;

    if (!toolsShownYet) {
	InitSUITMenu ();
	toolsShownYet = TRUE;
    }

    vp = SUIT_mapViewportToScreen (SUITMenu, OBJECT_VIEWPORT(SUITMenu));
    width2 = (vp.top_right.x - vp.bottom_left.x)/2;
    height2 = (vp.top_right.y - vp.bottom_left.y)/2;
    screenwidth = SUIT_deluxeGetInteger(NULL,SCREEN_WIDTH, GLOBAL);
    screenheight = SUIT_deluxeGetInteger(NULL,SCREEN_HEIGHT, GLOBAL);

    if (where.x < width2+10)
	where.x = width2+10;
    else if (where.x > screenwidth-width2-10)
	where.x = screenwidth-width2-10;
    if (where.y < height2+10)
	where.y = height2+10;
    else if (where.y > screenheight-height2-10)
	where.y = screenheight-height2-10;
	
    midx = vp.bottom_left.x + width2;
    midy = vp.bottom_left.y + height2;
    diffx = midx - where.x;
    diffy = midy - where.y;
    vp.bottom_left.x -= diffx;
    vp.bottom_left.y -= diffy;
    vp.top_right.x -= diffx;
    vp.top_right.y -= diffy;
    SUIT_setViewport (SUITMenu, VIEWPORT, vp);
    SUIT_setBoolean (SUITMenu, VISIBLE, TRUE);
    SUIT_bringToFront (SUITMenu);
}



PRIVATE SUIT_object getPermanentObjectInClass (SUIT_object o, char *class)
{
    int i, j, k;
    SUIT_object temp, obj = NULL;
    SUIT_display disp;

    if (o == NULL)
	return NULL;
    else if (SUIT_stringsMatch (OBJECT_CLASS(o), class) && OBJECT_PERMANENT(o))
	return o;
    else {
	obj = NULL;
	for (i = 0; obj == NULL && i < SUIT_numberOfChildren (o); i++) {
	    temp = SUIT_getChild (o, i);
	    obj = getPermanentObjectInClass (temp, class);
	}
	for (j = 0; obj == NULL && j < DynSize (o->displays); j++) {
	    disp = *(SUIT_display *) DynGet (o->displays, j);
	    for (k = 0; obj == NULL && k < SUIT_numberOfEmployees (o, disp.name); k++)
		obj = getPermanentObjectInClass (SUIT_getEmployee (o, disp.name, k), class);
	}
	return obj;
    }
}



SUIT_object SUIT_getOneObjectFromClass (char *classname)
{
    return (getPermanentObjectInClass (global.root, classname));
}



PRIVATE SUIT_object CopyObject (char *name, char *class)
{
    SUIT_object o, chase;
    SUIT_display *disp;
    int i;

    chase = SUIT_getOneObjectFromClass (class);
    o = SUIT_createObject (name, class);
    if (chase != NULL) {
	for (i = DynLow (chase->displays); i <= DynHigh (chase->displays); i++) {
	    int j;
	    disp = (SUIT_display *) DynGet (chase->displays, i); 
	    SUIT_addDisplayToObject (o, disp->name, disp->hit, disp->paint);
	    for (j=0; j < SUIT_numberOfEmployees(chase, disp->name); j++) {
		SUIT_object emp = SUIT_getEmployee(chase, disp->name, j);
		char buf[100];
		sprintf (buf, "%s: employee %d\n",name, j);
		SUIT_addEmployeeToDisplay(o, disp->name, CopyObject(buf, OBJECT_CLASS(emp)));
	    }
	}
	if (chase->objectInterest != NULL) {
	    o->objectInterest = DynCreate (sizeof (SUIT_objectInterestCallback), 1);
	    for (i = 0; i < DynSize(chase->objectInterest); i++)
		DynAdd (o->objectInterest, DynGet(chase->objectInterest, i));
	}
    }
    return (o);
}



SUIT_object SUIT_createObjectByClass (char *ObjectName, char *ClassName)
{
    SUIT_object o = NULL;
    SUIT_createProcedure funct;
    property *CreateProcProp;
    SUIT_class *theList = si_searchForClass (ClassName);

    ASSERT ((theList != NULL), (mes, "SUIT_createObjectByClass given a non-existent class (%s).\n",ClassName));

    if (ObjectName == NULL)
	ObjectName = ProduceNewName ();
    
    CreateProcProp = FindProperty (theList->props, "object create procedure");
    if (CreateProcProp == NULL)
	o = CopyObject (ObjectName, ClassName);
    else {
	funct = (SUIT_createProcedure) CreateProcProp->value;
	if (funct != NULL)
	    o = funct (ObjectName);
    }
    return o;
}

