/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#define MY_BUTTON    "my button"
#define MY_PLACEMAT  "my placemat"

#define RETURN_KEY   '\r'

PRIVATE DynArray DialogStack = NULL;


PRIVATE SUIT_object GrabReturn (SUIT_object over, SUIT_event *e)
{
    SUIT_object currentDBox;

    if (DialogStack == NULL)
	return over;
    
    currentDBox = * (SUIT_object *) DynGet (DialogStack, DynHigh(DialogStack));
    if (!SUIT_getBoolean (currentDBox, GRAB_RETURN_KEY))
	return over;
    
    if (e->type == KEYSTROKE && e->keyboard == RETURN_KEY) { /* the user hit Return (Enter) */
	SUIT_object okButton = SUIT_name (SUIT_relativeName(currentDBox, "ok button"));
	e->worldLocation.x = 0.5;
	e->worldLocation.y = 0.5;
	return okButton;
    }
    return over;
}



PRIVATE void PutApplicationOnHold (SUIT_object o)
{
    DynArray objectList = DynCreate(sizeof (SUIT_object), 1);
    static boolean firsttime = TRUE;

    DynAdd (objectList, (void *) &o);
    if (firsttime) {
	DialogStack = DynCreate(sizeof(SUIT_object), 1);
	firsttime = FALSE;
    }
    DynAdd (DialogStack, (void *) &o);

    if (SUIT_getBoolean (o, GRAB_RETURN_KEY))
	SUIT_registerTrapper (GrabReturn);

    while (!SUIT_getBoolean (o, FINISHED))
	SUIT_limitedCheckAndProcessInput(INDEFINITE, objectList);

    if (SUIT_getBoolean (o, GRAB_RETURN_KEY))
	SUIT_unregisterTrapper ();
    SUIT_performRedisplay();
    DynDelete (DialogStack, DynHigh(DialogStack));
    DynDestroy (objectList);
}



Reply SUIT_activateDialogBox (SUIT_object o)
{
    SUIT_setBoolean (o, VISIBLE, TRUE);  /* explicitly make it visible */
    SUIT_deluxeSetBoolean (o, FINISHED, FALSE, OBJECT);
      SUIT_makePropertyTemporary (o, FINISHED, OBJECT);
    SUIT_bringToFront(o);

    PutApplicationOnHold (o);

    return (Reply) SUIT_getInteger (o, BUTTON_PRESSED);
}



PRIVATE void YesButtonCallback (SUIT_object o)
{
    SUIT_object parent = SUIT_getParent(o);
    SUIT_suspendMarkingRedisplay(parent);
    SUIT_setInteger (parent, BUTTON_PRESSED, REPLY_YES);
    SUIT_deluxeSetBoolean (parent, FINISHED, TRUE, OBJECT);
    SUIT_makePropertyTemporary (parent, FINISHED, OBJECT);
    SUIT_resumeMarkingRedisplay(parent);
}


PRIVATE void NoButtonCallback (SUIT_object o)
{
    SUIT_object parent = SUIT_getParent(o);
    SUIT_suspendMarkingRedisplay(parent);
    SUIT_setInteger (parent, BUTTON_PRESSED, REPLY_NO);
    SUIT_deluxeSetBoolean (parent, FINISHED, TRUE, OBJECT);
    SUIT_makePropertyTemporary (parent, FINISHED, OBJECT);
    SUIT_resumeMarkingRedisplay(parent);
}


PRIVATE void CancelButtonCallback (SUIT_object o)
{
    SUIT_object parent = SUIT_getParent(o);
    SUIT_suspendMarkingRedisplay(parent);
    SUIT_setInteger (parent, BUTTON_PRESSED, REPLY_CANCEL);
    SUIT_deluxeSetBoolean (parent, FINISHED, TRUE, OBJECT);
    SUIT_makePropertyTemporary (parent, FINISHED, OBJECT);
    SUIT_resumeMarkingRedisplay(parent);
}


PRIVATE void OKButtonCallback (SUIT_object o)
{
    SUIT_object parent = SUIT_getParent(o);
    SUIT_validationFunction dataOK;

    SUIT_suspendMarkingRedisplay(parent);
    dataOK = (SUIT_validationFunction) SUIT_deluxeGetFunctionPointer(parent, VALIDATION_FUNCTION,
								     OBJECT);
    if (dataOK != NULL) {
	SUIT_object innards = SUIT_getChild(parent, 3);
	if (!dataOK(innards))
	    return;
    }
    SUIT_setInteger (parent, BUTTON_PRESSED, REPLY_OK);
    SUIT_deluxeSetBoolean (parent, FINISHED, TRUE, OBJECT);
    SUIT_makePropertyTemporary (parent, FINISHED, OBJECT);
    SUIT_resumeMarkingRedisplay(parent);
}


PRIVATE SUIT_object AddMessageButton(SUIT_object o, char *message)
{
    SUIT_object messageButton;
    int w, a, d;
    
    GP_pushGraphicsState();
    GP_setFont (SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
    GP_setViewport(SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
    GP_setWindow (OBJECT_WINDOW(o));
    GP_inquireTextExtentWithoutMapping (message, &w, &a, &d);
    GP_popGraphicsState();

    if (strchr(message,'\n') != NULL || w > 0.9*SUIT_deluxeGetInteger (NULL, SCREEN_WIDTH, GLOBAL)) {
	/* add a big text box */
	int width, height; 
	SUIT_viewport vp;

	messageButton = SUIT_createTextBox (SUIT_relativeName(o, "message"), message);
	SUIT_setFont (messageButton, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
	SUIT_setBoolean (messageButton, HAS_BORDER, FALSE);
	width = SUIT_deluxeGetInteger(NULL, SCREEN_WIDTH, GLOBAL);
	SUIT_optimizeSizeForText (messageButton, width/2);

	vp = OBJECT_VIEWPORT(messageButton);
	height = vp.top_right.y - vp.bottom_left.y;
	if (height > 0.95*SUIT_deluxeGetInteger (NULL, SCREEN_HEIGHT, GLOBAL)) {
            SUIT_optimizeSizeForText (messageButton, (int)(0.95*width));
	    vp = OBJECT_VIEWPORT(messageButton);
	    height = vp.top_right.y - vp.bottom_left.y;
	}
	SUIT_setSpringiness (messageButton, SPRINGINESS, LEFT_SPRINGINESS|BELOW_SPRINGINESS);
	SUIT_addChildToObject (o, messageButton);
    } else {
	/* just add a label */
	messageButton = SUIT_createLabel (SUIT_relativeName(o, "message"));
	SUIT_setText (messageButton, LABEL, message);
	SUIT_setBoolean (messageButton, HAS_BORDER, FALSE);
	SUIT_setBoolean (messageButton, SHRINK_TO_FIT, TRUE);
	SUIT_setFont (messageButton, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
	SUIT_setSpringiness (messageButton, SPRINGINESS, LEFT_SPRINGINESS|BELOW_SPRINGINESS);
	SUIT_addChildToObject (o, messageButton);
    }
    return messageButton;
}



PRIVATE char *CANCEL_STRING = "Cancel";
PRIVATE char *longest = NULL;

PRIVATE void LongestWord (char *word)
{
    longest = word;
}


PRIVATE void MakeStandardSize (SUIT_object o)
{
    int width, ascent, descent;
    int margin = SUIT_getInteger (o, MARGIN);
    char *label = SUIT_getText (o, LABEL);
    
    SUIT_setBoolean (o, SHRINK_TO_FIT, FALSE);
    GP_setFont (SUIT_getFont (o, FONT));
    SRGP_inquireTextExtent (longest, &width, &ascent, &descent);
    if (SUIT_stringsMatch(label, "OK"))
	SUIT_changeObjectSize (o, width+2*margin, ascent+descent+margin);
    else
	SUIT_changeObjectSize (o, width+4*margin, ascent+descent+margin);
}


PRIVATE SUIT_object AddYesButton (SUIT_object o, char *yesName)
{
    SUIT_object yesButton;
    
    yesButton = SUIT_createButton (SUIT_relativeName(o, "yes button"), YesButtonCallback);
    SUIT_addChildToObject (o, yesButton);
    SUIT_setText (yesButton, LABEL, yesName);
    MakeStandardSize (yesButton);
    SUIT_setSpringiness (yesButton, SPRINGINESS, ABOVE_SPRINGINESS|LEFT_SPRINGINESS);
    return yesButton;
}


PRIVATE SUIT_object AddNoButton (SUIT_object o, char *noName)
{
    SUIT_object noButton;
    
    noButton = SUIT_createButton (SUIT_relativeName(o, "no button"), NoButtonCallback);
    SUIT_addChildToObject (o, noButton);
    SUIT_setText (noButton, LABEL, noName);
    MakeStandardSize (noButton);
    SUIT_setSpringiness (noButton, SPRINGINESS, ABOVE_SPRINGINESS|LEFT_SPRINGINESS);
    return noButton;
}


PRIVATE SUIT_object AddCancelButton (SUIT_object o)
{
    SUIT_object cancelButton;
    
    cancelButton = SUIT_createButton (SUIT_relativeName(o, "cancel button"), CancelButtonCallback);
    SUIT_addChildToObject (o, cancelButton);
    SUIT_setText (cancelButton, LABEL, "Cancel");
    MakeStandardSize (cancelButton);
    SUIT_setSpringiness (cancelButton, SPRINGINESS, ABOVE_SPRINGINESS|LEFT_SPRINGINESS);
    return cancelButton;
}


PRIVATE void MovePlacemat (SUIT_object ok, SUIT_object placemat)
{
    int margin = SUIT_getInteger (ok, MARGIN);
    rectangle pvp, vp;

    vp = OBJECT_VIEWPORT(ok);
    pvp.bottom_left.x = vp.bottom_left.x - margin;
    pvp.bottom_left.y = vp.bottom_left.y - margin;
    pvp.top_right.x = vp.top_right.x + margin;
    pvp.top_right.y = vp.top_right.y + margin;
    SUIT_setViewport (placemat, VIEWPORT, pvp);
}


PRIVATE void MovePlacematCB (SUIT_object o, char *propname, char *proptype, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propname, VIEWPORT)) {
	if (SUIT_stringContains(OBJECT_NAME(o),"placemat") >= 0) /* This is the placemat. */
	    MovePlacemat (SUIT_getObject(o, MY_BUTTON), o);
	else /* This is the OK button */
	    MovePlacemat (o, SUIT_getObject(o, MY_PLACEMAT));
    }
}


PRIVATE SUIT_object AddOKButton (SUIT_object o)
{
    SUIT_object okButton, placemat;
    
    okButton = SUIT_createButton (SUIT_relativeName(o, "ok button"), OKButtonCallback);
    placemat = SUIT_createPlaceMat (SUIT_relativeName(o, "placemat"));
    SUIT_deluxeSetObject (okButton, MY_PLACEMAT, placemat, OBJECT);
    SUIT_makePropertyTemporary (okButton, MY_PLACEMAT, OBJECT);
    SUIT_deluxeSetObject (placemat, MY_BUTTON, okButton, OBJECT);
    SUIT_makePropertyTemporary (placemat, MY_BUTTON, OBJECT);
    SUIT_addChildToObject (o, okButton);
    SUIT_addChildToObject (o, placemat);
    SUIT_sendToBack (placemat);
    SUIT_registerInterest (okButton, MovePlacematCB);
    SUIT_registerInterest (placemat, MovePlacematCB);
    SUIT_setText (okButton, LABEL, "OK");
    MakeStandardSize (okButton);
    SUIT_setSpringiness (okButton, SPRINGINESS, ABOVE_SPRINGINESS|LEFT_SPRINGINESS);
    
    return okButton;
}



PRIVATE void EstablishDialogBoxSize (SUIT_object o)
{
    int wtotal = 0;
    int margin = SUIT_getInteger (o, "margin");
    int numKids = SUIT_numberOfChildren(o), i;
    SUIT_viewport mvp, bvp;
    int width, mheight, bheight, Height;
    double ratio;

    mvp = OBJECT_VIEWPORT(SUIT_getChild(o, 0));
    width = mvp.top_right.x - mvp.bottom_left.x + 2*margin;
    mheight = mvp.top_right.y - mvp.bottom_left.y;
    bvp = OBJECT_VIEWPORT (SUIT_getChild(o, 1));
    bheight = bvp.top_right.y - bvp.bottom_left.y;
    Height = mheight + bheight + 8*margin;
    ratio = (double)mheight / (double)Height;

    for (i=1; i < numKids; i++) {
	SUIT_viewport kvp;
	kvp = OBJECT_VIEWPORT(SUIT_getChild(o,i));
	wtotal += 2*(kvp.top_right.x - kvp.bottom_left.x);
    }
    if (wtotal > width)
	width = wtotal;

    SUIT_changeObjectSize (o, width, Height);
    SUIT_centerObjectOnScreen(o);

    SUIT_centerInParent (SUIT_getChild(o, 0), 0.5, 1.0-ratio/2.0);
    if (numKids == 3 && SUIT_stringsMatch(OBJECT_CLASS(SUIT_getChild(o,2)),"place mat"))
	SUIT_centerInParent (SUIT_getChild(o, 1), 0.75, 2.0*(1.0-ratio)/5.0);
    else
	for (i=1; i < numKids; i++)
	    SUIT_centerInParent (SUIT_getChild(o, i), (double)i*1.0/(double)numKids,
				 2.0*(1.0-ratio)/5.0);
}



PRIVATE SUIT_object SUIT_createDialogBox (char *name)
{
    SUIT_object o = SUIT_createObject (name, "dialog box");
    static boolean firsttime = TRUE;

    SUIT_addDisplayToObject (o, "bulletin board", SUIT_passEventDown, SUIT_paintChildren);
    SUIT_deluxeSetBoolean (o, FINISHED, FALSE, OBJECT);
    SUIT_makePropertyTemporary (o, FINISHED, OBJECT);
    SUIT_setBoolean (o, VISIBLE, FALSE);
    LongestWord(CANCEL_STRING);
    if (firsttime) {
	SUIT_setFont (o, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
	SUIT_deluxeSetEnumString (o, BORDER_TYPE, "fancy motif", CLASS);
	SUIT_deluxeSetInteger (o, BORDER_WIDTH, 8, CLASS);
	SUIT_deluxeSetBoolean (o, CACHE_USING_CANVAS, TRUE, CLASS);
	SUIT_deluxeSetBoolean (o, GRAB_RETURN_KEY, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, GRAB_RETURN_KEY, CLASS);
	SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, CAN_BE_OPENED, CLASS);
    }
    return o;
}



PRIVATE void PaintDBoxWithLine (SUIT_object o)
{
    int y, margin = SUIT_getInteger (o, MARGIN);
    GP_color border;
    rectangle vp;

    border = SUIT_getColor (o, BORDER_COLOR);
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));

    SUIT_paintChildren (o);

    if (SUIT_numberOfChildren(o) == 0)
	y = (vp.bottom_left.y + vp.top_right.y)/2;
    else {
	SUIT_viewport vp2;
	int height;
	vp2 = OBJECT_VIEWPORT(SUIT_getChild(o,0));
	height = vp2.top_right.y - vp2.bottom_left.y;
	y = height + 2*margin;
    }

    GP_setColor (GP_getShadowColor (border));
    SRGP_lineCoord (vp.bottom_left.x, vp.top_right.y - y, vp.top_right.x, vp.top_right.y - y);
    GP_setColor (GP_getHighlightColor (border));
    SRGP_lineCoord (vp.bottom_left.x, vp.top_right.y - y-1, vp.top_right.x, vp.top_right.y - y-1);
}



PRIVATE SUIT_object SUIT_createDialogBoxWithLine (char *name)
{
    SUIT_object o = SUIT_createObject (name, "dialog box");
    static boolean firsttime = TRUE;

    SUIT_addDisplayToObject (o, "bulletin board", SUIT_passEventDown, PaintDBoxWithLine);
    SUIT_deluxeSetBoolean (o, FINISHED, FALSE, OBJECT);
    SUIT_makePropertyTemporary (o, FINISHED, OBJECT);
    SUIT_setBoolean (o, VISIBLE, FALSE);
    LongestWord(CANCEL_STRING);
    if (firsttime) {
	SUIT_setFont (o, FONT, SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
	SUIT_deluxeSetEnumString (o, BORDER_TYPE, "fancy motif", CLASS);
	SUIT_deluxeSetInteger (o, BORDER_WIDTH, 8, CLASS);
	SUIT_deluxeSetBoolean (o, CACHE_USING_CANVAS, TRUE, CLASS);
	SUIT_deluxeSetBoolean (o, GRAB_RETURN_KEY, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, GRAB_RETURN_KEY, CLASS);
	SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, CLASS);
	SUIT_makePropertyTemporary (o, CAN_BE_OPENED, CLASS);
    }
    return o;
}



PRIVATE char *UniqueName (void)
{
    static int num = 1;
    static char buf[30];
    sprintf (buf, "dialog box %d", ++num);
    while (SUIT_name(buf) != NULL)
	sprintf (buf, "dialog box %d", ++num);
    return buf;
}



SUIT_object SUIT_createOKCancelDialogBox (char *name, SUIT_object innards,
					  SUIT_validationFunction dataOK)
{
    SUIT_object o = SUIT_createDialogBox (name);
    SUIT_object ok, cancel, placemat;
    SUIT_viewport cancelvp, okvp, oldvp;
    int border = SUIT_getInteger (o, MARGIN);
    int iwidth, iheight, width, height, dboxwidth, dboxheight;

    cancel = AddCancelButton (o);
    SUIT_setSpringiness (cancel, SPRINGINESS, LEFT_SPRINGINESS|BELOW_SPRINGINESS);

    ok = AddOKButton (o);
    placemat = SUIT_getObject(ok,MY_PLACEMAT);
    SUIT_setSpringiness (ok, SPRINGINESS, LEFT_SPRINGINESS|BELOW_SPRINGINESS);
    SUIT_setSpringiness (placemat, SPRINGINESS, LEFT_SPRINGINESS|BELOW_SPRINGINESS);

    SUIT_addChildToObject (o, innards);
    cancelvp = OBJECT_VIEWPORT(cancel);
    width = cancelvp.top_right.x - cancelvp.bottom_left.x;
    height = cancelvp.top_right.y - cancelvp.bottom_left.y;

    oldvp = OBJECT_VIEWPORT(innards);
    iwidth = oldvp.top_right.x - oldvp.bottom_left.x;
    iheight = oldvp.top_right.y - oldvp.bottom_left.y;
    iheight = MAX(2*height + 6*border, iheight);
    dboxwidth = iwidth + 11*border+width;
    dboxheight = iheight + 6*border;
    SUIT_changeObjectSize (o, dboxwidth, dboxheight);

    SUIT_setViewport (innards, VIEWPORT, SRGP_defRectangle (3*border, 3*border,
							    3*border+iwidth, 3*border+iheight));

    okvp = OBJECT_VIEWPORT(ok);
    width = okvp.top_right.x - okvp.bottom_left.x;
    height = okvp.top_right.y - okvp.bottom_left.y;
    SUIT_setViewport (ok, VIEWPORT, SRGP_defRectangle (dboxwidth - 5*border - width,
						       dboxheight - 4*border - height,
						       dboxwidth - 5*border,
						       dboxheight - 4*border));
    cancelvp = OBJECT_VIEWPORT(cancel);
    width = cancelvp.top_right.x - cancelvp.bottom_left.x;
    height = cancelvp.top_right.y - cancelvp.bottom_left.y;
    SUIT_setViewport (cancel, VIEWPORT, SRGP_defRectangle (dboxwidth- 4*border - width,
							   dboxheight- 7*border - 2*height,
							   dboxwidth - 4*border,
							   dboxheight - 7*border - height));

    SUIT_setFunctionPointer (o, VALIDATION_FUNCTION, (SUIT_functionPointer)dataOK);
    SUIT_centerObjectOnScreen (o);
    return o;
}



Reply SUIT_ask (char *message, char *button1Name, char *button2Name)
{
    SUIT_object o;
    int buttonPressed;
    
    o = SUIT_createDialogBoxWithLine (UniqueName());
    
    LongestWord ((strlen(button1Name)>strlen(button2Name))? button1Name : button2Name);
    AddMessageButton(o, message);
    AddYesButton(o, button1Name);
    AddNoButton(o, button2Name);

    EstablishDialogBoxSize(o);

    buttonPressed = SUIT_activateDialogBox (o);
    SUIT_destroyObject (o);
    return buttonPressed;
}



Reply SUIT_askWithCancel (char *message, char *button1Name, char *button2Name)
{
    SUIT_object o;
    int buttonPressed;
    
    o = SUIT_createDialogBoxWithLine (UniqueName());
    
    LongestWord ((strlen(button1Name)>strlen(button2Name))? button1Name : button2Name);
    AddMessageButton(o, message);
    AddYesButton(o, button1Name);
    AddNoButton(o, button2Name);
    AddCancelButton(o);

    EstablishDialogBoxSize(o);

    buttonPressed = SUIT_activateDialogBox (o);
    SUIT_destroyObject (o);
    return buttonPressed;
}



Reply SUIT_inform (char *message)
{  
    SUIT_object o;
    Reply buttonPressed;

    o = SUIT_createDialogBoxWithLine (UniqueName());

    AddMessageButton(o, message);
    AddOKButton(o);   
    
    EstablishDialogBoxSize(o);

    buttonPressed = SUIT_activateDialogBox (o);
    SUIT_destroyObject (o);
    return buttonPressed;
}


    
PRIVATE SUIT_object getStringTypeInBox = NULL;

PRIVATE SUIT_object getStringTrapper (SUIT_object intendedObject, SUIT_event *event)
{
    if (event->type == KEYSTROKE && event->keyboard != '\r')
	return getStringTypeInBox;
    else
	return intendedObject;
}



Reply SUIT_getString(char *message, char *defaultString, char answer[], int answerLength)
{
    SUIT_object insides, entryBox, o, mess;
    int height, width, mwidth, mheight, a, d, Width, Height;
    int buttonPressed;
    char *uniq = UniqueName();
    char *tempString;
    
    insides = SUIT_createBulletinBoardWithClass(uniq, "borderless bulletin board");
    SUIT_deluxeSetBoolean (insides, HAS_BORDER, FALSE, CLASS);
    
    entryBox = SUIT_createTypeInBox (SUIT_relativeName(insides, "type-in box"), NULL);
    SUIT_getObjectSize (entryBox, &width, &height);
    SUIT_setText (entryBox, CURRENT_VALUE, defaultString);
    GP_pushGraphicsState();
    GP_setFont (SUIT_getFont (entryBox, FONT));
    SRGP_inquireTextExtent (defaultString, &width, &a, &d);
    GP_popGraphicsState();
    SUIT_addChildToObject (insides, entryBox);

    mess = AddMessageButton(insides, message);
    SUIT_getObjectSize (mess, &mwidth, &mheight);
    Width = MAX(width, mwidth)+30;
    Height = height+mheight+10;
    SUIT_changeObjectSize (insides, Width, Height);
    SUIT_setViewport (mess, VIEWPORT, SUIT_defViewport(5,Height-height-5,Width-5,Height-5));
    SUIT_setViewport (entryBox, VIEWPORT, SUIT_defViewport(5,5,Width-5,5+height));

    o = SUIT_createOKCancelDialogBox (UniqueName(), insides, NULL);
    
    getStringTypeInBox = entryBox;
    SUIT_registerTrapper (getStringTrapper);

    buttonPressed = SUIT_activateDialogBox (o);
    SUIT_unregisterTrapper();
    
    tempString = SUIT_copyString(SUIT_getText(entryBox, CURRENT_VALUE));
    strncpy (answer, SUIT_getText(entryBox, CURRENT_VALUE), answerLength - 1);
    answer[answerLength - 1] = '\0';
    SUIT_destroyObject (o);
    return buttonPressed;
}

