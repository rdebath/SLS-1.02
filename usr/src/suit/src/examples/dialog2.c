/***************************************
*  dialog2.c --
*  
*       Exampleof the use of data validation functions
*       in the call to SUIT_createOKCancelDialogBox()
*       
***************************************** */

#include "suit.h"



/* ------------------------------
 * data validation function that makes sure 
 * that the string in the type in box 
 * is not empty 
 * ------------------------------
 */
   
boolean dataOK(SUIT_object typeIn)
{
    char *str;

    printf("in here\n");
    str = SUIT_getText(typeIn, CURRENT_VALUE);
    printf ("---%s---\n", str);

    if (strlen(str) == 0) {
 	SUIT_inform("You must fill in the text string"); 
	return FALSE;
    } else {
	return TRUE;
    }
}


/* 
 * ----------------------------------------
 * SUIT_createOKCancelDialogBox():
 * 
 * If you don't understand OKCancelDialogBoxes(), 
 * read over dialog1.c
 * 
 * Here, we use a data validation function to make
 * sure that the string value in the typein box
 * has a value (is not blank). 
 * 
 * ----------------------------------------*/
void ShowOKCancelDialog (SUIT_object obj)
{
    SUIT_object typeIn, dBox;
    char *str;

    typeIn = SUIT_createTypeInBox("fred", NULL);

    dBox = SUIT_createOKCancelDialogBox("anita", typeIn, dataOK);  
    SUIT_activateDialogBox(dBox);			     

    switch (SUIT_getInteger(dBox, "button pressed")) {	     
      case REPLY_CANCEL:
	printf("User selected CANCEL\n"); 
	break;
      case REPLY_OK:					     
	str = SUIT_getText(typeIn, CURRENT_VALUE);
	printf("Selected OK. String = %s\n", str);
	break;
    }
    SUIT_destroyObject(dBox);				     
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createButton("Hit me for SUIT_createOKCancelDialogBox()", 
		      ShowOKCancelDialog); 
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
