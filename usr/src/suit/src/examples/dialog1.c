/***************************************
*  DialogBoxes.c --
*      A demonstration program illustrating 
*      the use of SUIT's dialog boxes.
*      
*      The Dialog Boxes Demonstrated are:
*          SUIT_inform()
*          SUIT_askYesNo()
*          SUIT_askYesNoCancel()
*          SUIT_askOKCancel()
*          SUIT_ask()
*          SUIT_createOKCancelDialogBox()
*          
*          
****************************************          
*          IMPROTANT NOTES
****************************************          
*          
*       Dialog boxes always grab all input from the
*       user. You cannot interact with the application
*       until the dialog box has been answered.
*       
*       You should ALWAYS test against
*       the reply values for that particular box, not
*       logical true or false. For example:
*
*       WRONG:
*           if (SUIT_askOKCancel("really?")) {
*           ...
*       RIGHT
*           if (SUIT_askOKCancel("really?") == REPLY_OK) {
*           ...
* 
*       Besides making for easier to read code,     
*       the reason for this is that some of the "negative"
*       choices of the dialog boxes return a non-zero
*       value. Notice that all the examples in this code 
*       use switch statements, and explicitly test 
*       the value returned from the dialog box. 
*       You should do the same.
*       
****************************************          
*
* OUTPUT:
*      This program uses a label to display
*      the results of interacting with 
*      the SUIT dialog boxes. 
***************************************** */

#include "suit.h"
SUIT_object myLabel;

/*----------------------------------------
 * SUIT_inform():
 * 
 * The only thing the user can do while 
 * this box is on the screen is click the
 * OK button provided. Usually used for
 * warnings and important informational 
 * messages to the user
 *----------------------------------------*/
void ShowInform(SUIT_object obj)
{
  SUIT_inform("EXAMPLE MESSAGE: Printer Ready. Click OK to continue.");
  printf("User selected OK\n");
}


/* -----------------------------------------
 * SUIT_askYesNo():
 * 
 * This forces the user to
 * answer a yes/no question.
 * 
 * Returns: REPLY_YES
 *          REPLY_NO
 -----------------------------------------*/
void ShowYesNo (SUIT_object obj)
{
    switch (SUIT_askYesNo ("EXAMPLE MESSAGE: Are you sure?")) {
      case REPLY_YES:
	printf("User selected YES\n"); 
	break;
      case REPLY_NO:
	printf("User selected NO\n");
	break;
    }
}

/*----------------------------------------
 * SUIT askOKCancel():
 * 
 * This box also asks a yes no question,
 * where the response might sound odd if
 * expressed as a yes or no.
 * 
 * Returns: REPLY_OK
 *          REPLY_CANCEL
 * ----------------------------------------*/
void ShowAskOKCancel (SUIT_object obj)
{
    switch (SUIT_askOKCancel("EXAMPLE MESSAGE: Confirm -- About to transmit large file.")) {
      case REPLY_OK:
	printf("User selected OK\n"); 
	break;
      case REPLY_CANCEL:
	printf("User selected CANCEL\n");
	break;
    }
}




/* ----------------------------------------
 * SUIT_askYesNoCancel():
 * 
 * This dialog box asks a question, and lets
 * the user answer yes, a no, or allows the
 * user to dismiss the box with a cancel.
 * 
 * Returns: REPLY_YES
 *          REPLY_NO
 *          REPLY_CANCEL
 *          
 * ----------------------------------------*/
void ShowAskYesNoCancel (SUIT_object obj)
{ 
    switch (SUIT_askYesNoCancel ("EXAMPLE MESSAGE: Save changes before exiting?")) {
      case REPLY_YES:
	printf("User selected YES\n"); 
	break;
      case REPLY_NO:
	printf("User selected NO\n");
	break;
      case REPLY_CANCEL:
	printf("User selected CANCEL\n");
	break;
    }
}


/* 
 * ----------------------------------------
 * SUIT_ask():
 * 
 * This box asks a question where the answer 
 * is one of two choices.
 * 
 * Returns: REPLY_BUTTON1
 *             -- if the user picks choice #1
 *          REPLY_BUTTON2
 *             -- If the user picks choice #2
 *             
 * ----------------------------------------*/
void ShowAsk (SUIT_object obj)
{
    switch (SUIT_ask("EXAMPLE MESSAGE: Window seat or aisle?","Window","Aisle")) {
      case REPLY_BUTTON1:
	printf("User selected Window Seat\n"); 
	break;
      case REPLY_BUTTON2:
	printf("User selected Aisle Seat\n"); 
	break;

    }
}


/* ----------------------------------------
 * SUIT_askWithCancel():
 * 
 * This dialog box asks a question, and lets
 * the user answer with one of three responses: 
 * 
 * Returns: REPLY_BUTTON1
 *             -- if the user picks choice #1
 *          REPLY_BUTTON2
 *             -- If the user picks choice #2
 *          REPLY_CANCEL
 *             -- If the user presses cancel
 *          
 * ----------------------------------------*/

void ShowAskWithCancel(SUIT_object box)
{
    switch (SUIT_askWithCancel("Save changes before exiting?",
					 "Save", "Don't Save")) {
      case REPLY_BUTTON1: 
	printf ("User selected Save\n");
	break;
      case REPLY_BUTTON2:
	printf ("User selected Don't Save\n");
	break;
      case REPLY_CANCEL:
	/* Do nothing, user returns to application */
	break;
    }
}


/* ----------------------------------------
 * SUIT_getString():
 * 
 * This dialog box asks a question, and lets
 * the user type a string value in a typein box.
 * 
 * Returns: REPLY_OK
 *          REPLY_CANCEL
 *          
 * ----------------------------------------*/

void ShowGetString(SUIT_object box)
{
    char answer[50];
    if (SUIT_getString("What is your name?", 
		       "Bob the Amazing", answer, 50) == REPLY_OK) {
	printf("hello, %s\n", answer);
	
    }
}





/* 
 * ----------------------------------------
 * SUIT_createOKCancelDialogBox():
 * 
 * This dialog box is the most sophisticated 
 * of the group, in that it lets you display
 * a widget of your choosing to the user. The
 * user is allowed to interact with the widget
 * before being required to press OK, which 
 * accepts the changes made to the  widget, 
 * or CANCEL, which abandons all changes.
 * 
 * Returns: REPLY_OK
 *          REPLY_CANCEL
 *          
 * ----------------------------------------*/
void ShowOKCancelDialog (SUIT_object obj)
{
    /* 
     * Strategy with this call is always the same:
       1.) Create the objects to go in the dialog box
           If the dialog box needs to contain "many widgets"
           1a.) Create a bulletin board and make the 
	        "many widgets" that will go in it
	   1b.) Change the size of the bulletin board. The 
	        dialog box will change size to accomodate it.
	   1c.) Make the widgets be children of the bulletin board
	   1d.) Arrange the children inside the parent 
	        using the SUIT_mapToParent() and the 
		SUIT_setViewport() calls.
       2.) Create the Dialog Box, passing it 
           the widget as a parameter, and a validation function.
	   Here, the function is a NULL, meaning that we do no
	   checking of the data when the user presses OK. 
	   To see an example of a validation function at 
	   work, see the example file dialog2.c.
       3.) Activate the Dialog Box 
           THIS CAN HAPPEN AT ANY TIME.
       4.) Test the dialog box integer property 
           called "button pressed"
           Possible constants are: REPLY_CANCEL and REPLY_OK
       5.) If the user pressed OK, check the values of 
           the widget that was in the dialog box.
       6.) Destroy the dialog box to make it go away.
    */
    
    SUIT_object container, slider, checkbox, dBox;
    SUIT_viewport vp;
    char buf[100];
    double val;
    boolean bool;					      

    container = SUIT_createBulletinBoard ("MyContainer");     /* 1a */
    slider = SUIT_createBoundedValue("fred", NULL);
    checkbox = SUIT_createOnOffSwitch("wilma", NULL);

    SUIT_changeObjectSize(container, 300, 100);		      /* 1b */
    
    SUIT_addChildToObject(container, slider);		      /* 1c */
    SUIT_addChildToObject(container, checkbox);

							      /* 1d */
    SUIT_setViewport(slider, VIEWPORT,  
		     SUIT_mapToParent(slider, 0.05, 0.05, 0.45, 0.95));
    SUIT_setViewport(checkbox, VIEWPORT, 
		     SUIT_mapToParent(checkbox, 0.55, 0.05, 0.95, 0.95));

    dBox = SUIT_createOKCancelDialogBox("anita", container, NULL);/* 2 */
    SUIT_activateDialogBox(dBox);			      /* 3 */

    switch (SUIT_getInteger(dBox, "button pressed")) {	      /* 4 */
      case REPLY_CANCEL:
	printf("User selected CANCEL\n"); 
	break;
      case REPLY_OK:					      /* 5 */
	val = SUIT_getDouble(slider, CURRENT_VALUE);
	bool = SUIT_getBoolean(checkbox, CURRENT_VALUE);
	printf("Selected OK. Bounded val = %lf Check box = %s\n", val,
		 ((bool == 1)? "ON":"OFF"));
	break;
    }
    SUIT_destroyObject(dBox);				      /* 6 */
}


void DialogBoxDemo(void)
{
    SUIT_createLabel("SUIT Dialog Box Demo Program");

    SUIT_createButton("Hit me for SUIT_inform()", ShowInform);
    SUIT_createButton("Hit me for SUIT_askYesNo()", ShowYesNo);
    SUIT_createButton("Hit me for SUIT_askYesNoCancel()", ShowAskYesNoCancel);
    SUIT_createButton("Hit me for SUIT_askOKCancel()", ShowAskOKCancel);
    SUIT_createButton("Hit me for SUIT_ask()", ShowAsk);
    SUIT_createButton("Hit me for SUIT_createOKCancelDialogBox()", ShowOKCancelDialog); 
    SUIT_createButton("Hit me for SUIT_askWithCancel()", ShowAskWithCancel);
    SUIT_createButton("Hit me for SUIT_getString()", ShowGetString);
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    DialogBoxDemo();
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
