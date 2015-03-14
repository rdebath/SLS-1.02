#include "suit.h"

/*
   This sample program demostrates the use of a text editor widget and a 
   type-in box.

   Included in this example is a demostration of creating a new key binding
   that the text editor widget recognizes, and sending macros to the widget
   as if they had been typed.

   The text editor widget used is one that has a scroll bar.
*/

/* String to be initially stored in the text editor. */
char *textString = "Press the 'Load fruit file' button\nto load in fruit names from the file\n\n\t\tfruit.names\n\nThe file will replace this text string.\n\n\nPress control i to see a message.";

char *infoMessage = "This is a text editor widget which recognizes control i";

SUIT_object textEditor;  /* Text editor widget */
                         /* It is a global variable so that we can change the
			    widget's current value when the button is pressed. */

SUIT_object keyReport;   /* label that will report a user's keystrokes. */

void sendMacroToTextEditor(SUIT_object o)
    /* Callback function for the type-in box. */
{
    char *objectText;

    objectText = SUIT_getText(o, CURRENT_VALUE);
    SUIT_sendToEditor(textEditor, objectText);
}

void quitDemo(SUIT_object o)
    /* Callback function for the text editor. */
{
    exit(0);
}

void loadFruitFile(SUIT_object o)
    /* Callback function for the "Load fruit file" button. */
{
    char *fileText;

    fileText = SUIT_textOfFile("fruit.names");
    printf("%s",fileText);
    SUIT_setText(textEditor, CURRENT_VALUE, fileText);
}

void moveUp(SUIT_object o)
{
    SUIT_sendToEditor(textEditor, SUIT_getText(textEditor, "previous line key"));
}

void moveDown(SUIT_object o)
{
    SUIT_sendToEditor(textEditor, SUIT_getText(textEditor, "next line key"));
}

void moveRight(SUIT_object o)
{
    SUIT_sendToEditor(textEditor, SUIT_getText(textEditor, "forward char key"));
}

void moveLeft(SUIT_object o)
{
    SUIT_sendToEditor(textEditor, SUIT_getText(textEditor, "backward char key"));
}

void dealWithKeyInputs (SUIT_object o, char *propName, char *propType,
			Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, "input sequence")) {
	if (strlen((char *) new) > 0) {
	    SUIT_setText(keyReport, "label", (char *) new);
	    if (SUIT_stringsMatch ((char *) new, SUIT_getText(o, "info key")))
		SUIT_inform (infoMessage);
	}
    }
}

void main (int argc, char *argv[])
{
    SUIT_object upArrow, downArrow, rightArrow, leftArrow;

    SUIT_deluxeInit(&argc, argv);
    
    textEditor = SUIT_createTextEditorWithScrollBar("Type text here", quitDemo);
    SUIT_setText(textEditor, CURRENT_VALUE, textString);
   
    /* Add an new key sequence and bind it to control i */
    SUIT_setText(textEditor, "info key", "C-i");

    /* Rebind the key sequence that calls the callback function */
    SUIT_setText(textEditor, "done editing key", "C-xC-c");

    SUIT_registerInterest (textEditor, dealWithKeyInputs);
    
    keyReport = SUIT_createLabel("key");
    SUIT_createLabel("The user typed:");

    upArrow = SUIT_createArrowButton("up arrow", moveUp);
    downArrow = SUIT_createArrowButton("down arrow", moveDown);
    rightArrow = SUIT_createArrowButton("right arrow", moveRight);
    leftArrow = SUIT_createArrowButton("left arrow", moveLeft);

    SUIT_setEnumString(upArrow, DIRECTION, "up");
    SUIT_setEnumString(downArrow, DIRECTION, "down");
    SUIT_setEnumString(rightArrow, DIRECTION, "right");
    SUIT_setEnumString(leftArrow, DIRECTION, "left");

    SUIT_createTypeInBox("Enter one line here", sendMacroToTextEditor);
    SUIT_createButton("Load fruit file", loadFruitFile);
    SUIT_createDoneButton (NULL);

    SUIT_beginStandardApplication();
}
