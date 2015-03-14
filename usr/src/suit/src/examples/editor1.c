#include "suit.h"

/*
   This sample program demostrates the use of a text editor widget and a 
   type-in box.
*/

/* String to be initially stored in the text editor. */
char *textString = "Press the 'Load fruit file' button\nto load in fruit names from the file\n\n\t\tfruit.names\n\nThe file will replace this text string.";

SUIT_object textEditor;  /* Text editor widget */
                         /* It is a global variable so that we can change the
			    widget's current value when the button is pressed. */


void displayText(SUIT_object o)
    /* Callback function for both the text editor and the type-in box. */
{
    char *objectText;

    objectText = SUIT_getText(o, CURRENT_VALUE);
    printf("The user entered: %s\n",objectText);
}

void loadFruitFile(SUIT_object o)
    /* Callback function for the "Load fruit file" button. */
{
    char *fileText;

    fileText = SUIT_textOfFile("fruit.names");
    printf("%s",fileText);
    SUIT_setText(textEditor, CURRENT_VALUE, fileText);
}

void main (int argc, char *argv[])
{
    SUIT_deluxeInit(&argc, argv);

    textEditor = SUIT_createTextEditor("Type text here", displayText);
    SUIT_setText(textEditor, CURRENT_VALUE, textString);

    SUIT_createTypeInBox("Enter one line here", displayText);
    SUIT_createButton("Load fruit file", loadFruitFile);
    SUIT_createDoneButton (NULL);

    SUIT_beginStandardApplication();
}
