

#include "suit.h"


void printFileName(SUIT_object fileBrowser)
{
    printf("The file chosen was %s\n", 
	   SUIT_getText(fileBrowser, CURRENT_VALUE));
}

void PopUpFileBox (SUIT_object obj)
{
    SUIT_object dbox;
    dbox = SUIT_createFileBrowserDialogBox("fred", "/usr/bin", 
					   "Open file", "Open file named:");
    if (SUIT_activateDialogBox (dbox) == REPLY_OK) {
	printf ("user got the file called %s\n", 
		SUIT_getText(dbox,CURRENT_VALUE));
    } else
	printf ("User pressed cancel\n");
    SUIT_destroyObject(dbox);
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createFileBrowser("joe","/cs2/suit/","Print","File name:", printFileName);
    SUIT_createButton ("Open", PopUpFileBox);
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}

