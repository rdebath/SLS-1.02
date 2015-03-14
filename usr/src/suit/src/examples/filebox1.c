#include "suit.h"

void GetAFile() {
    char *fname;

    fname = SUIT_askForFileName("/usr/bin/","Print","File name:");

    if (fname != NULL)
	printf ("user asked for file %s\n", fname);
    else
	printf ("user pressed CANCEL\n");
}

void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createButton("get a File Name", GetAFile);

    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}

