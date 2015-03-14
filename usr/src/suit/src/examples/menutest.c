/***************************************
*  Menu.c --
*      A demonstration program illustrating 
*      the use of SUIT's menus.
*      
***************************************** */

#include "suit.h"

void Open  (SUIT_object menu)	{ printf("User selected Open\n");	}
void Save  (SUIT_object menu)	{ printf("User selected Save\n");	}
void Quit  (SUIT_object menu)	{ printf("User selected Quit\n");	}
void Cut   (SUIT_object menu)	{ printf("User selected Cut\n");	}
void Paste (SUIT_object menu)	{ printf("User selected Paste\n");	}


void main (int argc, char *argv[])
{
    SUIT_object menuBar, fileMenu, editMenu;

    SUIT_init(argv[0]);

    fileMenu = SUIT_createPullDownMenu ("File");
    SUIT_addToMenu(fileMenu, "Open", Open);
    SUIT_addToMenu(fileMenu, "Save", Save);
    SUIT_addToMenu(fileMenu, "Quit", Quit);

    editMenu = SUIT_createPullDownMenu ("Edit");
    SUIT_addToMenu(editMenu, "Cut", Cut);
    SUIT_addToMenu(editMenu, "Paste", Paste);

    /* the menu bar packs the menu buttons left to right */
    menuBar = SUIT_createMenuBar("my menu bar");
    SUIT_addChildToObject(menuBar, fileMenu);
    SUIT_addChildToObject(menuBar, editMenu);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
