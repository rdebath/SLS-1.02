#include "suit.h"


void SayHello(SUIT_object obj)
{
    printf("Hello there\n");
}


void MakePanelVisible(SUIT_object button)
{
    SUIT_setBoolean(SUIT_name("Panel"), VISIBLE, TRUE);
}

void MakePanelInvisible(SUIT_object button)
{
    SUIT_setBoolean(SUIT_name("Panel"), VISIBLE, FALSE);
}


void main (int argc, char *argv[])
{
    SUIT_object hideButton, panel, button1, button2;

    SUIT_init(argv[0]);
    SUIT_createButton("Show Panel", MakePanelVisible);

    panel = SUIT_createBulletinBoard("Panel");
    hideButton = SUIT_createButton("Hide Panel", MakePanelInvisible);
    button1=SUIT_createButton("some button", SayHello);
    button2=SUIT_createButton("another button", SayHello);
    SUIT_addChildToObject(panel, hideButton);
    SUIT_addChildToObject(panel, button1);
    SUIT_addChildToObject(panel, button2);
    
    SUIT_setViewport(hideButton, VIEWPORT, 
		     SUIT_mapToParent(hideButton, 0.1, 0.1, 0.9, 0.3));
    SUIT_setViewport(button1, VIEWPORT, 
		     SUIT_mapToParent(button1, 0.1, 0.4, 0.9, 0.6));
    SUIT_setViewport(button2, VIEWPORT, 
		     SUIT_mapToParent(button2, 0.1, 0.7, 0.9, 0.9));
    

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
