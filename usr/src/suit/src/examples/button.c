#include "suit.h"

void MyCallBackFunction(SUIT_object button)
{
    printf ("Yippee! User pressed the button!\n");
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createButton("Click Here", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
