#include "suit.h"

void MyCallBackFunction(SUIT_object chips)
{
    GP_color color;
    
    color = SUIT_getColor(chips, CURRENT_VALUE);
    
    printf ("Yippee! User selected %s\n", color.colorName);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createColorChips("my colors", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
