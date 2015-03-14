#include "suit.h"

void MyCallBackFunction(SUIT_object chips)
{
    int pattern;
    
    pattern = SUIT_getInteger(chips, CURRENT_VALUE);
    printf ("Yippee! User selected pattern number %d\n", pattern);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createPatternChips("my patterns", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
