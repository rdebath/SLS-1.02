#include "suit.h"

void MyCallBackFunction(SUIT_object typeinBox)
{
    printf("The user typed in %s\n", SUIT_getText(typeinBox, CURRENT_VALUE));

}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createTypeInBox("Click Here", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
