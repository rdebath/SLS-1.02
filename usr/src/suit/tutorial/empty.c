#include "suit.h"


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
