#include "suit.h"

void MyCallBackFunction(SUIT_object bounded_value)
{
    double num;

    num = SUIT_getDouble(bounded_value, CURRENT_VALUE);
    printf("Current value is %lf\n", num);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createBoundedValue("Click Here", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
