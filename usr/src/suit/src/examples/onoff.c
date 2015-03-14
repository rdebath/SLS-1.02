#include "suit.h"

void MyCallBackFunction(SUIT_object onoff)
{
    if (SUIT_getBoolean(onoff, CURRENT_VALUE))
	printf ("The button is OFF\n");
    else 
	printf ("The button is ON\n");

}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createOnOffSwitch("Click Here", MyCallBackFunction);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
