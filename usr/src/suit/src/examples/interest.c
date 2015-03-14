/**********************************
 * interest.c --
 *    demonstrates the use of interest 
 *    callbacks in a program.
 **********************************/

#include "suit.h"

static char buf[200];

static void BV_interestCallback (SUIT_object bounded_value, char *propertyName, 
				 char *propertyType, Pointer oldValue, 
				 Pointer newValue)
{
    double val;
    SUIT_object typeInBox;
    
    if (SUIT_stringsMatch(propertyName, CURRENT_VALUE)) {
	sprintf (buf, "Digital display for %s", 
		 OBJECT_NAME(bounded_value));                        /* 3 */
	typeInBox = SUIT_name (buf);
	if (typeInBox == NULL) {
	    return;
	}
	val = SUIT_getDouble (bounded_value, CURRENT_VALUE);
	sprintf (buf, "%lf", val);
	SUIT_setText (typeInBox, CURRENT_VALUE, buf);
    }
}

static void typeInBoxCallback (SUIT_object typeInBox)                 /* 4 */
{
    SUIT_object bounded_value;
    double val;
    
    sscanf (OBJECT_NAME(typeInBox),"Digital display for %[^\n]", buf);
    bounded_value = SUIT_name (buf);
    if (bounded_value == NULL) {
	fprintf(stderr,"Error:  Couldn't find object '%s'\n", buf);
	return;
    }
    sscanf (SUIT_getText (typeInBox, CURRENT_VALUE), " %lf", &val);
    SUIT_setDouble (bounded_value, CURRENT_VALUE, val);
}

SUIT_object addReadoutToBoundedValue (SUIT_object boundedValue)
{
    SUIT_object digital_readout;
    
    sprintf (buf,"Digital display for %s",OBJECT_NAME(boundedValue)); /* 1 */
    digital_readout = SUIT_createTypeInBox (buf, typeInBoxCallback);
    SUIT_registerInterest (boundedValue, BV_interestCallback);        /* 2 */
    
    return digital_readout;
}

void MyBoundedValueCallback(SUIT_object bounded_value)
{
    printf ("slider was changed. Now reading %lf\n", 
	    SUIT_getDouble(bounded_value, CURRENT_VALUE));
}

void main (int argc, char *argv[])
{
    SUIT_object myBoundedValue;

    SUIT_init(argv[0]);

    myBoundedValue=SUIT_createBoundedValue("slider1", MyBoundedValueCallback);
    addReadoutToBoundedValue(myBoundedValue);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}


/******************************
 * NOTES:
 * 
 * 1.) The addReadoutToBoundedValue() call creates a text box for the 
 *     bounded value in question. The name for the new type in box
 *     is the name of the bounded value with the string 
 *     "Digital display for " prepended.
 * 2.) Next, this function "registers an interest in the bounded value".
 *     This means that any time ANY PROPERTY of the bounded value 
 *     changes, the function supplied (BV_interestCallback, in this 
 *     case) will be called. 
 * 3.) This is the interest callback. It makes sure that the value read on the
 *     typein box is always the same as that on the bounded value itself.
 *     The function starts be by testing the name of the property that 
 *     just changed to see if it was the CURRENT_VALUE property. Remember, this
 *     function is called every time ANY property changes on the bounded 
 *     value.
 * 4.) The type in box callback is straightforward: it takes it's 
 *     CURRENT_VALUE and converts it into a double precision floating 
 *     point value, which it uses for the new CURRENT_VALUE on the slider.
 */
