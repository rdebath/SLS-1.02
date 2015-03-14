/***************************************
*  PropTemp.c --
*      A demonstration program illustrating 
*      the way to use temporary properties.
*      
*      This program contains a single type in 
*      box. Usually, the CURRENT_VALUE of the 
*      type in box would be preserved between runs of 
*      the program. Here, we make that property 
*      temporary, so the box comes up blank each time
*      the program is run, regardless of the value that 
*      was in the box the last time the program was run.
*      
***************************************** */

#include "suit.h"

void main (int argc, char *argv[])
{
    SUIT_object words;

    SUIT_init(argv[0]);

    words = SUIT_createTypeInBox("type", NULL);
    SUIT_makePropertyTemporary(words, CURRENT_VALUE, OBJECT); 
    SUIT_createDoneButton(NULL);

    SUIT_beginStandardApplication();
}	
