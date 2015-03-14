#include "suit.h"

SUIT_object num1, num2, result;

void PerformAddition(SUIT_object button) 
{ 
    double temp1, temp2; 
    char buffer[100];
    
    temp1 = atof(SUIT_getText(num1,"current value"));
    temp2 = atof(SUIT_getText(num2,"current value"));
    sprintf(buffer, "%f", temp1 + temp2); 
    SUIT_setText(result, "label", buffer); 
}


void main (int argc, char *argv[]) 
{ 
    SUIT_init(argv[0]);

    num1 = SUIT_createTypeInBox("num 1",NULL); 
    num2 = SUIT_createTypeInBox("num 2",NULL); 
    SUIT_createLabel("+"); 
    SUIT_createLabel("="); 
    result = SUIT_createLabel("result"); 
    SUIT_createButton("GO", PerformAddition);
    SUIT_createDoneButton(NULL); 

    SUIT_beginStandardApplication(); 
} 

