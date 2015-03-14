#include "suit.h"
SUIT_object p; /* for the tutorial */


/***************************************************************
  all words in the English  language that contain the sequence "suit"
  ****************************************************************/
char	*list[] = {
    "besuit", "cosuitor", "countersuit", "demisuit", "dissuit",
    "dissuitable", "dissuited", "Jesuit", "Jesuited", "Jesuitess",
    "Jesuitic", "Jesuitical", "Jesuitically", "Jesuitish",
    "Jesuitism", "Jesuitist", "Jesuitize", "Jesuitocracy",
    "Jesuitry", "lawsuit", "lawsuiting", "mispursuit", "nonpursuit",
    "nonsuit", "outsuitor", "overunsuitable", "playsuit",
    "presuitability", "presuitable", "presuitably", "pursuit",
    "pursuitmeter", "repursuit", "resuit", "snowsuit", "suit",
    "suitability", "suitable", "suitableness", "suitably",
    "suitcase", "suite", "suithold", "suiting", "suitor",
    "suitoress", "suitorship", "suity", "swimsuit", "undersuit",
    "unjesuited", "unjesuitical", "unjesuitically", "unsuit",
    "unsuitability", "unsuitable", "unsuitableness", "unsuitably",
    "unsuited", "unsuiting"
    };


/****************************************************************
 ****************************************************************
 
 Demo program showing off all the built-in widgets.
 
 ****************************************************************
 ****************************************************************/


/****************************************************************
  This "callback" routine is called when the user types in
  the text box.
  ****************************************************************/

void EchoText (SUIT_object typeinBox)
{
    printf("The user typed '%s'\n", SUIT_getText (typeinBox, "current value"));
}




void
    main (int argc, char *argv[])
{
    
    SUIT_object	menu, radio;

    /****************************************************
      This must be called before any objects can be created.
      ****************************************************/
    SUIT_init(argv[0]);
    
    
    /****************************************************
      We create a bunch of labels which are used to identify widgets
      ****************************************************/
    (void) SUIT_createLabel ("SUIT");
    (void) SUIT_createLabel ("The Simple User Interface Toolkit");
    (void) SUIT_createLabel ("Color Chips Widget");
    (void) SUIT_createLabel ("Bounded Value Widget"); 
    (void) SUIT_createLabel ("Menu Widget");
    (void) SUIT_createLabel ("Type In Widget");
    (void) SUIT_createLabel ("Text Editor Widget");
    (void) SUIT_createLabel ("Scrollable List Widget");
    (void) SUIT_createLabel ("UVa Logo Widget");
    (void) SUIT_createLabel ("On/Off Switch Widget");
    (void) SUIT_createLabel ("Radio Button Widget");
    (void) SUIT_createLabel ("Button Widgets");
    
    
    /****************************************************
      Each of the following widgets allow you to specify a "call-back"
      routine (that you write) which is called each time the user
      interacts with the object (for example, moves a thermometer up or
      down.  This demo program specifies NULL for them, since it's just
      a shell.
      ****************************************************/
    
    (void) SUIT_createBouncingBall("first ball");
    SUIT_setBoolean(SUIT_name("first ball"), "animated", TRUE);
    (void) SUIT_createBouncingBall("second ball");
    SUIT_setBoolean(SUIT_name("second ball"), "animated", TRUE);
    (void) SUIT_createBoundedValue ("my bounded value", NULL);
    (void) SUIT_createOnOffSwitch ("my switch", NULL);
    (void) SUIT_createColorChips ("my color chips", NULL);
    SUIT_createScrollableList ("list of SUIT words", NULL);
    SUIT_setTextList(SUIT_name("list of SUIT words"), LIST, SUIT_defTextList(list, 60));
    (void) SUIT_createUVALogo("my UVAlogo");
    
    /****************************************************
      To show how callbacks work, we specify one for the text
      box.  The callback routine takes one parameter, which is the
      SUIT_object altered by the user.  By examining the properties of
      the passed object, the callback routine can do whatever is
      appropriate.  The "EchoText" callback (found later in this file)
      simply prints out the text the user typed.
      ****************************************************/
    (void) SUIT_createTypeInBox("my typein box", EchoText);
    (void) SUIT_createTextEditor("my text editor", NULL);
    SUIT_setText(SUIT_name("my text editor"), "current value", "This text editor\nsupports emacs-style\nkey bindings\n");
    
    /****************************************************
      Some widgets are "composite," such as menus and radio buttons.
    ****************************************************/
    menu = SUIT_createPullDownMenu("my menu");
    SUIT_addToMenu(menu, "Monday",    NULL);
    SUIT_addToMenu(menu, "Tuesday",   NULL);
    SUIT_addToMenu(menu, "Wednesday", NULL);
    SUIT_addToMenu(menu, "Thursday",  NULL);
    SUIT_addToMenu(menu, "Friday",    NULL);
    
    radio = SUIT_createRadioButtons ("my radio buttons", NULL);
    SUIT_addButtonToRadioButtons (radio, "Chocolate");
    SUIT_addButtonToRadioButtons (radio, "Vanilla");
    SUIT_addButtonToRadioButtons (radio, "Strawberry");
    SUIT_addButtonToRadioButtons (radio, "Rocky Road");
    

    /* These calls inserted only for the tutorial. */
    p = SUIT_createPolygon("my poly");
    SUIT_setInteger(p, "number of sides", 7);
    SUIT_setBoolean(p, "filled", TRUE);
    SUIT_setColor(p, "foreground color", GP_defColor("red", TRUE));
    SUIT_createLabel("Polygon Widget");

    
    /****************************************************************
      After creating all the objects, we tell SUIT to begin the application.
      ****************************************************************/
    
    SUIT_createDoneButton(NULL);
    SUIT_createAbortButton(NULL);
    
    SUIT_beginStandardApplication();
}
