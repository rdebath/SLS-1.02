/* SUIT version 2.3 */

#define THE_SCREEN_WIDTH 960
#define THE_SCREEN_HEIGHT 600
#define THE_SCREEN_DEPTH 7

#include "suit.h"


/*--------------------------------------------------------------------------------*/
/*                                                                                */
/*                                  NOTE:                                         */
/*                                                                                */
/*    This file contains all the permanent properties of this application's       */
/*    objects and is read in as a data file.  Compiling this file as part of      */
/*    your application "hard codes" your interface.  Please see "shipping"        */
/*    in the SUIT reference manual for further information.                       */
/*                                                                                */
/*                                                                                */
/*                              !! DANGER !!                                      */
/*                                                                                */
/*      This file is machine-generated, and its contents are very important.      */
/*                                                                                */
/*          If you must edit this file, do so with extreme caution!               */
/*                                                                                */
/*                              !! DANGER !!                                      */
/*                                                                                */
/*--------------------------------------------------------------------------------*/


extern void SUIT_interiorInitFromCode (char *programName, SUIT_functionPointer suiRoutine,
                                       int width, int height, int depth);

extern SUIT_type *si_getType(char *typeName);

extern void si_addChildToObject(SUIT_object, SUIT_object, boolean);

static void MAKE (char *name, char *class, char *parent, boolean interactive) 
{ 
    SUIT_object o = SUIT_name(name); 
    SUIT_object p = SUIT_name(parent); 
    if (p != NULL) {
        if (o == NULL) 
            o = SUIT_createObjectByClass(name, class); 
    	if (interactive) {
    	    SUIT_setBoolean (o, INTERACTIVELY_CREATED, TRUE);
    	    SUIT_makePropertyTemporary (o, INTERACTIVELY_CREATED, OBJECT);
    	}
        si_addChildToObject(SUIT_name(parent), o, FALSE); 
    }
} 
 


static void SET (char *objOrClass, char *propertyName, char *propertyType, boolean atClass,
                 boolean locked, char *stringValue) 
{ 
    SUIT_type	*type; 
    boolean	errorStatus; 
    Pointer	retval; 
    SUIT_level level = OBJECT; 
    SUIT_object o; 
     
    if (atClass) 
        level = CLASS; 
    if (level == CLASS) 
        o = SUIT_dummyObjectInClass(objOrClass); 
    else 
        o = SUIT_name(objOrClass); 
    type = si_getType(propertyType); 
    retval = type->convertFromAscii(stringValue, &errorStatus); 
    if (errorStatus == FALSE) 
	SUIT_setProperty(o, propertyName, propertyType, retval, level); 

    if (locked) 
        SUIT_lockProperty(o, propertyName, level); 
}


static void INIT_suiRoutine(void)
{
/* This line is for parsing simplicity -- do NOT remove it!  @ */
MAKE("unnamed object 0","text box","ROOT",1);
MAKE("This @i(is) a test","label","ROOT",0);
MAKE("Done","button","ROOT",0);
SET("arrow button","border raised","boolean",1,0,"no");
SET("arrow button","changed class","boolean",1,0,"no");
SET("arrow button","direction","SUIT_enum",1,0,"\"up\" of {\"up\" \"down\" \"left\" \"right\"}");
SET("arrow button","draw filled","boolean",1,0,"no");
SET("arrow button","has background","boolean",1,0,"no");
SET("arrow button","intermediate feedback","boolean",1,0,"no");
SET("arrow button","shadow thickness","int",1,0,"3");
SET("billboard","border raised","boolean",1,0,"no");
SET("billboard","changed class","boolean",1,0,"no");
SET("borderless bulletin board","border raised","boolean",1,0,"no");
SET("borderless bulletin board","changed class","boolean",1,0,"no");
SET("borderless bulletin board","has border","boolean",1,0,"no");
SET("bounded value","arrowhead angle","int",1,0,"10");
SET("bounded value","arrowhead length","double",1,0,"0.200000");
SET("bounded value","button background color","GP_color",1,0,"black, black");
SET("bounded value","button foreground color","GP_color",1,0,"grey, white");
SET("bounded value","changed class","boolean",1,0,"no");
SET("bounded value","has arrow","boolean",1,0,"yes");
SET("bounded value","has tick marks","boolean",1,0,"yes");
SET("bounded value","increase clockwise","boolean",1,0,"yes");
SET("bounded value","needle color","GP_color",1,0,"black, black");
SET("bounded value","start angle","double",1,0,"0.000000");
SET("button","changed class","boolean",1,0,"no");
SET("button","disabled color","GP_color",1,0,"grey, black");
SET("button","interactively created","boolean",1,0,"no");
SET("button","justification","SUIT_enum",1,0,"\"center\" of {\"left\" \"center\" \"right\"}");
SET("button","shrink to fit","boolean",1,0,"yes");
SET("dialog box","border type","SUIT_enum",1,0,"\"fancy motif\" of {\"simple\" \"motif\" \"fancy motif\"}");
SET("dialog box","border width","int",1,0,"8");
SET("dialog box","cache using canvas","boolean",1,0,"yes");
SET("dialog box","changed class","boolean",1,0,"no");
SET("elevator","border raised","boolean",1,0,"no");
SET("elevator","changed class","boolean",1,0,"no");
SET("elevator","has background","boolean",1,0,"no");
SET("elevator","sliding","boolean",1,0,"no");
SET("font panel","border raised","boolean",1,0,"no");
SET("font panel","changed class","boolean",1,0,"no");
SET("font panel","has border","boolean",1,0,"no");
SET("label","changed class","boolean",1,0,"no");
SET("label","has border","boolean",1,0,"no");
SET("label","interactively created","boolean",1,0,"no");
SET("label","justification","SUIT_enum",1,0,"\"center\" of {\"left\" \"center\" \"right\"}");
SET("label","shrink to fit","boolean",1,0,"yes");
SET("list","border raised","boolean",1,0,"no");
SET("list","changed class","boolean",1,0,"no");
SET("list","text spacing","double",1,0,"1.200000");
SET("on/off switch","changed class","boolean",1,0,"no");
SET("on/off switch","disabled color","GP_color",1,0,"grey, black");
SET("on/off switch","shrink to fit","boolean",1,0,"yes");
SET("p.e.collection","border raised","boolean",1,0,"no");
SET("p.e.collection","changed class","boolean",1,0,"no");
SET("place mat","border raised","boolean",1,0,"no");
SET("place mat","changed class","boolean",1,0,"no");
SET("property editor","border raised","boolean",1,0,"no");
SET("property editor","changed class","boolean",1,0,"no");
SET("property list","border raised","boolean",1,0,"no");
SET("property list","changed class","boolean",1,0,"no");
SET("property list","text spacing","double",1,0,"1.500000");
SET("radio buttons","changed class","boolean",1,0,"no");
SET("radio buttons","current value","SUIT_enum",1,0,"\"default value\" of {\"default value\"}");
SET("scrollable list","border raised","boolean",1,0,"no");
SET("scrollable list","changed class","boolean",1,0,"no");
SET("scrollable list","has background","boolean",1,0,"no");
SET("scrollable list","has border","boolean",1,0,"no");
SET("scrollable property list","border raised","boolean",1,0,"no");
SET("scrollable property list","changed class","boolean",1,0,"no");
SET("scrollable property list","has background","boolean",1,0,"no");
SET("scrollable property list","has border","boolean",1,0,"no");
SET("stacker","changed class","boolean",1,0,"no");
SET("stacker","shrink to fit","boolean",1,0,"yes");
SET("type in box","any keystroke triggers","boolean",1,0,"no");
SET("type in box","append to buffer","boolean",1,0,"no");
SET("type in box","backward char key","text",1,0,"C-b");
SET("type in box","beginning of line key","text",1,0,"C-a");
SET("type in box","beginning of text key","text",1,0,"M-<");
SET("type in box","border raised","boolean",1,0,"no");
SET("type in box","calculate block","boolean",1,0,"no");
SET("type in box","calculate lines","boolean",1,0,"no");
SET("type in box","changed class","boolean",1,0,"no");
SET("type in box","cursor color","GP_color",1,0,"black, black");
SET("type in box","cursor index","int",1,0,"0");
SET("type in box","cursor style","SUIT_enum",1,0,"\"vertical bar\" of {\"i-beam\" \"vertical bar\"}");
SET("type in box","delete char key","text",1,0,"C-d");
SET("type in box","delete entire line key","text",1,0,"C-u");
SET("type in box","done editing key","text",1,0,"C-x");
SET("type in box","end of line key","text",1,0,"C-e");
SET("type in box","end of text key","text",1,0,"M->");
SET("type in box","forward char key","text",1,0,"C-f");
SET("type in box","highlight block","boolean",1,0,"no");
SET("type in box","input sequence","text",1,0,"");
SET("type in box","kill line key","text",1,0,"C-k");
SET("type in box","last click time","double",1,0,"0.000000");
SET("type in box","last mark index","int",1,0,"0");
SET("type in box","mark end index","int",1,0,"0");
SET("type in box","mark index","int",1,0,"0");
SET("type in box","next line key","text",1,0,"C-n");
SET("type in box","open line key","text",1,0,"C-o");
SET("type in box","post cursor index","int",1,0,"0");
SET("type in box","pre cursor index","int",1,0,"0");
SET("type in box","previous line key","text",1,0,"C-p");
SET("type in box","repaint key","text",1,0,"C-l");
SET("type in box","scroll down key","text",1,0,"M-v");
SET("type in box","scroll up key","text",1,0,"C-v");
SET("type in box","set mark key","text",1,0,"C-`");
SET("type in box","shrink to fit","boolean",1,0,"yes");
SET("type in box","spacing gap","int",1,0,"3");
SET("type in box","start x","int",1,0,"0");
SET("type in box","start y","double",1,0,"0.000000");
SET("type in box","tab length","int",1,0,"5");
SET("type in box","time between clicks for double click","double",1,0,"400000.000000");
SET("type in box","wipe block key","text",1,0,"C-w");
SET("type in box","yank key","text",1,0,"C-y");
SET("ROOT","animated","boolean",0,0,"no");
SET("ROOT","background color","GP_color",0,0,"grey, white");
SET("ROOT","border color","GP_color",0,0,"grey, black");
SET("ROOT","border raised","boolean",0,0,"yes");
SET("ROOT","border type","SUIT_enum",0,0,"\"motif\" of {\"simple\" \"motif\" \"fancy motif\"}");
SET("ROOT","border width","int",0,0,"2");
SET("ROOT","changed class","boolean",0,0,"no");
SET("ROOT","clip to viewport","boolean",0,0,"yes");
SET("ROOT","default object height","int",0,0,"80");
SET("ROOT","default object width","int",0,0,"80");
SET("ROOT","draw border on inside","boolean",0,0,"no");
SET("ROOT","font","GP_font",0,0,"new century schoolbook,,14.000000");
SET("ROOT","foreground color","GP_color",0,0,"black, black");
SET("ROOT","has background","boolean",0,0,"yes");
SET("ROOT","has border","boolean",0,0,"yes");
SET("ROOT","margin","int",0,0,"5");
SET("ROOT","show temporary properties","boolean",0,0,"no");
SET("ROOT","shrink to fit","boolean",0,0,"no");
SET("ROOT","springiness","SUIT_springiness",0,0,"63");
SET("ROOT","SUIT system font","GP_font",0,0,"helvetica,,14.000000");
SET("ROOT","viewport","viewport",0,1,"0 0 959 599");
SET("ROOT","visible","boolean",0,0,"yes");
SET("ROOT","window","window",0,1,"0.000000 0.000000 1.000000 1.000000");
SET("unnamed object 0","active display","SUIT_enum",0,0,"\"standard\" of {\"standard\"}");
SET("unnamed object 0","label","text",0,0,"This @yellow(@+(is)) a test @blue(of the) @red(emergency) @green(broadcasts) @+(@blue(@b(system))).");
SET("unnamed object 0","line spacing","double",0,0,"1.000000");
SET("unnamed object 0","viewport","viewport",0,0,"325 302 574 455");
SET("This @i(is) a test","active display","SUIT_enum",0,0,"\"standard\" of {\"standard\"}");
SET("This @i(is) a test","label","text",0,0,"f@'(o)@`(o)");
SET("This @i(is) a test","viewport","viewport",0,0,"103 416 132 442");
SET("Done","active display","SUIT_enum",0,0,"\"standard\" of {\"button with hotkey\" \"standard\"}");
SET("Done","border raised","boolean",0,0,"yes");
SET("Done","callback function","SUIT_functionPointer",0,0,"function ptr");
SET("Done","disabled","boolean",0,0,"no");
SET("Done","done callback function","SUIT_functionPointer",0,0,"function ptr");
SET("Done","has background","boolean",0,0,"yes");
SET("Done","label","text",0,0,"Done");
SET("Done","viewport","viewport",0,0,"184 279 228 305");


} /* end of INIT_suiRoutine */



void SUIT_initFromCode (char *programName)
{
    SUIT_interiorInitFromCode (programName, INIT_suiRoutine, THE_SCREEN_WIDTH,
                               THE_SCREEN_HEIGHT, THE_SCREEN_DEPTH);
}
