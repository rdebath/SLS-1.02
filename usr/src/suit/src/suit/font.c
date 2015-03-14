/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#if defined(SUN) && !defined(linux) /* compensate for compiler braindeath */
int sscanf (char*, ...);
#endif


/* Most of this code is a hack until SRGP has a better font model, 
   i.e. informs what the available fonts are in some sort of way   */


#define FAMILIES     0
#define STYLES       1
#define SIZES        2
#define EXAMPLE      3



PRIVATE void UpdateChildren (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, CURRENT_VALUE)) {
	char **possibleStyles = GP_possibleStyles();
	char **scan;
	GP_font newfont;
	SUIT_object names = SUIT_getChild (o, FAMILIES);
	SUIT_object styles = SUIT_getChild (o, STYLES);
	SUIT_object sizes = SUIT_getChild (o, SIZES);
	SUIT_object example = SUIT_getChild (o, EXAMPLE);
	char buf[10];
	void (*func)(SUIT_object);
	int i;
	
	newfont = * (GP_font *) new;
	SUIT_pressThisRadioButton (names, newfont.family);
	for (i=0, scan = possibleStyles; *scan != NULL; scan++, i++)
	    SUIT_setBoolean (SUIT_getChild(styles, i), CURRENT_VALUE,
			     (SUIT_stringContains (newfont.style, *scan) >= 0));
	sprintf (buf, "%d", (int)newfont.pointSize);
	SUIT_pressThisRadioButton (sizes, buf);
	SUIT_setFont (example, FONT, newfont);

	if ((func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer(o, CALLBACK_FUNCTION)) != NULL)
	    func (o);
    }
}



PRIVATE void StyleHit (SUIT_object button)
{
    char newstyle[100];
    GP_font newfont;
    SUIT_object parent = SUIT_getParent (SUIT_getParent (button));
    SUIT_object stylebank = SUIT_getChild (parent, STYLES);
    SUIT_object temp;
    int i, NumKids = SUIT_numberOfChildren(stylebank);
    
    strcpy (newstyle, "");
    for (i=0; i < NumKids; i++) {
	temp = SUIT_getChild (stylebank, i);
	if (SUIT_getBoolean (temp, CURRENT_VALUE))
	    strcat (newstyle, SUIT_getText(temp, LABEL));
    }
    newfont = SUIT_getFont (parent, CURRENT_VALUE);
    newfont = GP_defFont (newfont.family, newstyle, newfont.pointSize);

    SUIT_suspendMarkingRedisplay (parent);
    SUIT_setFont (parent, CURRENT_VALUE, newfont);
    SUIT_resumeMarkingRedisplay (parent);
}



PRIVATE void FontHit (SUIT_object fontradio)
{
    char *newfamily;
    GP_font newfont;
    SUIT_object parent = SUIT_getParent (fontradio);
    
    newfamily = SUIT_getEnumString (fontradio, CURRENT_VALUE);
    newfont = SUIT_getFont (parent, CURRENT_VALUE);
    newfont = GP_defFont (newfamily, newfont.style, newfont.pointSize);
    
    SUIT_suspendMarkingRedisplay (parent);
    SUIT_deluxeSetFont (parent, CURRENT_VALUE, newfont, OBJECT);
    SUIT_resumeMarkingRedisplay (parent);
}



PRIVATE void SizeHit (SUIT_object sizeradio)
{
    char *size;
    double newsize;
    GP_font newfont;
    SUIT_object parent = SUIT_getParent (sizeradio);
    
    size = SUIT_getEnumString(sizeradio, CURRENT_VALUE);
    sscanf (size, "%lf", &newsize);
    
    newfont = SUIT_getFont (parent, CURRENT_VALUE);
    newfont = GP_defFont(newfont.family, newfont.style, newsize);
    
    SUIT_suspendMarkingRedisplay (parent);
    SUIT_deluxeSetFont (parent, CURRENT_VALUE, newfont, OBJECT);
    SUIT_resumeMarkingRedisplay (parent);
}



SUIT_object SUIT_createFontPanel (char *name)
{
    SUIT_object stylebank, fontradio, sizeradio, example, o;
    char **fonts = GP_possibleFonts();
    char **styles = GP_possibleStyles();
    char **scan;
    int nameWidth = 0, styleWidth = 0, sizeWidth, totalHeight, totalWidth;
    int nameHeight = 0, styleHeight = 0, exampleHeight, margin, w, a, d;
    int border;
    
    o = SUIT_createBulletinBoardWithClass (name, "font panel");
    SUIT_deluxeSetBoolean (o, HAS_BORDER, FALSE, CLASS);
    margin = SUIT_getInteger (o, MARGIN);
    border = SUIT_getInteger (o, BORDER_WIDTH);
    
    GP_pushGraphicsState();
    GP_setFont (SUIT_deluxeGetFont (NULL, SUIT_SYSTEM_FONT, GLOBAL));
    
    fontradio = SUIT_createRadioButtons (SUIT_relativeName(o, "names"), FontHit);
      for (scan = fonts; *scan != NULL; scan++) {
  	SRGP_inquireTextExtent (*scan, &w, &a, &d);
  	w += a+d + 6*margin;
  	if (w > nameWidth)
  	    nameWidth = w;
  	nameHeight += a + d + 2*margin;
  	SUIT_addButtonToRadioButtons (fontradio, *scan);
      }
      SUIT_addChildToObject (o, fontradio);
    
    stylebank = SUIT_createStacker (SUIT_relativeName(o, "styles"));
      SUIT_setEnumString (stylebank, ACTIVE_DISPLAY, "vertical stacking");
      SUIT_setBoolean (stylebank, SHRINK_TO_FIT, FALSE);
      for (scan = styles; *scan != NULL; scan++) {
  	char buf[50];
  	SUIT_object temp;
  	sprintf (buf, "style %s", *scan);
  	temp = SUIT_createOnOffSwitch (SUIT_relativeName(stylebank, buf), StyleHit);
	SRGP_inquireTextExtent (*scan, &w, &a, &d);
	w += a+d + 6*margin;
  	if (w > styleWidth)
  		styleWidth = w;
  	styleHeight += a + d + 2*margin;
  	SUIT_setText (temp, LABEL, *scan);
 	SUIT_setBoolean (temp, HAS_BORDER, FALSE);
  	SUIT_setBoolean (temp, SHRINK_TO_FIT, FALSE);
  	SUIT_addChildToObject (stylebank, temp);
      }
      SUIT_addChildToObject (o, stylebank);

    
    sizeradio = SUIT_createRadioButtons (SUIT_relativeName(o, "sizes"), SizeHit);
      SUIT_addButtonToRadioButtons (sizeradio, "10");
      SUIT_addButtonToRadioButtons (sizeradio, "12");
      SUIT_addButtonToRadioButtons (sizeradio, "14");
      SUIT_addButtonToRadioButtons (sizeradio, "18");
      SUIT_addButtonToRadioButtons (sizeradio, "24");
      SRGP_inquireTextExtent ("24", &w, &a, &d);
      sizeWidth = w + a+d + 6*margin;
      SUIT_addChildToObject (o, sizeradio);
    
    example = SUIT_createLabel (SUIT_relativeName(o, "example text"));
      SUIT_setText (example, LABEL, "The quick brown fox jumps over the lazy dog");
      SUIT_setBoolean (example, SHRINK_TO_FIT, FALSE);
      SUIT_addChildToObject (o, example);
      exampleHeight = a + d + 2*margin;
    GP_popGraphicsState();
    
    totalWidth = nameWidth + 2*margin + styleWidth + sizeWidth;
    totalHeight = exampleHeight + margin + MAX(nameHeight, styleHeight);
    SUIT_changeObjectSize (o, totalWidth, totalHeight);
    SUIT_setViewport (fontradio, VIEWPORT,
		      SUIT_defViewport(border, exampleHeight+margin, nameWidth, totalHeight-border));
    SUIT_setViewport (stylebank, VIEWPORT,
		      SUIT_defViewport(nameWidth+margin, exampleHeight+margin,
				       nameWidth+margin+styleWidth, totalHeight-border));
    SUIT_setViewport (sizeradio, VIEWPORT,
		      SUIT_defViewport(totalWidth-sizeWidth, exampleHeight+margin,
				       totalWidth-border, totalHeight-border));
    SUIT_setViewport (example, VIEWPORT, SUIT_defViewport(0, 0, totalWidth, exampleHeight));
    /*SUIT_forceOnScreen (o);*/
    
    SUIT_registerInterest (o, UpdateChildren);
    return o;
}
