#include "HEADERS.h"
#include "srgplocal.h"

void
SRGP__initFont (void)
{
   register i;
   
#ifdef X11
   srgp__fontTable[0] = XLoadQueryFont (srgpx__display, SRGP_DEFAULT_FONT_0);
   if (srgp__fontTable[0] == (XFontStruct*)NULL) {
      fprintf (stderr, "System administration problem!\n\
	X could not find font named %s\n\
	System administrator should edit srgplocal.h and change the\n\
	definition of SRGP_DEFAULT_FONT_0.\n",
	SRGP_DEFAULT_FONT_0);
      exit(1);
   }
   for (i=1; i <= MAX_FONT_INDEX; i++)
      srgp__fontTable[i] = (XFontStruct*)NULL;
#endif

#ifdef THINK_C
   srgp__fontTable[0].txFont = 0;
   srgp__fontTable[0].txFace = 0;
   srgp__fontTable[0].txSize = 12;  
   for (i=1; i <= MAX_FONT_INDEX; i++)
      srgp__fontTable[i].txFont = -1;
#endif
}


void
SRGP_loadFont (int font_index, char *name)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      SRGP_trace (SRGP_logStream, "SRGP_loadFont %d '%s'\n", font_index, name);
      srgp_check_font_index(font_index);
      LeaveIfNonFatalErr();
   }

   /***** SOME DAY WE SHOULD CALL XUnloadFont TO CLEAN UP *****/

#ifdef X11
   if ( !
       (srgp__fontTable[font_index] =
	 XLoadQueryFont (srgpx__display, name))) {
      SRGP__error (ERR_BAD_FONT_FILENAME, name);
      return;
   }
#endif
#ifdef THINK_C
   {
      char *family=malloc((size_t)(strlen(name)+1));
      char *size, *style;
      int fnum, sizenum=-1, stylenum=0;
      
      strcpy (family, name);
      size = strchr(family, '.') + 1;
      if (size==(char*)1) {
         SRGP__error (ERR_BAD_FONT_FILENAME, name);
         return;
      }
      *(size-1) = '\0';
      CtoPstr(family);
      GetFNum (family, &fnum);
      if (fnum==0) {
         SRGP__error (ERR_BAD_FONT_FILENAME, name);
         return;
      }
      style = strchr(size, '.') + 1;
      if (style != (char*)1) {
         *(style-1) = '\0';
         while (*style) {
            switch (*style) {
               case 'b': stylenum|=bold; break;
               case 'i': stylenum|=italic; break;
               case 'u': stylenum|=underline; break;
               case 'o': stylenum|=outline; break;
               case 's': stylenum|=shadow; break;
               case 'c': stylenum|=condense; break;
               case 'e': stylenum|=extend; break;
            }
            style++;
         }
      }
      sscanf (size, "%d", &sizenum);
      if (sizenum<1) {
         SRGP__error (ERR_BAD_FONT_FILENAME, name);
         return;
      }
      free (family);
      srgp__fontTable[font_index].txFont = fnum;
      srgp__fontTable[font_index].txSize = sizenum;
      srgp__fontTable[font_index].txFace = stylenum;
   }
#endif
      
   if (font_index == srgp__curActiveCanvasSpec.attributes.font)
#ifdef X11
      XSetFont
	 (srgpx__display,
	  srgp__curActiveCanvasSpec.gc_frame,
	  srgp__fontTable[font_index]->fid);
#endif
#ifdef THINK_C
      TextFont(srgp__fontTable[font_index].txFont);
      TextSize(srgp__fontTable[font_index].txSize);
      TextFace(srgp__fontTable[font_index].txFace);
#endif
      
}





void
SRGP_inquireTextExtent (char *str, int *width, int *height, int *descent)
{
#ifdef X11
   register XFontStruct *fontstruct;
   register char *cp;  /* traverses through the string */
   XCharStruct overall;
   int dir, asc, desc;
   
   fontstruct = srgp__fontTable[srgp__curActiveCanvasSpec.attributes.font];
   XTextExtents (fontstruct, str, strlen(str), &dir, &asc, &desc, &overall);
   *height = asc;
   *descent = desc;
   *width = overall.width;
#endif

#ifdef THINK_C
   FontInfo theinfo;

   GetFontInfo (&theinfo);
   *height = theinfo.ascent;
   *descent = theinfo.descent;
   CtoPstr(str);
   *width = StringWidth(str);
   PtoCstr(str);
#endif

}


