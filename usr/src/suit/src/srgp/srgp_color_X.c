#include "HEADERS.h"
#include "srgplocal.h"

/** THIS FILE IS FOR X11 IMPLEMENTATION ONLY
**/

void 
SRGP__initColor (requested_planes)
{
   srgp__visual_class = DefaultVisual(srgpx__display, srgpx__screen)->class;
   srgp__available_depth = DefaultDepth(srgpx__display, srgpx__screen);

   if (srgp__available_depth == 1 || srgp__visual_class == StaticGray) {

      /***** PERFORMED FOR BILEVEL DISPLAYS */

      SRGP_BLACK = 1;
      SRGP_WHITE = 0;
      srgp__application_depth = srgp__max_pixel_value = 1;
      srgp__base_colorindex = 0;
   }
   

   else {

      /****** PERFORMED FOR COLOR DISPLAYS */

      SRGP_BLACK = 1;
      SRGP_WHITE = 0;

      if (requested_planes < 0) {
	 fprintf (stderr, "Fatal Error: insane parameter to SRGP_begin()\n\
            Application requesting negative number of planes.\n");
	 exit(1);
      }
      if ((requested_planes == 0) || 
	  (requested_planes > srgp__available_depth)) 
	 srgp__application_depth = srgp__available_depth;
      else
	 srgp__application_depth = requested_planes;
      srgp__max_pixel_value = (1 << srgp__application_depth) - 1;

      if (srgp__application_depth == srgp__available_depth) {

	 /***** APPL WANTS ENTIRE COLOR TABLE! */
	 srgpx__colormap = 
	    XCreateColormap (srgpx__display, 
			     srgp__curActiveCanvasSpec.drawable.win,
			     DefaultVisual(srgpx__display,srgpx__screen),
			     AllocAll);
	 XSetWindowColormap (srgpx__display, 
			     srgp__curActiveCanvasSpec.drawable.win,
			     srgpx__colormap);
      }
      else {

	 /***** APPL WANTS TO SHARE COLOR TABLE WITH THE OTHER CLIENTS */
	 unsigned long return_masks_here[8];
	 unsigned long return_pixels_here[1];
	 Status result;

	 srgpx__colormap = DefaultColormap(srgpx__display,srgpx__screen);

	 result = 
	    XAllocColorCells (srgpx__display,
			      srgpx__colormap,
			      TRUE /* contiguous planes desired */,
			      return_masks_here,
			      srgp__application_depth,
			      return_pixels_here,
			      1);
	 if (result == 0) {
	    fprintf (stderr, "%s\n%s\n%s\n%s\n%s\n",
		     "SRGP: Color table too full to share.",
		     "A solution is to have the SRGP application request",
		     "   0 planes in the 4th parameter to SRGP_begin.",
		     "For now, the application will have its own",
		     "   color table, rather than try to share.");
	    
	    srgpx__colormap = 
		XCreateColormap (srgpx__display, 
				 srgp__curActiveCanvasSpec.drawable.win,
				 DefaultVisual(srgpx__display,srgpx__screen),
				 AllocAll);
	    XSetWindowColormap (srgpx__display, 
				srgp__curActiveCanvasSpec.drawable.win,
				srgpx__colormap);
	    srgp__base_colorindex = 0;
	 } else
	     srgp__base_colorindex = return_pixels_here[0];
      }

      /* Only first two entries of LUT are init'd */
      XStoreNamedColor 
	 (srgpx__display, srgpx__colormap, "white", COLORINDEX(0), -1);
      XStoreNamedColor 
	 (srgpx__display, srgpx__colormap, "black", COLORINDEX(1), -1);
   }


   /*** DONE FOR ALL CONFIGURATIONS. */
   XSetWindowBackground (srgpx__display, 
			 srgp__curActiveCanvasSpec.drawable.win, XWHITE);
   XSetWindowBorder (srgpx__display, 
		     srgp__curActiveCanvasSpec.drawable.win, XBLACK);
}



void SRGP_loadColorTable
   (int startentry, int count,
    unsigned short *redi, 
    unsigned short *greeni,
    unsigned short *bluei)
{
   static XColor *x_color_structs = NULL;
   static int cursize_of_x_cs_array = 0;  /* number of XColor structs */
   register int i,j;
   int endi;
   register XColor *xcurcs;


   /* LEAVE IMMEDIATELY IF EXECUTING ON BILEVEL DISPLAY */
   if (srgp__available_depth == 1 || srgp__visual_class == StaticGray)
      return;

   endi = startentry + count;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadColorTable  %d  %d  %x %x %x\n",
		  startentry, count, redi, greeni, bluei);

      /* PERFORM CHECKING LEGALITY OF THE RANGE OF INDICES. */
      srgp_check_pixel_value (startentry, "start");
      srgp_check_pixel_value (endi-1, "end");
   }


   /* DYNAMICALLY (RE)ALLOCATE ARRAY OF XColor STRUCTURES */
   if (cursize_of_x_cs_array < count) {
      if (x_color_structs)
	 free ((char*)x_color_structs);
      x_color_structs = (XColor*) malloc (sizeof(XColor)*count);
      cursize_of_x_cs_array = count;

      /* Initialize a constant field of the XColor structs. */
      for (i=0; i<count; i++)
	 x_color_structs[i].flags = -1;
   }

   /* COPY INTENSITY VALUES INTO ARRAY. */
   for (i=startentry, j=0, xcurcs=x_color_structs; i<endi; i++,j++,xcurcs++){
      xcurcs->pixel = COLORINDEX(i);
      xcurcs->red = redi[j];
      xcurcs->green = greeni[j];
      xcurcs->blue = bluei[j];
   }

   XStoreColors (srgpx__display, srgpx__colormap, x_color_structs, count);
}




void
SRGP_inquireColorTable 
   (int startentry, int count,
    unsigned short *redi, 
    unsigned short *greeni,
    unsigned short *bluei)
{
   static XColor *x_color_structs = NULL;
   static int cursize_of_x_cs_array = 0;  /* number of XColor structs */
   register int i,j;
   int endi;
   register XColor *xcurcs;


   /* LEAVE IMMEDIATELY IF EXECUTING ON BILEVEL DISPLAY */
   if (srgp__available_depth == 1 || srgp__visual_class == StaticGray)
      return;

   endi = startentry + count;

   DEBUG_AIDS{
      /* PERFORM CHECKING LEGALITY OF THE RANGE OF INDICES. */
      srgp_check_pixel_value (startentry, "start");
      srgp_check_pixel_value (endi-1, "end");
   }


   /* !!!!!! LATER, THIS SHOULD USE SAME ARRAY AS IN color_X11.c */

   /* DYNAMICALLY (RE)ALLOCATE ARRAY OF XColor STRUCTURES */
   if (cursize_of_x_cs_array < count) {
      if (x_color_structs)
	 free ((char*)x_color_structs);
      x_color_structs = (XColor*) malloc (sizeof(XColor)*count);
      cursize_of_x_cs_array = count;
   }

   for (i=startentry, xcurcs=x_color_structs; i<endi; i++,xcurcs++) {
      xcurcs->pixel = COLORINDEX(i);
      xcurcs->flags = -1;
   }

   XQueryColors (srgpx__display, srgpx__colormap, x_color_structs, count);


   /* COPY INTENSITY VALUES INTO USER'S ARRAY. */
   for (j=0, xcurcs=x_color_structs; j<count; j++,xcurcs++){
      redi[j] = xcurcs->red;
      greeni[j] = xcurcs->green;
      bluei[j] = xcurcs->blue;
   }
}




void
SRGP_loadCommonColor (entry, name)
int entry;
char *name;   /* Null-terminated string of characters */
{
   /* IGNORE IF MONOCHROME */
   if (srgp__available_depth == 1 || srgp__visual_class == StaticGray)
      return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadCommonColor  %d  %s\n", entry, name);
      srgp_check_pixel_value (entry, "start/end");
   }

   XStoreNamedColor 
      (srgpx__display, srgpx__colormap, name, COLORINDEX(entry), -1);
}
