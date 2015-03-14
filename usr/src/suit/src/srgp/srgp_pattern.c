#include "HEADERS.h"
#include "srgplocal.h"
#include "default_patterns.h"


#define BUFFERSIZE   150

static char   inputline [BUFFERSIZE+1];

#ifdef X11
   static XImage *ximage_pix, *ximage_bit;
   static GC     putimagegc_pix, putimagegc_bit;
#endif



/** INITIALIZATION OF THE PATTERN TABLE
Bitmap pattern table is initialized; or at least a large portion of
it is initialized.

The pixmap pattern table's 0th element is initialized to all 0.
No other pixmap patterns are initialized.

WARNING: CURRENTLY THE MAC VERSION INIT'S NO PIXMAPS.

This routine also initializes some static variables that are 
necessary for making runtime changes to the pixmap table.
**/

void
SRGP__initDefaultPatterns()
{
   register i;

   /******** bitmap table */
   for (i=0; i <= 104; i++) {
#ifdef X11
      srgp__bitmapPatternTable[i] = 
	 XCreateBitmapFromData
	    (srgpx__display, srgp__canvasTable[0].drawable.win, 
	     default_bitpat[i], 8, 8);
#endif
#ifdef THINK_C
      memcpy (srgp__bitmapPatternTable[i], default_bitpat[i], (size_t)8);
#endif
   }
   
#ifdef X11
   for ( ; i <= MAX_PATTERN_INDEX; i++)
      srgp__bitmapPatternTable[i] = (pattern_table_entry) NULL;
#endif


   /******** pixmap table */
#ifdef X11
   srgp__pixmapPatternTable[0] = 
      XCreatePixmapFromBitmapData
	 (srgpx__display, srgp__canvasTable[0].drawable.win, 
	  default_bitpat[0], 8, 8,
	  COLORINDEX(SRGP_BLACK), COLORINDEX(SRGP_WHITE), 
	  srgp__available_depth);
#endif
#ifdef THINK_C
   srgp__pixmapPatternTable[0] = NULL;
#endif

   for (i=1; i <= MAX_PATTERN_INDEX; i++)
      srgp__pixmapPatternTable[i] = (pattern_table_entry) NULL;


#ifdef X11
   /******** imaging variables */
   putimagegc_pix =	
      XCreateGC (srgpx__display, srgp__canvasTable[0].drawable.win,
		 0L, NULL);
   putimagegc_bit =	
      XCreateGC (srgpx__display, srgp__bitmapPatternTable[0],
		 0L, NULL);
   ximage_pix = XGetImage (srgpx__display,
			   srgp__pixmapPatternTable[0],
			   0,0, 8,8,	
			   0xffff,
			   XYPixmap);
   ximage_bit = XGetImage (srgpx__display,
			   srgp__bitmapPatternTable[0],
			   0,0, 8,8,	
			   1,
			   XYPixmap);
#endif
}






/** LOAD A SINGLE BITMAP PATTERN
Application must send data in the form of 8 characters, the
same format created by the X bitmap editor.
**/

void SRGP_loadBitmapPattern (int pattern_id, char *data)
{
   register i,j;


   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadBitmapPattern %d %x\n", 
		  pattern_id, data);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

#ifdef X11
   /* CREATE THE BITMAP (if not already) */
   if ((pattern_table_entry) NULL == srgp__bitmapPatternTable[pattern_id]) 
      srgp__bitmapPatternTable[pattern_id] = 
	 XCreatePixmap
	    (srgpx__display, srgp__canvasTable[0].drawable.win, 8, 8,
	     1);

   if ((pattern_table_entry) NULL == srgp__bitmapPatternTable[pattern_id]) {
      SRGP__error (ERR_PATTERN_ALLOCATION);
      return;
   }

   /* PAINT THE PIXELS */
   for (i=0; i<8; i++)
      for (j=7; j>=0; j--)
	 XPutPixel (ximage_bit, i,j, (data[i]&(1<<j))?XBLACK:XWHITE);


   /* PUT THE XIMAGE INTO THE BITMAP */
   XPutImage (srgpx__display,
	      srgp__bitmapPatternTable[pattern_id],
	      putimagegc_bit,
	      ximage_bit,
	      0,0, 0,0, 8,8);
#endif

#ifdef THINK_C
   memcpy (srgp__bitmapPatternTable[pattern_id], data, 8);
#endif
}




#ifdef X11

/** LOAD A SINGLE PIXMAP PATTERN
Application must send data in the form of 8x8 array of integers.
The first integer is the color value for the top-left pixel.
The second integer is for the 2nd pixel on the top row.
   etc.
**/

void SRGP_loadPixmapPattern (int pattern_id, int *data)
{
   register i,j;


   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadPixmapPattern %d %x\n", 
		  pattern_id, data);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

   /* CREATE THE PIXMAP (if not already) */
   if ((pattern_table_entry) NULL == srgp__pixmapPatternTable[pattern_id]) 
      srgp__pixmapPatternTable[pattern_id] = 
	 XCreatePixmap
	    (srgpx__display, srgp__canvasTable[0].drawable.win, 8, 8,
	     srgp__available_depth);
   if ((pattern_table_entry) NULL == srgp__pixmapPatternTable[pattern_id]) {
      SRGP__error (ERR_PATTERN_ALLOCATION);
      return;
   }

   /* PAINT THE PIXELS */
   for (i=0; i<8; i++)
      for (j=7; j>=0; j--)
 	 XPutPixel (ximage_pix, i,j, XCOLOR(COLORINDEX(data[(i<<3)+j])));


   /* PUT THE XIMAGE INTO THE PIXMAP */
   XPutImage (srgpx__display,
	      srgp__pixmapPatternTable[pattern_id],
	      putimagegc_pix,
	      ximage_pix,
	      0,0, 0,0, 8,8);
}

#endif



/** LOAD BITMAP PATTERNS 
This command reads from an input stream (already
opened by the caller) that should contain one or more bitmap pattern specs.
It reads until EOF is seen, or until any parsing error occurs.

Each spec must occupy exactly two lines.  The first line must look like this:

static char bitpat_X[] = {

where X is a non-negative integer.

The second line must specify 8 hexademical numbers in this format:

   0x??, 0x??, 0x??, 0x??, 0x??, 0x??, 0x??, 0x??};

Each 2-hexdigit number is 8 bits and thus describes one "scan line" 
of the pattern.

This format is compatible with the output of
the X bitmap editor (called, appropriately, "bitmap") AS LONG AS
that editor is invoked in this manner:
   
    bitmap bitpat_X 8x8

The output files from several bitmap editing sessions can be concatenated
to form a single file defining many bitmap patterns, to be loaded
in a single call to SRGP_loadBitmapPatterns.

As a convenience, any lines beginning with '#' are ignored, but these
comment lines must not interrupt a single two-line spec sequence.

The closing of the input stream must be performed by the caller.

RETURNS 0 if no problem opening the file and parsing the input.
RETURNS 1 if any problems at all occurred.
**/

int
SRGP_loadBitmapPatternsFromFile (stream)
FILE *stream;
{ 
   int intbit[8];
   char charbit[8];
   int i, pattern_id, retval;


   DEBUG_AIDS{
      srgp_check_system_state();
      SRGP_trace (SRGP_logStream, "SRGP_loadBitmapPatternsFromFile %x\n", 
		  stream);
      LeaveIfNonFatalErr();
   }

   PUSH_TRACE;
   while (fgets(inputline, BUFFERSIZE, stream)) {   /* WHILE NOT EOF */

      /* IGNORE COMMENT LINES. */
      if (inputline[0] == '#')
	 continue;

      /* PARSE TO FIND PATTERN INDEX. */
      if (sscanf (inputline, "static char bitpat_%d", &pattern_id) != 1)
	 return 1;  /* ERROR! spec start doesn't match required format */

      /* GET NEXT LINE: IT CONTAINS THE PATTERN BITS. */
      if ( ! fgets(inputline, BUFFERSIZE, stream))
	 return 1;  /* ERROR! premature eof */

      /* PARSE THE PATTERN BITS LINE */
      retval = sscanf (inputline,
		       " 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x",
		       &intbit[0], &intbit[1], &intbit[2], &intbit[3], 
		       &intbit[4], &intbit[5], &intbit[6], &intbit[7]);
      if (retval != 8)
	 return 1;  /* ERROR! spec 2nd line doesn't match required format */

      /* TRANSFORM THE INT-ARRAY SPEC INTO CHAR-ARRAY SPEC. */
      for (i=0; i<8; i++)
	 charbit[i]=intbit[i];

      PUSH_TRACE;
      SRGP_loadBitmapPattern (pattern_id, charbit);
      POP_TRACE;
   }

   return 0;
}





#ifdef X11

/** LOAD PIXMAP PATTERNS FROM FILE
This command reads from an input stream (already
opened by the caller) that should contain one or more pixmap pattern specs.
It reads until EOF is seen, or until any parsing error occurs.

Each spec must occupy exactly nine lines.  The first line must look like this:

static int pixpat_X[] = {

where X is a non-negative integer.

The next eight lines each describes one "scan line" of the pixmap pattern,
looking like this:

   ?, ?, ?, ?, ?, ?, ?, ?

Each question mark represents a non-negative decimal color-value number.

As a convenience, any lines beginning with '#' are ignored, but these
comment lines must not interrupt a single nine-line spec sequence.

The closing of the input stream must be performed by the caller.

RETURNS 0 if no problem opening the file and parsing the input.
RETURNS 1 if any problems at all occurred.
**/
int
SRGP_loadPixmapPatternsFromFile (stream)
FILE *stream;
{
   register i;
   int pattern_id, retval;
   int data[8][8];
      

   DEBUG_AIDS{
      srgp_check_system_state();
      SRGP_trace (SRGP_logStream, "SRGP_loadBitmapPatternsFromFile %x\n", 
		  stream);
      LeaveIfNonFatalErr();
   }

   while (fgets(inputline, BUFFERSIZE, stream)) {   /* WHILE NOT EOF */

      /* IGNORE COMMENT LINES. */
      if (inputline[0] == '#')
	 continue;

      /* PARSE TO FIND PATTERN INDEX. */
      if (sscanf (inputline, "static int pixpat_%d", &pattern_id) != 1)
	 return 1;  /* ERROR! spec start doesn't match required format */

      /* GET AND PROCESS THE NEXT EIGHT LINES */
      for (i=0; i<8; i++) {

	 if ( ! fgets(inputline, BUFFERSIZE, stream))
	    return 1;  /* ERROR! premature eof */

	 retval = sscanf (inputline,
			  "%d, %d, %d, %d, %d, %d, %d, %d",
			  &data[i][0], &data[i][1], &data[i][2], &data[i][3], 
			  &data[i][4], &data[i][5], &data[i][6], &data[i][7]);
	 if (retval != 8)
	    return 1;  /* ERROR! spec line doesn't match required format */
      }

      PUSH_TRACE;
   SRGP_loadPixmapPattern (pattern_id, &(data[0][0]));
   POP_TRACE;
   }

   return 0;
}

#endif
