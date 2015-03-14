#include "HEADERS.h"
#define SRGP_BOSS
#include "srgplocal.h"

static char message[] =  "Unable to open the SRGP logfile!";

int SRGP_errorOccurred = 0;

errorHandlingMode srgp__curErrHndlMode = FATAL_ERRORS;



/** SRGP_tracing
**/
void
SRGP_tracing (boolean please_trace)
{

   if ( ! SRGP_logStream) {
      /* LOGGING FILE IN LINE-BUFFERED MODE. */
      if (SRGP_logStream = fopen ("SRGPlogfile", "w")) 
#ifdef sun
	 setvbuf (SRGP_logStream, NULL, _IOLBF, 512);
#else
      {}
#endif
      else {
#ifdef THINK_C
	 ReportSpecialError (message, NON_FATAL);
         return;  /* we can't enable it, but we can continue! */
#else
	 fprintf (stderr, "%s\n", message);
	 exit (1);
#endif
      }
   }

   SRGP_trace (SRGP_logStream, "---- TRACE DISABLED\n");
   srgp__traceDisabled = ! please_trace;
   SRGP_trace (SRGP_logStream, "++++ TRACE ENABLED\n");
}


void
SRGP_disableDebugAids ()
{
   srgp__userDebugAidsDisabled = TRUE;
}


void
SRGP_enableBlockedWait ()
{
   srgp__blockedWaitEnabled = TRUE;
}



static void
InitSRGP (char *name, int width, int height, int requested_planes, boolean debugasap)
{
   register cnt;
   
   
   unlink ("./SRGPlogfile");

   ALLOC_RECORDS (srgp__bitmapPatternTable, pattern_table_entry, MAX_PATTERN_INDEX+1);
   ALLOC_RECORDS (srgp__pixmapPatternTable, pixpat_table_entry, MAX_PATTERN_INDEX+1);
   ALLOC_RECORDS (srgp__fontTable, fontInfo, MAX_FONT_INDEX+1);
   ALLOC_RECORDS (srgp__cursorTable, cursorInfo, MAX_CURSOR_INDEX+1);
   ALLOC_RECORDS (srgp__canvasTable, canvas_spec, MAX_CANVAS_INDEX+1);
#ifdef X11
   ALLOC_RECORDS (Xformat_vertices, XPoint, MAX_POINTLIST_SIZE);
#endif

   
   srgp__curActiveCanvasId = 0;
   srgp__curActiveCanvasSpec.max_xcoord = (width-1);
   srgp__curActiveCanvasSpec.max_ycoord = (height-1);
   
   SRGP__initGraphicsDevice (name, requested_planes, debugasap);


   /*************** INIT. CANVAS TABLE TO SHOW THAT ONLY CANVAS 0 IS ALIVE */
   srgp__canvasTable[0].drawable = srgp__curActiveCanvasSpec.drawable;
   for (cnt=1; cnt <= MAX_CANVAS_INDEX; cnt++)
      srgp__canvasTable[cnt].drawable.win = 0;
      
   
   SRGP__initCursorTable();

   SRGP__initDefaultPatterns();

   SRGP__initFont();

   SRGP__setCanvasDefaults();

   srgp__resizeCallback = NULL;

   /* INIT TRACING ET.AL. */
   srgp__traceDisabled = TRUE;
   SRGP_logStream = NULL;

   SRGP__initInputModule();
}




/*!*/
void SRGP_beginWithDebug 
   (char *name, int width, int height, int planes, boolean enable_trace)
/* FOR USE BY SYSTEM ADMINS ONLY */
{
   if (srgp__enabled)
      SRGP__error (ERR_ALREADY_ENABLED);
   else {
      srgp__enabled = TRUE;
      InitSRGP (name, width, height, planes, TRUE);
   }

   if (enable_trace) 
      SRGP_tracing (TRUE);
}


/*!*/
void SRGP_begin 
   (char *name, int width, int height, int planes, boolean enable_trace)
{
   if (srgp__enabled)
      SRGP__error (ERR_ALREADY_ENABLED);
   else {
      srgp__enabled = TRUE;
      InitSRGP (name, width, height, planes, FALSE);
   }

   if (enable_trace) 
      SRGP_tracing (TRUE);
}


/*!*/
void SRGP_registerResizeCallback (funcptr resizeCall)
{
   srgp__resizeCallback = resizeCall;
}



/*!*/
void SRGP_end ()
{
   SRGP_trace (SRGP_logStream, "SRGP_end\n");
   
#  ifdef THINK_C
   SRGP__cleanupMacMemory();
   DisposeWindow (srgpmac__cwindow);
#  endif

   srgp__enabled = FALSE;
   if (SRGP_logStream)
      fclose (SRGP_logStream);
}



void SRGP_setErrorHandlingMode (errorHandlingMode newmode)
{
   srgp__curErrHndlMode = newmode;
}


/** ROUTINES ALLOWING APPLICATION TO CHANGE SIZES OF TABLES
These may only be called *before* SRGP is enabled.
**/

static boolean CheckNotEnabledYet (void)
{
   if (srgp__enabled) {
      SRGP__error (ERR_ALREADY_ENABLED);
      return FALSE;
   }
   else
      return TRUE;
}


void SRGP_setMaxCanvasIndex (int i)
{
if (CheckNotEnabledYet())
   MAX_CANVAS_INDEX = i;
}

void SRGP_setMaxPatternIndex (int i)
{
if (CheckNotEnabledYet())
   MAX_PATTERN_INDEX = i;
}

void SRGP_setMaxCursorIndex (int i)
{
if (CheckNotEnabledYet())
   MAX_CURSOR_INDEX = i;
}

void SRGP_setMaxFontIndex (int i)
{
if (CheckNotEnabledYet())
   MAX_FONT_INDEX = i;
}

void SRGP_setMaxPointlistSize (int i)
{
if (CheckNotEnabledYet())
   MAX_POINTLIST_SIZE = i;
}

void SRGP_setMaxStringSize (int i)
{
if (CheckNotEnabledYet())
   MAX_STRING_SIZE = i;
}



void
SRGP__reactToScreenResize (int www, int hhh)
{
   if (srgp__curActiveCanvasId == 0)
      srgp__canvasTable[0] = srgp__curActiveCanvasSpec;

   srgp__canvasTable[0].max_xcoord = www - 1;
   srgp__canvasTable[0].max_ycoord = hhh - 1;

   if (srgp__curActiveCanvasId == 0)
      srgp__curActiveCanvasSpec = srgp__canvasTable[0];
   
   /* The locator measure needs to be updated, since its y coord is a
      function of the max_ycoord of the screen canvas. */
   srgp__cur_locator_measure.position.y =
      srgp__canvasTable[0].max_ycoord - srgp__cur_Xcursor_y;
   
   if (srgp__resizeCallback)
      (*srgp__resizeCallback) (www, hhh);

}


void
SRGP_changeScreenCanvasSize (int newwidth, int newheight)
{
   SRGP__forceScreenResize (newwidth, newheight);
   SRGP__reactToScreenResize (newwidth, newheight);
}
