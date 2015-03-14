#include "HEADERS.h"
#include "srgplocal.h"
#include "ChooseWhichQuickDraw.h"

/** Mac VERSION OF state.c
This contains no conditionally compiled code because it should be used
only in the Mac version.
**/

void ReportSpecialError (char *message, boolean is_fatal)
{
   GrafPtr saveport = thePort;
   WindowPtr tempwin;
   
   Rect window_bounds = {50, 50,   200,300};
   Rect te_bounds = {5,5, 145,245};
   SetPort (tempwin = NewWindow (0L, &window_bounds, 
		       (is_fatal ? "\pFatal Error" : "\pWarning"), true, 
                       rDocProc, -1L, false, 0));
   
   TextBox (message, (long)(strlen(message)), &te_bounds, teJustLeft);
   SRGP_beep();
   sleep (5);
   if (is_fatal)
      ExitToShell();
   else {
      DisposeWindow (tempwin);
      SetPort (saveport);
   }
}

static void InitMacintosh (void)
{
   MaxApplZone();
	
   InitGraf(&thePort);
   InitFonts();
   FlushEvents(everyEvent, 0);
   InitWindows();
   InitMenus();
   TEInit();
   InitDialogs(0L);
   InitCursor();

	
}

char res_message[] = "Resource file not found!  Read the MacSRGP_README file!";



void SRGP__initGraphicsDevice (char *name, int requested_planes, boolean debugasap)
{
   Rect windowBounds;
   char resource_proclaimer[256];
   
   InitMacintosh();
   
   GetIndString (resource_proclaimer, 12345, 1);
   PtoCstr(resource_proclaimer);
   if (0 != strcmp("SRGP resources",resource_proclaimer))
      ReportSpecialError (res_message, FATAL);
   
   SetRect (&windowBounds, 0, 40, 
   			  srgp__curActiveCanvasSpec.max_xcoord+1,
   			  40+srgp__curActiveCanvasSpec.max_ycoord+1);
   CtoPstr(name);
   
#ifdef COLOR_QUICKDRAW
   srgpmac__cwindow = 
      NewCWindow (0L, &windowBounds, name, true, noGrowDocProc, -1L, true, 0);
#else
   srgpmac__cwindow = 
      NewWindow (0L, &windowBounds, name, true, noGrowDocProc, -1L, true, 0);
#endif
   srgp__curActiveCanvasSpec.drawable.win = srgpmac__cwindow;   
   SetPort(srgpmac__cwindow);
   PtoCstr(name);
  
   SRGP__initColor (requested_planes);

   /* INIT TIME STAMP */
   srgpx__starttime = srgpx__cur_time = TickCount();

   /* SET GRAPHICS CONTEXT */

   /* INIT INPUT */

   /* SET 0th CANVAS-WINDOW PROPERTIES */

   /* We set up for backing store. */

   /* Load the sole menu in the menu bar. */	
   ClearMenuBar();
   InsertMenu (srgpmac__srgpmenu=GetMenu(200), 0);
   DisableItem (srgpmac__srgpmenu, 1);
   DrawMenuBar();
}



void SRGP__cleanupMacMemory()
{
   SRGP__cleanupColor();
   /* later must clean up canvases */
}


/** SRGP_refresh
**/

void
SRGP_refresh ()
{
/* only needed on X11 workstations */
}



/** SRGP_enableSynchronous
No-op for Mac version.
**/

void
SRGP_enableSynchronous ()
{
}



/*!*/
void
SRGP_allowResize (boolean allow)
{
   EnableItem (srgpmac__srgpmenu,1);
}


void
SRGP__forceScreenResize (int newwidth, int newheight)
{
   SizeWindow (srgpmac__cwindow, newwidth, newheight, FALSE);
}
