/******************************************************************************
** XLander - A three-dimensional view-oriented lunar landing simulation for X
**
** Authors:
** Paul Riddle (paulr@umbc3.umbc.edu)
** Mike Friedman (mikef@umbc3.umbc.edu)
**
** University of Maryland, Baltimore Campus
**
** This program may be freely distributed in any form, providing the authors'
** names stay with it.  If you use any portion of this code, please give us
** credit.  Let us know if you like it!
******************************************************************************/

/*
 * Initialize.c - contains routines to set things up
 */

#include "xlander.h"
#include "globals.h"

#define EDGE_LENGTH 1000

extern float SIN[], COS[];

/*
 * A landing pad...
 */
LINE landingpad[] = {
   { 0, 0, 0, 750, 0, 0 },
   { 0, 0, 0, 0, 0, 750, },
   { 750, 0, 0, 750, 0, 750 },
   { 0, 0, 750, 750, 0, 750 },
   { 0, 0, 0, 0, 750, 0 },
   { 0, 750, 0, 150, 750, 0 },
   { 150, 750, 0, 150, 550, 0 },
   { 150, 550, 0, 0, 550, 0 },
};
#define PADSIZE (sizeof landingpad / sizeof (LINE))

static XKeyboardState keyboard_state;

/******************************************************************************
** InitializeLander
**
** Set up initial values for the lander when beginning a new game or
** a new landing.
******************************************************************************/

void InitializeLander (craft, lander)
   DATABASE *craft;
   LANDER *lander;
{
   lander->px = craft->off_x = 0;
   lander->py = craft->off_y = 8000;
   craft->off_z = -HALF_WORLD_LENGTH;
   lander->pz = craft->off_z - 2000;
   lander->front_thruster = lander->rear_thruster = 0;
   lander->left_thruster = lander->right_thruster = 0;
   lander->retro_thruster = 0;
   lander->vert_speed = 0.0;
   lander->heading = 1.36;
   lander->lat_veloc = 100.0;
   lander->fuel = 320.0;
   lander->alt = craft->off_y / PIXELS_PER_FOOT;
}

/******************************************************************************
** CleanupAndExit
**
** Restore original keyboard state and exit program.
******************************************************************************/

void CleanupAndExit ()
{
   XKeyboardControl control;

   control.auto_repeat_mode = keyboard_state.global_auto_repeat;
   XChangeKeyboardControl (d, KBAutoRepeatMode, &control);
   XCloseDisplay (d);
   exit (0);
}

/******************************************************************************
** Xinitialize
**
** This routine sets up all the necessary windows, graphics contexts, etc.
** needed by the program, and draws the instrument display.
******************************************************************************/

void Xinitialize ()
{
   Window win;
   unsigned long black, white;
   Pixmap stipple;
   XSizeHints size_hints;
   static char stipple_bits[] = {
      0x01, 0x02,
   };
   void setupBuffer (), setupInstrBuffer ();

   if (!(d = XOpenDisplay (""))) {
      (void) fprintf (stderr, "Error:  Can't open display\n");
      exit (1);
   }

   black = BlackPixel (d, DefaultScreen (d));
   white = WhitePixel (d, DefaultScreen (d));

   if (!(font = XLoadQueryFont (d, FONT)))
      if (!(font = XLoadQueryFont (d, "fixed"))) {
	 (void) fprintf (stderr, "Error:  Can't find default font\n");
	 exit (1);
      }

   /*
    * Create a window for the view, a window for the control panel,
    * and a parent that holds both of them
    */
   win = XCreateSimpleWindow (d, DefaultRootWindow (d),
			      0, 0, viewWidth, viewHeight + panelHeight, 1,
			      white, black);
   viewWin = XCreateSimpleWindow (d, win,
				  0, 0, viewWidth, viewHeight, 1,
				  white, black);
   instrWin = XCreateSimpleWindow (d, win, 0, viewHeight, viewWidth,
				   panelHeight, 1,
				   black, white);
   XSelectInput (d, viewWin,
		 ExposureMask |
		 KeyPressMask |
		 KeyReleaseMask |
		 ButtonPressMask);
   XStoreName (d, win, "XLander");

   /*
    * Create the graphics contexts for the view, the control panel
    * and the instruments.
    */
   gcView = XCreateGC (d, viewWin, 0L, (XGCValues *) 0);
   gcInstr = XCreateGC (d, instrWin, 0L, (XGCValues *) 0);
   gcPanel = XCreateGC (d, instrWin, 0L, (XGCValues *) 0);
   gcXor = XCreateGC (d, instrWin, 0L, (XGCValues *) 0);
   XSetFunction (d, gcXor, GXxor);
   XSetForeground (d, gcXor, black ^ white);
   XSetBackground (d, gcXor, 0L);
   XSetLineAttributes (d, gcXor, 6, LineSolid, CapRound, JoinRound);
   XSetBackground (d, gcInstr, black);
   XSetForeground (d, gcView, white);
   XSetBackground (d, gcView, black);
   XSetForeground (d, gcPanel, black);
   XSetBackground (d, gcPanel, white);
   stipple = XCreateBitmapFromData (d, instrWin, stipple_bits, 2, 2);
   XSetStipple (d, gcPanel, stipple);
   XSetFillStyle (d, gcPanel, FillStippled);
   XSetLineAttributes (d, gcInstr, 6, LineSolid, CapRound, JoinRound);

   /*
    * Since resizing the window serves no purpose, set the min_size and
    * max_size hints to be the window size
    */
   size_hints.flags = PMinSize | PMaxSize;
   size_hints.min_width = size_hints.max_width = viewWidth;
   size_hints.min_height = size_hints.max_height = viewHeight + panelHeight;
   XSetNormalHints (d, win, &size_hints);

   /*
    * Finally, map all our windows, create pixmaps for simulated double
    * buffering.
    */
   XMapWindow (d, win);
   XMapSubwindows (d, win);
   setupBuffer ();
   setupInstrBuffer ();

   /*
    * Shut off keyboard autorepeat for the duration of the game
    */
   XGetKeyboardControl (d, &keyboard_state);
   (void) signal (SIGINT, CleanupAndExit);
}

/******************************************************************************
** DrawInstruments
**
** This plots the instrument panel in its background pixmap.
******************************************************************************/

void DrawInstruments ()
{
   char buf[50], *type;
   char *authors = "by Paul Riddle and Mike Friedman";
   char *score = "Score:  0";
   XFontStruct *instrFont;
   XrmValue value;

   XFillRectangle (d, instrBuffer, gcPanel, 0, 0, viewWidth, panelHeight);
   XSetForeground (d, gcInstr, BlackPixel (d, DefaultScreen (d)));

   /* Heading indicator */
   XFillArc (d, instrBuffer, gcInstr, 10, 10, 80, 80, 0, 23040);

   /* Fuel gauge */
   XFillRectangle (d, instrBuffer, gcInstr, 210, 10, 30, 80);

   /* "Radar" display */
   XFillRectangle (d, instrBuffer, gcInstr, 290, 10, 80, 80);

   /* Title/credit and score areas */
   XFillRectangle (d, instrBuffer, gcInstr, 380, 10, 200, 40);
   XFillRectangle (d, instrBuffer, gcInstr, 380, 70, 200, 20);

   XSetForeground (d, gcInstr, WhitePixel (d, DefaultScreen (d)));

   /* Indicate position of landing pad on "Radar" display */
   XDrawArc (d, instrBuffer, gcInstr,
	     WorldToRadarX (landingpad[0].x1),
             WorldToRadarY (landingpad[0].z1), 1, 1, 0, 23040);

   /* Gauge needles */
   XDrawLine (d, instrBuffer, gcInstr, 50, 50, 50, 15);

   XDrawPoint (d, instrBuffer, gcInstr, 290, 10);

   /* Credits and gauge markings */
   (void) sprintf (buf, "XLander Patchlevel %d", PATCHLEVEL);

   if (XrmGetResource (resources, "xlander.font", "Xlander.Font", &type,
		       &value) == True) {
      if (!(instrFont = XLoadQueryFont (d, value.addr))) {
	 (void) fprintf (stderr,
			 "Can't find font `%s', using default\n",
			 value.addr);
      } else {
	 XFreeFont (d, font);
	 font = instrFont;
      }
   }
   XSetFont (d, gcInstr, font->fid);
   XSetFont (d, gcXor, font->fid);
   XDrawString (d, instrBuffer, gcInstr, 231, 22, "F", 1);
   XDrawString (d, instrBuffer, gcInstr, 231, 86, "E", 1);
   XFillRectangle (d, instrBuffer, gcInstr, 210, 10, 20, 80);
   XDrawString (d, instrBuffer, gcInstr, 420, 25, buf, strlen (buf));
   XDrawString (d, instrBuffer, gcInstr, 381, 40, authors, strlen (authors));
   XDrawString (d, instrBuffer, gcInstr, 381, 84, score, strlen (score));
   XDrawImageString
      (d, instrBuffer, gcInstr,
       245, 50 + ((font->ascent + font->descent) >> 1) - font->descent,
       "Fuel", 4);
   XDrawString (d, instrBuffer, gcXor, 30, 30, "Heading", 7);
}

/******************************************************************************
** SetupSinCosTable
**
** This loads a table with sine and cosine values.  The table is later
** accessed to speed up computations when plotting.
******************************************************************************/

void SetupSinCosTable ()
{
   int i;
   float t = 0.0;
   
   for (i = 0; i < TWOPI100; i++, t += TWOPI / TWOPI100) {
      SIN[i] = (float) sin (t);
      COS[i] = (float) cos (t);
   }
}

/******************************************************************************
** LoadDataBase
**
** This routine loads the "world" database, which is basically an array of
** line segments in 3-space.  The lunar surface is a grid of hexagons with
** some of the endpoints raised to form mountains.  The effect is a pretty
** interesting random landscape.
** The landing pad and a few other things are also drawn in here.
******************************************************************************/

DATABASE *LoadDataBase ()
{
   int count = 0, x, y, r = 0, height, goal_x, goal_y;
   float x_offset = (float) EDGE_LENGTH * cos (60.0 * PI / 180.0);
   float y_offset = (float) EDGE_LENGTH * sin (60.0 * PI / 180.0);
   LINE line;
   DATABASE *world = DBInit ();
   long random ();
#ifndef sgi
   void srandom ();
#endif

   world->min_x = world->min_y = -HALF_WORLD_LENGTH;
   world->max_x = world->max_y = HALF_WORLD_WIDTH;
   srandom ((long) time ((int *) 0));
   for (x = -HALF_WORLD_WIDTH; x < HALF_WORLD_WIDTH;
	r ^= 1, x += EDGE_LENGTH + (int) x_offset)
      for (y = r * (int) y_offset - HALF_WORLD_LENGTH; y < HALF_WORLD_LENGTH;
	   y += (int) (2.0 * y_offset)) {
	 height = random () % 3 ? 0 : random () % 2500;
	 line.x1 = x;
	 line.y1 = height;
	 line.z1 = y;
	 line.x2 = x + EDGE_LENGTH;
	 line.y2 = 0;
	 line.z2 = y;
	 DBInsert (world, &line);
	 line.x1 = x;
	 line.y1 = height;
	 line.z1 = y;
	 line.x2 = x - (int) x_offset;
	 line.y2 = 0;
	 line.z2 = y - (int) y_offset;
	 DBInsert (world, &line);
	 line.x1 = x;
	 line.y1 = height;
	 line.z1 = y;
	 line.x2 = x - (int) x_offset;
	 line.y2 = 0;
	 line.z2 = y + (int) y_offset;
	 DBInsert (world, &line);
      }

   /*
    * Drop the landing pad "somewhere on the ground"
    */
   goal_x = (random () % WORLD_WIDTH) - HALF_WORLD_WIDTH;
   goal_y = (random () % WORLD_LENGTH) - HALF_WORLD_LENGTH;
   for (count = 0; count < PADSIZE; count++) {
      landingpad[count].x1 += goal_x;
      landingpad[count].x2 += goal_x;
      landingpad[count].z1 += goal_y;
      landingpad[count].z2 += goal_y;
      DBInsert (world, &landingpad[count]);
   }
   DBFinish (world);
   return world;
}
