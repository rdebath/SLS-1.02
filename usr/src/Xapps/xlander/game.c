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
 * game.c contains routines that actually move the craft around in the
 * 3-d world.
 */

#include "xlander.h"
#include "globals.h"

static int score = 0;

/******************************************************************************
** Pause
**
** Wait for the user to hit a button, while displaying a message
******************************************************************************/

int Pause (string)
   char *string;
{
   XEvent event;
   int x, y;
   extern int mask;
   void DisplayWorld ();

   (void) sigsetmask (mask);
   x = (viewWidth >> 1) - (XTextWidth (font, string, strlen (string)) >> 1);
   y = (viewHeight >> 2) - ((font->ascent + font->descent) >> 1);
   DisplayWorld ();
   XDrawString (d, viewWin, gcXor, x, y, string, strlen (string));
   for (;;) {
      XNextEvent (d, &event);
      switch (event.type) {
      case ButtonPress:
#ifdef POSIX
	 mask = sigsetmask (siggetmask()|sigmask (SIGINT));
#else
	 mask = sigblock (sigmask (SIGINT));
#endif
	 return event.xbutton.button;
      case Expose:
	 if (event.xexpose.count == 0) {
	    DisplayWorld ();
	    XDrawString (d, viewWin, gcXor, x, y, string, strlen (string));
	 }
	 break;
      default:
	 break;
      }
   }
}

/******************************************************************************
** CrunchLander
**
** End the game.
******************************************************************************/

void CrunchLander ()
{
   char buf[32];
   void CleanupAndExit (), DisplayWorld ();
   
   sprintf (buf, "CRASH!!     Final Score: %d", score);
   XDrawImageString (d, instrBuffer, gcInstr, 381, 84, buf, strlen (buf));
   DisplayWorld ();
   Pause ("Press button to exit");
   printf ("Final Score: %d\n", score);
   CleanupAndExit ();
}

/******************************************************************************
** RateLanding
**
** Award points based on how well the player landed
******************************************************************************/

void RateLanding (db, lander)
   DATABASE *db;
   LANDER *lander;
{
   char buf[32];
   extern LINE landingpad[];
   int x_distance, z_distance;  /* Distance from center of pad */
   int abs ();
   void InitializeLander (), DisplayAcceleration ();

   x_distance =
      abs ((int) (db->off_x - (landingpad[0].x1 + (PAD_WIDTH >> 1))));
   z_distance =
      abs ((int) (db->off_z - (landingpad[0].z1 + (PAD_HEIGHT >> 1))));
   if (x_distance > 250 || z_distance > 250)
      sprintf (buf, "Landed Off Pad!  Score: %d", score);
   else {
      score += (int)
	 (100.0 * (1 - ((lander->vert_speed + lander->lat_veloc) / 80.0)));
      sprintf (buf, "Nice Landing!    Score: %d", score);
   }
   XDrawImageString (d, instrBuffer, gcInstr, 381, 84, buf, strlen (buf));
   Pause ("Press mouse button to continue");
   InitializeLander (db, lander);

   /*
    * Make things a little tougher
    */
   acceleration -= 5.0;
   lander->retro_thrust += 5.0;
   DisplayAcceleration ();
}

/******************************************************************************
** DisplayAcceleration
**
** Display acceleration due to gravity in window
******************************************************************************/

void DisplayAcceleration ()
{
   char buf[32];

   XSetForeground (d, gcInstr, BlackPixel (d, DefaultScreen (d)));
   XFillRectangle (d, instrBuffer, gcInstr, 380, 30, 200, 20);
   XSetForeground (d, gcInstr, WhitePixel (d, DefaultScreen (d)));
   sprintf (buf, "Gravity:  %f ft*s^-2", acceleration);
   XDrawString (d, instrBuffer, gcInstr,
		480 - (XTextWidth (font, buf, strlen (buf)) >> 1), 40,
		buf, strlen (buf));
}

/******************************************************************************
** UpdateOrientation
**
** This routine, given the disposition of the craft, computes the craft's
** next position and updates the gauges.  It also takes care of all mouse
** input.
**
** Pitch and roll work, but are unused in this version of the simulation.
******************************************************************************/

void UpdateOrientation (world, craft, lander)
   DATABASE *world, *craft;
   LANDER *lander;
{
   float lat_accel_x, lat_accel_y;
   float lat_veloc_x, lat_veloc_y;
   XEvent event;
   KeySym keysym;
   char ch;
   void UpdateInstruments ();

   if (XCheckMaskEvent (d,
			ButtonPressMask |
			KeyPressMask |
			KeyReleaseMask, &event) == True) {
      switch (event.type) {
      case KeyPress:
	 if (lander->fuel > 0.0) {
	    XLookupString (&event, &ch, 1, &keysym, (XComposeStatus *) 0);
	    if (ch == lander->controls[0] || keysym == XK_Up)
	       lander->rear_thruster = lander->lateral_thrust;
	    else if (ch == lander->controls[1] || keysym == XK_Down)
	       lander->front_thruster = lander->lateral_thrust;
	    else if (ch == lander->controls[2] || keysym == XK_Left)
	       lander->left_thruster = lander->lateral_thrust;
	    else if (ch == lander->controls[3] || keysym == XK_Right)
	       lander->right_thruster = lander->lateral_thrust;
	    else if (ch == lander->controls[4])
	       lander->retro_thruster = lander->retro_thrust;
	 }
	 break;
      case KeyRelease:
	 XLookupString (&event, &ch, 1, &keysym, (XComposeStatus *) 0);
	 if (ch == lander->controls[0] || keysym == XK_Up)
	    lander->rear_thruster = 0;
	 else if (ch == lander->controls[1] || keysym == XK_Down)
	    lander->front_thruster = 0;
	 else if (ch == lander->controls[2] || keysym == XK_Left)
	    lander->left_thruster = 0;
	 else if (ch == lander->controls[3] || keysym == XK_Right)
	    lander->right_thruster = 0;
	 else if (ch == lander->controls[4])
	    lander->retro_thruster = 0;
	 break;
      default:
         break;
      }
   }

   if (lander->retro_thruster > 0)
      lander->fuel -= RETRO_BURN;
   if (lander->front_thruster > 0)
      lander->fuel -= LATERAL_BURN;
   if (lander->rear_thruster > 0)
      lander->fuel -= LATERAL_BURN;
   if (lander->left_thruster > 0)
      lander->fuel -= LATERAL_BURN;
   if (lander->right_thruster > 0)
      lander->fuel -= LATERAL_BURN;
   lander->vert_speed +=
      (lander->retro_thruster + acceleration) / TICKS_PER_SECOND;
   lander->alt += lander->vert_speed / TICKS_PER_SECOND;
   lat_accel_x = lander->right_thruster - lander->left_thruster;
   lat_accel_y = lander->rear_thruster - lander->front_thruster;
   lat_veloc_x = lander->lat_veloc * cos (lander->heading) + lat_accel_x;
   lat_veloc_y = lander->lat_veloc * sin (lander->heading) + lat_accel_y;
   craft->off_x += (lat_veloc_x / TICKS_PER_SECOND) * PIXELS_PER_FOOT;
   craft->off_z += (lat_veloc_y / TICKS_PER_SECOND) * PIXELS_PER_FOOT;
   lander->lat_veloc =
      sqrt (lat_veloc_x * lat_veloc_x + lat_veloc_y * lat_veloc_y);

   if (lander->lat_veloc > MAX_VELOC)
      lander->lat_veloc = MAX_VELOC;
   if (lander->vert_speed > MAX_VELOC)
      lander->vert_speed = MAX_VELOC;

   lander->heading =
      (lat_veloc_x != 0.0) ? atan2 (lat_veloc_y, lat_veloc_x) : 0.0;

   /*
    * Wrap the coordinates around as the craft hits the boundaries
    * of the "world".  Also update the view so that it follows the craft
    * around with a little bit of lag.  This gives a pretty neat effect.
    */

   lander->px += (craft->off_x - lander->px) / 5;
   lander->py += (craft->off_y - lander->py) / 5;
   lander->pz += (craft->off_z - 2000 - lander->pz) / 5;

   if (craft->off_x < world->min_x) {
      craft->off_x = world->max_x;
      lander->px += WORLD_WIDTH;
   }
   else if (craft->off_x > world->max_x) {
      craft->off_x = world->min_x;
      lander->px -= WORLD_WIDTH;
   }
   if (craft->off_z < world->min_y) {
      craft->off_z = world->max_y;
      lander->pz += WORLD_LENGTH;
   }
   else if (craft->off_z > world->max_y) {
      craft->off_z = world->min_y;
      lander->pz -= WORLD_LENGTH;
   }
   
   if (lander->heading < 0.0)
      lander->heading += TWOPI;
   else if (lander->heading > TWOPI)
      lander->heading -= TWOPI;

   if (lander->alt < 0.0) {
      if (-lander->vert_speed > VERT_SPEED ||
	  lander->lat_veloc > LAT_SPEED) {
	 (void) printf ("CRASH!  Vertical speed = %f ft/sec\n",
			lander->vert_speed);
	 (void) printf ("        Lateral speed =  %f ft/sec\n",
			lander->lat_veloc);
	 CrunchLander ();
      }
      else
	 RateLanding (craft, lander);
   }

   craft->off_y = (int) lander->alt * PIXELS_PER_FOOT;
   UpdateInstruments (lander->heading, lander->vert_speed, lander->fuel,
		      craft->off_x, craft->off_z);
}
