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
 * instrument.c contains routines that update the gauges at the bottom of
 * the window.
 */

#include "xlander.h"
#include "globals.h"

/******************************************************************************
** setupInstrBuffer
**
** This creates and zeros out a pixmap to be used when updating the control
** panel.
******************************************************************************/

void setupInstrBuffer ()
{
   instrBuffer = XCreatePixmap (d, instrWin, viewWidth, panelHeight,
				DefaultDepth (d, DefaultScreen (d)));
   XSetForeground (d, gcInstr, WhitePixel (d, DefaultScreen (d)));
   XFillRectangle (d, instrBuffer, gcInstr, 0, 0, viewWidth, panelHeight);
}

/******************************************************************************
** UpdateInstruments
**
** Given heading and direction of thrust in radians, this routine updates
** the indicators on the control panel.
******************************************************************************/

void UpdateInstruments (heading, roc, fuel, x, y)
   float heading, roc, fuel;
   int x, y;
{
   static int heading_x = 50, heading_y = 15;
   static int fuel_level = 80, old_x = 290, old_y = 10;
   int new_fuel_level = (int) fuel / 4;
   char buf[32];

   /*
    * Update heading indicator
    */
   XDrawLine (d, instrBuffer, gcXor, 50, 50, heading_x, heading_y);
   heading_x = 50 + 35 * cos (heading);
   heading_y = 50 - 35 * sin (heading);
   XDrawLine (d, instrBuffer, gcXor, 50, 50, heading_x, heading_y);

   /*
    * Update fuel gauge
    */
   XFillRectangle (d, instrBuffer, gcXor, 210, 90 - fuel_level, 20,
		   fuel_level);
   XFillRectangle (d, instrBuffer, gcXor, 210, 90 - new_fuel_level, 20,
		   new_fuel_level);
   fuel_level = new_fuel_level;

   /*
    * Display vertical speed
    */
   sprintf (buf, "%06.2f ft/s", roc);
   XDrawImageString
      (d, instrBuffer, gcInstr,
       120, 50 + ((font->ascent + font->descent) >> 1) - font->descent,
       buf, strlen (buf));

   /*
    * Update "radar"
    */
   XDrawPoint (d, instrBuffer, gcXor, old_x, old_y);
   old_x = WorldToRadarX (x);
   old_y = WorldToRadarY (y);
   XDrawPoint (d, instrBuffer, gcXor, old_x, old_y);
}
