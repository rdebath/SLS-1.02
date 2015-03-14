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

#include "xlander.h"
#include "globals.h"

/*
 * A lander
 */
static LINE lander_data[] = {
   /* BODY */
   /* Top half */
   { 0, 800, 0, 300, 500, 0 },
   { 0, 800, 0, -300, 500, 0 },
   { 0, 800, 0, 0, 500, 300 },
   { 0, 800, 0, 0, 500, -300 },
   /* Sides */
   { 0, 500, 300, 300, 500, 0 },
   { 300, 500, 0, 0, 500, -300 },
   { 0, 500, -300, -300, 500, 0 },
   { -300, 500, 0, 0, 500, 300 },
   /* Bottom half */
   { 0, 200, 0, 300, 500, 0 },
   { 0, 200, 0, -300, 500, 0 },
   { 0, 200, 0, 0, 500, 300 },
   { 0, 200, 0, 0, 500, -300 },
   /* LEGS */
   { 300, 500, 0, 300, 0, 0 },
   { -300, 500, 0, -300, 0, 0 },
   { 0, 500, 300, 0, 0, 300 },
   { 0, 500, -300, 0, 0, -300 },
};
#define LANDERSIZE (sizeof lander_data / sizeof (LINE))

/*
 * Thrust flames
 */
static LINE thrust_data[] = {
   { 0, 200, 0, 100, -100, 30 },
   { 0, 200, 0, -50, -100, 100 },
   { 0, 200, 0, 10, -100, 40 },
   { 0, 200, 0, -100, -100, -40 },
};
#define THRUSTSIZE (sizeof thrust_data / sizeof (LINE))
   
/*
 * Shadow on ground
 */
static LINE shadow_data[] = {
   { 300, 0, 0, 0, 0, 300 },
   { 0, 0, 300, -300, 0, 0 },
   { -300, 0, 0, 0, 0, -300 },
   { 0, 0, -300, 300, 0, 0 },
};
#define SHADOWSIZE (sizeof shadow_data / sizeof (LINE))

static DATABASE *world, *craft, *thrust, *shadow;
static LANDER lander;
int mask;

/******************************************************************************
** DisplayWorld
**
** Display the world on the screen.
******************************************************************************/

void DisplayWorld ()
{
   DBPlot (world, &lander);
   DBPlot (craft, &lander);

   /*
    * Display thrust flames if we are thrusting
    */
   if (lander.retro_thruster != 0) {
      thrust->off_x = craft->off_x;
      thrust->off_y = craft->off_y;
      thrust->off_z = craft->off_z;
      DBPlot (thrust, &lander);
   }
   shadow->off_x = craft->off_x;
   shadow->off_z = craft->off_z;
   DBPlot (shadow, &lander);
   SwapBuffers ();
   XCopyArea (d, instrBuffer, instrWin, gcPanel, 0, 0,
	      viewWidth, panelHeight, 0, 0);
   XSync (d,False);
}

void main (argc, argv)
   int argc;
   char *argv[];
{
   XEvent event;
   void InitializeLander (), SetupSinCosTable (), Xinitialize ();
   void LoadResources (), DrawInstruments ();
   void UpdateOrientation (), DisplayAcceleration ();
#ifndef POSIX
   int sigblock (), sigsetmask (), Pause ();
#endif

   world = LoadDataBase ();
   craft = DBInitFromData (lander_data, LANDERSIZE);
   thrust = DBInitFromData (thrust_data, THRUSTSIZE);
   shadow = DBInitFromData (shadow_data, SHADOWSIZE);

   /*
    * Initial coordinates of the lander...
    */
   lander.retro_thrust = RETRO;
   lander.lateral_thrust = LATERAL_THRUST;
   strcpy (lander.controls, "8246 ");
   InitializeLander (craft, &lander);
   
   SetupSinCosTable ();
   Xinitialize ();
   LoadResources (&argc, argv, &lander);
   DrawInstruments ();
   
   for (;;) {
      XNextEvent (d, &event);
      if (event.type == Expose)
	 break;
   }

   /*
    * Mask SIGINT while doing X operations.  This prevents things
    * from getting screwed up when the handler is called at the
    * wrong time.
    */
#ifdef POSIX
   mask = sigsetmask (siggetmask()|sigmask (SIGINT));
#else
   mask = sigblock (sigmask (SIGINT));
#endif
   Pause ("Press any mouse button to begin");
   DisplayAcceleration ();
   for (;;) {
      UpdateOrientation (world, craft, &lander);
      DisplayWorld ();
      (void) sigsetmask (mask);
#ifdef POSIX
      mask = sigsetmask (siggetmask()|sigmask (SIGINT));
#else
      mask = sigblock (sigmask (SIGINT));
#endif
   }
}
