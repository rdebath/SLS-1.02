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

static XrmOptionDescRec options[] = {
   { "-controls", ".controls", XrmoptionSepArg, (caddr_t) NULL },
   { "-gravity",  ".gravity",  XrmoptionSepArg, (caddr_t) NULL },
   { "-fn",       ".font",     XrmoptionSepArg, (caddr_t) NULL },
   { "-repeat",   ".repeat",   XrmoptionNoArg,  (caddr_t) "on" },
   { "-retro",    ".retro",    XrmoptionSepArg, (caddr_t) NULL },
   { "-lateral",  ".lateral",  XrmoptionSepArg, (caddr_t) NULL },
};
#define numOptions (sizeof options / sizeof (XrmOptionDescRec))

static struct surface {
   char *name;
   float acceleration;  /* ft/sec^2 */
   float retro;
} surfaces[] = {
   { "mercury", -12.236,  50.0 },
   { "venus",   -29.170,  60.0 },
   { "earth",   -32.200,  60.0 },
   { "moon",     -5.310,  35.0 },
   { "mars",    -12.236,  50.0 },
   { "jupiter", -85.008, 150.0 },
   { "saturn",  -34.454,  60.0 },
   { "uranus",  -28.336,  60.0 },
   { "neptune", -36.708,  65.0 },
   { "pluto",    -1.610,  15.0 },
};
#define numSurfaces (sizeof surfaces / sizeof (struct surface))

/******************************************************************************
** Usage
**
** Print usage of command line arguments
******************************************************************************/

void Usage ()
{
   static char *usage[] = {
      "usage:  xlander [options]",
      "  where options are:",
      "  -controls <controls>",
      "  -fn <font-name>",
      "  -gravity <planet-name>",
      "  -lateral <thrust>",
      "  -repeat",
      "  -retro <thrust>",
      (char *) 0,
   };
   char **ptr;

   for (ptr = usage; *ptr; ptr++)
      fprintf (stderr, "%s\n", *ptr);
   exit (1);
}

/******************************************************************************
** LoadResources
**
** This creates the resource database for xlander and reads defaults out
** of various files (and the command line).  It also sets a few global
** variables based on values in the resource database.
******************************************************************************/

void LoadResources (argc, argv, lander)
   int *argc;
   char **argv;
   LANDER *lander;
{
   XrmDatabase defaults;
   XrmValue value;
   char *type, *xrm_string;
   int count;

   XrmInitialize ();
   defaults =
      XrmGetStringDatabase
	 ((xrm_string = XResourceManagerString (d)) ? xrm_string : "");
   if (defaults)
      XrmMergeDatabases (defaults, &resources);
   XrmParseCommand (&resources, options, numOptions, "xlander", argc, argv);
   if (*argc != 1)
      Usage ();
   if (XrmGetResource (resources, "xlander.gravity", "Xlander.Gravity",
		       &type, &value) == True) {
      acceleration = 0.0;
      for (count = 0; count < numSurfaces; count++)
	 if (!strcmp (value.addr, surfaces[count].name)) {
	    acceleration = surfaces[count].acceleration;
	    lander->retro_thrust = surfaces[count].retro;
	 }
      if (acceleration == 0.0) {
	 fprintf (stderr,
		  "Unknown planet `%s', using Earth gravity\n", value.addr);
	 acceleration = ACCELERATION;
      }
   } else
      acceleration = ACCELERATION;
   if (XrmGetResource (resources, "xlander.controls", "Xlander.Controls",
		       &type, &value) == True) {
      if (strlen (value.addr) != 5)
	 fprintf (stderr, "Invalid control key string; using default\n");
      else
	 strcpy (lander->controls, value.addr);
   }
   if (XrmGetResource (resources, "xlander.retro", "Xlander.Retro",
		       &type, &value) == True)
      lander->retro_thrust = (float) atof (value.addr);
   if (XrmGetResource (resources, "xlander.lateral", "Xlander.Lateral",
		       &type, &value) == True)
      lander->lateral_thrust = (float) atof (value.addr);
   if (XrmGetResource (resources, "xlander.repeat", "Xlander.Repeat",
		       &type, &value) != True)
      XAutoRepeatOff (d);
}
