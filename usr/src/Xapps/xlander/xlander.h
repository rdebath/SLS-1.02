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
 * xlander.h contains global #includes and macro definitions used just
 * about everywhere
 */

#ifndef _xlander_h_
#define _xlander_h_

#include <stdio.h>
#include <math.h>
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xresource.h>
#include <X11/keysym.h>

#include "patchlevel.h"

#define FONT "-*-helvetica-bold-r-normal--12-*"

#define viewWidth 600            /* Width and height of view window */
#define viewHeight 300
#define panelHeight 100          /* Height of control panel */
#define WORLD_LENGTH 20000       /* Length of world in pixels */
#define WORLD_WIDTH 20000        /* Width of world in pixels */
#define HALF_WORLD_LENGTH (WORLD_LENGTH >> 1)
#define HALF_WORLD_WIDTH (WORLD_WIDTH >> 1)
#define PIXELS_PER_FOOT 6        /* Number of pixels per foot */
#define TICKS_PER_SECOND 3       /* Number of frames per second */
#define ACCELERATION -5.310      /* Acceleration of gravity (ft/sec^2) */
#define RETRO 35.0               /* Acceleration due to retroactive thruster */
#define LATERAL_THRUST 5.0       /* Acceleration due to lateral thruster */
#ifndef PI
#define PI 3.1415926535897932384
#endif
#define HALFPI 1.5707963         /* pi/2 */
#define TWOPI100 628
#define TWOPI 6.2831853
#define VERT_SPEED 30.0          /* Maximum vertical speed without crashing */
#define LAT_SPEED 30.0           /* Maximum lateral speed without crashing */
#define RETRO_BURN 1.6           /* Retroactive thruster fuel consumption */
#define LATERAL_BURN 0.4         /* Lateral thruster fuel consumption */
#define FULL_TANK 320            /* Full tank of fuel */
#define MAX_VELOC 640.0          /* Maximum velocity */

#define LANDER_WIDTH 600
#define LANDER_HEIGHT 600
#define PAD_WIDTH 750
#define PAD_HEIGHT 750

/*
 * Lines in 3-space
 */

typedef struct {
   float x1, y1, z1;
   float x2, y2, z2;
} LINE;

/*
 * Data structure for the "world".  The world consists of a linked list
 * of lines in 3-space, an array of segments used to plot the world in the
 * window, and the view orientation.
 */

struct dbentry {
   LINE line;
   struct dbentry *next;
};

typedef struct database {
   struct dbentry *lines;
   int linecount, segcount;
   XSegment *segments;
   int min_x, min_y, max_x, max_y;
   int off_x, off_y, off_z;
} DATABASE;

typedef struct {
   int px, py, pz;
   int pitch, roll, yaw;
   int front_thruster, rear_thruster;
   int left_thruster, right_thruster;
   int retro_thruster;
   float retro_thrust, lateral_thrust;
   char controls[7];
   float vert_speed, heading, lat_veloc;
   float fuel, alt;
} LANDER;

/*
 * Various routines
 */

DATABASE *DBInit (), *DBInitFromData (), *LoadDataBase ();
void DBInsert (), DBFinish (), DBPlot (), SwapBuffers (), exit ();
#ifndef _AIX
char *malloc ();
#endif
double atof ();
#ifdef sun
int printf (), fprintf (), time ();
#endif

/*
 * Transform x,y coordinate pairs from world space to position on
 * "radar" screen
 */
#define WorldToRadarX(x) (290 + (((int) (x) + (WORLD_WIDTH >> 1)) / 250))
#define WorldToRadarY(y) (90 - (((int) (y) + (WORLD_LENGTH >> 1)) / 250))

#endif _xlander_h_
