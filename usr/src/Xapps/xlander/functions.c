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
 * Functions.c - contains 3-d plotting routines
 */

#include "xlander.h"
#include "globals.h"

#define   CRASHCODE   2

float  SIN[TWOPI100],COS[TWOPI100];

void Xinitialize ();

void LoadPerspectiveMatrix (CTM,z)
   float CTM[4][4];
   float z;
{
   float S, Tx, Ty, fd = 200.0;
   
   if (z + fd)
      S = fd / (z + fd);
   else
      S = 0;
   
   Tx = (1.0 - S) * (viewWidth >> 1);
   Ty = (1.0 - S) * (viewWidth >> 2);
   CTM[0][0] = S;
   CTM[3][0] = Tx;
   CTM[1][1] = S;
   CTM[3][1] = Ty;
}

void MultPerspective (CM,CTM,RM)
   float CM[],CTM[4][4],RM[];
{
   RM[0]=CTM[0][0]*CM[0]+CTM[1][0]*CM[1]+CTM[2][0]*CM[2]+CTM[3][0]*CM[3];
   RM[1]=CTM[0][1]*CM[0]+CTM[1][1]*CM[1]+CTM[2][1]*CM[2]+CTM[3][1]*CM[3];
   RM[2]=CTM[0][2]*CM[0]+CTM[1][2]*CM[1]+CTM[2][2]*CM[2]+CTM[3][2]*CM[3];
   RM[3]=CTM[0][3]*CM[0]+CTM[1][3]*CM[1]+CTM[2][3]*CM[2]+CTM[3][3]*CM[3];
}

int Clip (CM1,CM2,RM1,RM2)
   float CM1[],CM2[],RM1[],RM2[];
{
   
   if ((CM1[2]< 0) && (CM2[2]< 0))
      return (0);            /***  line is completely behind plane        ***/
   
   else if ((CM1[2]>= 0) && (CM2[2]>= 0))
      return (1);            /***  line is in front of plane,no clipping  ***/
   
   else if (CM1[2]< 0) {
      if (CM1[2] != CM2[2]) {
	 RM1[0] = CM2[0] - ((CM1[0]-CM2[0])/(CM1[2]-CM2[2]))*CM2[2];
	 RM1[1] = CM2[1] - ((CM1[1]-CM2[1])/(CM1[2]-CM2[2]))*CM2[2];
	 RM1[2]= 0;           /***  calculate intersection point (x,y,0)   ***/
         if (((RM1[0] < viewWidth) && (RM1[0] > 0)) &&
              ((RM1[1] < viewHeight) && (RM1[1] > 0)))
	    return (CRASHCODE);
         else 
            return (1);
      }
      else
	 return (1);
   }
   
   else {
      if (CM1[2] != CM2[2]) {
	 RM2[0] = CM2[0] - ((CM1[0]-CM2[0])/(CM1[2]-CM2[2]))*CM2[2];
	 RM2[1] = CM2[1] - ((CM1[1]-CM2[1])/(CM1[2]-CM2[2]))*CM2[2];
	 RM2[2]= 0;          /***  calculate intersection point (x,y,0)   ***/
         if (((RM2[0] < viewWidth) && (RM2[0] > 0)) &&
              ((RM2[1] < viewHeight) && (RM2[1] > 0)))
	    return (CRASHCODE);
         else 
            return (1);
      }
      else
	 return (1);
   }
}

int WorldToDisplay (lander, line, segment)
   LANDER *lander;
   LINE *line;
   XSegment *segment;
{
   float  VP1[4],VP2[4];             /* viewing coordinates                  */
   float  NP1[4],NP2[4];             /* normalized coordinates               */
   register float  tx1,tx2;          /* temporary storage for view transform */
   register float  ty1,ty2;
   register float  tz1,tz2;
   static float PTM[4][4] = {
      { 0.0, 0.0, 0.0, 0.0 },     /* matrix for perspective and norm. coord. */
      { 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 1.0, 0.0 },
      { 0.0, 0.0, 0.0, 1.0 },
   };
   int px = lander->px;
   int py = lander->py;
   int pz = lander->pz;
   int pitch = lander->pitch;
   int roll = lander->roll;
   int yaw = lander->yaw;

   int crash;

   /*
    * Wrap the stuff that the user can't see.  This gives the illusion
    * of an "infinite" world.  (Well, almost...)
    */
   if (line->z1 < lander->pz) {
      line->z1 += WORLD_LENGTH;
      line->z2 += WORLD_LENGTH;
   }
   else if (line->z1 > lander->pz + WORLD_LENGTH) {
      line->z1 -= WORLD_LENGTH;
      line->z2 -= WORLD_LENGTH;
   }
   if (line->x1 < lander->px - (WORLD_WIDTH >> 1)) {
      line->x1 += WORLD_WIDTH;
      line->x2 += WORLD_WIDTH;
   }
   else if (line->x1 > lander->px + (WORLD_WIDTH >> 1)) {
      line->x1 -= WORLD_WIDTH;
      line->x2 -= WORLD_WIDTH;
   }

   /*
    * Translate viewpoint to world origin
    */
   VP1[0] = line->x1 - px;
   VP1[1] = line->y1 - py;
   VP1[2] = line->z1 - pz;
   VP1[3] = 1;
   VP2[0] = line->x2 - px;
   VP2[1] = line->y2 - py;
   VP2[2] = line->z2 - pz;
   VP2[3] = 1;

   /*
    * orient the view axes with world axes
    */
   tx1 = VP1[0]*(COS[roll]*COS[yaw] + SIN[roll]*SIN[pitch]*SIN[yaw]) +
      VP1[1]*(-SIN[roll]*COS[yaw] + COS[roll]*SIN[pitch]*SIN[yaw]) +
	 VP1[2]*COS[pitch]*SIN[yaw];
      
   tx2 = VP2[0]*(COS[roll]*COS[yaw] + SIN[roll]*SIN[pitch]*SIN[yaw]) +
      VP2[1]*(-SIN[roll]*COS[yaw] + COS[roll]*SIN[pitch]*SIN[yaw]) +
	 VP2[2]*COS[pitch]*SIN[yaw];
      
   ty1 = VP1[0]*SIN[roll]*COS[pitch] +
      VP1[1]*COS[roll]*COS[pitch] -
	 VP1[2]*SIN[pitch];
   
   ty2 = VP2[0]*SIN[roll]*COS[pitch] +
      VP2[1]*COS[roll]*COS[pitch] -
	 VP2[2]*SIN[pitch];
   
   tz1 = VP1[0]*(-COS[roll]*SIN[yaw] + SIN[roll]*SIN[pitch]*COS[yaw]) +
      VP1[1]*(SIN[roll]*SIN[yaw] + COS[roll]*SIN[pitch]*COS[yaw]) +
	 VP1[2]*COS[pitch]*COS[yaw];
   
   tz2 = VP2[0]*(-COS[roll]*SIN[yaw] + SIN[roll]*SIN[pitch]*COS[yaw]) +
      VP2[1]*(SIN[roll]*SIN[yaw] + COS[roll]*SIN[pitch]*COS[yaw]) +
	 VP2[2]*COS[pitch]*COS[yaw];


   VP1[0] = tx1 + (viewWidth >> 1);
   VP2[0] = tx2 + (viewWidth >> 1);
   VP1[1] = ty1;
   VP2[1] = ty2;
   VP1[2] = tz1;
   VP2[2] = tz2;
   
   /*
    * Clip the line segment at z = 0
    */
   crash = Clip (VP1,VP2,VP1,VP2);
   if (crash == 0)
      return 0;
#if 0
   else if (crash == CRASHCODE)
     printf ("crashed\n");
#endif

   /*
    * Do perspective projection
    */
   LoadPerspectiveMatrix (PTM,VP1[2]);
   MultPerspective (VP1,PTM,NP1);
   LoadPerspectiveMatrix (PTM,VP2[2]);
   MultPerspective (VP2,PTM,NP2);

   NP1[1] = (float) viewHeight - NP1[1];
   NP2[1] = (float) viewHeight - NP2[1];

   /*
    * clip all lines completely out of window
    */
   if ((NP1[0] < 0.0 && NP2[0] < 0.0) ||
       (NP1[1] < 0.0 && NP2[1] < 0.0) ||
       (NP1[0] > (float) viewWidth && NP2[0] > (float) viewWidth) ||
       (NP1[1] > (float) viewHeight && NP2[1] > (float) viewHeight))
      return 0;

   /*
    * convert to device coordinates
    */
   segment->x1 = (short) NP1[0];
   segment->y1 = (short) NP1[1];
   segment->x2 = (short) NP2[0];
   segment->y2 = (short) NP2[1];
   return 1;
}

/******************************************************************************
** StoreLine
**
** Inserts the given line segment into the XSegment array for later plotting.
******************************************************************************/

void StoreLine (database, segment)
   DATABASE *database;
   XSegment *segment;
{
   int index = database->segcount;
   void bcopy ();

   bcopy (segment, &database->segments[index], sizeof (XSegment));
   database->segcount++;
}

/******************************************************************************
** SwapBuffers
**
** This copies the background buffer to the view window and then zeros it out
** so we can plot the next view in it.
******************************************************************************/

void SwapBuffers ()
{
   XCopyArea (d,buffer,viewWin,gcView,0,0,viewWidth,viewHeight,0,0);
   XSetForeground (d,gcView,BlackPixel(d, DefaultScreen (d)));
   XFillRectangle (d,buffer,gcView,0,0,viewWidth,viewHeight);
   XSetForeground (d,gcView,WhitePixel(d, DefaultScreen (d)));
}

/******************************************************************************
** DBPlot
**
** This plots the entire line database in the background buffer.  Then zeros
** out the segment index so we can store more lines for the next view.
******************************************************************************/

void DBPlot (database, lander)
   DATABASE *database;
   LANDER *lander;
{
   struct dbentry *entry;
   XSegment segment;
   LINE line;

   for (entry = database->lines; entry; entry = entry->next) {
      line.x1 = entry->line.x1 + database->off_x;
      line.x2 = entry->line.x2 + database->off_x;
      line.y1 = entry->line.y1 + database->off_y;
      line.y2 = entry->line.y2 + database->off_y;
      line.z1 = entry->line.z1 + database->off_z;
      line.z2 = entry->line.z2 + database->off_z;
      if (WorldToDisplay (lander, &line, &segment))
	 StoreLine (database, &segment);
   }
   if (!(database->segcount))
      return;
   XDrawSegments (d, buffer, gcView, database->segments, database->segcount);
   database->segcount = 0;
}

/******************************************************************************
** setupBuffer
**
** This routine creates a background pixmap so the program can simulate
** double buffering.
******************************************************************************/

void setupBuffer ()
{
   buffer = XCreatePixmap (d, viewWin, viewWidth, viewHeight,
			   DefaultDepth (d, DefaultScreen (d)));
   XSetForeground (d,gcView,BlackPixel(d, DefaultScreen (d)));
   XFillRectangle (d, buffer, gcView, 0, 0, viewWidth, viewHeight);
   XSetForeground (d,gcView,WhitePixel(d, DefaultScreen (d)));
}

/******************************************************************************
** DBInit
**
** This initializes a new world database and returns a pointer to it.
******************************************************************************/

DATABASE *DBInit ()
{
   DATABASE *db;

   if (!(db = (DATABASE *) malloc (sizeof (DATABASE)))) {
      (void) fprintf (stderr, "Error:  Insufficient memory\n");
      exit (1);
   }
   db->lines = (struct dbentry *) 0;
   db->linecount = db->segcount = 0;
   db->segments = (XSegment *) 0;
   db->off_x = db->off_y = db->off_z = 0;
   return db;
}

/******************************************************************************
** DBInitFromData
**
** Create a pre-loaded database from a list of line segments
******************************************************************************/

DATABASE *DBInitFromData (lines, nlines)
   LINE *lines;
   int nlines;
{
   int count;
   DATABASE *db;

   db = DBInit ();
   for (count = 0; count < nlines; count++)
      DBInsert (db, &lines[count]);
   DBFinish (db);
   return db;
}

/******************************************************************************
** DBFinish
**
** This routine should be called when all entries have been inserted into
** the database.  It allocates space for the 2-d segment array used internally
** by the plotting routines.
******************************************************************************/

void DBFinish (database)
   DATABASE *database;
{
   void *calloc ();

   if (!(database->segments =
	 (XSegment *) calloc (database->linecount, sizeof (XSegment)))) {
      (void) fprintf (stderr, "Error:  Insufficient memory\n");
      exit (1);
   }
}

/******************************************************************************
** DBFree
**
** This frees up the entire database
******************************************************************************/

void DBFree (database)
   DATABASE *database;
{
   struct dbentry *entry = database->lines;
   void free ();

   while (entry) {
      struct dbentry *temp = entry;
      entry = entry->next;
      free (temp);
   }

   /*
    * Free up the segment list, if it's been allocated.
    */
   if (database->segments)
      (void) free (database->segments);
}

/******************************************************************************
** DBAlloc
**
** This allocates a new database entry.  It exits if memory cannot be
** allocated.
******************************************************************************/

struct dbentry *DBAlloc ()
{
   struct dbentry *entry;

   if (!(entry = (struct dbentry *) malloc (sizeof (struct dbentry)))) {
      (void) fprintf (stderr, "Error:  Insufficient memory\n");
      exit (1);
   }
   return entry;
}

/******************************************************************************
** DBInsert
**
** DBInsert takes a pointer to a LINE, creates a new database entry, and
** adds it to the supplied database.
******************************************************************************/

void DBInsert (database, line)
   DATABASE *database;
   LINE *line;
{
   struct dbentry *entry = DBAlloc ();
   void bcopy ();

   database->linecount++;
   bcopy (line, &entry->line, sizeof (LINE));
   entry->next = database->lines;
   database->lines = entry;
}
