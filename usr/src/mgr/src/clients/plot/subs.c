/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* mgr plot filter (tmgr) */

#include "term.h"
#include "bitmap.h"

#define Yf(y) (ymax - (y))    /* flip vertically (khera@cs.duke.edu) */
#define Y(x)  (scale?(int)((Yf(x)-ymin) * yscale):Yf(x))
#define X(x)	(scale?(int)(((x)-xmin) * xscale):(x))
#define dprintf	if(debug)fprintf
#define GMAX 	999

static int scale = 0;			/* TRUE if scaling on */
static int xmin = 0 ,ymin = 0;		/* minimum plotting coord */
static int xmax = GMAX ,ymax = GMAX;	/* maximum plotting coord */
static int xgmax = GMAX;		/* to override GMAX from environ() */
static int ygmax = GMAX;
static float xscale,yscale;		/* scale values */
static int debug = 0;			/* iff TRUE debug -> stderr */
static int text;			/* true if last item was a label */
static int win_id = 0;			/* true for alternate window */
static int pause = 0;			/* pause before clear */
static int points=0;			/* # points plotted (>0) */

openpl()
   {
   char *getenv();
   int n;
   char *v;

   if ((v=getenv("MAX_X")) && (n=atoi(v))>0)
      xgmax = n;
   if ((v=getenv("MAX_Y")) && (n=atoi(v))>0)
      ygmax = n;
   if ((v=getenv("WINDOW_ID")) && (n=atoi(v))>0)
      win_id = n;
   pause = (int) getenv("PAUSE");
   points = 0;

   xscale = (float)xgmax/(xmax-xmin);
   yscale = (float)ygmax/(ymax-ymin);
   text = 0;
   debug = (int)getenv("DEBUG");
   dprintf(stderr,"OPEN\n");
   m_setup(M_DEBUG);
   m_push(P_FLAGS);
   m_func(BIT_SET);
   if (win_id) {
      m_selectwin(win_id);
      m_setmode(M_ACTIVATE);
      }
   }

erase()
   {
   if (points && pause) {
      m_flush();
      getpass("\0330,0M");
      }
   m_clear();
   m_flush();
   dprintf(stderr,"ERASE\n");
   points = 0;
   }

label(s)
char *s;
   {
   if (text == 0)
      m_aligntext();
   m_setmode(M_OVERSTRIKE);
   m_func(BIT_OR);
   m_printstr(s);
   m_func(BIT_SET);
   m_clearmode(M_OVERSTRIKE);
   dprintf(stderr,"LABEL [%s]\n",s);
   text++;
   points++;
   }

line(x1, y1, x2, y2)
int x1,y1,x2,y2;
   {
   text=0;
   m_line(X(x1),Y(y1),X(x2),Y(y2));
   m_go(X(x2),Y(y2));		/* this should be redundant */
   dprintf(stderr,"LINE: %d,%d  %d,%d\n",X(x1),Y(y1),X(x2),Y(y2));
   points++;
   }

circle(x, y, r)
int x,y,r;
   {
   m_circle(X(x),Y(y),X(r));
   dprintf(stderr,"CIRCLE %d,%d  %d\n",X(x),Y(y),X(r));
   points++;
   }

arc(x, y, x0, y0, x1, y1)
int x,y,x0,y0,x1,y1;
   {
   m_arc(X(x),Y(y),X(x0),Y(y0),X(x1),Y(y1));
   dprintf(stderr,"ARC at %d,%d from  %d,%d to %d,%d\n",
                X(x),Y(y),X(x0),Y(y0),X(x1),Y(y1));
   points++;
   }

move(x, y)
int x,y;
   {
   text=0;
   m_go(X(x),Y(y));
   dprintf(stderr,"MOVE %d,%d\n",X(x),Y(y));
   }

cont(x, y)
int x,y;
   {
   text=0;
   m_draw(X(x),Y(y));
   dprintf(stderr,"DRAW %d,%d\n",X(x),Y(y));
   points++;
   }

point(x, y)
int x,y;
   {
   m_line(X(x),Y(y),X(x),Y(y));
   dprintf(stderr,"POINT %d,%d\n",X(x),Y(y));
   points++;
   }

linemod(s)
char *s;
   {
   dprintf(stderr,"LINEMODE [%s]\n",s);
   }

space(x0, y0, x1, y1)
int x0,y0,x1,y1;
   {
   xmin = x0;
   ymin = y0;
   xmax = x1;
   ymax = y1;
   xscale = (float)xgmax/(xmax-xmin);
   yscale = (float)ygmax/(ymax-ymin);
   scale = 1;
   dprintf(stderr,"SPACE %d,%d to %d,%d: scale (%g,%g)\n",
            xmin,ymin,xmax,ymax,xscale,yscale);
   }

closepl()
   {
   if (points && pause) {
      m_flush();
      getpass("\0330,0M");
      }
   if (win_id) {
      m_selectwin(0);
      m_setmode(M_ACTIVATE);
      }
   m_pop();
   m_flush();
   points++;
   dprintf(stderr,"CLOSE\n");
   }

dot()
  {
  }
