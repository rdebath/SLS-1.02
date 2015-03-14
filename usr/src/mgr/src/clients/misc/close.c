/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: close.c,v 4.6 88/08/08 09:43:38 sau Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/close.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/close.c,v $$Revision: 4.6 $";

/* close a window  - keep icon uncovered */

#include <signal.h>
#include "term.h"


static int debug = 0;			/* for debugging */

static int wide, high, xmax, ymax, border;	/* global mgr state info */

#define Max(x,y)	((x)>(y)?(x):(y))
#define Min(x,y)	((x)<(y)?(x):(y))
#define dprintf		if (debug) fprintf

#define XSLOP		6	/* extra horizontal space around window */
#define YSLOP		2	/* extra vertical space around window */
#define WAIT		20	/* seconds until next look for an opening */
#define RAND		11	/* max time to wait before moving */

#define Active		"A\n"
#define Covered		"C\n"
#define Redraw		"R\n"


int main(argc,argv)
int argc;
char **argv;
   {
   char host[16];			/* host name */
   char line[MAXLINE];			/* input buffer for events */
   char text[MAXLINE];			/* text for icon */
   char moving[MAXLINE];		/* text for icon while moving */
   int font;
   int clean();
   int sigalrm();
   char *getenv();

   ckmgrterm( *argv );

   /* turn on debugging */

   if (getenv("DEBUG"))
      debug++;

   /* get icon text */

   srand(getpid());
   if (debug)
      setbuf(stderr,NULL);
   m_setup(M_FLUSH);
   m_push(P_ALL & (~P_MOUSE));

   signal(SIGINT,clean);
   signal(SIGTERM,clean);
   signal(SIGALRM,sigalrm);

   m_ttyset();

   if (argc>3)
      usage(*argv);

   if (argc>1  &&  *argv[1] ) {
      /* There is a message and it is not zero length */
      text[0] = '\f';
      /* If the string in argv[1] contains a %d, then the window set ID will
         be included in the message.
      */
      sprintf(&text[1], argv[1], m_setid());
      sprintf(moving, "\fMoving %s", &text[1]);
   }
   else {
      /* No message or it is zero length. */
      gethostname(host,sizeof(host));
      sprintf(text, "\f%s(%d)", host, m_setid());
      sprintf(moving, "\fMoving(%d)", m_setid());
      }

   if (!debug)
      m_setmode(M_NOINPUT);
   if (argc==3  &&  (font=atoi(argv[2])) > 0)
      m_font(font);

   setupwindow(text);

   /* set events */

   m_setevent(ACTIVATED,Active);
   m_setevent(COVERED,Covered); 
   m_setevent(REDRAW,Redraw); 

   /* bury it and wait */

   if (!goto_spot(wide,high)) {
      /* Window was too big; use font zero and try again */
      m_font( 0 );
      setupwindow( text );
      }
   goto_spot(wide,high);
   	/* no place to go; we'll stay where we are until something opens up */

   while(1) {
      m_clearmode(M_ACTIVATE);
      if (!debug)
         m_setmode(M_NOINPUT);
      m_gets(line);
      alarm(0);
      m_push(P_EVENT);
      m_setevent(COVERED,Covered); 
      dprintf(stderr,"Read [%s]\n",line);

      if (*line == *Active)			/* activate window */
         clean();

      else if (*line == *Covered) {
         m_printstr(moving);
         get_size(0,0,&wide,&high);
         wide += 2*border+XSLOP;
         high += 2*border+YSLOP;
         dprintf(stderr,"going to ?,? %d %d\n",wide,high);
         sleep(((unsigned)rand()) % RAND);
         if (goto_spot(wide,high)) {
	    m_clearevent(COVERED);
            m_clearmode(M_ACTIVATE);
            }
         else {				/* no place to go */
            alarm(WAIT);
            }
         }
      m_printstr(text);
      m_pop();
      }
   }

/* restore window state and exit */

static
clean()
   {
   m_ttyreset();
   m_popall();
   exit(1);
   }

/* find an unused spot for a window */

static
int
goto_spot(wide, high)
int wide,high;					/* minimum spot size */
   {
   struct window_data	coords[1000];	/* present window coords. go here */
   register int		c,
			count,
			intersection,
			setid = m_setid(),
			x,
			y, nexty;

   while( (count = get_all(coords)) == 0 )
	;
   dprintf(stderr,"found %d windows\n", count);

   /*	Find the best spot.  We want to avoid too exhaustive a search.
	We march through the screen, trying to fit the moving window into
	spaces.  Any time we collide with a window, we skip to the right edge
	of that window and note if it's top edge is the lowest one we've seen
	which is still above where we are.  This allows us to skip over the
	larger areas of occupied screen quickly.
   */
   for( y = ymax-high;  y >= 0;  y = nexty - 1 ) {
      nexty = y;
      for( x = xmax-wide;  x >= 0;  x -= 1 ) {
	 intersection = 0;
	 for( c = 0;  c < count;  c++ ) {
	    if( coords[c].setid == setid )
	       continue;
            if( in_win( coords+c, x, y, x + wide, y + high ) ) {
	       intersection = 1;
	       nexty = Max( y, Max( nexty, coords[c].y - high ) );
	       x = coords[c].x - wide;
	       break;
	       }
	    }
	 if( !intersection ) {
            dprintf(stderr,"going to %d, %d\n", x, y);
	    m_push(P_EVENT);
	    m_movewindow( x + XSLOP/2, y + YSLOP/2 );
	    m_pop();
	    return( 1 );
	    }
	 }
      }
   dprintf(stderr,"no openings\n");
   return( 0 );
   }


/* check for window-rectangle intersection */

static
int
in_win(list,x0,y0,x1,y1)
register struct window_data *list;		/* window coordinates */
register int x0,y0,x1,y1;			/* rectangle coordinates */
   {
   return(
   (
      list->x + list->w < x0  ||  x1 < list->x  ||
      list->y + list->h < y0  ||  y1 < list->y
   ) ?  0  :  1);
   }


/* send an alarm signal */

static
sigalrm()
   {
   m_sendme(Covered);
   }


static
int
m_setid()
{
	static int		setid = -1;
	struct window_data	window;

	if( setid == -1 ) {
		while( get_eachclientwin( &window ) )
			;
		setid = window.setid;
	}
	return setid;
}


static
usage( pgm )
char	*pgm;
{
	fprintf( stderr, "Usage:  %s [ message [ fontnumber ] ]\n", pgm );
	fputs( "\
If the message is zero-length, the default message is printed.\n\
If fontnumber is non-numberic or not available, zero is assumed.\n\
", stderr );
	exit( 255 );
}


static
setupwindow( text )
char	*text;
{
	/* change window size */

	m_size(Max(strlen(text)-1, 5), 1);
	m_setmode(M_NOWRAP);
	m_printstr(text);

	/* how big is it */

	get_size(0, 0, &wide, &high);
	get_param(0, &xmax, &ymax, &border);
	wide += 2*border+XSLOP;
	high += 2*border+YSLOP;
}
