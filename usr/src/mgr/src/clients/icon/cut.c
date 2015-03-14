/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* save current cut buffer in a file */
/*}}}  */
/*{{{  #includes*/
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>

#include "term.h"
#include "bitmap.h"
/*}}}  */
/*{{{  #defines*/
#define TERM		"mgr"			/* name of valid terminal id */

#define RESTORE	0				/* restore previous icon */
#define CLOSED		1
#define OPEN		2
#define WINK		3

#define ICON_CLOSED	"file_shut"
#define ICON_OPEN		"file_open"
#define ICON_WINK		"file_shutb"
#define CMDNAME		"hm -s"	/* default command */
#define SLOP			3			/* space around the window */

#define MENU_COUNT		(sizeof(menu)/sizeof(struct menu_entry))
#define max(x,y)	((x)>(y)?(x):(y))
#define min(x,y)	((x)<(y)?(x):(y))
#define dprintf	if(debug)fprintf
/*}}}  */

void clean();
void setup();
int append_file(), down_load();

int border;
FILE *debug;

int main(argc,argv)
int argc;
char **argv;
{
register int i;
char *term = getenv ("TERM");
char *name = argv[0];
char *cmd;				/* command to pipe data to */
char line[80];			/* event input buffer */

int x,y,wide,high;	/* main window size */
int w,h;					/* icon size */
int max_w, max_h;		/* max icon size */
int ix, iy;				/* icon position */
int fw,fh,font;				/* font for vi window */
int got=0;
int shape = 1;			/* window reshape? */
FILE *test;				/* temporary file */


	if (getenv("DEBUG"))
		debug = stderr;
	else
		debug = NULL;

	/* make sure environment is ok */

	if (term!=NULL && strcmp(term,TERM)!=0) {
		fprintf(stderr,"%s only runs on %s terminals\n",name,TERM);
		exit(1);
	   }

	/* get args */

   	if (argc>1 && strcmp(argv[1],"-s")==0) {	/* don't redhape window */
      		argc--, argv++;
      		shape=0;
      	}

   	if (argc>1)
      		cmd = argv[1];
   	else if ((cmd=getenv("CUT")) == NULL)
      		cmd = CMDNAME;

   	/* make sure we can run the command */

   	if ((test = popen(cmd,"w")) == NULL || pclose(test)!=0) {
      		fprintf(stderr,"%s: Can't start (%s)\n",name,cmd);
      		exit(1);
      	}

	/* set up window environment */

	m_setup(0);
	m_ttyset();

	signal(SIGHUP,clean);
	signal(SIGTERM,clean);
	signal(SIGINT,clean);

	m_push(P_FONT|P_POSITION|P_EVENT|P_FLAGS);
	get_size(&x,&y,&wide,&high);
	get_param(0,0,0,&border);
	m_setmode(M_NOWRAP);
	m_func(BIT_SRC);

	/* get icons */

	if (!down_load(ICON_CLOSED,CLOSED,&w,&h)) {
      		fprintf(stderr,"%s: Can't find %s\n",name,ICON_CLOSED);
		clean(1);
	}
	max_h = h; max_w = w;

	if (!down_load(ICON_OPEN,OPEN,&w,&h)) {
      		fprintf(stderr,"%s: Can't find %s\n",name,ICON_OPEN);
		clean(1);
	}
	max_h = max(max_h,h);
	max_w = max(max_w,w);

	if (!down_load(ICON_WINK,WINK,&w,&h)) {
      		fprintf(stderr,"%s: Can't find %s\n",name,ICON_WINK);
		clean(1);
	}

	max_h = max(max_h,h);
	max_w = max(max_w,w);

	/* get icon position */

	m_font(1);	/* a small font */
   	if (shape) {
		m_push(P_FLAGS|P_EVENT);
		m_setmode(M_ABS);
		m_setevent(BUTTON_1,"Z %p\n");
		fprintf(stderr,"Click button 1 to indicate icon position\n");
		while (*m_gets(line) != 'Z')
			;
		sscanf(line,"Z %d %d",&w,&h);
		ix = w + x + border; 
		iy = h + y + border;
		m_pop();

		/* iconify window */

		m_shapewindow(ix,iy,SLOP+2*border+max_w, SLOP+2*border+max_h);
		dprintf(debug,"Shaping window to %d x %d\n",border+max_w, border+max_h);
      	}

	/* setup events */

	m_setevent(ACTIVATE,"A\n");
	m_setevent(DEACTIVATE,"D\n");
	m_setevent(REDRAW,"R\n");
	m_setevent(RESHAPE,"S\n");
	m_setevent(SNARFED,"C %c\n");
	m_clearmode(M_ACTIVATE);

	/* display icon */

	setup(CLOSED);

	/* wait for an event */

	while(!feof(m_termin)) {
		m_flush();
		m_gets(line);
		switch(*line) {
		case 'A':	/* window is activated */
            		if (got) {
               			setup(OPEN);
				m_flush();
				m_put();
				m_sendme("\n\005 done\n");
				m_flush();
				append_file(cmd,got);
              			got = 0;
               		}
            		sleep(1);
			m_clearmode(M_ACTIVATE);
			break;
		case 'D':	/* window is deactivated */
				setup(CLOSED);
				break;
		case 'S':	/* window is reshaped */
       			if (shape) {
				get_size(&ix,&iy,0,0);
				m_shapewindow(ix,iy,SLOP+2*border+max_w, SLOP+2*border+max_h);
               		}
			setup(RESTORE);
			break;
		case 'R':	/* screen is redrawn */
          		setup(RESTORE);
			break;
		case 'C':	/* Someone cut something */
			got = atoi(line+2);		/* # of bytes */
			setup(WINK);
			m_flush();
			sleep(1);
			setup(CLOSED);
		}
	}
        exit(0);
}

/*	Clean up and exit */

void
clean()
{
	m_popall();
	m_ttyreset();
	exit(1);
}

/* setup an icon */

void
setup(where)
int where;		/* bitmap # */
{
   static int was=0;

   if (where==0)		/* restore previous icon */
      	where = was;

   m_clear();
   m_bitcopyto(0,0,999,999,0,0,0,where);
   dprintf(debug,"displaying icon [%d] \n",where);
   m_flush();
   was = where;
}

/* download icon */

int
down_load(name,where,w,h)
char *name;	/* name of icon */
int where;	/* scratchpad bitmap # */
int *w, *h;	/* icon size */
{
char buff[20];
int n;

   	m_bitfromfile(where,name);
   	m_flush();
   	m_gets(buff);
   	n = sscanf(buff,"%d %d",w,h);
	dprintf(debug,"Getting icon [%s] into %d (%d x %d)\n",
		name,where,*w,*h);
	if (n < 2) 
		return(0);
	else
		return(1);
	}

int
append_file(cmd,bytes)
char *cmd;		/* command to pipe file to */
int bytes;
{
FILE *file;
long now = time(0);
register int c,count = 0;
int gotn = 0;

   dprintf(stderr,"appending %d bytes\n",bytes);
   file = popen(cmd,"w");
   fprintf(file,"\n> %d bytes at: %s  ",bytes,ctime(&now));

	while((c=getc(m_termin)) != '\005') {
		if (c=='\n' && gotn++ ==0) {
		   	count++;
			putc(c,file);
			putc(' ',file);
			putc(' ',file);
		}
		else if (c!= '\n') {
			putc(c,file);
			gotn = 0;
		}
	}
	fflush(file);
   	pclose(file);
	return(count);
}
