/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* check for new messages */
/* icon version */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include "term.h"
#include "bitmap.h"

#include "msgs_icons.h"

#define Isflag(arg,flag)	(!strncmp(arg,flag,strlen(flag)))
#define Max(x,y)		((x)>(y)?(x):(y))
#define MENU_COUNT	(sizeof(menu)/sizeof(struct menu_entry))
#define SCMP(x,y)	(strcmp(x+(strlen(x)-strlen(y)),y)==0)

static char	MSG_reading[] = "\freading msgs ...\r";
static char	MSG_done[] = "\rdone? [?,-,!]";
static char	MSG_help[] = "\r\
Available commands:\r\n\
 ?*          this message\r\n\
 -           return to most recently seen message\r\n\
 -<number>   skip back <number> messages\r\n\
 !<cmd>      escape to the shell and issue <cmd>\r\n\
 <anything>  exit\r\n";

#define W_WIDE		80		/* character width of msgs window */
#define W_HIGH		24		/* character height of msgs window */

#define MSGSCMD	"msgs -p"
#define BOUNDS	"/usr/msgs/bounds"
#define RC	".msgsrc"
#define POLL	 30				/* polling interval */
#define XPOS	220				/* start of msgs window */
#define YPOS	170				/* start of msgs window */
#define MAX	50				/* max number of messages */

#define MSGS()	(1 + get_bounds(bounds) - get_rc(rc))
#define dprintf	if(debug) fprintf

static FILE	*bounds, *rc;		/* pntrs to bounds and rc files */
static int	msg_cnt, old_msg_cnt=0;	/* # of messages */
static char	line[MAXLINE];		/* input buffer */
static int	poll=POLL;		/* poll interval */

static struct menu_entry menu[] = {
   {"yes",    "y\n"},
   {"skip",   "n\n"},
   {"again",  "-\n"},
   {"save",   "s\n"},
   {"quit",   "q\n"},
   };

static int	x, y, w, h;	/* window location, width and height */
static int	border;		/* size of border */
static int	debug = 0;	/* debug printout flag */
static int	local=0;	/* use local machine for icons */
static int	cwide, chigh;	/* width and height of font characters. */
static int my_win_id = 0;	/* my window id */

void clean(), update(), download_icon();
void usage(), draw(), set_icon();
int do_msgs(), get_rc(), get_bounds();

int
main(argc,argv)
int argc;
char **argv;
{
   register int i;
   int xpos = XPOS;
   int ypos = YPOS;
   int font = -1;
   int shape = 1;

   char *home = getenv("HOME");

   /* make sure we have a valid environment to run in */

   ckmgrterm( *argv );

   if (home==NULL || *home=='\0') {
      fprintf(stderr,"%s: Can't find your home directory\n",argv[0]);
      exit(1);
      }

   if ((bounds = fopen(BOUNDS,"r")) == NULL) {
      fprintf(stderr,"%s: Can't find a bounds file\n",argv[0]);
      exit(2);
      }

   sprintf(line,"%s/%s",home,RC);

   if ((rc = fopen(line,"r")) == NULL) {
      fprintf(stderr,"%s: Can't find %s\n",argv[0],line);
      exit(3);
      }

   /* process arguments */

   for(i=1;i<argc;i++) {
      if (Isflag(argv[i],"-s"))
         shape = 0;
      else if (Isflag(argv[i],"-x"))
         xpos = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-y"))
         ypos = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-f"))
         font = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-p"))
         poll  = Max(atoi(argv[i]+2),10);
      else if (Isflag(argv[i],"-d"))
         debug=1;
      else if (Isflag(argv[i],"-l"))
         local=1;
      else
         usage(argv[0],argv[i]);
      }

   /* setup mgr stuff */

   m_setup(M_FLUSH);
   m_push(P_BITMAP|P_MENU|P_EVENT|P_FONT|P_FLAGS|P_POSITION);
   if (font < 0)
	font = 0;
   m_font(font);
   get_font( &cwide, &chigh );
	
   signal(SIGHUP,clean);
   signal(SIGTERM,clean);
   signal(SIGINT,clean);
   signal(SIGALRM,update);

   m_ttyset();
   m_setmode(M_NOWRAP);
   m_setmode(M_ABS);
   m_func(BIT_SRC);

	/* get window id */

	m_getinfo(G_ID);
   m_gets(line);
	my_win_id = atoi(line);

   download_icon(&msg_board,1);
   download_icon(&msg_note,2);
   download_icon(&msg_read,3);
   download_icon(&msg_none,4);

   get_size(&x,&y,&w,&h);
   get_param(0,0,0,&border);
   m_movecursor(x+30,0);

   old_msg_cnt = MSGS();
   if (shape && !debug) {
      m_shapewindow(x,y,2*border+msg_board.w,2*border+msg_board.h);
      }
   else if (debug)
      fprintf(stderr,"would shape (%d,%d) + %d\r\n",
               msg_board.w, msg_board.h, 2*border);
   m_setevent(REDRAW,"R\n");
   m_setevent(ACTIVATED,"A\n");
   update(0);
   m_clearmode(M_ACTIVATE);

   while(1) {
      if( m_gets(line) == NULL ) {
	 clearerr(m_termin);
	 continue;
      }
      alarm(0);

      /* read msgs */

      old_msg_cnt = msg_cnt;
      msg_cnt = MSGS();
      if (msg_cnt > 0 && *line == 'A') {
         do_msgs(MSGSCMD,font,xpos,ypos);
      }

      /* wait for window to deactivate */

      else if (*line == 'A') {
         set_icon(msg_none,0,0);
         sleep(2);
      }
      m_clearmode(M_ACTIVATE);
      update(0);
   }
   exit(0);
}
    
int
get_rc(file)
FILE *file;
   {
   char line[100], *fgets();
   fseek(file,0,0);
   if (fgets(line,sizeof(line),file) != NULL) 
      return(atoi(line));
   else
      return(0);
   }

int
get_bounds(file)
FILE *file;
   {
   char buff[100], *line, *fgets();
   fseek(file,0,0);
   if ((line=fgets(buff,sizeof(buff),file)) != NULL) {
      while(*line != ' ') line++;
      while(*line == ' ') line++;
      return(atoi(line));
      }
   else return(0);
   }

void
clean(n)
int n;
   {
   m_ttyreset();
   m_selectwin(my_win_id);
   m_popall();
   exit(n);
   }

void
update(flag)
int flag;
   {
   alarm(0);
   msg_cnt = MSGS();
   if (msg_cnt != old_msg_cnt || flag==0) {
      if (msg_cnt > old_msg_cnt) 		/* new messages */
         m_printstr("\007");
      draw(msg_cnt,old_msg_cnt,flag==0);
      }
   old_msg_cnt = msg_cnt;
   alarm(poll);
}

void
usage(name,error)
char *name, *error;
{
	fprintf(stderr,"Invalid argument: %s\n",error);
	fprintf(stderr,"usage: %s -[s|x<pos>|y<pos>|f<font>|p<poll>\n",name);
	exit(1);
}

/* down load an icon */

void
download_icon(icon,where)
register struct icon *icon;	/* name of icon to download */
int where;			/* bitmap to download icon to */
   {
   static int local_mode = -1;
   int size;
   int w_in=0, h_in=0;

   /* first try the local machine */

   if (!local) {
      dprintf(stderr, "looking for %s\n", icon->name);
      m_bitfile(where, icon->name , &w_in, &h_in);
      }

   if (h_in==0 || w_in==0) {	/* can't find icon */
      dprintf(stderr,"Couldn't find %s, downloading\n",icon->name);
#ifdef v7
      if (local_mode == -1)
         ioctl(fileno(m_termout),TIOCLGET,&local_mode);
      local_mode |= LLITOUT;
      ioctl(fileno(m_termout),TIOCLSET,&local_mode);
#endif
      size = icon->h * (((icon->w+15)&~15)>>3);
      m_bitldto(icon->w,icon->h,0,0,where,size);
      m_flush();
      write(fileno(m_termout),icon->data,size);
#ifdef v7
      local_mode &= ~LLITOUT;
      ioctl(fileno(m_termout),TIOCLSET,&local_mode);
#endif
      }
   else {
      dprintf(stderr,"Found %s (%d x %d) expected %d x %d\n",
               icon->name,w_in,h_in,icon->w,icon->h);
      icon->w = w_in;
      icon->h = h_in;
      }
   icon->type = where;
} 

#define Rand(min,max)	((min) + random()%((max)-(min)))
#define MAXNOTES	100		/* enough to make the message board
					look filled, but not take too long
					to draw when there are hundreds of
					messages. */
void
draw(new,old,redraw)
int new,old;				/* number of messages */
int redraw;
{
   register int i;
   long random();

   dprintf(stderr,"draw %d -> %d (redraw=%d)\n",old,new,redraw);

   if (redraw || new<old)
      set_icon(msg_board,0,0);

   if (new>old)
      new -= old;

   for(i=0;  i<new && i<MAXNOTES;  i++) {
      set_icon( msg_note, Rand(0, msg_board.w-msg_note.w),
		Rand(12, msg_board.w-msg_note.h) );
      }
}
   
void
set_icon(name,x0,y0)
struct icon name;		/* name of icon */
int x0,y0;			/* where it goes */
{
   m_bitcopyto(x0,y0,name.w,name.h,0,0,0,name.type);
   m_flush();
}
   
/* run msgs in a subwindow */

int
do_msgs(command,font,xpos,ypos)
char *command;
int font,xpos,ypos;
{
	int n;

	alarm(0);
	m_push(P_EVENT | P_FONT);
	n = m_makewindow(xpos, ypos, W_WIDE*cwide + 2*border,
		W_HIGH*chigh + 2*border );
	if (n==0) {	/* can't make window */
		m_printstr("\007\fCan't open msgs window, sorry");
		m_pop();
		return(0);
	}
  	set_icon(msg_read, 0, 0);
	m_selectwin(n);
	m_font(font);
	menu_load(1,MENU_COUNT,menu);
	m_selectmenu(1);
	m_printstr(MSG_reading);
	m_ttyreset();
	system(command);
	m_printstr(MSG_done);
        while( m_gets(line) != NULL ) {
		switch( *line ) {
		case '?':
			m_printstr(MSG_help);
			break;
		case '!':	/* shell escape */
			system( &line[1] );
			break;
		case '-': {	/* go back; -N goes back N messages */
				char	buf[ 2*MAXLINE ];
				if( line[1] == '\n' )
					strcpy( line, "-1" );
				sprintf( buf, "%s %s", command, line );
				system( buf );
			}
			break;
		default:
			goto nomore;	/* bianchi, how could you do such a horrible thing
					to my code????? */
		}
		m_printstr(MSG_done);
	}
	clearerr(m_termin);
    nomore:
	m_ttyset();
	m_selectwin(my_win_id);
	m_destroywin(n);
	m_pop();
	m_clearmode(M_ACTIVATE);
	return 1;
}
