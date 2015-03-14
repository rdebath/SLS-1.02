/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* check for new mail  (icon version) */

#include <sys/stat.h>
#include <sys/time.h>		/* for fsleep */
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#ifdef v7
#include <sgtty.h>
#endif
#include <stdio.h>

#include "term.h"
#include "bitmap.h"

#include "mail_icons.h"

#define MSG_READING	"\freading mail ...\r"
#define MSG_CHECKING	"\rChecking for new mail..."

#define MAILF		"/usr/spool/mail"	/* spool file */
#define MAIL		"mail"			/* name of mail command */
#define POLL		60			/* polling interval */
#define XPOS		240			/* x start of mail window */
#define YPOS		190			/* y start of mail window */
#define W_WIDE		80			/* width of mail window */
#define W_HIGH		24			/* height of mail window */
#define MAX_ICON	64			/* max icon size */

#define PROCESSED	2			/* new mail already processed */

#define S(x)			statb.x
#define Isflag(arg,flag)	(!strncmp(arg,flag,strlen(flag)))
#define Max(x,y)		((x)>(y)?(x):(y))
#define dprintf			if(debug) fprintf

#define fsleep() \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = 330000; \
   select(0,0,0,0,&time); \
   }

#define MENU_COUNT		(sizeof(menu)/sizeof(struct menu_entry))

struct menu_entry menu[] = {
	{"print","t\n"},
	{"delete","dt\n"},
	{"next","n\n"},
	{"quit","q\n"},
	{"help","?\n"},
	{"headers","h *\n"},
	{"abort","x\n"},
};

static struct	stat statb;	/* spool file status */
static char	mail[255];	/* spool file path name */
static long	omtime=0l;	/* previous file mod. time */
static int	state = 0;	/* mail & window state */
static int	poll = POLL;	/* poll interval */
static int	debug=0;	/* for mgrmail -d >& /dev/tty?? */
static int	x,y;		/* window position */
static int	w,h;		/* window size */
static int	border;		/* size of mgr border */
static int	local=0;	/* use local icon only */
static int	local_mode = -1;/* local mode bits for tty */
static int	cwide, chigh;	/* width and height of font characters */
static char	*termcap;

void clean(), update(), download_icon(), usage(), set_icon();
int do_mail();

void
main(argc,argv)
char **argv;
int argc;
{
	register int i;
	int xpos = XPOS;		/* screen position of mail subwindow */
	int ypos = YPOS;
	int font = -1;			/* font to use for mail subwindow */
	int shape = 1;			/* initially reshape window */
	char *command = MAIL;		/* name of readmail command */

	char *user = getenv("USER");
	char line[MAXLINE];		/* event input buffer */

	/* make sure environment is ok */

	ckmgrterm( *argv );

	if (user==NULL || *user=='\0') {
		fprintf(stderr, "%s: No USER environment variable value.\n",
			argv[0]);
		exit(2);
	}

	/* process arguments */

	for(i=1;i<argc;i++) {
		if (Isflag(argv[i],"-s"))
			shape = 0;
		else if (Isflag(argv[i],"-d"))
			debug = 1;
		else if (Isflag(argv[i],"-l"))
			local = 1;
		else if (Isflag(argv[i],"-x"))
			xpos = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-y"))
			ypos = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-f"))
			font = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-p"))
			poll  = Max(atoi(argv[i]+2),10);
		else if (Isflag(argv[i],"-M"))
			command  = argv[i]+2;
		else
			usage(argv[0],argv[i]);
	}
	sprintf(mail,"%s/%s",MAILF,user);

	/* set up window environment */

	m_setup(M_FLUSH);
	m_push(P_MENU|P_BITMAP|P_FONT|P_EVENT|P_FLAGS|P_POSITION);
	if (font < 0)
		font = 0;
	m_font(font);
	get_font( &cwide, &chigh );

	signal(SIGHUP,clean);
	signal(SIGTERM,clean);
	signal(SIGINT,clean);
	signal(SIGALRM,update);

	dprintf(stderr,"pushing environment\n"); fflush(stderr);
	m_ttyset();
	m_setmode(M_NOWRAP);
	m_setmode(M_ABS);
	m_func(BIT_SRC);

	download_icon(&mbox_closed,1);
	download_icon(&mbox_full,2);
	download_icon(&mbox_zip,5);
	download_icon(&mbox_open,6);

	get_size(&x,&y,&w,&h);
        get_param(0,0,0,&border);
	m_movecursor(x+30,0);

	m_setmode(M_ACTIVATE);
	if (shape) {
		m_shapewindow(x,y,2*border+MAX_ICON, 2*border+MAX_ICON);
                get_size(&x,&y,&w,&h);
        }

	m_setevent(ACTIVATE,"A\n");
	m_setevent(REDRAW,"R\n");

	m_clearmode(M_ACTIVATE);
        set_icon(mbox_closed);

	dprintf(stderr,"Starting state 0x%x\n",state); fflush(stderr);

	update();

	/* wait for an event */

	while(1) {
		if( m_gets(line) == NULL ) {
			clearerr( m_termin );
			continue;
		}
		dprintf(stderr,"state 0x%x line : %c\n",state,*line);
		fflush(stderr);
		switch(*line) {
		case 'A':	/* window is activated */
			if (!stat(mail,&statb) && S(st_size))
				do_mail(command,font,&xpos,&ypos);
			else {
				set_icon(mbox_open);
				sleep(2);
				m_clearmode(M_ACTIVATE);
			}
			state &= ~PROCESSED;
			update();
			break;
		case 'R':	/* screen is redrawn */
			state &= ~PROCESSED;
			get_size(&x,&y,&w,&h);
			m_movecursor(x+30,0);
			update();
			break;
		}
	}
}

/* run readmail in a subwindow */

int 
do_mail(command,font,xp,yp)
char *command;
int font,*xp,*yp;
{
	int xpos = *xp, ypos = *yp;
	int code;
	int n;
	int x,y;

	alarm(0);
	m_push(P_EVENT | P_FONT);
	dprintf(stderr,"doing mail\n"); fflush(stderr);
	n = m_makewindow(xpos, ypos, W_WIDE*cwide + 2*border,
		W_HIGH*chigh + 2*border);
	if (n==0) {	/* can't make window */
		m_printstr("\007\fCan't open mail window, sorry");
		m_pop();
		return(0);
	}
  	set_icon(mbox_zip);
	m_selectwin(n);
	m_font(font);
	menu_load(1,MENU_COUNT,menu);
	m_selectmenu(1);
	m_printstr(MSG_READING);
	m_ttyreset();
	code = system(command);
	m_printstr(MSG_CHECKING);
	sleep(1);	/* for "New mail arrived" message */
	dprintf(stderr,"Readmail completed code %d\n",code); fflush(stderr);
	m_ttyset();

	/* see if window was moved - remember for next time */
	
	get_size(&x,&y,0,0);
	if (abs(x-xpos) > 10 || abs(y-ypos) > 10) {
		*xp = x;
		*yp = y;
	}
	m_selectwin(0);
	m_destroywin(n);
	m_pop();
	m_clearmode(M_ACTIVATE);
	dprintf(stderr,"window deactivated\n"); fflush(stderr);
	return 1;
}

/* check the spool file for new mail and update message */

void
update()
{
	alarm(0);
	dprintf(stderr,"checking mail state 0x%x\n",state); fflush(stderr);
	if (!stat(mail,&statb) && S(st_mtime)>S(st_atime) && S(st_size)) {
		state &= ~PROCESSED;
		if (S(st_mtime) != omtime) {
		dprintf(stderr,"	First time New mail\n"); fflush(stderr);
                        m_printstr("");
  			set_icon(mbox_full);
			omtime = S(st_mtime);
		}
	}
	else if (!(state&PROCESSED)) {
		dprintf(stderr,"	Clearing new mail\n"); fflush(stderr);
  		set_icon(mbox_closed);
		state |= PROCESSED;
	}
	alarm(poll);
}

/*	Clean up and exit */

void
clean(n)
int	n;
{
	m_ttyreset();
	m_selectwin(0);
	m_popall();
	exit(n);
}

void
usage(name,error)
char *name, *error;
{
	fprintf(stderr,"Invalid flag: %s\n",error);
	fprintf(stderr,
		"usage: %s -[s|x<pos>|y<pos>|f<font>|p<poll>|M<mail_program>]\n"
		,name);
	exit(1);
}

/* down load an icon */

void
download_icon(icon,where)
register struct icon *icon;	/* name of icon to download */
int where;			/* bitmap to download icon to */
   {

   int size;
   int w_in=0, h_in=0;

   if (!local) {
	   /* first try the local machine */
      dprintf(stderr,"looking for %s\n",icon->name);
      m_bitfile(where, icon->name, &w_in, &h_in );
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

void
set_icon(name)
struct icon name;		/* name of icon */
{
   int x0 = (w-name.w)/2;
   int y0 = (h-name.h)/2;

   m_clear();
   m_bitcopyto(x0,y0,name.w,name.h,0,0,0,name.type);
   dprintf(stderr,"copy %s to %d,%d (%d x %d)from %d\n",
           name.name,x0,y0,name.w,name.h,name.type);
   m_flush();
}
