/* ify.c - iconify an mgr window
   Jim Blandy - July, 1989 - Bell Communications Research

This program is designed to replace the 'close' program that comes
with MGR - ify doesn't require you to stop the program running in a
window and start up another one, like close does.

When you first click on a window, ify finds out which pseudo-tty that
window is attached to, finds all the other windows attached to that
psuedo-tty, and closes them by sending the appropriate escape codes to
the pseudo-tty, changing the window's NOTIFY string to
"CLOSED:<tty>:<old notify name>", using the window's original NOTIFY
as a title for the closed window, and then doing a TIOCSTOP ioctl on
it, basically equivalent to a ^S.  After sending the ^S, ify sends the
escape codes to restore the window's position and content; since the
tty has been ^S'ed, these don't take effect yet.

When you click on a closed window, ify notices that the window is
already closed by looking at its NOTIFY string, opens the window's
ptty, and does a TIOCSTART ioctl on it (equivalent to a ^Q).  This
lets MGR see the escape codes ify queued up when the window was
closed, so MGR opens the window.

The list of closed windows displayed on the menu is obtained by asking
MGR for everyone's NOTIFY strings, and selecting those windows whose
NOTIFY strings start with "CLOSED:".  This menu is refreshed when a
window is closed or opened.

Note that MGR divides its attention among several ttys; if you send
data to someone else's tty, as ify does, MGR takes its own sweet time
to notice what you've sent; you have to wait for it to take effect.
After sending the codes to close up a group of windows, ify sends a
m_sendto() message to itself; when it gets the message, it knows that
the window is all done.  It pulls the same trick when opening windows,
in order to keep the menu up to date.  The only kink in this setup is
that, in order to receive m_sendto() messages, ify needs to be able to
turn off general write permission to its tty, which means it needs to
own its tty.  This can only happen if you're running MGR with the
setuid bit on.  If ify can't change its tty, it gives up on the
message-passing strategy and just sleeps a bit (see the WAITTIME
constant) after each operation.
					-JimB
*/

#include <sys/ioctl.h>		/* for ioctl(): TIOCSTOP & TIOCSTART */
#include <sys/types.h>		/* these two for fstat() */
#include <sys/stat.h>		/* ... also for fstat() */
#include <signal.h>		/* signal-catching stuff */
#include <strings.h>		/* for strchr() */
#include <string.h>		/* for other stuff */
#include <ctype.h>		/* for isspace() */

#include "term.h"		/* for MGR declarations */
#include "bitmap.h"

/* compatibility with different versions of MGR */
#ifdef OLDMGR
#define m_setcursor(s)
#undef m_whatsat
#define m_whatsat(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,x,y,E_GETINFO),m_flush())
/* older version of m_whatsat() forgets to send the y co-ord */
#endif

/* general tty mashing stuff: */
#define TTYSUFFIXLEN (3)	/* length of ptty suffix + 1 */
#define TTYPREFIX "/dev/tty"	/* prefix for pttys */
#define TTYPREFIXLEN (sizeof(TTYPREFIX)+1) /* length of TTYPREFIX+1 */
char ttytemp[TTYPREFIXLEN+TTYSUFFIXLEN];	/* for FULLTTYNAME() */
#define FULLTTYNAME(suff)	/* make full name from suffix - temporary! */\
       (strcat(strcpy(ttytemp, TTYPREFIX), suff))

/* notify string stuff: */
char closednotify[] = "CLOSED:"; /* notify prefix for closed windows */
#define CLNLEN (7)		/* length of above */

/* ify's icon stuff: */
#define LOCALICONNAME "./.ify.icon" /* name for local icon */
#define MGRICONNAME "ify.icon"	/* name for global mgr icon */
#define ICONBITMAP 1		/* scratch bitmap for icon's image */
int icon_w, icon_h;		/* size of icon */
int icon_x, icon_y;		/* position of icon within window */

/* ify window position stuff: */
#define MINW (50)		/* minimum window size */
#define MINH (font_h)		/* minimum height */
int wind_x, wind_y;		/* position of our window on the screen */
int wind_w, wind_h;		/* desired width & height */
int shapetofit = 1;		/* should we shape to fit? */

/* stuff for figuring out where to put closed windows: */
int disp_w, disp_h;		/* display size */
int font_w, font_h;		/* font size */
int border;			/* border thickness */
struct window_data layout[200];	/* space for list of windows on the screen */
int layoutlen;			/* # of windows in layout */
#define Max(x,y)	((x)>(y)?(x):(y))
#define XSLOP (6)		/* extra horizontal space around window */
#define YSLOP (2)		/* extra vertical space around window */

/* error reporting stuff: */
char *progname;			/* name of this program */
extern int errno, sys_nerr;
extern char *sys_errlist[];
char errtemp[100];		/* space for THEERROR() */
#define THEERROR(msg)\
  (strcat(strcpy(errtemp, msg),\
    (errno<sys_nerr?sys_errlist[errno]:"unknown error")))


#define iocuddle ioctl		/* to be cute */

/* stuff for changing our general write permission: */
char ourtty[TTYPREFIXLEN+TTYSUFFIXLEN];	/* the tty we're running on */
int windowpid;			/* process id representing this window */
#define WAITTIME (2)		/* # of seconds to wait for MGR to finish */
				/* closing windows.  Not used if we can */
				/* change the protection on our ptty. */
unsigned short oldmode;		/* protection bits of the tty when the
				   program started */

/* debugging error messages & stuff: */
#define dprintf			/* define dprintf or eprintf to be fprintf */
#define eprintf			/* to turn on debugging messages */

/* Note that the dupkey we use for this program is ^E, written here
   as \005.  I didn't bother defining it, since it appears in strings
   and it's a pain to include a constant in a string with a non-ANSI
   compiler. */
#define DUPKEYON { if (dupkeyokay) m_dupkey('\005'); }
#define DUPKEYOFF { if (dupkeyokay) m_clearmode(M_DUPKEY); }
int dupkeyokay, hasdupkey();	/* is dupkey available? */

#define CNULL ((char *) 0)

char usage[] = "\
While this program's window is active, click on windows with the right\n\
button to open or close.  The middle button's menu lets you quit or choose\n\
a closed window to open.\n";

main(argc, argv)
     int argc;
     char **argv;
{
    int catchint();
#   define BUFLEN (80)
    char buf[BUFLEN+1];		/* space for info from MGR */

    /* ==================== process arguments ==================== */
    progname = argv[0];
    for (argc--, argv++; argc>0; argc--,argv++)
      if ((*argv)[0] == '-')
	switch((*argv)[1]) {
	  case 's':
	    shapetofit = 0;
	    break;
	  default:
	    fprintf(stderr, "%s: unrecognized option: %s\n",
		    progname, *argv);
	    /* fall through */
	  case '\0':
	    fprintf(stderr, "Usage: %s\n%s", progname, usage);
	    exit(1);
	}
      else {
	  fprintf(stderr, "Usage: %s\n%s", progname, usage);
	  exit(1);
      }

    /* ==================== Generic Initialization ==================== */

    m_setup(M_FLUSH);
    dupkeyokay = hasdupkey();
    m_ttyset();
    m_push(P_EVENT | P_FLAGS | P_MENU | P_POSITION);
    signal(SIGINT, catchint);
    DUPKEYOFF;
    m_setmode(M_ABS);
    m_setmode(M_NOWRAP);
    m_setcursor(9);
    m_func(BIT_SRC);
    m_flush();

    {
	char *ttyname();
	char *t = ttyname(2);	/* get tty attached to stderr */

	if (t == CNULL)
	    closedown("Can't find name of tty");
	strcpy(ourtty, t);	/* make a copy of ttyname()'s value */
    }

    /* turn off general write permission to the tty, so we can accept
       m_sendme() messages */
    {
	struct stat ttymode;

	if (stat(ourtty, &ttymode) == -1)
	  closedown(THEERROR("ourtty stat: "));
	oldmode = ttymode.st_mode; /* get the old protection bits */
    }

    /* Can we use m_sendto messages? */
    if ((oldmode & 077) == 0 || chmod(ourtty, 00600) != -1)
      windowpid = getwid();
    else {
	static char mesg[] =
	  "Ify runs better if MGR has its setuid bit on.";

	windowpid = 0;
	m_clear();
	m_push(P_POSITION);
	m_size(sizeof(mesg)-1, 1);
	m_printstr(mesg);
	m_flush();
	sleep(5);
	m_pop();
	m_flush();
    }

    get_param(NULL, &disp_w, &disp_h, &border);
    get_font(&font_w, &font_h);
    DUPKEYON;

    /* ==================== try to read in icon file ==================== */
    m_bitfromfile(ICONBITMAP, LOCALICONNAME); /* try to find local icon */
    skipgarbage(); m_gets(buf);
    eprintf(stderr, "buf: \"%s\"\n", buf);
    if (buf[0] == '\n') {	/* did we get the file? */
	m_bitfromfile(ICONBITMAP, MGRICONNAME); /* no - try global icon */
	skipgarbage(); m_gets(buf);
	eprintf(stderr, "buf: \"%s\"\n", buf);
	if (buf[0] == '\n')	/* no go there either? */
	  loadicon(ICONBITMAP, &icon_w, &icon_h);
    }
    if (buf[0] != '\n')		/* unless we used our built-in icon... */
      sscanf(buf, "%d %d", &icon_w, &icon_h); /* get the icon size */
    /* now we've got the icon in ICONBITMAP, and its size in icon_[wh]. */
    DUPKEYOFF;
    get_size(&wind_x, &wind_y, &wind_w, &wind_h);
    DUPKEYON;
    if (shapetofit) {
	wind_w = Max(MINW, icon_w);
	wind_h = Max(MINH, icon_h);
    }
    icon_x = (wind_w - icon_w)/2;
    icon_y = (wind_h - icon_h)/2;
    wind_w += 2*border;
    wind_h += 2*border;
    if (shapetofit) {
    	m_shapewindow(wind_x, wind_y, wind_w, wind_h);
    }
    m_clear();
    m_bitcopyto(icon_x, icon_y, icon_w, icon_h, 0, 0, 0, ICONBITMAP);
    m_flush();

    eprintf(stderr, "icon: %dx%d\n", icon_w, icon_h);
    eprintf(stderr, "buf: \"%s\"\n", buf);

    /* ==================== set up events ==================== */

    /* clicked on a window */
    m_setevent(BUTTON_1, "\005b%p,%n\n");

    /* screen is being redrawn */
    m_setevent(REDRAW, "\005r\n");

    /* window was reshaped */
    m_setevent(RESHAPE, "\005s\n");

    /* a window is done with being opened - rebuild the menu */
    m_setevent(ACCEPT, "\005n\n");

    /* This window's name */
    m_setevent(NOTIFY, "== Ify ==");

    /* build the initial menu */
    makemenu();

    /* ==================== main event loop ==================== */
    for (;;) {

	skipgarbage();
	if (fgets(buf, BUFLEN, m_termin) == CNULL) /* end of file? */
	  closedown(CNULL);
	buf[strlen(buf)-1] = '\0'; /* zap the newline */

	switch(buf[0]) {

	  case 'q':		/* quit program */
	    closedown(CNULL);
	    break;

	  case 'b':		/* button pressed */
	    {
		int x, y;
		char *notify;

		/* extract information from the event string */
		sscanf(buf+1, "%d %d", &x, &y);
		if ((notify = strchr(buf, ',')) == CNULL)
		  closedown("no comma in button event!");
		notify++;
		clickon(x, y, notify);
	    }
	    break;

	  case 's':		/* they've reshaped the window - unshape it */
	    DUPKEYOFF;
	    get_size(&wind_x, &wind_y, NULL, NULL);
	    DUPKEYON;
	    m_shapewindow(wind_x, wind_y, wind_w, wind_h);
	    /* fall through to redraw */

	  case 'r':		/* redraw the screen */
	    m_clear();
	    m_bitcopyto(icon_x, icon_y, icon_w, icon_h, 0, 0, 0, ICONBITMAP);
	    m_flush();
	    break;

	  case 'n':		/* remake the menu */
	    makemenu();
	    m_flush();
	    break;

	  case 'm':		/* window selected from menu */
	    openwindows(buf+1);	/* open the windows attached to the given */
				/* ptty */
	    break;

	  default:		/* something we weren't expecting */
	    {
		char errtmp[BUFLEN*2];
		sprintf(errtmp, "unrecognized event: %s", buf);
		closedown(errtmp);
		break;
	    }
	    break;
	}
    }
}


/* =============== get our events and only our events =============== */

skipgarbage()
/* scan the input stream until we find a ^E followed by a non-
   ^E character.  Skip any spaces or tabs.  The first non-space/tab
   is pushed back into the input stream. */
{
    int c;
    
    m_flush();
    
    if (dupkeyokay) {
	do {
	    while ((c = m_getchar()) != EOF && c != '\005')
	      ;
	    if (c != EOF)
	      c = m_getchar();
	} while (c == '\005');
	
	while (isspace(c) && c != '\n')
	  c = m_getchar();
	
	if (c != EOF)
	  ungetc(c, m_termin);
    }
    else {
	if ((c = m_getchar()) != '\005')
	  ungetc(c, m_termin);
    }
}


/* ===================== Exit Cleanly ==================== */

catchint()
/* function to call on interrupt - exit with error status */
{
    signal(SIGINT, SIG_DFL);	/* turn off interrupt catching */
    closedown("");
}

closedown(msg)
     char *msg;
/* Clean up window, restore window to its original shape.  If msg == NULL,
   shut down with no error status.  If msg == "", exit with an error
   status.  Otherwise, print <progname>: <msg>\n and exit with an error. */
{
    m_popall();
    m_setcursor(CS_BLOCK);
    m_clear();
    m_flush();
    m_ttyreset();

    /* restore old protection, if we ever changed it */
    if ((oldmode & 077) != 0 && windowpid && chmod(ourtty, oldmode) == -1)
      msg = THEERROR("chmod ourtty II: ");

    if (msg == CNULL)
      exit(0);

    if (msg[0] != '\0')
	fprintf(stderr, "%s: %s\n", progname, msg);
    exit(2);
}


/* ==================== Open or close a window ==================== */

clickon(x, y, notify)
     int x, y;
     char *notify;
/* deal with a click on the given position - we get the notify string at
   that position for free */
{
    char tty[TTYSUFFIXLEN];	/* the tty for that window */
    int win, pid;		/* alternate window #, pid */
    void foreachwindow();	/* iterate over some windows */
    void dontnotice();		/* mark window to be ignored */
    void shapeclose();		/* reshape & relabel a window as closed */
    void queueopen();		/* send the escape codes to re-open a
				   window when the output is re-enabled */

    DUPKEYOFF;
    get_size(&wind_x, &wind_y, NULL, NULL); /* where are we? */
    DUPKEYON;

    x += wind_x; y += wind_y;	/* adjust click location to display */
				/* co-ordinates */

    /* what did the user click on? */
    if (get_whatsat(x, y, tty, &win, &pid) != 0) {
	if (strncmp(notify, closednotify, CLNLEN) == 0)
	  openwindows(tty);
	else {
	    /* get a handle on their tty */
	    FILE *ttyfp, *oldtermout;
	    int ttyfd;

	    if ((ttyfp = fopen(FULLTTYNAME(tty), "w")) == NULL)
	      closedown(THEERROR("open tty II: "));
	    ttyfd = fileno(ttyfp);

	    /* disconnect the menu - the menu isn't going to be valid
	       until this process completes */
	    m_nomenu();
	    m_flush();

	    DUPKEYOFF;
	    layoutlen = get_all(layout); /* get list of all the windows */
	    DUPKEYON;
	    
	    /* make sure that the window isn't currently ^S-ed */
#ifdef _POSIX_SOURCE
	    if (tcflow(ttyfd,TCOON) == -1)
#endif
#ifdef v7
	    if (iocuddle(ttyfd, TIOCSTART) == -1)
#endif
	      closedown(THEERROR("iocuddle TIOCSTART II: "));

	    /* don't notice the windows that haven't been closed yet */
	    foreachwindow(layout, layoutlen, tty, dontnotice, CNULL);

	    /* close the windows attached to this tty */
	    foreachwindow(layout, layoutlen, tty, shapeclose, notify);

	    /* We need to know when MGR is finished interpreting all those
	       commands we've queued up there so we don't iocuddle too soon
	       and freeze the tty before MGR has read the commands.  If we
	       succeeded in turning off general write permission up at the
	       top, we can have the closee send the closer a message with
	       the m_sendto()/ACCEPT mechanism.  Otherwise, we'll just wait
	       an arbitrary amount of time for MGR to finish up, and then
	       iocuddle.  windowpid is set iff we could set up the tty to have
	       write permission turned off. */
		
	    if (windowpid != 0) {
		/* have mgr tell me when it's finished with all that
		   stuff */
		m_push(P_EVENT);
		m_setevent(ACCEPT, "\005done!\n");
		m_flush();
		oldtermout = m_termout; m_termout = ttyfp;
		m_sendto(windowpid, "fini!");
		m_flush();
		m_termout = oldtermout;
		skipgarbage();
		while (m_getchar() != '\n')
		  ;
		m_pop();
		m_flush();
	    }
	    else
	      sleep(WAITTIME);

	    /* stop all output to that ptty */
#ifdef _POSIX_SOURCE
	    if (tcflow(ttyfd, TCOOFF) == -1)
#endif
#ifdef v7
	    if (iocuddle(ttyfd, TIOCSTOP) == -1)
#endif
	      closedown(THEERROR("iocuddle TIOCSTOP: "));
	    
	    /* queue escape codes to re-open them when output is
	       re-enabled */
	    foreachwindow(layout, layoutlen, tty, queueopen, CNULL);

	    fclose(ttyfp);	/* close the tty - we're done */

	    makemenu();		/* refresh the menu's view of life */
	}
    }
}


/* =============== the nitty-gritty of opening and closing =============== */

/*ARGSUSED*/
void
dontnotice(t, w)
     FILE *t;
     struct window_data *w;
/* mark *w as a window for find_spot to ignore */
{
    if (w->w > 0) w->w = -w->w;
}

void
shapeclose(t, w, notify)
     FILE *t;
     struct window_data *w;
     char *notify;
/* reshape, retitle, reposition, and renotify the given window.  Use
   layout[] to find a nice position for the closed window, and ignore
   windows with negative widths in layout[] - these are windows that
   will be closed, but haven't been dealt with yet.  Once we've closed
   this window, record its new position in layout[]. */
{
    int width, height;
    int new_x, new_y;
    char title[BUFLEN*2];	/* space for window's title */

    /* quick hack for positioning windows on a full screen */
    static int next_x, next_y, widest;

    if (w->num == 0)
      sprintf(title, "%-5s",
	      (notify[0] == '\0' ? FULLTTYNAME(w->tty) : notify));
    else
      sprintf(title, "%-5s(%d)",
	      (notify[0] == '\0' ? FULLTTYNAME(w->tty) : notify),
	      w->num);

    width = strlen(title)*font_w + 2*border;
    height = font_h + 2*border;
    if (find_spot(width, height, layout, layoutlen, &new_x, &new_y) == 0) {

	/* quick hack to position a window on a full screen */
	new_x = next_x; new_y = next_y;
	width += XSLOP;
	height += YSLOP;
	if (width > widest) widest = width;
	if ((next_y += height) > disp_h - height) {
	    next_y = 0;
	    next_x += widest;
	    widest = 0;
	}
    }

    {
	FILE *oldtermout = m_termout;
	char newnotify[500];
	m_termout = t;

	/* build up the new notify string: CLOSED:<tty>:<window title> */
	sprintf(newnotify, "%s%s:%s", closednotify, w->tty, title);
    
	m_selectwin(w->num);
	m_push(P_CURSOR|P_EVENT|P_FLAGS|P_FONT|P_POSITION|P_TEXT|P_WINDOW);
				/* push window state */
	m_envcount--;		/* that push isn't OUR push */
	m_font(0);		/* use the default font */
	m_size(strlen(title),1); /* resize it to display message */
	m_movewindow(new_x, new_y); /* move it to a good place */
	m_setevent(NOTIFY,newnotify); /* mark window as closed */
	m_setmode(M_NOWRAP);	/* turn off line wrap */
	m_clearmode(M_ACTIVATE); /* and bury the window */
	m_clear();		/* make it blank */
	m_printstr(title);	/* print new label */
	m_flush();

	m_termout = oldtermout;
    }

    /* record this window's new size and shape in layout[], and thus
       mark it as something to be considered in subsequent find_spot()s */
    w->x = new_x; w->y = new_y;
    w->w = width; w->h = height;
}

void
queueopen(t, w)
     FILE *t;
     struct window_data *w;
/* queue an m_selectwin and an m_pop for this window */
{
    FILE *oldtermout = m_termout;

    m_termout = t;

    m_selectwin(w->num);
    m_envcount++;		/* that pop's not OUR pop */
    m_pop();
    m_flush();

    m_termout = oldtermout;
}


openwindows(tty)
     char *tty;
/* Do the stuff to open a window- do a TIOCSTART iocuddle to let the
   commands sent by queueopen() take effect, and tell ify when it's done. */
{
    FILE *oldtermout, *ttyfp;
    int ttyfd;
    
    if ((ttyfp = fopen(FULLTTYNAME(tty), "r+")) == NULL)
      closedown(THEERROR("open tty: "));
    ttyfd = fileno(ttyfp);
    
    /* disconnect the menu - the menu isn't going to be valid
       until this process completes */
    m_nomenu();
    m_flush();

    /* re-enable output to that ptty */
#ifdef _POSIX_SOURCE
    if (tcflow(ttyfd, TCOON) == -1)
#endif
#ifdef v7
    if (iocuddle(ttyfd, TIOCSTART) == -1)
#endif
      closedown(THEERROR("iocuddle TIOCSTART: "));
    
    /* queue escape codes to let us know when they're all
       opened, if we can */
    if (windowpid != 0) {
	oldtermout = m_termout;
	m_termout = ttyfp;
	m_sendto(windowpid, "fini!");
	m_flush();
	m_termout = oldtermout;
    }
    else {
	sleep(WAITTIME);	/* just wait for it to finish */
	makemenu();		/* make a new menu */
    }

    fclose(ttyfp);
}


/* =============== What's on the screen here? =============== */

int
get_whatsat(x, y, tty, win, pid)
     int x, y;
     char *tty;
     int *win, *pid;
/* find the window under position x, y, and return the suffix of its tty
   in tty and its pid in *pid.  If there is no window at (x, y), or
   if the window there is one of our own, return 0.  Otherwise, return 1. */
{
    char buf[80];		/* space for the reply */
    char fulltty[TTYPREFIXLEN + TTYSUFFIXLEN];

    m_whatsat(x, y);		/* send the request */
    m_flush();
    skipgarbage(); m_gets(buf);	/* read the response */

    if (buf[0] == '\n')		/* empty line?  Nothing there */
      return 0;

    /* old MGR gives you "/dev/ttyxx win pid", newer mgr gives you
       "xx yy win pid", where xx and yy are tty suffixes.  Deal with
       both. */
    sscanf(buf, "%s", fulltty);
    if (fulltty[0] == '/') {	/* full tty name? */
	sscanf(buf, "/dev/tty%s %d %d", tty, win, pid);
	return(windowpid == 0 || windowpid != *pid);
    }
    else {
	sscanf(buf, "%s %s %d %d", fulltty, tty, win, pid);
	if (windowpid == 0)
	  return(strcmp(fulltty, tty) != 0);
	else
	  return(windowpid != *pid);
    }
}


/* =============== Iteration over a list of windows =============== */

void
foreachwindow(win, count, tty, func, data)
     struct window_data win[];
     int count;
     char *tty;
     void (*func)();
     char *data;
/* find every window in win[0..count-1] attached to the given tty, and
   apply func to a file pointer to the tty and the window's parameters.
   if tty==NULL, apply func to all the windows.  tty should be just the
   suffix of the pseudo-tty's name.

   func is unsigned long (*func)(FILE *t, struct window_data *w,
                                 char *data),
   where t is the window's tty, w is a pointer to the window's parameters,
   and data is the same value passed to foreachwindow(). */
{
    FILE *ttyfp = NULL;
    char *lastttyopened = "";
    int i;

    if (tty && (ttyfp = fopen(FULLTTYNAME(tty), "r+")) == NULL)
      closedown(THEERROR("open tty foreachwindow: "));
    
    for (i = 0; i < count; i++) {
	
	/* if the user didn't specify a particular tty and the next window
	   is attached to the one different from the one we've already
	   opened, open the new one. */
	if (!tty && strcmp(win[i].tty, lastttyopened)!=0) {
	    if (ttyfp)
	      fclose(ttyfp);
	    if ((ttyfp = fopen(FULLTTYNAME(win[i].tty), "r+")) == NULL)
	      closedown(THEERROR("open tty foreachwindow II: "));
	    lastttyopened = win[i].tty;
	}

	/* apply the function to the window and its stuff */
	if (!tty || strcmp(tty, win[i].tty)==0)
	  (*func)(ttyfp, &win[i], data);
    }

    if (ttyfp)
      fclose(ttyfp);
}


/* ==================== Put the window someplace nice ==================== */


int
find_spot(wide, high, coords, count, newx, newy)
     int wide,high;		/* minimum spot size */
     struct window_data coords[]; /* current screen layout */
     int count;			/* # of entries in coords[] */
     int *newx, *newy;		/* put resulting co-ords here */
/* scan the screen (as described in coords[]) for a free spot of the given
   size (wide x high).  Place the resulting co-ordinates in *newx and
   *newy.  Ignore entries in coords whose widths are negative.
   
   =============== This Code Was Stolen From Steve Uhler. ===============
                                       (credit where credit is due dept.)*/
{
    register int c, intersection, x, y, nexty;
    static int in_win();
    
    dprintf(stderr,"found %d windows\n", count);
    
    wide += XSLOP;
    high += YSLOP;

    /* Find the best spot.  We want to avoid too exhaustive a search.
       We march through the screen, trying to fit the moving window into
       spaces.  Any time we collide with a window, we skip to the right edge
       of that window and note if it's top edge is the lowest one we've seen
       which is still above where we are.  This allows us to skip over the
       larger areas of occupied screen quickly. */
    for( y = disp_h-high;  y >= 0;  y = nexty - 1 ) {
	nexty = 0;
	for( x = disp_w-wide;  x >= 0;  x -= 1 ) {
	    intersection = 0;
	    dprintf(stderr,"  - Checking %d,%d\n",x,y);
	    for( c = 0;  c < count;  c++ ) {
		if( coords[c].w < 0 )
		  continue;
		if( in_win( coords+c, x, y, x + wide, y + high ) ) {
		    intersection = 1;
		    nexty = Max(nexty, coords[c].y - high);   
		    x = coords[c].x - wide;
		    break;
		}
	    }
	    if( !intersection ) {
		dprintf(stderr,"going to %d, %d\n", x, y);
		*newx = x+XSLOP/2; *newy = y+YSLOP/2;
		return( 1 );
	    }
	}
    }
    dprintf(stderr,"no openings\n");
    return( 0 );
}


/* check for window-rectangle intersection */

static int
in_win(list,x0,y0,x1,y1)
     register struct window_data *list;	/* window coordinates */
     register int x0,y0,x1,y1;	/* rectangle coordinates */
{
    return((list->x + list->w < x0  ||  x1 < list->x ||
	    list->y + list->h < y0  ||  y1 < list->y)
	   ?  0  :  1);
}

/* ==================== get window id ==================== */

getwid()
{
    char name[80], buf[80];

    /* establish a unique notify name */
    sprintf(name, "getwid %d", getpid());
    m_push(P_EVENT);
    m_setevent(NOTIFY, name);
    m_getinfo(G_NOTIFY);
    /* skipgarbage() would go here, but this gets called with no dupkey. */
    for (;;) {
	m_gets(buf);
	if (buf[0] == '\n')
	  break;
	buf[strlen(buf)-1] = '\0'; /* wipe out newline */
	if (strcmp(buf+strlen(buf)-strlen(name), name) == 0) {
	    int wid = atoi(buf);
	    do
	      m_gets(buf);
	    while (buf[0] != '\n');
	    m_pop();
	    return(wid);
	}
    }

    m_pop();
    return(0);
}


/* ==================== build the menu ==================== */

#define MAXITEMS (500)
#define MAXTEXTLEN (80)
#define MAXEVENTLEN (30)

makemenu()
/* Look at the list of NOTIFY-ing windows and build a menu listing
   the closed windows, where the event string for each item provides
   the name of the ptty running the window. */
{
    static char toptext[] = "don't quit,quit,remake menu,";
    static char topevents[] = ",\005q\n,\005n\n,";

    char data[80]; 		/* hold a line of data */
    char text[MAXITEMS*MAXTEXTLEN]; /* text of menu items */
    char events[MAXITEMS*MAXEVENTLEN]; /* text of events */
    static char all[MAXITEMS*(MAXTEXTLEN+MAXEVENTLEN)]; /* the whole menu */

    char *textend, *eventend;	/* free space at end of text[] and event[] */
    char *windowname;		/* name of the window within data[] */

    static char line[] = "------------------------------";
    char *partline;
    int longesttext = 11;	/* length of the longest menu item */

    DUPKEYOFF;			/* turn dupkey off, since m_getinfo(G_NOTIFY)
				   won't work with dupkey on (MGR bug! Poor
				   Steve!) */

    textend = text;
    eventend = events;
    text[0] = events[0] = '\0';

    /* Get the list of available windows, store their ttys in ttys[],
       and use their names to build up the text of the menu */
    m_push(P_EVENT);
    m_nomenu();
    m_clearmenu(1);
    m_getinfo(G_NOTIFY);	/* ask for the data */
    m_flush();
    for (;;) {
	if (fgets(data, sizeof(data), m_termin) == NULL)
	  break;
	if (data[0] == '\n')
	  break;
	
	data[strlen(data)-1] = '\0';
	
	/* Is this a valid G_NOTIFY info string?  Check delimiters.*/
	{
	    char *p;
	    if ((p = strchr(data, '.')) == NULL ||
		(p = strchr(p+1, ' ')) == NULL ||
		(p = strchr(p+1, ' ')) == NULL)
	      continue;
	    
	    windowname = p+1;
	    if ((p = strchr(p+1, ':')) == NULL ||
		(p = strchr(p+1, ':')) == NULL)
	      continue;
	}
	
	/* do we want to include this menu item in the list?  Does its
	   name start with closednotify[]? */
	if (strncmp(windowname, closednotify, CLNLEN) == 0) {
	    
	    /* zap all occurrences of the menu separator */
	    {
		char *p;
		
		for (p = windowname; p = strchr(p, ',');)
		  *p = ' ';
	    }
	    
	    strcat(eventend, "\005m"); eventend += strlen(eventend);
	    sscanf(windowname, "CLOSED:%[^:]:%[^:]", eventend, textend);
	    if (strlen(textend) > longesttext)
	      longesttext = strlen(textend);
	    strcat(textend, ",");
	    strcat(eventend, "\n,");
	    
	    textend += strlen(textend);
	    eventend += strlen(eventend);
	}
    }
    
    /* build up a dividing line with the appropriate length */
    if (longesttext > sizeof(line)-1)
      longesttext = sizeof(line)-1;
    partline = line+(sizeof(line)-1-longesttext);

    /* now put all the pieces together */
    sprintf(all, ",%s%s,%s%s,%s",
	    toptext,partline,text,topevents, events);

    m_pop();
    m_loadmenu(1, all);
    m_selectmenu(1);
    DUPKEYON;
    m_flush();
}


/* ==================== Does this MGR have a dupkey? ==================== */

int
hasdupkey()
{
    int c, gotdupkey;

    m_push(P_FLAGS);
    m_dupkey('\005');		/* turn on dupkey mode (maybe) */
    m_ttyset();
    m_getinfo(G_WINSIZE);	/* ask for something innocuous */
    
    /* examine what we got back and see if we got a dupkey with it */
    gotdupkey = 0;
    while ((c = getchar()) != '\n')
      if (c == '\005')
	gotdupkey = 1;
    m_ttyreset();
    m_pop();

    return gotdupkey;
}


print(s)
     char *s;
{
    write(2, s, strlen(s));
}
