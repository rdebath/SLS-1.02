#include <X11/X.h>       			/*    winX11.c		*/
#include <X11/Xlib.h>				/* Csound Xwin graphs	*/
						/*   dpwe,15may90 	*/
#include "window.h"

/*** yukky ansi bodge ***/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double		/* you can't have floats in prototypes! */
#else
#define FLOATARG float
#endif /* def __STDC__ */
#endif /* ndef FLOATARG */

#define XINIT    10      /* set default window location */
#define YINIT    50
#define WIDTH    450     /* start off small - user resizes */
#define HEIGHT   120
#define BDR      2  
#define MSGX     16	 /* offset for message in window */
#define MSGY     16
#define TXHGHT   14	 /* baseline offset for text */

#define MAXLSEGS 2048	 /* X can only deal with so many linesegs .. */

#define MAXXPTS  MAXLSEGS /* maximum size of XPoint array */

static Display *xdisp = NULL;
static int     xscrn;
static GC      xdfgc;
static GContext	    xgcon;
static Window       xlwin = NULL;
static Font         xfont, xbfont;
static XFontStruct  *xfsp, *xbfsp;
static XPoint	    xpts[MAXXPTS];

static winstance = 0;     /* counts windows to offset across screen */
#define STACKDEPTH 4      /* number of windows down to go */

extern char *getenv();

/* static function prototypes */
#ifdef __STDC__
static void myXprintLines(Window win,char *msg,int x,int y,int *pw,int *ph);
static void myXwinWait(Window win,char *msg);
static int  myXwait(char *msg);
static void myXWaitForExpose(Window win);
static Window MakeWindow(int type, char *name);	  /* 0 => graph, 1 => xyinp */
static void KillWin(Window *pxwin);
#else
static void myXprintLines();
static void myXwinWait();
static int myXwait();
static void myXWaitForExpose();
static Window MakeWindow();
static void KillWin();
#endif /* __STDC__ */

static void myXprintLines(win,msg,x,y,pw,ph) /* prints str with embdd '\n's */
    Window win;                              /*           on multiple lines */
    char   *msg;           /* returns overall width & depth of area printed */
    int    x,y;
    int    *pw,*ph;
    {
    int    lin = 0, w, slen;
    char   *s1,*s2,*index();
    XTextItem    txitem[1];

    *pw = 0;   *ph = 0;
    txitem[0].delta = 1;
    txitem[0].font  = xbfont;
    s1 = msg;
    do {
          s2 = index(s1, '\n');
	  if(s2) *s2 = '\0';
	  txitem[0].chars  = s1;
	  txitem[0].nchars = slen = strlen(s1);    
	  XDrawText(xdisp,win,xdfgc,MSGX,MSGY+lin*TXHGHT,txitem,1);
	  if(s2) *s2 = '\n';
	  if( (w=XTextWidth(xbfsp, s1, slen)) > *pw)
	    *pw = w;
	  *ph += TXHGHT;
	  s1 = s2+1;
	  ++lin;
	  } while(s2 != NULL);
  }
    
static void myXwinWait(win,msg)
    Window win;
    char   *msg;
    {
    XEvent       event;
    int	         msgW,msgH;

    myXprintLines(win,msg,MSGX,MSGY, &msgW, &msgH);
    do  {
	XWindowEvent(xdisp, win, 
		     ButtonPressMask|ExposureMask|ButtonMotionMask, &event);
	if(event.type == Expose)
	    myXprintLines(win,msg,MSGX,MSGY, &msgW, &msgH);
	} while (!(event.type == ButtonPress));
    do  {
	XWindowEvent(xdisp, win, 
		     ButtonReleaseMask|ExposureMask|ButtonMotionMask, &event);
	if(event.type == Expose)
	    myXprintLines(win,msg,MSGX,MSGY, &msgW, &msgH);
	} while (!(event.type == ButtonRelease));
    XClearArea(xdisp,win,MSGX,MSGY-TXHGHT+4,msgW,msgH,0);
    XFlush(xdisp);                    /* make sure wdw is cleared NOW */
    }

static int myXwait(msg)
    char *msg;
    {
    if(xlwin != NULL) {
	myXwinWait(xlwin, msg);
	return(0);
    }
    else return(1);
    }

static void myXWaitForExpose(win)    /* wait until it's ready to draw */
    Window win;
    {
    XEvent    event;

    while ( (XCheckWindowEvent(xdisp, win, ExposureMask, &event)));
    while (!(XCheckWindowEvent(xdisp, win, ExposureMask, &event)));
    }

int Graphable()	/* called during program initialisation */
{		/* decides whether to use X or not; initializes X if so */
    char 	*term;
    int	  	rc = 0;		/* default : don't use X, use tty ascii */

    if (xdisp != NULL)	return 1;		/* already open - signal ok  */
    if ((xdisp = XOpenDisplay(NULL)) != NULL) { /* if this OK, X is possible */
	xscrn = XDefaultScreen(xdisp);
	xdfgc = XDefaultGC(xdisp, xscrn);
	xgcon = XGContextFromGC(xdfgc);
	rc = 1; 			        /*       so set return code  */
    }
    return(rc);
}

static Window MakeWindow(type,name)
    int	type;		/* 0 => graph, 1 => xyinp */
    char *name;
{
    char   *fontname;
    Window xwin;
    XSetWindowAttributes watts;
    unsigned long        wmask;

    watts.background_pixel = XWhitePixel(xdisp, xscrn);
    watts.backing_store = WhenMapped;
    watts.event_mask    = ExposureMask | ButtonPressMask | ButtonReleaseMask;
    if(type == 1)	/* input windows need more events */
	watts.event_mask |= ButtonMotionMask;
    wmask = CWBackingStore | CWEventMask | CWBackPixel;
    xwin = XCreateWindow(xdisp, XDefaultRootWindow(xdisp),
			XINIT + (winstance / STACKDEPTH)*(int)(1.2*WIDTH), 
			YINIT + (winstance % STACKDEPTH)*(int)(1.2*HEIGHT), 
			WIDTH, HEIGHT, BDR, 
			CopyFromParent, InputOutput, CopyFromParent, 
			wmask, &watts);
 
    ++winstance;
    XStoreName(xdisp, xwin, name);
    XMapWindow(xdisp, xwin);   /* map the window to the screen */
    myXWaitForExpose(xwin);    /* wait until it's ready to draw */
    xlwin = xwin;		    /* keep track of latest window for msgs */
    fontname = "6x10";                             /* set up font info... */
    xfsp  = XLoadQueryFont(xdisp,fontname);
    xfont = xfsp->fid;
    fontname = "6x13";                             /* set up font info... */
    xbfsp  = XLoadQueryFont(xdisp,fontname);
    xbfont = xbfsp->fid;
    myXwait("New window: \nPosition & size, \nclick to go on");
    xlwin = NULL;		/* first draw to win has no wait */
    return(xwin);
}

void MakeGraph(wdptr,name)
    WINDAT *wdptr;
    char   *name;
    {
    wdptr->windid = MakeWindow(0,name);
    }

#define GUTTERH 20           /* space for text at top & bottom */
#define BORDERW 10           /* inset from L & R edge */

void MakeXYin(wdptr,x,y)
    XYINDAT *wdptr;
    FLOATARG	x,y;	/* initial proportions */
    {
    Window	xwin;
    XWindowAttributes info;
	int 	b;
	short	win_x, win_y, win_w, win_h;
	short	gra_x, gra_y, gra_w, gra_h;

    wdptr->windid = xwin = MakeWindow(1,"XY input");
    wdptr->down = 0;	/* by def released after Make */

    XGetWindowAttributes(xdisp,xwin,&info);
    win_w = info.width;  win_h = info.height;
    win_x = 0;	win_y = 0;		/* window pixels addressed relative */

    /* set new width and height so we leave a 20% border around the plot */
    gra_w = win_w - 2*BORDERW;		gra_h = win_h - 2*GUTTERH;
    gra_x = win_x + BORDERW;		gra_y = win_y + GUTTERH;
    wdptr->m_x = gra_x + (int)(x * (float)gra_w);
    wdptr->m_y = gra_y + (int)(y * (float)gra_h);
		/* draw up new xhairs */
	    XDrawLine(xdisp, xwin, xdfgc, gra_x, wdptr->m_y, 
		            (gra_x + gra_w), wdptr->m_y);
	    XDrawLine(xdisp, xwin, xdfgc, wdptr->m_x, gra_y, 
		            wdptr->m_x, (gra_y + gra_h));  
    wdptr->x = x;	wdptr->y = y;
    }

void DrawGraph(wdptr)
    WINDAT	*wdptr;
{
    float	*fdata = wdptr->fdata;
    long 	npts   = wdptr->npts;
    char	*msg   = wdptr->caption;

/*    XPoint 	*xpts;		now a static global */
    XWindowAttributes info;
    XTextItem   txitem[1];
    Window	xwin;
    short	win_x, win_y, win_w, win_h;	/* window rect */
    short	gra_x, gra_y, gra_w, gra_h;	/* graph rect is inset */
    short	y_axis;
    int 	lsegs,pts_pls;
    int 	pol;
    char	string[80];

    xwin = wdptr->windid;
    pol  = wdptr->polarity;

    if (wdptr->waitflg) 
	myXwait("Click here to continue..");
    xlwin = xwin;		    /* keep track of latest window for msgs */
    /* setting xlwin here rather than in MakeWin avoids first pause */

    XClearWindow(xdisp, xwin);
    XGetWindowAttributes(xdisp,xwin,&info);
    win_w = info.width;  win_h = info.height;
    win_x = 0;	win_y = 0;		/* window pixels addressed relative */

    /* set new width and height so we leave a 20% border around the plot */
    gra_w = win_w - 2*BORDERW;
    gra_h = win_h - 2*GUTTERH;
    gra_x = win_x + BORDERW;
    gra_y = win_y + GUTTERH;
    /* figure height of Y axis - top, middle or bottom */
    if(pol == (short)BIPOL)
	y_axis = gra_y + (gra_h/2);
    else if(pol == (short)NEGPOL)
	y_axis = gra_y;
    else 		/* POSPOL */
	y_axis = gra_y + gra_h;
    
/*    if (npts < gra_w)
/*	lsegs = npts;
/*    else 
/*	lsegs = gra_w;		/* max one datum per w pixel */
	if(npts < MAXLSEGS)
		{
		lsegs = npts;			/* one lineseg per datum */
		pts_pls = 1;
		}
	else{
		pts_pls = 1 + (npts/MAXLSEGS);
		lsegs = ((long)npts + pts_pls - 1)/(long)pts_pls;
		}

    /* THIS ASSIGNMENT ASSUMES LONG INT FOR NPTS > 32767 */

	/* alloc point array */
/*	xpts = (XPoint *) mmalloc((long)lsegs * sizeof(XPoint));    */

	{       /* take scale factors out of for-loop for faster run-time */
	register float x_scale = gra_w / (float)(lsegs-1);   
	register float y_scale = gra_h / wdptr->oabsmax; /* unipolar default */
	register XPoint *ptptr = xpts;
	register float  f,ma,mi,*fdptr = fdata;
	register int c,i = 0, j = lsegs;

	if(pol == (short)BIPOL)
	    y_scale /= 2.0;  		/* max data scales to h/2 */
                     /* put x-y pairs into a point list for XDraw */
	while(j--)
	    {
	    ptptr->x = gra_x + (short)((float)i++ * x_scale);
	    if(pts_pls == 1)
	    	f = *fdptr++;
	    else
	    	{
	    	ma = mi = *fdptr++;
	    	for(c = 1; c < pts_pls; ++c)
	    		if ( (f = *fdptr++) > ma)	ma = f;
	    		else if ( f<mi )			mi = f;
	    	if(ma < 0)		f = mi;
	    	else if(mi > 0)	f = ma;
	    	else if(ma > -mi) f = ma;
			else f = mi;
	    	}
	    (ptptr++)->y = y_axis - (short)(f * y_scale);
	    }
	}

    XDrawLines(xdisp, xwin, xdfgc, xpts, lsegs, CoordModeOrigin);
/*    free(pts);				/* ! return data array */

                    /* now draw axes: y-axis is always on the left edge,
                       x-axis height is determined by the case we're in */
    XDrawLine(xdisp, xwin, xdfgc, gra_x, y_axis, (gra_x + gra_w), y_axis);
    XDrawLine(xdisp, xwin, xdfgc, gra_x, gra_y, gra_x, (gra_y + gra_h));  

    if(wdptr->danflag)	/* flag to add dotted divider */
	{
/*	SetDottedXorPen();	*/
	XDrawLine(xdisp, xwin, xdfgc, win_x+win_w/2, win_y+GUTTERH,
		  win_x+win_w/2, win_y+win_h-GUTTERH);
/*	RestoreSolidCopyPen();	*/
	}
	
    sprintf(string,"%s  %ld points, max %5.3f",msg,npts,wdptr->oabsmax);
    txitem[0].chars  = string;		/* draw the label under the curve */
    txitem[0].nchars = strlen(string);    
    txitem[0].delta  = 1;
    txitem[0].font   = xfont;
    XDrawText(xdisp, xwin, xdfgc, gra_x, (gra_y + gra_h + TXHGHT), txitem, 1);
    XFlush(xdisp);                    /* finally, flush output to the screen */
    }

void ReadXYin(wdptr)
    XYINDAT *wdptr;
    {
    XEvent	theEv;
    Window	xwin;
    XWindowAttributes info;
    int 	b;
    short	win_x, win_y, win_w, win_h;
    short	gra_x, gra_y, gra_w, gra_h;
    short	m_x, m_y;

    xwin = wdptr->windid;
    m_x  = wdptr->m_x;
    m_y  = wdptr->m_y;
    while( XCheckWindowEvent(xdisp, xwin, 
		ButtonReleaseMask|ButtonMotionMask|ButtonPressMask, &theEv) )
	/* there was an event.. */
	switch(theEv.type)
	    {
	case ButtonPress:
	    m_x = theEv.xbutton.x;
	    m_y = theEv.xbutton.y;
	    wdptr->down = 1;
	    break;
	case ButtonRelease:
	    m_x = theEv.xbutton.x;
	    m_y = theEv.xbutton.y;
	    wdptr->down = 0;
	    break;
	case MotionNotify:
	    m_x = theEv.xmotion.x;
	    m_y = theEv.xmotion.y;
	    break;
	default:
	    break;
	    }
    /* cycle until no events pending */
    XGetWindowAttributes(xdisp,xwin,&info);
    win_w = info.width;  win_h = info.height;
    win_x = 0;	win_y = 0;	/* window pixels addressed relative */

    /* set new width and height so we leave a 20% border around the plot */
    gra_w = win_w - 2*BORDERW;		gra_h = win_h - 2*GUTTERH;
    gra_x = win_x + BORDERW;		gra_y = win_y + GUTTERH;

    /* clip mouse position */
    if(m_x < gra_x) 				m_x = gra_x;
    else if(m_x > gra_x+gra_w)		m_x = gra_x+gra_w;
    if(m_y < gra_y)					m_y = gra_y;
    else if(m_y > gra_y+gra_h)		m_y = gra_y+gra_h;

    if(m_x != wdptr->m_x || m_y != wdptr->m_y)
	{			/* only redo if changed */
	/* undraw old crosshairs */
	XSetForeground(xdisp, xdfgc, XWhitePixel(xdisp, xscrn));
	XDrawLine(xdisp, xwin, xdfgc, gra_x, wdptr->m_y, 
		  (gra_x + gra_w), wdptr->m_y);
	XDrawLine(xdisp, xwin, xdfgc, wdptr->m_x, gra_y, wdptr->m_x, 
		  (gra_y + gra_h));  
	/* draw up new xhairs */
	XSetForeground(xdisp, xdfgc, XBlackPixel(xdisp, xscrn));
	XDrawLine(xdisp, xwin, xdfgc, gra_x, m_y, (gra_x + gra_w), m_y);
	XDrawLine(xdisp, xwin, xdfgc, m_x, gra_y, m_x, (gra_y + gra_h));  
	wdptr->m_x = m_x;	wdptr->m_y = m_y;
	wdptr->x = ((float)m_x-gra_x)/(float)gra_w;
	wdptr->y = ((float)m_y-gra_y)/(float)gra_h;
	} 
    }

static void KillWin(pxwin)
    Window *pxwin;
    {
    XUnmapWindow(xdisp, *pxwin);
    XDestroyWindow(xdisp, *pxwin);
    *pxwin = NULL;
    }

void KillGraph(wdptr)
    WINDAT *wdptr;
    {
    KillWin((Window *)&(wdptr->windid));
    }

void KillXYin(wdptr)
    XYINDAT *wdptr;
    {
    KillWin((Window *)&(wdptr->windid));
    }

int ExitGraph()        /* print click-Exit message in most recently active window */
{
    return(myXwait("click here to EXIT"));
}

