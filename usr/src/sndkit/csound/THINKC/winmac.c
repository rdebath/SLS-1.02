#include "cs.h"					/*    winmac.c              */
               					/* Csound Macintosh graphs  */
               					/*   18aug90 dpwe           */
#include <QuickDraw.h>
#include <MacTypes.h>
#include <WindowMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include "window.h"

/* #include "NetBuild.h" */

#define XINIT    10      /* set default window location */
#define YINIT    50
#define WIDTH    300     /* start off v small - user resizes */
#define HEIGHT   120
#define BDR      2  
#define MSGX     16	 /* offset for message in window */
#define MSGY     16
#define TXHGHT   14	 /* baseline offset for text */
#define SBARWIDTH 16

/* protoypes */
static void myWwait(char *msg);
static void myWwinWait(WindowPtr win,char *msg);

/* static variables */

static winstance = 0;      /* counts windows to offset across screen */
#define STACKDEPTH 4      /* number of windows down to go */

WindowPtr	csWindow, lwin=NULL;
Rect		dragRect;
Rect		windowBounds = { 40, 40, 150, 150 };
int			width = 5;

#define	MAXWIN 16
WindowPtr	ourWins[MAXWIN] = { NULL, NULL, NULL, NULL,
							NULL, NULL, NULL, NULL,
							NULL, NULL, NULL, NULL,
							NULL, NULL, NULL, NULL };


UpdateWindow(theWindow)
WindowPtr	theWindow;
{
	GrafPtr	savePort;
	
	GetPort( &savePort );
	SetPort( theWindow );
	BeginUpdate( theWindow );
	EraseRect(&theWindow->portRect);
	DrawGrowIcon( theWindow );
	
	DrawContents( theWindow );
	
	EndUpdate( theWindow );
	SetPort( savePort );
}


/*
DoContent(theWindow, theEvent)		/* Mouse down in our window *\
WindowPtr	theWindow;
EventRecord	*theEvent;
{
	int				cntlCode;
	ControlHandle 	theControl;
	int				pageSize;
	GrafPtr			savePort;
	
	GetPort(&savePort);
	SetPort(theWindow);
	GlobalToLocal( &theEvent->where );
	if ((cntlCode = FindControl(theEvent->where, theWindow, &theControl)) == 0) {
		if (PtInRect( theEvent->where, &myWindow->portRect ))
			DoMyContent(theWindow, theEvent);		/* in tini.conts *\
	}
	else if (cntlCode == inThumb) {
		TrackControl(theControl, theEvent->where, 0L);
		AdjustText();
	}
	else
		TrackControl(theControl, theEvent->where, &ScrollProc);

	SetPort(savePort);
}
*/
MyGrowWindow( w, p )
WindowPtr w;
Point p;
{
	GrafPtr	savePort;
	long	theResult;
	Rect 	r, oView;
	
	GetPort( &savePort );
	SetPort( w );

	SetRect(&r, 80, 80, screenBits.bounds.right, screenBits.bounds.bottom);
	theResult = GrowWindow( w, p, &r );
	if (theResult == 0)
	  return;
	SizeWindow( w, LoWord(theResult), HiWord(theResult), 1);

	InvalRect(&w->portRect);
	oView = w->portRect;
	
	HidePen();
	DrawContents(w);
	ShowPen();

	SetPort( savePort );
}


/*
CloseMyWindow()
{
	HideWindow( myWindow );
	DisposeWindow( myWindow );
}

*/

int Graphable()	/* called during program initialisation */
	{		/* Always use graphs on mac, so just sets it up */
	/* mac screen initialisation ? */
	InitGraf(&thePort);
	return(1);		/* graphics available */
	}

void MakeGraph(wdptr,title)
    WINDAT *wdptr;
    char *title;
	{
	dragRect = screenBits.bounds;
	
	windowBounds.left = 
			XINIT + (winstance / STACKDEPTH)*(int)(1.25*WIDTH); 
	windowBounds.top = 
			YINIT + (winstance % STACKDEPTH)*(int)(1.25*HEIGHT); 
	windowBounds.right = windowBounds.left + WIDTH;
	windowBounds.bottom = windowBounds.top + HEIGHT;
	CtoPstr(title);
	csWindow = NewWindow(0L, &windowBounds, title, true, documentProc, -1L, true, 0);
	PtoCstr(title);
	RegWin(csWindow);
	SetPort(csWindow);

/*	WaitForUpdate(csWindow);	*/
    lwin = csWindow;		  
    myWwait("New window: \nPosition & size, \nclick to go on");
	lwin = NULL;		/* first draw to win has no wait */
	++winstance;
    wdptr->windid = (long)csWindow;
}

WaitForUpdate(WindowPtr w)
	{
	MainEvent(updateEvt);
	}

typedef struct macgdat		/* macintosh graph data */
	{
	int		type;			/* zero if graph vertices, all else treated as string */
	short	y_axis;
	short	gra_x,gra_y,gra_w,gra_h;
	Str255	caption;
	short	danflag;		/* flag for mid-graph 2nd axis */
	long	npts;			/* point count */
	Point	pts;			/* variable length point record */
	} MACGDAT;

#define MCG_LSEG 0

#define GUTTERH 20           /* space for text at top & bottom */
#define BORDERW 10           /* inset from L & R edge */

void DrawGraph(wdptr)
    WINDAT	*wdptr;
{
    float	*fdata = wdptr->fdata;
    long 	npts   = wdptr->npts;
    char	*msg   = wdptr->caption;
	MACGDAT	*gdat;

    short	win_x, win_y, win_w, win_h;	/* window rect */
    short	gra_x, gra_y, gra_w, gra_h;	/* graph rect is inset */
    short	y_axis;
    long 	pts_to_disp;
    int 	pol;
    char	string[80];

	Rect	myRect;
	long	n;
	Point	*pp;

    csWindow = (WindowPtr)wdptr->windid;
    pol  = wdptr->polarity;

    if (wdptr->waitflg) 
		myWwait("Click here to continue..");
    lwin = csWindow;		    /* keep track of latest window for msgs */
    /* setting xlwin here rather than in MakeWin avoids first pause */
	
	SetPort(csWindow);
	myRect = csWindow->portRect;
	myRect.bottom -= SBARWIDTH;
	myRect.right  -= SBARWIDTH;
	EraseRect(&myRect);		
	InsetRect(&myRect,BDR,BDR);
	
    win_x = myRect.left;			win_y = myRect.top;	
    win_w = myRect.right - win_x;	win_h = myRect.bottom - win_y;

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
/*	pts_to_disp = npts;
/*    else 
/*	pts_to_disp = gra_w;		/* max one datum per w pixel */
	pts_to_disp = npts;		/* No - one lineseg per datum */

	/* alloc point array */
	gdat = (MACGDAT *)mmalloc((long)((pts_to_disp-1) * sizeof(Point))+sizeof(MACGDAT)); 
	gdat->type = MCG_LSEG;
	gdat->npts = pts_to_disp;
	gdat->y_axis = y_axis;
	gdat->gra_x = gra_x;
	gdat->gra_y = gra_y;
	gdat->gra_w = gra_w;
	gdat->gra_h = gra_h;
	gdat->danflag = wdptr->danflag;		/* propogate the famous danflag */
	
	{       /* take scale factors out of for-loop for faster run-time */
	register float x_scale = gra_w / (float)(pts_to_disp-1);   
	register float y_scale = gra_h / wdptr->oabsmax; /* unipolar default */
	register short x_0 = gra_x;
	register short y_0 = y_axis;
	register Point *ptptr = &(gdat->pts);
	register float  *fdptr = fdata;
	register int i = 0, j = pts_to_disp;

	if(pol == (short)BIPOL)
	    y_scale /= 2.0;  		/* max data scales to h/2 */
                     /* put x-y pairs into a point list for XDraw */
	while(j--)
	    {
	    ptptr->h = x_0 + (short)((float)i++ * x_scale);
	    (ptptr++)->v = y_0 - (short)(*fdptr++ * y_scale);
	    }
	}
    sprintf((char *)gdat->caption,"%s  %ld points, scalemax %5.3f",msg,npts,wdptr->oabsmax);
	((WindowPeek)csWindow)->refCon = (long)gdat;
	RedrawGWin(csWindow);
	while(Button())			;		/* mouse down pauses */
	}


RedrawGWin(WindowPtr w)
	{
	MACGDAT	*gdat = (MACGDAT *)((WindowPeek)w)->refCon;
	Rect	myRect;
	long	n;
	Point	*pp;

	n = gdat->npts;
	pp = &(gdat->pts);
	MoveTo(pp->h, pp->v);
	++pp;
	while(--n)		/* pre-inc because only n-1 linesegs for n data */
		{
		LineTo(pp->h,pp->v);
		++pp;
		}
	/* now draw axes: y-axis is always on the left edge,
	  x-axis height is determined by the case we're in */
	MoveTo(gdat->gra_x, gdat->y_axis);	LineTo((gdat->gra_x + gdat->gra_w), gdat->y_axis);
	MoveTo(gdat->gra_x, gdat->gra_y);	LineTo(gdat->gra_x, (gdat->gra_y + gdat->gra_h));
	if(gdat->danflag == 1)		/* danflag means add another vertical axis halfway thru */
		{
		MoveTo(gdat->gra_x + (gdat->gra_w/2), gdat->gra_y);
		LineTo(gdat->gra_x + (gdat->gra_w/2), gdat->gra_y + gdat->gra_h);
		}
	AddText(gdat->gra_x, (gdat->gra_y + gdat->gra_h + TXHGHT), gdat->caption);
    }

static void myWprintLines(win,msg,x,y,pw,ph) /* prints str with embdd '\n's*/
    WindowPtr win;                           /*          on multiple lines */
    char   *msg;          /* returns overall width & depth of area printed */
    int    x,y;
    int    *pw,*ph;
    {
    int    lin = 0, w, slen;
    char   *s1,*s2;

    *pw = 0;   *ph = 0;
    s1 = msg;
    do {
      s2 = strchr(s1, '\n');
	  if(s2) *s2 = '\0';
	  AddText(MSGX,MSGY+lin*TXHGHT, s1);
	  if(s2) *s2 = '\n';
	  if( (w = 8*(s2-s1)) > *pw)	/* hack to figure width of text */
	    *pw = w;
	  *ph += TXHGHT;
	  s1 = s2+1;
	  ++lin;
	  } while(s2 != NULL);
  }
    
DrawContents(WindowPtr win)
	{
	Rect	myRect;
	int		msgW,msgH,color = true;
	MACGDAT	*gdat = (MACGDAT *)((WindowPeek)win)->refCon;

	SetPort(win);
	myRect = win->portRect;
	myRect.bottom -= SBARWIDTH;
	myRect.right  -= SBARWIDTH;
/*	EraseRect(&myRect);		*/
	ClipRect(&myRect);
	InsetRect(&myRect,BDR,BDR);
	
	if(gdat == NULL)
		{
		while(myRect.left < myRect.right)
		  {
		  FillOval(&myRect, color ? black : white);
		  InsetRect(&myRect, width, width);
		  color = !color;
		  }
		}
	else if(gdat->type == MCG_LSEG)
		RedrawGWin(win);
	else
  		myWprintLines(win,(char *)gdat,MSGX,MSGY, &msgW, &msgH);
	}

void KillGraph(wdptr)
    WINDAT *wdptr;
    {
    WindowPtr win;

    csWindow = (WindowPtr)wdptr->windid;
	HideWindow( csWindow );
	DisposeWindow( csWindow );
	RmvWin(csWindow);
    wdptr->windid = 0L;
    }

int ExitGraph()  /* print click-Exit message in most recently active window */
{
    myWwait("click here to EXIT");
    return(lwin==NULL);		/* if no window, we didn't wait, so ask to do it later */
}

static void myWwait(msg)
    char *msg;
    {
    if(lwin != NULL)
		myWwinWait(lwin, msg);
    }

static void myWwinWait(win,msg)
    WindowPtr win;
    char   *msg;
    {
    int	    msgW,msgH;
    Rect	r;
	char	dummy;
	long	oldRefCon;

	cursNorm();
	oldRefCon = ((WindowPeek)win)->refCon;
	((WindowPeek)win)->refCon = (long)msg;
    myWprintLines(win,msg,MSGX,MSGY, &msgW, &msgH);
    dummy = MainEvent(mouseDown);
    r.top = MSGY-TXHGHT+4;	r.left = MSGX;
    r.bottom = r.top + msgH;	r.right = r.left + msgW;
	EraseRect(&r);
	((WindowPeek)win)->refCon = oldRefCon;
	cursPop();
    }

AddText(x,y,s)						/* Just put a string on screen */
	int	x,y;
	char *s;						/* null terminated */
	{
    TextFont(4);			/* Monaco */
    TextSize(9);			/* 9 pt   */
	MoveTo(x,y);			/* Add text label in top left of unit */
	CtoPstr(s);
	DrawString(s);
	PtoCstr(s);
	}

int MainEvent(int waitforevent) 
	{
	EventRecord		myEvent;
	WindowPtr		whichWindow;
	Rect			r;
	int 			done = 0;

	SystemTask();
/*	Debugger();	*/
	do{
		if (GetNextEvent(everyEvent, &myEvent))
			{
			done = 0;
			switch (myEvent.what)
				{
			case mouseDown:
				switch (FindWindow( myEvent.where, &whichWindow ))
					{
				case inDesk: 
					SysBeep(10L);
					SysBeep(10L);
					break;
		/*		case inGoAway:
					if (ours(whichWindow))
						if (TrackGoAway( whichWindow, myEvent.where) )
							DoFile(fmClose);
					break;
				case inMenuBar:
					return( DoCommand( MenuSelect(myEvent.where) ) );	*/
				case inSysWindow:
					SystemClick( &myEvent, whichWindow );
					break;
				case inDrag:
					if (ours(whichWindow))
						{
						DragWindow( whichWindow, myEvent.where, &dragRect );
						}
					break;
				case inGrow:
					if (ours(whichWindow))
						MyGrowWindow( whichWindow, myEvent.where );
					break;
				case inContent:
					if (whichWindow != FrontWindow())
						SelectWindow(whichWindow);
					else 
						if (ours(whichWindow))
						/*	DoContent(whichWindow, &myEvent);  */
							done = 1;
 					break;
				default: ;
					} /* end switch FindWindow	*/
					break;
			case keyDown:
			case autoKey: 
			/*	{
				register char	theChar;
			
				theChar = myEvent.message & charCodeMask;
				if ((myEvent.modifiers & cmdKey) != 0) 
					return( DoCommand( MenuKey( theChar ) ));
				else{
					ShowSelect();
					dirty = 1;
					}
				}	*/
				done = 1;	/* that will do */
				break;
			case activateEvt:
				if (ours(whichWindow = (WindowPtr)myEvent.message))
					{
					r=(*whichWindow).portRect;
				/*	r.top = r.bottom - (SBarWidth+1);
					r.left = r.left - (SBarWidth+1);	*/
					InvalRect(&r);
				/*	if ( myEvent.modifiers & activeFlag )
						{
						ShowControl( vScroll );
						DisableItem( myMenus[editM], undoCommand );
						}	
					else{
						HideControl( vScroll );
						ZeroScrap();
						}	*/
					}
				break;
			case updateEvt: 
				if (ours(whichWindow = (WindowPtr)myEvent.message)) 
					{
					UpdateWindow(whichWindow);
					done = 1;
					}
				break;	
			default: ;
				} /* end of case myEvent.what */
			}
		} while(done == 0 || myEvent.what != waitforevent);
	return(myEvent.what);
	}

RegWin(WindowPtr w)
	{
	int i;
	
	for(i=0; i<MAXWIN; ++i)
		if(ourWins[i] == NULL)
			{
			ourWins[i] = w;
			i = MAXWIN;
			}
	}

RmvWin(WindowPtr w)
	{
	int i;
	
	for(i=0; i<MAXWIN; ++i)
		if(ourWins[i] == w)
			{
			ourWins[i] = NULL;
			i = MAXWIN;
			}
	}

int ours(WindowPtr w)
	{
	int i;
	int rc = 0;
	
	for(i=0; i<MAXWIN; ++i)
		if(ourWins[i] == w)
			{
			rc = 1;
			i = MAXWIN;
			}
	return(rc);
	}

void MakeXYin(wdptr,x,y)
    XYINDAT *wdptr;
    float	x,y;	/* initial proportions */
    {
	MkXYDummy(wdptr,x,y);
    }

void ReadXYin(wdptr)
    XYINDAT *wdptr;
    {
	RdXYDummy(wdptr);
	}



