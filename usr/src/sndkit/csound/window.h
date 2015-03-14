/*******************************************************\
*	window.h					*
*	portable window graphs stolen from Csound	*
*	necessary header declarations			*
*	08nov90 dpwe					*
\*******************************************************/

#ifndef NULL
#define NULL 0L
#endif

/*** yukky ansi bodge ***/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double		/* you can't have floats in prototypes! */
#else
#define FLOATARG float
#endif /* def __STDC__ */
#endif /* ndef FLOATARG */

#define CAPSIZE  60

typedef struct {
	long	windid;			/* set by MakeGraph() */
	float   *fdata;			/* data passed to DrawGraph */
	long    npts;			/* size of above array */
	char    caption[CAPSIZE];	/* caption string for graph */
	short   waitflg;		/* set =1 to wait for ms after Draw */
	short	polarity;		/* controls positioning of X axis */
	float   max, min;		/* workspace .. extrema this frame */
	float	absmax;			/* workspace .. largest of above */
	float   oabsmax;		/* Y axis scaling factor */
	int	danflag;		/* set to 1 for extra Yaxis mid span */
} WINDAT;

enum {        /* symbols for WINDAT.polarity field */
        NOPOL,
	NEGPOL,
	POSPOL,
	BIPOL
};

typedef struct {	/* for 'joystick' input window */
        long     windid;	/* xwindow handle */
	int	 m_x,m_y;	/* current crosshair pixel adr */
	float	 x,y;		/* current proportions of fsd */
	int	 down;
} XYINDAT;

#ifdef __STDC__		/* full prototypes */
    
/* WINDAT *SetDisp(); */
void dispset(WINDAT *, float *, long, char *, int, char *), display(WINDAT *);
WINDAT *NewWin(char *name, int wait);
void   DoDisp(WINDAT *wdptr,float *data,int len);

int  Graphable();		/* initialise windows.  Returns 1 if X ok */
void MakeGraph(WINDAT *wdptr, char *name);	/* create wdw for a graph */
void MakeXYin(XYINDAT *wdptr, FLOATARG x, FLOATARG y);
                                /* create a mouse input window; init scale */
void DrawGraph(WINDAT *wdptr);	/* update graph in existing window */
void ReadXYin(XYINDAT *wdptr);	/* fetch latest value from ms input wdw */
void KillGraph(WINDAT *wdptr);	/* remove a graph window */
void KillXYin(XYINDAT *wdptr);	/* remove a ms input window */
int  ExitGraph(); /* print click-Exit message in most recently active window */

#else /* not STDC */

void dispset(), display();
WINDAT *NewWin();
void   DoDisp();

int  Graphable();
void MakeGraph();
void MakeXYin();
void DrawGraph();
void ReadXYin();
void KillGraph();
void KillXYin();
int  ExitGraph();

#endif /* STDC */
