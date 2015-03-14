#include "cs.h" 	      			/* 	WINDOW.C       	*/
#include "window.h"				/*  graph window mgr 	*/
						/*  dpwe 16may90	*/
extern int  displays, graphsoff;

static void (*makeFn)();	/* pointer to window make fn - */
extern void MakeAscii();	/*     either teletype         */
extern void MakeGraph();	/*     or some graphics system */

static void (*drawFn)();	/* pointer to appropriate drawing fn */
extern void DrawAscii();
extern void DrawGraph();

static void (*killFn)();	/* pointer to window destroy fn */
extern void KillAscii();
extern void KillGraph();

       void (*mkxyFn)();	/* pointer to xyinput window creator */
       void MkXYDummy();
extern void MakeXYin();

       void (*rdxyFn)();	/* pointer to xyinput window reader */
       void RdXYDummy();
extern void ReadXYin();

static int (*exitFn)();	/* pointer to window last exit fn - returns 0 to exit immediately */
extern int ExitGraph();

void DummyFn()		/* somewhere to invoke for no display */
    {
    }

int DummyRFn()		/* somewhere to invoke that returns 1 (!) for dummy exit fn */
    {
	return(1);
    }

void MkXYDummy(wdptr,x,y)
    XYINDAT *wdptr;
    float	x,y;	/* initial proportions */
    {
    wdptr->windid = 0; 	/* xwin = MakeWindow(1);	*/
    wdptr->down = 0;	/* by def released after Make */

    wdptr->m_x = 0;
    wdptr->m_y = 0;
    wdptr->x = x;	wdptr->y = y;
    }

void RdXYDummy(wdptr)
    XYINDAT *wdptr;
    {
/*	wdptr->m_x = m_x;	wdptr->m_y = m_y;
	wdptr->x = ((float)m_x-gra_x)/(float)gra_w;
	wdptr->y = ((float)m_y-gra_y)/(float)gra_h;
	*/
	}

void dispinit()		/* called once on initialisation of program to 	*/
    {			/*   choose between teletype or bitmap graphics */
    if (!displays)
	{
        printf("displays suppressed\n");
	makeFn = DummyFn;
	drawFn = DummyFn;
	killFn = DummyFn;
	mkxyFn = MkXYDummy;
	rdxyFn = RdXYDummy;
	exitFn = DummyRFn;
	}
#ifdef WINDOWS
    else if(!graphsoff && Graphable())
	{               /* provided by window driver: is this session able? */
	makeFn = MakeGraph;
	drawFn = DrawGraph;
	killFn = KillGraph;
	mkxyFn = MakeXYin;
	rdxyFn = ReadXYin;
	exitFn = ExitGraph;
	}
#endif
    else {
        printf("graphics %s, ascii substituted\n",
	       (graphsoff)? "suppressed" : "not supported on this terminal");
	makeFn = MakeAscii;
	drawFn = DrawAscii;
	killFn = KillAscii;
	mkxyFn = MkXYDummy;
	rdxyFn = RdXYDummy;
	exitFn = DummyRFn;
	}
    }

void dispset(wdptr,fdata,npts,caption,waitflg,label)  /* setup a new window */
     WINDAT *wdptr;                             /*   & init the data struct */
     float  *fdata;
     long   npts;
     char   *caption;
     int    waitflg;
     char   *label;
{
    register char *s = caption;
    register char *t = wdptr->caption;
    register char *tlim = t + CAPSIZE - 1;

    if (!displays) {
        printf("%s\n",caption);      /* displays disabled? just print caption */
	return;
    }
    if (!wdptr->windid) 		/* if no window defined for this str  */
	(*makeFn)(wdptr,label);         /*    create one  */
    wdptr->fdata    = fdata;            /* init remainder of data structure   */
    wdptr->npts     = npts;
    while (*s != '\0' && t < tlim)
        *t++ = *s++;                    /*  (copy the caption) */
    *t = '\0';
    wdptr->waitflg  = waitflg;
    wdptr->polarity = (short)NOPOL;
    wdptr->max      = 0.0;
    wdptr->min      = 0.0;
    wdptr->absmax   = 0.0;
    wdptr->oabsmax  = 0.0;
    wdptr->danflag  = 0;
}

void dispkill(wdptr)
    WINDAT *wdptr;
{
    (*killFn)(wdptr);
}

void dispexit()
{
    (*exitFn)();                  /* prompt for exit from last active window */
}

void display(wdptr)        /* prepare a float array, then call the graphing fn */
 register WINDAT *wdptr;   /* window data_struct */
{
 register float *fp, *fplim;
    float	max, min, absmax, fval;
    int 	pol;

    if (!displays)  return;              /* displays disabled? return */
    fp = wdptr->fdata;
    fplim = fp + wdptr->npts;
    for (max = *fp++, min = max; fp < fplim; )   /* find max & min values */
      {
	if ((fval = *fp++) > max)	max = fval;
        else if (fval < min)    	min = fval;
      }
    absmax = (-min > max )? (-min):max;
    wdptr->max    = max;                 /* record most pos and most */
    wdptr->min    = min;                 /*  neg this array of data  */
    wdptr->absmax = absmax;              /* record absmax this data  */
    if (absmax > wdptr->oabsmax)
      wdptr->oabsmax = absmax;           /* & absmax over life of win */

    pol = wdptr->polarity;     /* adjust polarity flg for life of win */
    if (pol == (short)NOPOL)
      {
	if (max > 0. && min < 0.)  				pol = (short)BIPOL;
	else if (max <= 0.&& min <0.)			pol = (short)NEGPOL;
	else									pol = (short)POSPOL;
      }
    else if (pol == (short)POSPOL && min < 0)	pol = (short)BIPOL;
    else if (pol == (short)NEGPOL && max > 0)	pol = (short)BIPOL;
    wdptr->polarity = pol;

    (*drawFn)(wdptr);                    /* now graph the function */
}

