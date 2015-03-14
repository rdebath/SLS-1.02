/*  3dlife.c

    Graphical Human-Computer Interfaces (Prof. Glinert), Project #1 

    by Kevin Martin, 3/21/91
    Courtesy of Fractured Reality Software

    This program runs under X/Windows.  It presents a fixed-angle three-
    dimensional view of a cube in which two plant "armies" fight for
    survival and dominance according to rules similar to those in the
    original game of Life by Conway.

    Disclaimer!  This program was written merely for academic purposes.
    The code and techniques presented here are, as much as is possible,
    hereby put into the public domain.  However, any use of this code
    arouses my interest - drop me a note if you've done something
    with it.

    I can be reached as sigma@rpi.edu on a good day.

    Thoughts on possible improvements -

      Provide information like generation number, number of living cells
    of each type (red army, blue army, green poison cells), statistics of
    births and death since previous generation, backup more than one
    generation at a time, faster graphics for slower machines (it's
    intolerable on an IBM XStation, for example), user-customizable rules,
    et cetera.

    To compile:

      cc -g 3dlife.c -o 3dlife -lm -lXaw -lXmu -lXt -lXext -lX11 -lXau

    although you don't really need all the libraries.  Note that you do
    need the Athena Widget Set, and I don't know what happens if you
    don't actually have a color screen - but hey, it's my first X/Windows
    program - you get what you pay for.

*/

/* the header files needed to run under X/Windows */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
/* these are our widgets */
#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

/* other various header files, possibly unnecessary */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern double drand48(); /* for randomly seeding the cube */

/* various other constants controlling window setup and size */
#define APPNAME "Kevin Martin, this is his Life" /* window title */
#define MAX 12 /* maximum size of cube */
#define XFRAME 15 /* offset into window from edge */
#define YFRAME 45
#define WIDTH 800 /* size of window */
#define HEIGHT 600

/* constants controlling the rules of Life */
#define B_RURAL 8 /* birth cutoff */
#define B_URBAN 12
#define U_RURAL 4 /* undercrowding, death cutoff */
#define U_URBAN 12
#define O_RURAL 27 /* overcrowding, death cutoff */
#define O_URBAN 19

/* constant array describing influences of neighbors */
int weight[3][3][3] = {{{1,2,1},{2,3,2},{1,2,1}},
                       {{2,3,2},{3,0,3},{2,3,2}},
                       {{1,2,1},{2,3,2},{1,2,1}}};

/* Type declaration for managing cells in the simulation. */
enum celltypes {EMPTY,GOOD,EVIL,POISON};

/* X/Windows related variables. */
XtAppContext app; /* the first two are very important; */
Display *dpy; /* they determine our connection with the X server */
Arg arg[10]; /* for initialising widgets */
Widget Wshell,Wbox,Wbutton;
Window Winbox; /* easy reference to XtWindow(Wbox) */
Colormap Cmap; /* the rest control colors on-screen */
XColor Cred,Cblue,Cgreen,Cblack,Cgray,Cwhite;
XGCValues gcv;
GC redgc,bluegc,greengc,blackgc,graygc,whitegc;

/* Non X/Windows related variables. */
Boolean available; /* are we busy with something right now? */
Boolean usingold; /* indicates if we're backed up one stage */
Boolean usegrid; /* indicates if we're showing the wireframe grid */
int gennum; /* how many generations we've been through */
enum celltypes cell[MAX][MAX][MAX]; /* main playfield */
enum celltypes ocell[MAX][MAX][MAX]; /* previous generation */
int xsize,ysize,n; /* size of ellipses and cube */
int openx,openy; /* indicates which Z-column is "opened" on screen */
int k1,k2; /* common subexpressions for graphics placement */

/* giveup gives a rudimentary error indication and exits the program. */
void giveup(number)
  int number;
{
  fprintf(stderr,"Failure point #%d\n",number);
  exit(number);
} /* giveup */

/* gcptr returns a pointer to the appropriate graphics context for a
   given cell. */
GC *gcptr(cell)
  enum celltypes cell;
{
  switch (cell) {
    case EMPTY: return &blackgc;
    case GOOD: return &bluegc;
    case EVIL: return &redgc;
    case POISON: return &greengc;
    default: giveup(1);
  }
} /* gcptr */

/* putmkr draws a blue or red marker wherever needed. */
void putmkr(usegc,x,y)
  GC *usegc;
  int x,y;
{
  XFillArc(dpy,Winbox,*usegc,x,y,xsize,ysize,0,360*64);
  XDrawArc(dpy,Winbox,graygc,x,y,xsize,ysize,0,360*64);
} /* putmkr */

/* drawopen displays the opened Z-column beneath the cube, if any.  If told
   to, it also redraws the Z-column in the regular cube display. */
void drawopen(redisp)
  Boolean redisp;
{
  int i;

  /* Draw a border line between cube and opened column. */
  XFillRectangle(dpy,Winbox,blackgc,
    XFRAME-1,
    n*k2+YFRAME+ysize*(n-1)/2,
    n*k1+1,3);
  if (openx >= 0) { /* draw the Z-opened column */
    XDrawRectangle(dpy,Winbox,blackgc, /* mark column */
      openx*k1+XFRAME-1,openy*k2+YFRAME-1,k1,k2);
    for (i = 0; i < n; i++) /* show Z-column from side */
      putmkr(gcptr(cell[openx][openy][i]),
        i*(xsize+2)+XFRAME*2,
        n*k2+YFRAME+ysize*(n-1)/2+5);
    if (redisp) /* redisplay in original cube as well */
      for (i = 0; i < n; i++) /* redraw a single Z-column */
        putmkr(gcptr(cell[openx][openy][i]),
          openx*k1+xsize*i/2+XFRAME,
          openy*k2+ysize*i/2+YFRAME);
  } else /* just clear the area */
    XFillRectangle(dpy,Winbox,whitegc,
      XFRAME*2,n*k2+YFRAME+ysize*(n-1)/2+5,
      n*(xsize+2)+1,ysize+2);
} /* drawopen */

/* show clears the window area and draws the current playfield. */
void show()
{
  int x,y,z;

  XClearWindow(dpy,Winbox); /* clear our drawing space */
  for (z = 0; z < n; z++) {
    for (y = 0; y < n; y++)
      for (x = 0; x < n; x++)
        putmkr(gcptr(cell[x][y][z]),
          x*k1+xsize*z/2+XFRAME,
          y*k2+ysize*z/2+YFRAME);
    if (usegrid) /* draw the wireframe for this z-layer */
      for (y = 0; y < n; y++) {
        /* horizontal lines */
        XDrawLine(dpy,Winbox,graygc,
          xsize*(z+1)/2+XFRAME,y*k2+ysize*(z+1)/2+YFRAME, 
          (n-1)*k1+xsize*(z+1)/2+XFRAME,y*k2+ysize*(z+1)/2+YFRAME); 
        /* vertical lines */
        XDrawLine(dpy,Winbox,graygc,
          y*k1+xsize*(z+1)/2+XFRAME,ysize*(z+1)/2+YFRAME, 
          y*k1+xsize*(z+1)/2+XFRAME,(n-1)*k2+ysize*(z+1)/2+YFRAME); 
        if (z != (n - 1)) /* don't connect last layer */
          for (x = 0; x < n; x++) /* connectors between layers */
            XDrawLine(dpy,Winbox,graygc,
              x*k1+xsize*(z+1)/2+XFRAME,y*k2+ysize*(z+1)/2+YFRAME, 
              x*k1+xsize*(z+2)/2+XFRAME,y*k2+ysize*(z+2)/2+YFRAME); 
      }
  } /* next layer */
  drawopen(True);
} /* show */

/* xchgstate swaps cell and ocell. */
void xchgstate()
{
  int x,y,z;
  enum celltypes w; /* for swapping each entry */

  for (x = 0; x < MAX; x++)
    for (y = 0; y < MAX; y++)
      for (z = 0; z < MAX; z++) {
        w = ocell[x][y][z];
        ocell[x][y][z] = cell[x][y][z];
        cell[x][y][z] = w;
      }
} /* xchgstate */

/* legal tests a set of coordinates to see if they fit the current cube. */
Boolean legal(x,y,z)
  int x,y,z;
{
  if ((x >= 0) && (x < n) && (y >= 0) && (y < n) && (z >= 0) && (z < n))
    return True;
  else
    return False;
} /* legal */

/* neighbors counts the neighbors of a given cell in ocell. */
void neighbors(x,y,z,b,r,p)
  int x,y,z;
  int *b,*r,*p;
{
  int i,j,k,w;

  *b = *r = *p = 0;
  for (i = -1; i <= 1; i++)
    for (j = -1; j <= 1; j++)
      for (k = -1; k <= 1; k++)
        if ((i || j || k) && legal(x+i,y+j,z+k)) {
          w = weight[i+1][j+1][k+1];
          switch (ocell[x+i][y+j][z+k]) {
            case GOOD: *b += w; break;
            case EVIL: *r += w; break;
            case POISON: *p += w;
          }
        }
} /* neighbors */

/* produce advances us to the next generation by applying our rules. */
void produce()
{
  int i,x,y,z,red,blue,poison;
  enum celltypes newt;
  
  xchgstate(); /* we will fill into cell */
  for (x = 0; x < n; x++)
    for (y = 0; y < n; y++)
      for (z = 0; z < n; z++) {
        newt = ocell[x][y][z]; /* assume no change */
        neighbors(x,y,z,&blue,&red,&poison); /* sound off! */
        switch (newt) {
          case EMPTY: /* nothing here, perhaps a birth? */
            i = blue - red;
            switch (poison) { /* different heuristic for poison counts */
              case 0: /* safe to live here */
                if (i >= B_RURAL) newt = GOOD;
                if (i <= -B_RURAL) newt = EVIL;
              break;
              case 1: /* urban living */
                if (i >= B_URBAN) newt = GOOD;
                if (i <= -B_URBAN) newt = EVIL;
            }
          break; /* end of EMPTY case */ 
          case GOOD: case EVIL:
            i = (newt == GOOD) ? (blue - red) : (red - blue);
            switch (poison) {
              case 0: /* safe living */
                if ((i <= U_RURAL) || (i >= O_RURAL)) newt = EMPTY;
              break;
              case 1: /* sort of hazardous here */
                if ((i <= U_URBAN) || (i >= O_URBAN)) newt = EMPTY;
              break;
              default: /* way too much poison here! */
                newt = EMPTY;
            }
        } /* switch (newt) */
        cell[x][y][z] = newt;
      }
} /* produce */

/* Aselect is a registered event handler for button releases.  The basic
   purpose is to determine which Z-column this release was made on by
   checking the XEvent.  We then open that column for modification. */
void Aselect(W,A,X)
  Widget W;
  caddr_t A;
  XEvent *X;
{
  XButtonReleasedEvent *xee = (XButtonReleasedEvent *) X;
  int x,y,z,k;
  enum celltypes tmpc;

  if ((available) && (X -> type == ButtonRelease)) {
    x = (xee -> x - XFRAME) / k1;
    y = (xee -> y - YFRAME) / k2;
    /* check to see if this selection was made on the open column */
    k = n*k2+YFRAME+ysize*(n-1)/2+5;
    if ((openx >= 0) && (y >= n) &&
        (xee -> y >= k) && (xee -> y <= k + ysize)) {
      z = (xee -> x - XFRAME*2) / (xsize+2);
      if ((z >= 0) && (z < n)) { /* a legal selection made? */
        tmpc = (usingold) ? ocell[openx][openy][z] : cell[openx][openy][z];
        switch (tmpc) { /* cycle through possible values */
          case EMPTY: tmpc = GOOD; break;
          case GOOD: tmpc = EVIL; break;
          case EVIL: tmpc = POISON; break;
          case POISON: tmpc = EMPTY;
        }
        if (usingold)
          ocell[openx][openy][z] = tmpc;
        else
          cell[openx][openy][z] = tmpc;
        drawopen(True); /* redisplay this column in both places */
        return; /* no further processing, please */
      } /* legal selection? */
    } /* open column case */
    if (openx >= 0) /* erase old mark */
      XDrawRectangle(dpy,Winbox,whitegc,
        openx*k1+XFRAME-1,openy*k2+YFRAME-1,k1,k2);
    if ((x >= n) || (y >= n))
      openx = openy = -1;
    else {
      XDrawRectangle(dpy,XtWindow(Wbox),blackgc,
        x*(xsize+2+xsize*(n-1)/2)+XFRAME-1,
        y*(ysize+2+ysize*(n-1)/2)+YFRAME-1,
        xsize+2+xsize*(n-1)/2,ysize+2+ysize*(n-1)/2);
      openx = x;
      openy = y;
    }
    drawopen(False);
  }
} /* Aselect */

/* Ashow is a registered event handler for redrawing notifications. */
void Ashow(W,A,X)
  Widget W;
  caddr_t A;
  XEvent *X;
{
  XExposeEvent *xee = (XExposeEvent *) X;

  if ((available) && (xee -> count == 0)) { /* compress repeated events */
    available = False;
    show();
    available = True;
  }
} /* Ashow */

/* Anext is a registered callback procedure for the "next" button.  If the
   program is not otherwise busy, this will advance the system to the next
   stage, applying the rules of Life if we aren't backed up one stage. */
void Anext(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if (available) { /* make sure we're not busy */
    available = False; /* we're going to be busy now */
    if (usingold) { /* are we backed up one stage? */
      xchgstate(); /* switch to current stage */
      usingold = False;
    }
    else /* we need a new generation */
      produce();
    gennum++;
    show();
    available = True;
  }
} /* Anext */

/* Aprevious is a registered callback procedure for the "previous" button.
   It functions in a fashion analogous to the "next" button, except that
   we are limited to backing up one stage only. */
void Aprevious(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if (available) { /* make sure we're not busy */
    available = False; /* we're going to be busy now */
    if ((!usingold) && gennum) { /* can we back up any further? */
      xchgstate();
      usingold = True;
      gennum--;
      show();
    }
    available = True;
  }
} /* Aprevious */

/* recalc determines the appropriate size of the markers. */
void recalc()
{
  xsize = nint(floor(((WIDTH-XFRAME*2)*1.0/n-2)/(1.0+(n-1)*1.0/2)))-1;
  ysize = nint(floor((HEIGHT*1.0/(n+1)-2)/(1.0+n*1.0/2)))-1;
  k1 = xsize+2+xsize*(n-1)/2;
  k2 = ysize+2+ysize*(n-1)/2;
} /* recalc */

/* Abigger is a registered callback procedure for the "bigger" button.
   It increases the cube size (n) to the next larger value, and recalculates
   all related variables such as the size of the chips. */
void Abigger(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if ((available) && (n < MAX)) { /* make sure we're not busy */
    available = False; /* we're going to be busy now */
    n++;
    recalc();
    openx = openy = -1; /* unmark any selected column */
    show();
    available = True;
  }
} /* Abigger */

/* Asmaller is a registered callback procedure for the "smaller" button.
   It decreases the cube size (n) to the next smallest value, and recalcs
   all related variables.  Minimum size is one. */
void Asmaller(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if ((available) && (n > 1)) {
    available = False;
    n--;
    recalc();
    openx = openy = -1; /* unmark any selected column */
    show();
    available = True;
  }
} /* Asmaller */

/* Atoggle is a registered callback procedure for the "toggle" button.
   It switches the layout grid on and off. */
void Atoggle(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if (available) {
    available = False;
    if (usegrid)
      usegrid = False;
    else
      usegrid = True;
    show();
    available = True;
  }
} /* Atoggle */

/* seedcube randomly seeds the matrix - with no poison. */
void seedcube()
{
  int x,y,z;

  srand48((long) time(NULL)); /* initialize random number generator */
  for (x = 0; x < MAX; x++)
    for (y = 0; y < MAX; y++)
      for (z = 0; z < MAX; z++)
        switch (nint(floor(drand48() * 4.0))) {
          case 1: cell[x][y][z] = GOOD; break;
          case 2: cell[x][y][z] = EVIL; break;
          case 3: cell[x][y][z] = EMPTY; break;
        }
} /* seedcube */

/* Aseed is a registered callback procedure for the "reseed" button.
   It randomly reseeds the cube. */
void Aseed(W,A,B)
  Widget W;
  caddr_t A,B;
{
  if (available) {
    available = False;
    seedcube();
    show();
    available = True;
  }
} /* Aseed */

/* Aquit is a registered callback procedure for the "quit" button. */
void Aquit(W,A,B)
  Widget W;
  caddr_t A,B;
{
  exit(0); /* just quit; X will clean up everything */
} /* Aquit */

/* initialize does various bits of initialization for the simulation. */
void initialize()
{
  int i,j,k;

  usingold = False;
  usegrid = True;
  gennum = 0;
  openx = openy = -1;
  n = 4; /* for example */
  recalc();
  for (i = 0; i < MAX; i++)
    for (j = 0; j < MAX; j++)
      for (k = 0; k < MAX; k++)
        cell[i][j][k] = EMPTY;
} /* initialize */

/* xinitialize makes the calls needed to prepare a window in X/Windows,
   as well as set up the widgets - buttons and such. */
int xinitialize(argcptr,argv)
  int *argcptr;
  char **argv;
{
  int i,x,y;

  XtToolkitInitialize();
  app = XtCreateApplicationContext();
  dpy = XtOpenDisplay(app,NULL,APPNAME,"app",NULL,0,argcptr,argv);
  i = 0;
  XtSetArg(arg[i],XtNwidth,WIDTH); i++; /* request particular size */
  XtSetArg(arg[i],XtNheight,HEIGHT); i++;
  Wshell = XtAppCreateShell(APPNAME,"app",applicationShellWidgetClass,
             dpy,arg,i);
  Wbox = XtCreateManagedWidget("Box",boxWidgetClass,Wshell,arg,i);
  XtAddEventHandler(Wbox,ExposureMask,False,Ashow,NULL);
  XtAddEventHandler(Wshell,ButtonPressMask,True,Aselect,NULL);
  XtAddEventHandler(Wshell,ButtonReleaseMask,True,Aselect,NULL);
  /* set up all the buttons */
  Wbutton = XtCreateManagedWidget("Next",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Anext,NULL);
  Wbutton = XtCreateManagedWidget("Previous",commandWidgetClass,Wbox,
              NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Aprevious,NULL);
  Wbutton = XtCreateManagedWidget("Bigger",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Abigger,NULL);
  Wbutton = XtCreateManagedWidget("Smaller",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Asmaller,NULL);
  Wbutton = XtCreateManagedWidget("Grid",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Atoggle,NULL);
  Wbutton = XtCreateManagedWidget("Reseed",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Aseed,NULL);
  Wbutton = XtCreateManagedWidget("Quit",commandWidgetClass,Wbox,NULL,0);
  XtAddCallback(Wbutton,XtNcallback,Aquit,NULL);
  /* set up the colors we'll be needing */
  Cmap = DefaultColormap(dpy,DefaultScreen(dpy));
  XParseColor(dpy,Cmap,"Red",&Cred);
  XParseColor(dpy,Cmap,"Blue",&Cblue);
  XParseColor(dpy,Cmap,"Green",&Cgreen);
  XParseColor(dpy,Cmap,"White",&Cwhite);
  XParseColor(dpy,Cmap,"Gray",&Cgray);
  XParseColor(dpy,Cmap,"Black",&Cblack);
  XAllocColor(dpy,Cmap,&Cred);
  XAllocColor(dpy,Cmap,&Cblue);
  XAllocColor(dpy,Cmap,&Cgreen);
  XAllocColor(dpy,Cmap,&Cwhite);
  XAllocColor(dpy,Cmap,&Cgray);
  XAllocColor(dpy,Cmap,&Cblack);
  gcv.foreground = Cred.pixel;
  redgc = XtGetGC(Wshell,GCForeground,&gcv); /* for red markers */
  gcv.foreground = Cblue.pixel;
  bluegc = XtGetGC(Wshell,GCForeground,&gcv); /* for blue markers */
  gcv.foreground = Cgreen.pixel;
  greengc = XtGetGC(Wshell,GCForeground,&gcv); /* for green markers */
  gcv.foreground = Cwhite.pixel;
  whitegc = XtGetGC(Wshell,GCForeground,&gcv); /* for clearing areas */
  gcv.foreground = Cgray.pixel;
  graygc = XtGetGC(Wshell,GCForeground,&gcv); /* for gray frames */
  gcv.foreground = Cblack.pixel;
  blackgc = XtGetGC(Wshell,GCForeground,&gcv);
  available = False;
  XtRealizeWidget(Wshell); /* make the windows appear */
  do ; while (!XtIsRealized(Wbox));
  Winbox = XtWindow(Wbox);
  available = True;
} /* xinitialize */

main(argc,argv)
  int argc;
  char **argv;
{
  initialize(); /* do normal initialization */
  seedcube();
  xinitialize(&argc,argv); /* open our window */
  XtAppMainLoop(app); /* wait for button events */
} /* main */

