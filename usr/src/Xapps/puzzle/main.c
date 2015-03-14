/* $XConsortium: main.c,v 1.15 91/02/18 18:04:16 converse Exp $ */

/* Puzzle - (C) Copyright 1987, 1988 Don Bennett.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 */

#define DEBUG
#define USE_PICTURE
#define SERVER_BUG

/**  Puzzle
 **
 ** Don Bennett, HP Labs 
 ** 
 ** this is the interface code for the puzzle program.
 **/

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "ac.cursor"
#include "ac_mask"

#define max(x,y)	((x)>(y)?(x):(y))
#define min(x,y)	((x)>(y)?(y):(x))
#define abs(x)		((x)>0?(x):-(x))

#define PUZZLE_BORDER_WIDTH	2

#define TITLE_WINDOW_HEIGHT	25
#define BOUNDARY_HEIGHT		3

#define BOX_WIDTH		10
#define BOX_HEIGHT		10

#define MIN_TILE_HEIGHT		30
#define MIN_TILE_WIDTH		30

#define MAX_STEPS		1000
#define DEFAULT_SPEED		5

#define TITLE_TILES	0
#define TITLE_TEXT	1
#define TITLE_ANIMATED	2

int 	BoxWidth  =	BOX_WIDTH;
int	BoxHeight =	BOX_HEIGHT;

int	PuzzleSize = 4;
int	PuzzleWidth=4, PuzzleHeight=4;
char    defaultPuzzleSize[] = "4x4";

int	TileHeight, TileWidth;
int	TextXStart;
int     TitleWinHeight, BoundaryHeight, TileWinHeight;

int	FgPixel, BgPixel;

Display 	*dpy;
int		screen;
GC		gc, rect_gc;
Colormap	PuzzleColormap;

typedef struct {
    Window	root;
    int		x,y;
    unsigned int	width, height;
    unsigned int	border_width;
    unsigned int	depth;
} WindowGeom;

WindowGeom	PuzzleWinInfo;

Window 		PuzzleRoot, TitleWindow=0, TileWindow,
    		ScrambleWindow, SolveWindow;

char		*ProgName;

char		*TitleFontName    = "8x13";
char		*TileFontName     = "8x13bold";

XFontStruct	*TitleFontInfo,
		*TileFontInfo;

extern int	OutputLogging;
extern int	*position;
extern int	space_x, space_y;

int	UsePicture = 0;
int	UseDisplay = 0;
int	CreateNewColormap = 0;
char	*PictureFileName;

long	PictureWidth;
long	PictureHeight;
Pixmap	PicturePixmap;

int	TilesPerSecond;
int	MoveSteps;
int	VertStepSize[MAX_STEPS];
int	HoriStepSize[MAX_STEPS];

#define LEFT	0
#define RIGHT	1
#define UP	2
#define	DOWN	3

#define indx(x,y)	(((y)*PuzzleWidth) + (x))
#define isdigit(x)	((x)>= '0' && (x) <= '9')

#define ulx(x,y)	((x)*TileWidth)
#define llx(x,y)	((x)*TileWidth)
#define urx(x,y)	(((x)+1)*TileWidth - 1)
#define lrx(x,y)	(((x)+1)*TileWidth - 1)
#define uly(x,y)	((y)*TileHeight)
#define ury(x,y)	((y)*TileHeight)
#define lly(x,y)	(((y)+1)*TileHeight - 1)
#define lry(x,y)	(((y)+1)*TileHeight - 1)

/*
 * PuzzlePending - XPending entry point fo the other module.
 */

PuzzlePending()
{
    return(XPending(dpy));
}

/*
 * SetupDisplay - eastablish the connection to the X server.
 */

SetupDisplay(server)
char *server;
{
    dpy = XOpenDisplay(server);
    if (dpy == NULL) {
	fprintf(stderr, "%s: unable to open display '%s'\n",
		ProgName, XDisplayName (server));
	exit(1);
    } 
    screen = DefaultScreen(dpy);
#ifdef DEBUG
    XSynchronize(dpy,1);
#endif /* DEBUG */
}

XQueryWindow(window,frame)
Window window;
WindowGeom *frame;
{
    XGetGeometry(dpy, window,
		 &(frame->root),
		 &(frame->x), &(frame->y),
		 &(frame->width), &(frame->height),
		 &(frame->border_width),
		 &(frame->depth));
}

RectSet(W,x,y,w,h,pixel)
Window W;
int x,y;
unsigned int w,h;
unsigned long pixel;
{
    XSetForeground(dpy, rect_gc, pixel);
    XFillRectangle(dpy, W, rect_gc, x, y, w, h);
}

MoveArea(W,src_x,src_y,dst_x,dst_y,w,h)
Window W;
int src_x, src_y, dst_x, dst_y;
unsigned int w, h;
{
    XCopyArea(dpy,W,W,gc,src_x,src_y,w,h,dst_x,dst_y);
}

/** RepaintTitle - puts the program title in the title bar **/

RepaintTitle(method)
int method;
{
    int Twidth,Theight;
    int i,j, startColor,color2,tinyBoxSize;
    int Tx, Ty;

    /*
     * applications painting their own title is out of style,
     * so don't just leave it there;
     */

    tinyBoxSize = 5;
    Twidth  = PuzzleWinInfo.width*3/4;
    Tx      = (PuzzleWinInfo.width-Twidth)/2;
    TextXStart = Tx;

    if (method == TITLE_TEXT) {
	Twidth  = XTextWidth(TitleFontInfo,ProgName,strlen(ProgName));
	Theight = TitleFontInfo->ascent + TitleFontInfo->descent;
	Tx	    = (PuzzleWinInfo.width-Twidth)/2;
	Ty	    = (TitleWinHeight-Theight)/2 + TitleFontInfo->ascent;
    
	XSetFont(dpy, gc, TitleFontInfo->fid);
	XDrawString(dpy, TitleWindow, gc,Tx, Ty, ProgName,strlen(ProgName));
	XFlush(dpy);
    }
    else if (method == TITLE_TILES) {
	for (i=0,startColor=0; i<TitleWinHeight; i+=tinyBoxSize,startColor++)
	    for (j=0,color2=startColor; j<Twidth; j+=tinyBoxSize,color2++)
		RectSet(TitleWindow,j+TextXStart,i,tinyBoxSize,tinyBoxSize,
			color2%2);
    }
    else {
	/** method == TITLE_ANIMATED **/

	unsigned char *colorVal;
	int *xLoc, *yLoc, *permute;
	int tilesHigh, tilesWide, numTiles, counter, swapWith, tmp;

	tilesHigh = (TitleWinHeight+tinyBoxSize-1)/tinyBoxSize;
	tilesWide = (Twidth+tinyBoxSize-1)/tinyBoxSize;
	numTiles = tilesHigh * tilesWide;

	colorVal = (unsigned char *) malloc(numTiles);
	xLoc = (int *) malloc(numTiles * sizeof(int));
	yLoc = (int *) malloc(numTiles * sizeof(int));
	permute = (int *) malloc(numTiles * sizeof(int));

	for (i=0; i<numTiles; i++)
	    permute[i] = i;

	for (i=numTiles-1; i>1; i--) {
	    swapWith = rand()%i;
	    tmp = permute[swapWith];
	    permute[swapWith] = permute[i];
	    permute[i] = tmp;
	}

	counter = 0;
	for (i=0,startColor=0; i<TitleWinHeight; i+=tinyBoxSize,startColor++)
	    for (j=0,color2=startColor; j<Twidth; j+=tinyBoxSize,color2++) {
		colorVal[counter] = color2%2;
		xLoc[counter] = j+TextXStart;
		yLoc[counter] = i;
		counter++;
	    }

	for (i=0; i<numTiles; i++) {
	    j = permute[i];
	    RectSet(TitleWindow,xLoc[j],yLoc[j],tinyBoxSize,tinyBoxSize,
		    colorVal[j]);
	    XFlush(dpy);
	}

	free(colorVal);
	free(xLoc);
	free(yLoc);
	free(permute);
    }
}

/*
 * RepaintBar - Repaint the bar between the title window and
 *              the tile window;
 */
RepaintBar()
{
    XFillRectangle(dpy, PuzzleRoot, gc,
		   0, TitleWinHeight,
		   PuzzleWinInfo.width, BoundaryHeight);
}

/**
 ** RepaintTiles - draw the numbers in the tiles to match the
 **                locations array;
 **/
RepaintTiles()
{
#ifdef USE_PICTURE
   if (UsePicture)
      RepaintPictureTiles();
   else
#endif /* USE_PICTURE */
      RepaintNumberTiles();
}

RepaintNumberTiles()
{
    int i,j,counter;
    int width,height;
    int x_offset,y_offset;
    char str[30];
    
    /** cut the TileWindow into a grid of nxn pieces by inscribing
     ** each rectangle with a black border;
     ** I don't want to use subwindows for each tile so that I can
     ** slide groups of tiles together as a single unit, rather than
     ** being forced to move one tile at a time.
     **/

#define line(x1,y1,x2,y2) XDrawLine(dpy,TileWindow,gc,(x1),(y1),(x2),(y2))

#define rect(x,y)	(line(ulx(x,y),uly(x,y),urx(x,y),ury(x,y)),	\
			 line(urx(x,y),ury(x,y),lrx(x,y),lry(x,y)),	\
			 line(lrx(x,y),lry(x,y),llx(x,y),lly(x,y)),	\
			 line(llx(x,y),lly(x,y),ulx(x,y),uly(x,y)))

    height = TileFontInfo->ascent + TileFontInfo->descent;
    y_offset = (TileHeight - height)/2 + TileFontInfo->ascent;

    XSetFont(dpy, gc, TileFontInfo->fid);

    counter = 0;
    for (i=0; i<PuzzleHeight; i++)
	for (j=0; j<PuzzleWidth; j++) {
	    if (position[counter] == 0) {
		RectSet(TileWindow,ulx(j,i),uly(j,i),
			TileWidth,TileHeight,FgPixel);
	    }
	    else {
		RectSet(TileWindow,ulx(j,i),uly(j,i),TileWidth,TileHeight,
			BgPixel);
		rect(j,i);
		sprintf(str,"%d",position[counter]);
		width = XTextWidth(TileFontInfo,str,strlen(str));
		x_offset = (TileWidth - width)/2;
		XDrawString(dpy, TileWindow, gc,
			    ulx(j,i)+x_offset,uly(j,i)+y_offset,
			    str,strlen(str));
	    }
	    counter++;
	}    
}

#ifdef USE_PICTURE
RepaintPictureTiles()
{
    int i, j, counter;
    int tmp, orig_x,orig_y;

    counter = 0;
    for (i=0; i<PuzzleHeight; i++)
	for (j=0; j<PuzzleWidth; j++) {
	    if (position[counter] == 0)
		RectSet(TileWindow,ulx(j,i),uly(j,i),
			TileWidth,TileHeight,FgPixel);
	    else {
		tmp = position[counter] - 1;
		orig_x = tmp % PuzzleWidth;
		orig_y = tmp / PuzzleWidth;

		XCopyArea(dpy,PicturePixmap,TileWindow,gc,
			  ulx(orig_x,orig_y), uly(orig_x,orig_y),
			  TileWidth, TileHeight,
			  ulx(j,i), uly(j,i));
	    }
	    counter++;
	}    
    
}
#endif /* USE_PICTURE */

/**
 ** Setup - Perform initial window creation, etc.
 **/

Setup (geom,argc,argv)
char *geom;
int argc;
char *argv[];
{
    int minwidth, minheight;
    Pixmap PictureSetup();
    Visual visual;
    XGCValues xgcv;
    XSetWindowAttributes xswa;
    XSizeHints sizehints;

    /*******************************************/
    /** let the puzzle code initialize itself **/
    /*******************************************/
    initialize();
    OutputLogging = 1;

    FgPixel = BlackPixel(dpy,screen);
    BgPixel = WhitePixel(dpy,screen);

    TitleWinHeight = TITLE_WINDOW_HEIGHT;
    BoundaryHeight = BOUNDARY_HEIGHT;

#ifdef USE_PICTURE
    /*****************************************************/
    /** if we want to use a picture file, initialize it **/
    /*****************************************************/
    if (UsePicture) {
	/**
	 ** This was fun to do back with X10 when you could create
	 ** a pixmap from the current display contents;  No more, I guess.
	 **/
#ifdef UNDEFINED
	if (UseDisplay) {
	    WindowGeom RootWinInfo;
	    int x,y;

	    x = PUZZLE_BORDER_WIDTH;
	    y = TITLE_WINDOW_HEIGHT + BOUNDARY_HEIGHT + PUZZLE_BORDER_WIDTH;
	    XQueryWindow(RootWindow(dpy, screen),&RootWinInfo);
	    PictureWidth  = RootWinInfo.width  - x;
	    PictureHeight = RootWinInfo.height - y;
	    PicturePixmap = XPixmapSave(RootWindow(dpy,screen),
					x,y,PictureWidth,PictureHeight);
	}
	else
#endif /* UNDEFINED */
	    PicturePixmap = PictureSetup(PictureFileName,&PictureWidth,
				     &PictureHeight);
    }
#endif /* USE_PICTURE */

#ifdef USE_PICTURE
    if (UsePicture) {
	minwidth = PictureWidth;
	minheight = PictureHeight + TITLE_WINDOW_HEIGHT + BOUNDARY_HEIGHT;
    }
    else {
#endif /* USE_PICTURE */
	minwidth = MIN_TILE_WIDTH * PuzzleWidth;
	minheight = MIN_TILE_HEIGHT * PuzzleHeight + TITLE_WINDOW_HEIGHT +
	    BOUNDARY_HEIGHT;
#ifdef USE_PICTURE 
    }
#endif /* USE_PICTURE */

    /*************************************/
    /** configure the window size hints **/
    /*************************************/

    {
	int x, y, width, height;
	int tileHeight, tileWidth;
	int flags;

	sizehints.flags = PMinSize | PPosition | PSize | PResizeInc;
	sizehints.min_width = minwidth;
	sizehints.min_height = minheight;
	sizehints.width = minwidth;
	sizehints.height = minheight;
	sizehints.x = 100;
	sizehints.y = 300;
	sizehints.width_inc = PuzzleWidth;
	sizehints.height_inc = PuzzleHeight;

#ifdef USE_PICTURE	
	if (UsePicture) {
	    sizehints.flags |= PMaxSize;
	    sizehints.max_width = sizehints.min_width;
	    sizehints.max_height = sizehints.min_height;
	}
#endif /* USE_PICTURE */

	if(strlen(geom)) {
	    flags = XParseGeometry(geom, &x, &y,
				   (unsigned int *)&width,
				   (unsigned int *)&height);
	    if(WidthValue & flags) {
		sizehints.flags |= USSize;
		if (width > sizehints.min_width)
		    sizehints.width = width;
	    }
	    if(HeightValue & flags) {
		sizehints.flags |= USSize;
		if (height > sizehints.min_height)
		    sizehints.height = height;
	    }
	    if(XValue & flags) {
		if(XNegative & flags)
		    x = DisplayWidth(dpy, DefaultScreen(dpy)) + x 
			- sizehints.width;
		sizehints.flags |= USPosition;
		sizehints.x = x;
	    }
	    if(YValue & flags) {
		if(YNegative & flags)
		    y = DisplayHeight(dpy, DefaultScreen(dpy)) + y
			-sizehints.height;
		sizehints.flags |= USPosition;
		sizehints.y = y;
	    }

	    tileHeight = (sizehints.height-TitleWinHeight-BoundaryHeight)/PuzzleHeight;
	    sizehints.height = tileHeight*PuzzleHeight+TitleWinHeight+BoundaryHeight;

	    tileWidth = sizehints.width/PuzzleWidth;
	    sizehints.width = tileWidth * PuzzleWidth;
	}
    }

    /*******************************************************************/
    /** create the puzzle main window and set its standard properties **/
    /*******************************************************************/

    xswa.event_mask = ExposureMask;
    visual.visualid = CopyFromParent;

    PuzzleRoot = XCreateSimpleWindow(dpy, RootWindow(dpy,screen),
			       sizehints.x, sizehints.y,
			       sizehints.width, sizehints.height,
			       PUZZLE_BORDER_WIDTH, FgPixel,FgPixel);

    XSetStandardProperties(dpy, PuzzleRoot,"puzzle","Puzzle",
			   None, argv, argc, &sizehints);

   if (CreateNewColormap)
       XSetWindowColormap(dpy, PuzzleRoot, PuzzleColormap);

    xgcv.foreground = FgPixel;
    xgcv.background = BgPixel;
    xgcv.line_width = 1;
    gc = XCreateGC(dpy, PuzzleRoot,
		   GCForeground|GCBackground|GCLineWidth,
		   &xgcv);

    /*********************************/
    /** load the arrow-cross cursor **/
    /*********************************/
    
    {
	Pixmap ACPixmap, ACMask;
	Cursor ACCursor;
	XColor FGcolor, BGcolor;

	FGcolor.red = 0;	FGcolor.green = 0;	FGcolor.blue = 0;
	BGcolor.red = 0xffff;	BGcolor.green = 0xffff;	BGcolor.blue = 0xffff;

	ACPixmap = XCreateBitmapFromData(dpy,RootWindow(dpy,screen),
					 (char *) ac_bits,
					 ac_width, ac_height);
	ACMask = XCreateBitmapFromData(dpy,RootWindow(dpy,screen),
				       (char *) ac_mask_bits,
				       ac_mask_width, ac_mask_height);
	ACCursor = XCreatePixmapCursor(dpy,ACPixmap,ACMask,
				       &FGcolor,&BGcolor,
				       ac_x_hot, ac_y_hot);
	if (ACCursor == NULL)
	    error("Unable to store ArrowCrossCursor.");
    
	XDefineCursor(dpy,PuzzleRoot,ACCursor);
    }

    /*****************************************/
    /** allocate the fonts we will be using **/
    /*****************************************/

    TitleFontInfo    = XLoadQueryFont(dpy,TitleFontName);
    TileFontInfo     = XLoadQueryFont(dpy,TileFontName);

    if (TitleFontInfo    == NULL) error("Opening title font.\n");
    if (TileFontInfo     == NULL) error("Opening tile font.\n");

    XSelectInput(dpy, PuzzleRoot, ExposureMask|VisibilityChangeMask);
    XMapWindow(dpy,PuzzleRoot);
}

static short old_height = -1;
static short old_width = -1;

SizeChanged()
{
    XQueryWindow(PuzzleRoot,&PuzzleWinInfo);
    
    if (PuzzleWinInfo.width == old_width &&
	PuzzleWinInfo.height == old_height)
	return(0);
    else
	return(1);
}

Reset()
{
    int Box_x,Box_y;
    int TileBgPixel;
    
    /** TileWindow is that portion of PuzzleRoot that contains
     ** the sliding pieces;
     **/

    if (UsePicture)
	TileBgPixel = BlackPixel(dpy,screen);
    else
	TileBgPixel = WhitePixel(dpy,screen);
    
#ifdef SERVER_BUG
    /* seems I need to do this, or the next title window will be obscured
     * by the old title window! This must be a server bug, right?
     */
    if (TitleWindow) XUnmapWindow(dpy,TitleWindow);
/*    if (TitleWindow) XDestroyWindow(dpy,TitleWindow); */
#endif /* SERVER_BUG */
    XDestroySubwindows(dpy,PuzzleRoot);

    /** fix the dimensions of PuzzleRoot so the height and width
     ** of the TileWindow will work out to be multiples of PuzzleSize;
     **/

    /** If we're dealing with a picture, the tile region can be no larger
     ** than the picture!
     **/

#ifdef USE_PICTURE
    if (UsePicture) {
	int tmp;

	tmp = PuzzleWinInfo.height - TitleWinHeight - BoundaryHeight;
	if (tmp > PictureHeight)
	    PuzzleWinInfo.height = PictureHeight+TitleWinHeight+BoundaryHeight;
	if (PuzzleWinInfo.width > PictureWidth)
	    PuzzleWinInfo.width = PictureWidth;
    }
#endif /* USE_PICTURE */

    TileHeight=(PuzzleWinInfo.height-TitleWinHeight-BoundaryHeight)/PuzzleHeight;
    /* PuzzleWinInfo.height = TileHeight*PuzzleHeight+TitleWinHeight+BoundaryHeight; */

    TileWidth = PuzzleWinInfo.width/PuzzleWidth;
    /* PuzzleWinInfo.width = TileWidth * PuzzleWidth; */

    /** fixup the size of PuzzleRoot **/

    /* XResizeWindow(dpy,PuzzleRoot,PuzzleWinInfo.width,PuzzleWinInfo.height); */
    old_width  = PuzzleWinInfo.width;
    old_height = PuzzleWinInfo.height;

    TileWinHeight = PuzzleWinInfo.height - TitleWinHeight;

    TitleWindow = XCreateSimpleWindow(dpy, PuzzleRoot,
			0,0,
			PuzzleWinInfo.width, TitleWinHeight,
			0,0,BgPixel);

    TileWindow  = XCreateSimpleWindow(dpy, PuzzleRoot,
			0,TitleWinHeight+BoundaryHeight,
			PuzzleWinInfo.width, TileWinHeight,
			0,0,TileBgPixel);
   
    rect_gc = XCreateGC(dpy,TileWindow,0,0);
    XCopyGC(dpy, gc, -1, rect_gc);

    XMapWindow(dpy,TitleWindow);
    XMapWindow(dpy,TileWindow);
    XSync(dpy,0);

    RepaintBar();
    RepaintTitle(TITLE_TEXT);

    /** locate the two check boxes **/

    Box_x = TextXStart/2 - BoxWidth/2;
    Box_y = TitleWinHeight/2 - BoxHeight/2;
    
    ScrambleWindow = XCreateSimpleWindow(dpy, TitleWindow,
				 Box_x, Box_y,
				 BoxWidth, BoxHeight,
				 1,FgPixel,BgPixel);

    Box_x = PuzzleWinInfo.width - Box_x - BoxWidth;
    
    SolveWindow = XCreateSimpleWindow(dpy, TitleWindow,
				 Box_x,Box_y,
				 BoxWidth,BoxHeight,
				 1,FgPixel,BgPixel);

    XMapWindow(dpy,ScrambleWindow);
    XMapWindow(dpy,SolveWindow);
    XSync(dpy,0);

    XSelectInput(dpy, TitleWindow,   ButtonPressMask|ExposureMask);
    XSelectInput(dpy, TileWindow,    ButtonPressMask|ExposureMask|
		 			VisibilityChangeMask);
    XSelectInput(dpy, ScrambleWindow,ButtonPressMask|ExposureMask);
    XSelectInput(dpy, SolveWindow,   ButtonPressMask|ExposureMask);

    RepaintTiles();
    RepaintTitle(TITLE_ANIMATED);
    CalculateSpeed();
    CalculateStepsize();
    XSync(dpy,0);
}

/*
 * Sets the global variable MoveSteps based on speed
 * specified on the command line;
 */

#if defined(USG) && !defined(Cray)	/* tv_usec never changes */
#define MIN_DELTA_T 100L
#else					/* don't divide by zero */
#define MIN_DELTA_T 1L
#endif

/** delta-t in miliseconds **/
#define DeltaT(tv2,tv1)				\
    ( ((tv2.tv_sec  - tv1.tv_sec )*1000L)	\
     +((tv2.tv_usec - tv1.tv_usec)/1000L))

CalculateSpeed()
{
    struct timeval tv1, tv2;
    struct timezone tz;
    int i, x, y;
    long timePerTile;
    static int firstCall = 1;
    long delta;

    if (!firstCall)
	return;
    firstCall = 0;

    x = space_x * TileWidth;
    y = space_y * TileHeight;
    timePerTile = (long)(1000/TilesPerSecond);

    XSync(dpy,0);
    gettimeofday(&tv1, &tz);
    tv2 = tv1;

    MoveSteps = 0;
    delta = 0L;
    while (delta < timePerTile) {
	MoveArea(TileWindow,x,y,x+1,y,TileWidth,TileHeight);
	RectSet(TileWindow,x,y,1,TileHeight,FgPixel);
	XSync(dpy,0);
	gettimeofday(&tv2, &tz);
	delta = DeltaT(tv2,tv1);
	delta = max(MIN_DELTA_T, delta);
	if (delta >= 0) MoveSteps++;	/* crock for broken systems */
    }

    /*
     * now, see how long this takes without all the extra b.s.
     * and compensate;       
     */

    XSync(dpy,0);
    gettimeofday(&tv1, &tz);
    for (i=0; i<MoveSteps; i++) {
	MoveArea(TileWindow,x,y,x+1,y,TileWidth,TileHeight);
	RectSet(TileWindow,x,y,1,TileHeight,FgPixel);
    }
    XFlush(dpy);
    gettimeofday(&tv2, &tz);
    delta = DeltaT(tv2, tv1);
    delta = max(MIN_DELTA_T, delta);
    MoveSteps = (((long)MoveSteps) * timePerTile)/(delta ? delta : 1L);
    if (MoveSteps <= 0)
	MoveSteps = 1;
}

CalculateStepsize()
{
    int i, rem;
    int error,sum;

    for (i=0; i<MoveSteps; i++)
	VertStepSize[i] = TileHeight/MoveSteps;
         
    rem = TileHeight % MoveSteps;
    error = - MoveSteps/2;

    if (rem > 0)
	for (i=0; i<MoveSteps; i++) {
	    if (error >= 0) {
		VertStepSize[i]++;
		error -= MoveSteps;
	    }
	    error += rem;
	}   
    
    for (i=0; i<MoveSteps; i++)
	HoriStepSize[i] = TileWidth/MoveSteps;
         
    rem = TileWidth % MoveSteps;
    error = - MoveSteps/2;

    if (rem > 0)
	for (i=0; i<MoveSteps; i++) {
	    if (error >= 0) {
		HoriStepSize[i]++;
		error -= MoveSteps;
	    }
	    error += rem;
	}   

    /** This code is a little screwed up and I don't want to fix it
     ** right now, so just do a little hack to make sure the total
     ** distance comes out right;
     **/

    sum = 0;
    for (i=0; i<MoveSteps; i++)
	sum += HoriStepSize[i];
    HoriStepSize[0] += TileWidth - sum;

    sum = 0;
    for (i=0; i<MoveSteps; i++)
	sum += VertStepSize[i];
    VertStepSize[0] += TileHeight - sum;
}

SlidePieces(event)
XButtonEvent *event;
{
    int x,y;

    x = (*event).x / TileWidth;
    y = (*event).y / TileHeight;
    if (x == space_x || y == space_y)
	move_space_to(indx(x,y));   
    flushLogging();
}

ProcessVisibility(event)
XVisibilityEvent *event;
{
    if (event->state != VisibilityUnobscured) AbortSolving();
}

ProcessExpose(event)
XExposeEvent *event;
{
    int loop  = 1;
    int reset = 0,
        title = 0,
    	tiles = 0,
    	bar   = 0;

    loop = 1;
    while (loop) {
	if (event->count == 0) {
	    if (event->window == TitleWindow)
		title++;
	    else if (event->window == TileWindow)
		tiles++;
	    else if (event->window == PuzzleRoot)
		bar++;
	}
	loop = XCheckMaskEvent(dpy, ExposureMask, (XEvent *)event);
    }

    if (SizeChanged())
	reset++;

    if (reset)
	Reset();
    else {
	if (title) RepaintTitle(TITLE_TILES);
	if (tiles) RepaintTiles();
	if (bar)   RepaintBar();
    }
}

ProcessButton(event)
XButtonEvent *event;
{
    Window w;

    w = event->window;
    if (w == TileWindow) {
	if (SolvingStatus())
	    AbortSolving();
	else
	    SlidePieces(event);
    }
    else if (w == ScrambleWindow) {
	AbortSolving();
	Scramble();
	RepaintTiles();
    }
    else if (w == SolveWindow)
	Solve();
    else if ((w == TitleWindow) && (*event).button == Button2)
	exit(0);
}

ProcessInput()
{
    XEvent event;

    while(1) {
	GetNextEvent(&event);  
	ProcessEvent(&event);
    }
}

ProcessEvents()
{
    XEvent event;

    while(XPending(dpy)) {
	GetNextEvent(&event);  
	ProcessEvent(&event);
    }
}

GetNextEvent(event)
XEvent *event;
{
    if (!XCheckMaskEvent(dpy,VisibilityChangeMask,event) &&
	!XCheckMaskEvent(dpy,ExposureMask,event))
	XNextEvent(dpy,event);  
}

ProcessEvent(event)
XEvent *event;
{
    switch(event->type) {
      case ButtonPress:
	ProcessButton(&event->xbutton);
	break;
      case Expose:
	ProcessExpose(&event->xexpose);
	break;
      case VisibilityNotify:
	ProcessVisibility(&event->xvisibility);
	break;
      default:
	break;
   }
}

main(argc,argv)
int argc;
char *argv[];
{
   int i;
   char *ServerName, *Geometry;
   char *puzzle_size = NULL;
   char *option;

   ProgName = argv[0];

   ServerName = "";
   Geometry   = "";  
   TilesPerSecond = -1;

   /********************************/
   /** parse command line options **/
   /********************************/

   for (i=1; i<argc; i++) {
      char *arg = argv[i];

      if (arg[0] == '-') {
	switch (arg[1]) {
	    case 'd':				/* -display host:dpy */
		if (++i >= argc) usage ();
		ServerName = argv[i];
		continue;
	    case 'g':				/* -geometry geom */
		if (++i >= argc) usage ();
		Geometry = argv[i];
		continue;
	    case 's':				/* -size WxH or -speed n */
		if (arg[2] == 'i') {
		    if (++i >= argc) usage ();
		    puzzle_size = argv[i];
		    continue;
		} else if (arg[2] == 'p') {
		    if (++i >= argc) usage ();
		    TilesPerSecond = atoi (argv[i]);
		    continue;
		} else 
		    usage ();
		break;
	    case 'p':				/* -picture filename */
		if (++i >= argc) usage ();
		UsePicture++;
		PictureFileName = argv[i];
		continue;
	    case 'c':				/* -colormap */
		CreateNewColormap++;
		continue;
	    default:
		usage ();
	}					/* end switch */
      }	else
	usage ();
   }						/* end for */

   SetupDisplay (ServerName);

   if (!Geometry) {
	Geometry = XGetDefault (dpy, ProgName, "Geometry");
   }

   if (!puzzle_size) {
	option = XGetDefault (dpy, ProgName, "Size");
	puzzle_size = option ? option : defaultPuzzleSize;
   }

   if (TilesPerSecond <= 0) {
	option = XGetDefault (dpy, ProgName, "Speed");
	TilesPerSecond = option ? atoi (option) : DEFAULT_SPEED;
   }

   if (!UsePicture) {
	option = XGetDefault (dpy, ProgName, "Picture");
	if (option) {
	    UsePicture++;
	    PictureFileName = option;
	}
   }

   if (!CreateNewColormap) {
	option = XGetDefault (dpy, ProgName, "Colormap");
	if (option) {
	    CreateNewColormap++;
	}
   }

   sscanf (puzzle_size, "%dx%d", &PuzzleWidth, &PuzzleHeight);
   if (PuzzleWidth < 4 || PuzzleHeight < 4) {
	fprintf (stderr, "%s:  Puzzle size must be at least 4x4\n",
		 ProgName);
	exit (1);
   }
   PuzzleSize = min((PuzzleWidth/2)*2,(PuzzleHeight/2)*2);

   Setup (Geometry,argc,argv);
   ProcessInput();
   exit (0);
}

static char *help_message[] = {
"where options include:",
"    -display host:dpy                X server to use",
"    -geometry geom                   geometry of puzzle window",
"    -size WxH                        number of squares in puzzle",
"    -speed number                    tiles to move per second",
"    -picture filename                image to use for tiles",
"    -colormap                        create a new colormap",
NULL};


usage()
{
    char **cpp;

    fprintf (stderr, "usage:  %s [-options ...]\n\n", ProgName);
    for (cpp = help_message; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    fprintf (stderr, "\n");
    exit (1);
}

error(str)
char *str;
{
   fprintf(stderr,"Error %s\n",str);
   exit(1);
}

/**
 ** Output Routines -
 **/

resetLogging()
{ }
flushLogging()
{ }
saveLoggingState()
{ }
LogMoveSpace(first_x,first_y,last_x,last_y,dir)
int first_x,first_y,last_x,last_y,dir;
{
    displayLogMoveSpace(first_x,first_y,last_x,last_y,dir);
}


#ifdef UNDEFINED
/** this stuff really isn't worth it; **/

static int prevDir = -1;
static int prevFirstX, prevFirstY, prevLastX, prevLastY; 

resetLogging()
{
    prevDir = -1;
}

flushLogging()
{
    if (prevDir != -1)
	displayLogMoveSpace(prevFirstX,prevFirstY,prevLastX,prevLastY,prevDir);
    prevDir = -1;
}

saveLoggingState(fx,fy,lx,ly,dir)
int fx,fy,lx,ly,dir;
{
    prevDir = dir;
    prevFirstX = fx;
    prevFirstY = fy;
    prevLastX = lx;
    prevLastY = ly;
}

LogMoveSpace(first_x,first_y,last_x,last_y,dir)
int first_x,first_y,last_x,last_y,dir;
{
    if (prevDir == -1)
	/** we don't already have something to move **/
	saveLoggingState(first_x,first_y,last_x,last_y,dir);
    else if (prevDir == dir) {
	/** we're going in the same direction **/
	prevLastX = last_x;
	prevLastY = last_y;
    }
    else {
	flushLogging();
	saveLoggingState(first_x,first_y,last_x,last_y,dir);
    }
}
#endif /* UNDEFINED */

displayLogMoveSpace(first_x,first_y,last_x,last_y,dir)
int first_x,first_y,last_x,last_y,dir;
{
   int min_x,min_y,max_x,max_y;
   int x,y,w,h,dx,dy,x2,y2;
   int i, clear_x, clear_y;


   max_x = max(first_x,last_x);
   min_x = min(first_x,last_x);
   max_y = max(first_y,last_y);
   min_y = min(first_y,last_y);

   x = ulx(min_x,0);
   y = uly(0,min_y);   
   w = (max_x - min_x + 1)*TileWidth;
   h = (max_y - min_y + 1)*TileHeight;

   dx = x;
   dy = y;

   x2 = x;
   y2 = y;

   switch(dir) {
   case UP:	clear_x = llx(max_x,0);
                clear_y = lly(0,max_y) + 1;

		for (i=0; i<MoveSteps; i++) {
                   dy = VertStepSize[i];
                   y2 = y - dy;
                   clear_y -= dy;
                   
                   MoveArea(TileWindow,x,y,x2,y2,w,h);
                   RectSet(TileWindow,clear_x,clear_y,
		           TileWidth,dy,FgPixel);
                   y -= dy;
                }
		break;
   case DOWN:	clear_x = llx(max_x,0);
		clear_y = uly(0,min_y);

		for (i=0; i<MoveSteps; i++) {
		   dy = VertStepSize[i];
		   y2 = y + dy;

		   MoveArea(TileWindow,x,y,x2,y2,w,h);
                   RectSet(TileWindow,clear_x,clear_y,
		           TileWidth,dy,FgPixel);
		   y += dy;
		   clear_y += dy;
		}
		break;
   case LEFT:	clear_x = urx(max_x,0) + 1;
		clear_y = ury(0,max_y);

		for (i=0; i<MoveSteps; i++) {
		   dx = HoriStepSize[i];
		   x2 = x - dx;
		   clear_x -= dx;

		   MoveArea(TileWindow,x,y,x2,y2,w,h);
                   RectSet(TileWindow,clear_x,clear_y,
		           dx,TileHeight,FgPixel);
		   x -= dx;
		}
		break;
   case RIGHT:	clear_x = ulx(min_x,0);
		clear_y = uly(0,max_y);
		
                for (i=0; i<MoveSteps; i++) {
		   dx = HoriStepSize[i];
		   x2 = x + dx;

		   MoveArea(TileWindow,x,y,x2,y2,w,h);
                   RectSet(TileWindow,clear_x,clear_y,
		           dx,TileHeight,FgPixel);
		   x += dx;
		   clear_x += dx;
		}
		break;
   }

   XFlush(dpy);
}
