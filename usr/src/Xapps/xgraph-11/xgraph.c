/*
 * xgraph - A Simple Plotter for X
 *
 * David Harrison
 * University of California,  Berkeley
 * 1986, 1987, 1988, 1989
 *
 * Please see copyright.h concerning the formal reproduction rights
 * of this software.
 */

#include "copyright.h"
#include <stdio.h>
#include <math.h>
#include <pwd.h>
#include <ctype.h>
#include "xgout.h"
#include "xgraph.h"
#include "xtb.h"
#include "hard_devices.h"
#include "params.h"

#define ZOOM
#define TOOLBOX
#define NEW_READER

/* Portability */
#ifdef  CRAY
#undef  MAXFLOAT
#define MAXFLOAT 10.e300
#endif  /* CRAY */

#ifndef MAXFLOAT
#define MAXFLOAT	HUGE
#endif

#define BIGINT		0xfffffff

#define GRIDPOWER 	10
#define INITSIZE 	128

#define CONTROL_D	'\004'
#define CONTROL_C	'\003'
#define TILDE		'~'

#define BTNPAD		1
#define BTNINTER	3

#define MAX(a,b)	((a) > (b) ? (a) : (b))
#define MIN(a,b)	((a) < (b) ? (a) : (b))
#define ABS(x)		((x) < 0 ? -(x) : (x))
#define ZERO_THRES	1.0E-07

/* To get around an inaccurate log */
#define nlog10(x)	(x == 0.0 ? 0.0 : log10(x) + 1e-15)

#define ISCOLOR		(wi->dev_info.dev_flags & D_COLOR)

#define PIXVALUE(set) 	((set) % MAXATTR)

#define LINESTYLE(set) \
(ISCOLOR ?  ((set)/MAXATTR) : ((set) % MAXATTR))

#define MARKSTYLE(set) \
(colorMark ? COLMARK(set) : BWMARK(set))

#define COLMARK(set) \
((set) / MAXATTR)

#define BWMARK(set) \
((set) % MAXATTR)

#define LOG_X	0x01
#define LOG_Y	0x02


/*
 * Default settings for xgraph parameters
 */

#define DEF_BORDER_WIDTH	"2"
#define DEF_BORDER_COLOR	"Black"
#define DEF_TITLE_TEXT		"X Graph"
#define DEF_XUNIT_TEXT		"X"
#define DEF_YUNIT_TEXT		"Y"
#define DEF_TICK_FLAG		"off"
#define DEF_MARK_FLAG		"off"
#define DEF_PIXMARK_FLAG	"off"
#define DEF_LARGEPIX_FLAG	"off"
#define DEF_DIFFMARK_FLAG	"off"
#define DEF_BB_FLAG		"off"
#define DEF_NOLINE_FLAG		"off"
#define DEF_LOGX_FLAG		"off"
#define DEF_LOGY_FLAG		"off"
#define DEF_BAR_FLAG		"off"
#define DEF_BAR_BASE		"0.0"
#define DEF_BAR_WIDTH		"-1.0"
#define DEF_LINE_WIDTH		"0"
#define DEF_GRID_SIZE		"0"
#define DEF_GRID_STYLE		"10"
#define DEF_LABEL_FONT		"helvetica-12"
#define DEF_TITLE_FONT		"helvetica-18"
#define DEF_GEOMETRY		""
#define DEF_REVERSE		"off"
#define DEF_DEVICE		""
#define DEF_DISPOSITION		"To Device"
#define DEF_FILEORDEV		""

#define DEF_MARKER_FLAG		"off"
#define DEF_DIFFMARK_FLAG	"off"
#define DEF_PIXMARK_FLAG	"off"
#define DEF_LARGEPIX_FLAG	"off"

/* Low > High means set it based on the data */
#define DEF_LOW_LIMIT		"1.0"
#define DEF_HIGH_LIMIT		"0.0"

/* Black and white defaults */
#define DEF_BW_BACKGROUND	"white"
#define DEF_BW_BORDER		"black"
#define DEF_BW_ZEROCOLOR	"black"
#define DEF_BW_ZEROWIDTH	"3"
#define DEF_BW_ZEROSTYLE	"1"
#define DEF_BW_FOREGROUND	"black"

/* Color defaults */
#define DEF_COL_BACKGROUND	"#ccc"
#define DEF_COL_BORDER		"black"
#define DEF_COL_ZEROCOLOR	"white"
#define DEF_COL_ZEROWIDTH	"0"
#define DEF_COL_ZEROSTYLE	"1"
#define DEF_COL_FOREGROUND	"black"
#define DEF_COL_FIRSTSTYLE	"1"

/* Default line styles */
static char *defStyle[MAXATTR] = {
    "1", "10", "11110000", "010111", "1110",
    "1111111100000000", "11001111", "0011000111"
};

/* Default color names */
static char *defColors[MAXATTR] = {
    "red", "SpringGreen", "blue", "yellow",
    "cyan", "sienna", "orange", "coral"
};


extern void init_X();
extern void do_error();

static char *tildeExpand();
static void ReverseIt();
static void set_mark_flags();
static void Traverse();

typedef struct point_list {
    int numPoints;		/* Number of points in group */
    int allocSize;		/* Allocated size            */
    double *xvec;		/* X values                  */
    double *yvec;		/* Y values                  */
    struct point_list *next;	/* Next set of points        */
} PointList;

typedef struct new_data_set {
    char *setName;		/* Name of data set     */
    PointList *list;		/* List of point arrays */
} NewDataSet;

static NewDataSet PlotData[MAXSETS];

static XSegment *Xsegs;		/* Point space for X */

/* Basic transformation stuff */
static double llx, lly, urx, ury; /* Bounding box of all data */

#define HARDCOPY_IN_PROGRESS	0x01

typedef struct local_win {
    double loX, loY, hiX, hiY;	/* Local bounding box of window         */
    int XOrgX, XOrgY;		/* Origin of bounding box on screen     */
    int XOppX, XOppY;		/* Other point defining bounding box    */
    double UsrOrgX, UsrOrgY;	/* Origin of bounding box in user space */
    double UsrOppX, UsrOppY;	/* Other point of bounding box          */
    double XUnitsPerPixel;	/* X Axis scale factor                  */
    double YUnitsPerPixel;	/* Y Axis scale factor                  */
    xgOut dev_info;		/* Device information                   */
    Window close, hardcopy;	/* Buttons for closing and hardcopy     */
    Window about;		/* Version information                  */
    int flags;			/* Window flags                         */
} LocalWin;

#define SCREENX(ws, userX) \
	(((int) (((userX) - ws->UsrOrgX)/ws->XUnitsPerPixel + 0.5)) + ws->XOrgX)
#define SCREENY(ws, userY) \
	(ws->XOppY - ((int) (((userY) - ws->UsrOrgY)/ws->YUnitsPerPixel + 0.5)))

static XContext win_context = (XContext) 0;

/* Other globally set defaults */

Display *disp;			/* Open display            */
Visual *vis;			/* Standard visual         */
Colormap cmap;			/* Standard colormap       */
int screen;			/* Screen number           */
int depth;			/* Depth of screen         */

static int numFiles = 0;		/* Number of input files   */
static char *inFileNames[MAXSETS]; 	/* File names              */

/* Total number of active windows */
static int Num_Windows = 0;
static char *Prog_Name;



static int XErrHandler();	/* Handles error messages */

main(argc, argv)
int argc;
char *argv[];
/*
 * This sets up the hard-wired defaults and reads the X defaults.
 * The command line format is: xgraph [host:display].
 */
{
    Window primary, NewWindow();
    XEvent theEvent;
    LocalWin *win_info;
    Cursor zoomCursor;
    FILE *strm;
    XColor fg_color, bg_color;
    char keys[MAXKEYS], *disp_name;
    int nbytes, idx, maxitems, flags;
    int errs = 0;

    /* Open up new display */
    Prog_Name = argv[0];
    disp_name = "";
    for (idx = 1;  idx < argc-1;  idx++) {
	if (strcmp(argv[idx], "-display") == 0) {
	    disp_name = argv[idx+1];
	    break;
	}
    }
    disp = XOpenDisplay(disp_name);
    if (!disp) {
	(void) fprintf(stderr, "%s: cannot open display `%s'\n", argv[0], disp_name);
	abort();
    }
    XSetErrorHandler(XErrHandler);

    /* Set up hard-wired defaults and allocate spaces */
    InitSets();

    /* Read X defaults and override hard-coded defaults */
    ReadDefaults();

    /* Parse the argument list looking for input files */
    ParseArgs(argc, argv, 0);

    /* Read the data into the data sets */
    llx = lly = MAXFLOAT;
    urx = ury = -MAXFLOAT;
    for (idx = 0;  idx < numFiles;  idx++) {
	strm = fopen(inFileNames[idx], "r");
	if (!strm) {
	    (void) fprintf(stderr, "Warning:  cannot open file `%s'\n",
			   inFileNames[idx]);
	} else {
	    if ((maxitems = ReadData(strm, inFileNames[idx])) < 0) {
		errs++;
	    }
	    (void) fclose(strm);
	}
    }
    if (!numFiles) {
	if ((maxitems = ReadData(stdin, (char *) 0)) < 0) {
	    errs++;
	}
    }
    if (errs) {
	(void) fprintf(stderr, "Problems found with input data.\n");
	exit(1);
    }

    /* Parse the argument list to set options */
    ParseArgs(argc, argv, 1);

    Xsegs = (XSegment *) malloc((unsigned) (maxitems * sizeof(XSegment)));

    /* Reverse Video Hack */
    if (PM_BOOL("ReverseVideo")) ReverseIt();
    hard_init();
    if (PM_BOOL("Debug")) {
	(void) XSynchronize(disp, 1);
	param_dump();
    }

    /* Logarithmic and bounding box computation */
    flags = 0;
    if (PM_BOOL("LogX")) flags |= LOG_X;
    if (PM_BOOL("LogY")) flags |= LOG_Y;
    Traverse(flags);

    /* Nasty hack here for bar graphs */
    if (PM_BOOL("BarGraph")) {
	double base;

	llx -= PM_DBL("BarWidth");
	urx += PM_DBL("BarWidth");
	base = PM_DBL("BarBase");
	if (base < lly) lly = base;
	if (base > ury) ury = base;
    }

    /* Create initial window */
    xtb_init(disp, screen, PM_PIXEL("Foreground"), PM_PIXEL("Background"),
	     PM_FONT("LabelFont"));
    primary = NewWindow(Prog_Name,
			PM_DBL("XLowLimit"), PM_DBL("YLowLimit"),
			PM_DBL("XHighLimit"), PM_DBL("YHighLimit"),
			1.0);
    if (!primary) {
	(void) fprintf(stderr, "Main window would not open\n");
	exit(1);
    }

    zoomCursor = XCreateFontCursor(disp, XC_sizing);
    fg_color = PM_COLOR("Foreground");
    bg_color = PM_COLOR("Background");
    XRecolorCursor(disp, zoomCursor, &fg_color, &bg_color);

    Num_Windows = 1;
    while (Num_Windows > 0) {
	XNextEvent(disp, &theEvent);
	if (xtb_dispatch(&theEvent) != XTB_NOTDEF) continue;
	if (XFindContext(theEvent.xany.display,
			 theEvent.xany.window,
			 win_context, (caddr_t *) &win_info)) {
	    /* Nothing found */
	    continue;
	}
	switch (theEvent.type) {
	case Expose:
	    if (theEvent.xexpose.count <= 0) {
		XWindowAttributes win_attr;

		XGetWindowAttributes(disp, theEvent.xany.window, &win_attr);
		win_info->dev_info.area_w = win_attr.width;
		win_info->dev_info.area_h = win_attr.height;
		init_X(win_info->dev_info.user_state);
		DrawWindow(win_info);
	    }
	    break;
	case KeyPress:
	    nbytes = XLookupString(&theEvent.xkey, keys, MAXKEYS,
				   (KeySym *) 0, (XComposeStatus *) 0);
	    for (idx = 0;  idx < nbytes;  idx++) {
		if (keys[idx] == CONTROL_D) {
		    /* Delete this window */
		    DelWindow(theEvent.xkey.window, win_info);
		} else if (keys[idx] == CONTROL_C) {
		    /* Exit program */
		    Num_Windows = 0;
		} else if (keys[idx] == 'h') {
		    PrintWindow(theEvent.xany.window, win_info);
		}
	    }
	    break;
	case ButtonPress:
	    /* Handle creating a new window */
	    Num_Windows += HandleZoom(Prog_Name,
				      &theEvent.xbutton,
				      win_info, zoomCursor);
	    break;
	default:
	    (void) fprintf(stderr, "Unknown event type: %x\n", theEvent.type);
	    break;
	}
    }
    return 0;
}


#define BLACK_THRES	30000

static void ReversePix(param_name)
char *param_name;		/* Name of color parameter */
/*
 * Looks up `param_name' in the parameters database.  If found, the
 * color is examined and judged to be either black or white based
 * upon its red, green, and blue intensities.  The sense of the
 * color is then reversed and reset to its opposite.
 */
{
    params val;

    if (param_get(param_name, &val)) {
	if ((val.pixv.value.red < BLACK_THRES) &&
	    (val.pixv.value.green < BLACK_THRES) &&
	    (val.pixv.value.blue < BLACK_THRES)) {
	    /* Color is black */
	    param_reset(param_name, "white");
	} else {
	    /* Color is white */
	    param_reset(param_name, "black");
	}
    } else {
	(void) fprintf(stderr, "Cannot reverse color `%s'\n", param_name);
    }
}

static void ReverseIt()
/*
 * This routine attempts to implement reverse video.  It steps through
 * all of the important colors in the parameters database and makes
 * black white (and vice versa).
 */
{
    int i;
    char buf[1024];
    
    for (i = 0;  i < MAXATTR;  i++) {
	(void) sprintf(buf, "%d.Color", i);
	ReversePix(buf);
    }
    ReversePix("Foreground");
    ReversePix("Border");
    ReversePix("ZeroColor");
    ReversePix("Background");
}


static void Traverse(flags)
int flags;			/* Options */
/*
 * Traverses through all of the data applying certain options to the
 * data and computing the overall bounding box.  The flags are:
 *   LOG_X	Take the log of the X axis
 *   LOG_Y	Take the log of the Y axis
 */
{
    int i, j;
    PointList *spot;

    for (i = 0;  i < MAXSETS;  i++) {
	for (spot = PlotData[i].list;  spot;  spot = spot->next) {
	    for (j = 0;  j < spot->numPoints;  j++) {
		if (flags & LOG_Y) {
		    if (spot->yvec[j] > 0.0) {
			spot->yvec[j] = log10(spot->yvec[j]);
		    } else {
			(void) fprintf(stderr, "Cannot plot non-positive Y values\n\
when the logarithmic option is selected.\n");
			exit(1);
		    }
		}
		if (flags & LOG_X) {
		    if (spot->xvec[j] > 0.0) {
			spot->xvec[j] = log10(spot->xvec[j]);
		    } else {
			(void) fprintf(stderr, "Cannot plot non-positive X values\n\
when the logarithmic option is selected.\n");
			exit(1);
		    }
		}
		/* Update global bounding box */
		if (spot->xvec[j] < llx) llx = spot->xvec[j];
		if (spot->xvec[j] > urx) urx = spot->xvec[j];
		if (spot->yvec[j] < lly) lly = spot->yvec[j];
		if (spot->yvec[j] > ury) ury = spot->yvec[j];
	    }
	}
    }
}



/*
 * Button handling functions
 */

/*ARGSUSED*/
xtb_hret del_func(win, bval, info)
Window win;			/* Button window    */
int bval;			/* Button value     */
char *info;			/* User information */
/*
 * This routine is called when the `Close' button is pressed in
 * an xgraph window.  It causes the window to go away.
 */
{
    Window the_win = (Window) info;
    LocalWin *win_info;
    
    xtb_bt_set(win, 1, (char *) 0, 0);
    if (!XFindContext(disp, the_win, win_context, (caddr_t *) &win_info)) {
	if (win_info->flags & HARDCOPY_IN_PROGRESS) {
	    do_error("Can't close window while\nhardcopy dialog is posted.\n");
	    xtb_bt_set(win, 0, (char *) 0, 0);
	} else {
	    DelWindow(the_win, win_info);
	}
    }
    return XTB_HANDLED;
}

/*ARGSUSED*/
xtb_hret hcpy_func(win, bval, info)
Window win;			/* Button Window    */
int bval;			/* Button value     */
char *info;			/* User Information */
/*
 * This routine is called when the hardcopy button is pressed
 * in an xgraph window.  It causes the output dialog to be
 * posted.
 */
{
    Window the_win = (Window) info;
    LocalWin *win_info;
    
    xtb_bt_set(win, 1, (char *) 0, 0);
    if (!XFindContext(disp, the_win, win_context, (caddr_t *) &win_info)) {
	win_info->flags |= HARDCOPY_IN_PROGRESS;
	PrintWindow(the_win, win_info);
	win_info->flags &= (~HARDCOPY_IN_PROGRESS);
    }
    xtb_bt_set(win, 0, (char *) 0, 0);
    return XTB_HANDLED;
}

static 

/*ARGSUSED*/
xtb_hret abt_func(win, bval, info)
Window win;			/* Button window    */
int bval;			/* Button value     */
char *info;			/* User information */
{
    static char *msg_fmt =
"Version %s\n\
David Harrison\n\
University of California, Berkeley\n\
Send comments or suggestions to:\n\
(davidh@ic.Berkeley.EDU or \n\
...!ucbvax!ucbic!davidh)";
    static int active = 0;
    char msg_buf[1024];

    if (!active) {
	active = 1;
	xtb_bt_set(win, 1, (char *) 0, 0);
	(void) sprintf(msg_buf, msg_fmt, VERSION_STRING);
	msg_box("XGraph", msg_buf);
	xtb_bt_set(win, 0, (char *) 0, 0);
	active = 0;
    }
    return XTB_HANDLED;
}


#define NORMSIZE	600
#define MINDIM		100

Window NewWindow(progname, lowX, lowY, upX, upY, asp)
char *progname;			/* Name of program    */
double lowX, lowY;		/* Lower left corner  */
double upX, upY;		/* Upper right corner */
double asp;			/* Aspect ratio       */
/*
 * Creates and maps a new window.  This includes allocating its
 * local structure and associating it with the XId for the window.
 * The aspect ratio is specified as the ratio of width over height.
 */
{
    Window new_window;
    LocalWin *new_info;
    static Cursor theCursor = (Cursor) 0;
    XSizeHints sizehints;
    XSetWindowAttributes wattr;
    XWMHints wmhints;
    XColor fg_color, bg_color;
    int geo_mask;
    int width, height;
    unsigned long wamask;
    char defSpec[120];
    double pad;
    
    new_info = (LocalWin *) malloc(sizeof(LocalWin));

    if (upX > lowX) {
	new_info->loX = lowX;
	new_info->hiX = upX;
    } else {
	new_info->loX = llx;
	new_info->hiX = urx;
    }
    if (upY > lowY) {
	new_info->loY = lowY;
	new_info->hiY = upY;
    } else {
	new_info->loY = lly;
	new_info->hiY = ury;
    }

    /* Increase the padding for aesthetics */
    if (new_info->hiX - new_info->loX == 0.0) {
	pad = MAX(0.5, fabs(new_info->hiX/2.0));
	new_info->hiX += pad;
	new_info->loX -= pad;
    }
    if (new_info->hiY - new_info->loY == 0) {
	pad = MAX(0.5, fabs(ury/2.0));
	new_info->hiY += pad;
	new_info->loY -= pad;
    }

    /* Add 10% padding to bounding box (div by 20 yeilds 5%) */
    pad = (new_info->hiX - new_info->loX) / 20.0;
    new_info->loX -= pad;  new_info->hiX += pad;
    pad = (new_info->hiY - new_info->loY) / 20.0;
    new_info->loY -= pad;  new_info->hiY += pad;

    /* Aspect ratio computation */
    if (asp < 1.0) {
	height = NORMSIZE;
	width = ((int) (((double) NORMSIZE) * asp));
    } else {
	width = NORMSIZE;
	height = ((int) (((double) NORMSIZE) / asp));
    }
    height = MAX(MINDIM, height);
    width = MAX(MINDIM, width);
    (void) sprintf(defSpec, "%dx%d+100+100", width, height);

    wamask = CWBackPixel | CWBorderPixel | CWColormap;
    wattr.background_pixel = PM_PIXEL("Background");
    wattr.border_pixel = PM_PIXEL("Border");
    wattr.colormap = cmap;

    sizehints.flags = PPosition|PSize;
    sizehints.x = sizehints.y = 100;
    sizehints.width = width;
    sizehints.height = height;

    new_window = XCreateWindow(disp, RootWindow(disp, screen),
			       sizehints.x, sizehints.y,
			       (unsigned int) sizehints.width,
			       (unsigned int) sizehints.height,
			       (unsigned int) PM_INT("BorderSize"),
			       depth, InputOutput, vis,
			       wamask, &wattr);


    if (new_window) {
	xtb_frame cl_frame, hd_frame, ab_frame;

	XStoreName(disp, new_window, progname);
	XSetIconName(disp, new_window, progname);

	wmhints.flags = InputHint | StateHint;
	wmhints.input = True;
	wmhints.initial_state = NormalState;
	XSetWMHints(disp, new_window, &wmhints);

	geo_mask = XParseGeometry(PM_STR("Geometry"), &sizehints.x, &sizehints.y,
				  (unsigned int *) &sizehints.width,
				  (unsigned int *) &sizehints.height);
	if (geo_mask & (XValue | YValue)) {
	    sizehints.flags = (sizehints.flags & ~PPosition) | USPosition;
	}
	if (geo_mask & (WidthValue | HeightValue)) {
	    sizehints.flags = (sizehints.flags & ~PSize) | USSize;
	}
	XSetNormalHints(disp, new_window, &sizehints);

	/* Set device info */
	set_X(new_window, &(new_info->dev_info));

	/* Make buttons */
	xtb_bt_new(new_window, "Close", del_func,
		   (xtb_data) new_window, &cl_frame);
	new_info->close = cl_frame.win;
	XMoveWindow(disp, new_info->close, (int) BTNPAD, (int) BTNPAD);
	xtb_bt_new(new_window, "Hardcopy", hcpy_func,
		   (xtb_data) new_window, &hd_frame);
	new_info->hardcopy = hd_frame.win;
	XMoveWindow(disp, new_info->hardcopy,
		    (int) (BTNPAD + cl_frame.width + BTNINTER),
		    BTNPAD);
	xtb_bt_new(new_window, "About", abt_func,
		   (xtb_data) new_window, &ab_frame);
	new_info->about = ab_frame.win;
	XMoveWindow(disp, new_info->about,
		    (int) (BTNPAD + cl_frame.width + BTNINTER +
			   hd_frame.width + BTNINTER), BTNPAD);

	new_info->flags = 0;
	XSelectInput(disp, new_window,
		     ExposureMask|KeyPressMask|ButtonPressMask);
	if (!theCursor) {
	    theCursor = XCreateFontCursor(disp, XC_top_left_arrow);
	    fg_color = PM_COLOR("Foreground");
	    bg_color = PM_COLOR("Background");
	    XRecolorCursor(disp, theCursor, &fg_color, &bg_color);
	}
	XDefineCursor(disp, new_window, theCursor);
	if (!win_context) {
	    win_context = XUniqueContext();
	}
	XSaveContext(disp, new_window, win_context, (caddr_t) new_info);
	XMapWindow(disp, new_window);
	return new_window;
    } else {
	return (Window) 0;
    }
}


DelWindow(win, win_info)
Window win;			/* Window     */
LocalWin *win_info;		/* Local Info */
/*
 * This routine actually deletes the specified window and
 * decrements the window count.
 */
{
    xtb_data info;

    XDeleteContext(disp, win, win_context);
    xtb_bt_del(win_info->close, &info);
    xtb_bt_del(win_info->hardcopy, &info);
    xtb_bt_del(win_info->about, &info);
    free((char *) win_info);
    XDestroyWindow(disp, win);
    Num_Windows -= 1;
}

PrintWindow(win, win_info)
Window win;			/* Window       */
LocalWin *win_info;		/* Local Info   */
/*
 * This routine posts a dialog asking about the hardcopy
 * options desired.  If the user hits `OK',  the hard
 * copy is performed.
 */
{
    ho_dialog(win, Prog_Name, (char *) win_info);
}


static XRectangle boxEcho;
static GC echoGC = (GC) 0;

#define DRAWBOX \
if (startX < curX) { \
   boxEcho.x = startX; \
   boxEcho.width = curX - startX; \
} else { \
   boxEcho.x = curX; \
   boxEcho.width = startX - curX; \
} \
if (startY < curY) { \
   boxEcho.y = startY; \
   boxEcho.height = curY - startY; \
} else { \
   boxEcho.y = curY; \
   boxEcho.height = startY - curY; \
} \
XDrawRectangles(disp, win, echoGC, &boxEcho, 1);

#define TRANX(xval) \
(((double) ((xval) - wi->XOrgX)) * wi->XUnitsPerPixel + wi->UsrOrgX)

#define TRANY(yval) \
(wi->UsrOppY - (((double) ((yval) - wi->XOrgY)) * wi->YUnitsPerPixel))


int HandleZoom(progname, evt, wi, cur)
char *progname;
XButtonPressedEvent *evt;
LocalWin *wi;
Cursor cur;
{
    Window win, new_win;
    Window root_rtn, child_rtn;
    XEvent theEvent;
    int startX, startY, curX, curY, newX, newY, stopFlag, numwin;
    int root_x, root_y;
    unsigned int mask_rtn;
    double loX, loY, hiX, hiY, asp;

    win = evt->window;
    if (XGrabPointer(disp, win, True,
		     (unsigned int) (ButtonPressMask|ButtonReleaseMask|
				     PointerMotionMask|PointerMotionHintMask),
		     GrabModeAsync, GrabModeAsync,
		     win, cur, CurrentTime) != GrabSuccess) {
	XBell(disp, 0);
	return 0;
    }
    if (echoGC == (GC) 0) {
	unsigned long gcmask;
	XGCValues gcvals;

	gcmask = GCForeground | GCFunction;
	gcvals.foreground = PM_PIXEL("ZeroColor") ^ PM_PIXEL("Background");
	gcvals.function = GXxor;
	echoGC = XCreateGC(disp, win, gcmask, &gcvals);
    }
    startX = evt->x;  startY = evt->y;
    XQueryPointer(disp, win, &root_rtn, &child_rtn, &root_x, &root_y,
		  &curX, &curY, &mask_rtn);
    /* Draw first box */
    DRAWBOX;
    stopFlag = 0;
    while (!stopFlag) {
	XNextEvent(disp, &theEvent);
	switch (theEvent.xany.type) {
	case MotionNotify:
	    XQueryPointer(disp, win, &root_rtn, &child_rtn, &root_x, &root_y,
			  &newX, &newY, &mask_rtn);
	    /* Undraw the old one */
	    DRAWBOX;
	    /* Draw the new one */
	    curX = newX;  curY = newY;
	    DRAWBOX;
	    break;
	case ButtonRelease:
	    DRAWBOX;
	    XUngrabPointer(disp, CurrentTime);
	    stopFlag = 1;
	    if ((startX-curX != 0) && (startY-curY != 0)) {
		/* Figure out relative bounding box */
		loX = TRANX(startX);   loY = TRANY(startY);
		hiX = TRANX(curX);     hiY = TRANY(curY);
		if (loX > hiX) {
		    double temp;
		    
		    temp = hiX;
		    hiX = loX;
		    loX = temp;
		}
		if (loY > hiY) {
		    double temp;
		    
		    temp = hiY;
		    hiY = loY;
		    loY = temp;
		}
		/* physical aspect ratio */
		asp = ((double) ABS(startX-curX))/((double) ABS(startY-curY));
		new_win = NewWindow(progname, loX, loY, hiX, hiY, asp);
		if (new_win) {
		    numwin = 1;
		} else {
		    numwin = 0;
		}
	    } else {
		numwin = 0;
	    }
	    break;
	default:
	    printf("unknown event: %d\n", theEvent.xany.type);
	    break;
	}
    }
    return numwin;
}


int InitSets()
/*
 * Initializes the data sets with default information.  Sets up
 * original values for parameters in parameters package.
 */
{
    int idx;
    char buf[1024];

    /*
     * Used to do all kinds of searching through visuals, etc.
     * Got complaints -- so back to the simple version.
     */
    vis = DefaultVisual(disp, DefaultScreen(disp));
    cmap = DefaultColormap(disp, DefaultScreen(disp));
    screen = DefaultScreen(disp);
    depth = DefaultDepth(disp, DefaultScreen(disp));

    param_init(disp, cmap);

    param_set("Debug", BOOL, "false");
    param_set("Geometry", STR, DEF_GEOMETRY);
    param_set("ReverseVideo", BOOL, DEF_REVERSE);

    param_set("BorderSize", INT, DEF_BORDER_WIDTH);
    param_set("TitleText", STR, DEF_TITLE_TEXT);
    param_set("XUnitText", STR, DEF_XUNIT_TEXT);
    param_set("YUnitText", STR, DEF_YUNIT_TEXT); /* YUnits */
    param_set("Ticks", BOOL, DEF_TICK_FLAG);

    param_set("Markers", BOOL, DEF_MARKER_FLAG); /* markFlag (-m) */
    param_set("StyleMarkers", BOOL, DEF_DIFFMARK_FLAG); /* colorMark (-M) */
    param_set("PixelMarkers", BOOL, DEF_PIXMARK_FLAG); /* pixelMarks  (-p) */
    param_set("LargePixels", BOOL, DEF_LARGEPIX_FLAG); /* bigPixel (-P) */

    param_set("BoundBox", BOOL, DEF_BB_FLAG);
    param_set("NoLines", BOOL, DEF_NOLINE_FLAG);
    param_set("LogX", BOOL, DEF_LOGX_FLAG);
    param_set("LogY", BOOL, DEF_LOGY_FLAG); /* logYFlag */
    param_set("BarGraph", BOOL, DEF_BAR_FLAG);
    param_set("BarBase", DBL, DEF_BAR_BASE);
    param_set("BarWidth", DBL, DEF_BAR_WIDTH);
    param_set("LineWidth", INT, DEF_LINE_WIDTH);
    param_set("GridSize", INT, DEF_GRID_SIZE);
    param_set("GridStyle", STYLE, DEF_GRID_STYLE);

    param_set("Device", STR, DEF_DEVICE);
    param_set("Disposition", STR, DEF_DISPOSITION);
    param_set("FileOrDev", STR, DEF_FILEORDEV);

    /* Set the user bounding box */
    param_set("XLowLimit", DBL, DEF_LOW_LIMIT);
    param_set("YLowLimit", DBL, DEF_LOW_LIMIT);
    param_set("XHighLimit", DBL, DEF_HIGH_LIMIT);
    param_set("YHighLimit", DBL, DEF_HIGH_LIMIT);

    /* Depends critically on whether the display has color */
    if (depth < 4) {
	/* Its black and white */
	param_set("Background", PIXEL, DEF_BW_BACKGROUND);
	param_set("Border", PIXEL, DEF_BW_BORDER);
	param_set("ZeroColor", PIXEL, DEF_BW_ZEROCOLOR);
	param_set("ZeroWidth", INT, DEF_BW_ZEROWIDTH);
	param_set("ZeroStyle", STYLE, DEF_BW_ZEROSTYLE);
	param_set("Foreground", PIXEL, DEF_BW_FOREGROUND);
	/* Initialize set defaults */
	for (idx = 0;  idx < MAXATTR;  idx++) {
	    (void) sprintf(buf, "%d.Style", idx);
	    param_set(buf, STYLE, defStyle[idx]);
	    (void) sprintf(buf, "%d.Color", idx);
	    param_set(buf, PIXEL, DEF_BW_FOREGROUND);
	}
    } else {
	/* Its color */
	param_set("Background", PIXEL, DEF_COL_BACKGROUND);
	param_set("Border", PIXEL, DEF_COL_BORDER);
	param_set("ZeroColor", PIXEL, DEF_COL_ZEROCOLOR);
	param_set("ZeroWidth", INT, DEF_COL_ZEROWIDTH);
	param_set("ZeroStyle", STYLE, DEF_COL_ZEROSTYLE);
	param_set("Foreground", PIXEL, DEF_COL_FOREGROUND);
	/* Initalize attribute colors defaults */
	for (idx = 0;  idx < MAXATTR;  idx++) {
	    (void) sprintf(buf, "%d.Style", idx);
	    param_set(buf, STYLE, defStyle[idx]);
	    (void) sprintf(buf, "%d.Color", idx);
	    param_set(buf, PIXEL, defColors[idx]);
	}
    }

    param_set("LabelFont", FONT, DEF_LABEL_FONT);
    param_set("TitleFont", FONT, DEF_TITLE_FONT);

    /* Initialize the data sets */
    for (idx = 0;  idx < MAXSETS;  idx++) {
	(void) sprintf(buf, "Set %d", idx);
	PlotData[idx].setName = STRDUP(buf);
	PlotData[idx].list = (PointList *) 0;
    }
}



static char *def_str;

#define DEF(name, type) \
if (def_str = XGetDefault(disp, Prog_Name, name)) { \
    param_set(name, type, def_str); \
}

int ReadDefaults()
/*
 * Reads X default values which override the hard-coded defaults
 * set up by InitSets.
 */
{
    char newname[100];
    int idx;

    DEF("Debug", BOOL);
    DEF("Geometry", STR);
    DEF("Background", PIXEL);
    DEF("BorderSize", INT);
    DEF("Border", PIXEL);
    DEF("GridSize", INT);
    DEF("GridStyle", STYLE);
    DEF("Foreground", PIXEL);
    DEF("ZeroColor", PIXEL);
    DEF("ZeroStyle", STYLE);
    DEF("ZeroWidth", INT);
    DEF("LabelFont", FONT);
    DEF("TitleFont", FONT);
    DEF("Ticks", BOOL);
    DEF("Device", STR);
    DEF("Disposition", STR);
    DEF("FileOrDev", STR);
    DEF("PixelMarkers", BOOL);
    DEF("LargePixels", BOOL);
    DEF("Markers", BOOL);
    DEF("StyleMarkers", BOOL);
    DEF("BoundBox", BOOL);
    DEF("NoLines", BOOL);
    DEF("LineWidth", INT);

    /* Read device specific parameters */
    for (idx = 0;  idx < hard_count;  idx++) {
	sprintf(newname, "%s.Dimension", hard_devices[idx].dev_name);
	DEF(newname, DBL);	/* hard_devices[idx].dev_max_dim */
	sprintf(newname, "%s.OutputTitleFont", hard_devices[idx].dev_name);
	DEF(newname, STR);	/* hard_devices[idx].dev_title_font */
	sprintf(newname, "%s.OutputTitleSize", hard_devices[idx].dev_name);
	DEF(newname, DBL);	/* hard_devices[idx].dev_title_size */
	sprintf(newname, "%s.OutputAxisFont", hard_devices[idx].dev_name);
	DEF(newname, STR);	/* hard_devices[idx].dev_axis_font */
	sprintf(newname, "%s.OutputAxisSize", hard_devices[idx].dev_name);
	DEF(newname, DBL);	/* hard_devices[idx].dev_axis_size */
    }


    /* Read the default line and color attributes */
    for (idx = 0;  idx < MAXATTR;  idx++) {
	(void) sprintf(newname, "%d.Style", idx);
	DEF(newname, STYLE);	/* AllAttrs[idx].lineStyleLen */
	(void) sprintf(newname, "%d.Color", idx);
	DEF(newname, PIXEL);	/* AllAttrs[idx].pixelValue */
    }

    DEF("ReverseVideo", BOOL);
}


#define FS(str)	(void) fprintf(stderr, str)

int argerror(err, val)
char *err, *val;
{
    (void) fprintf(stderr, "Error: %s: %s\n\n", val, err);

    FS("format: xgraph [-<digit> set_name] [-bar] [-bb] [-bd border_color]\n");
    FS("         [-bg background_color] [-brb bar_base] [-brw bar_width]\n");
    FS("         [-bw bdr_width] [-db] [-fg foreground_color] [-gw grid_size]\n");
    FS("         [-gs grid_style] [-lf label_font] [-lnx] [-lny] [-lw line_width]\n");
    FS("         [-lx x1,x2] [-ly y1,y2] [-m] [-M] [-nl] [-p] [-P] [-rv]\n");
    FS("         [-t title] [-tf title_font] [-tk] [-x x_unit_name]\n");
    FS("         [-y y_unit_name] [-zg zero_color] [-zw zero_size]\n");
    FS("         [=WxH+X+Y] [-display <host>:<disp>.<screen>] file...\n\n");
    FS("-bar   Draw bar graph with base -brb and width -brw\n");
    FS("-bb    Draw bounding box around data\n");
    FS("-db    Turn on debugging\n");
    FS("-lnx   Logarithmic scale for X axis\n");
    FS("-lny   Logarithmic scale for Y axis\n");
    FS("-m -M  Mark points distinctively (M varies with color)\n");
    FS("-nl    Don't draw lines (scatter plot)\n");
    FS("-p -P  Mark points with dot (P means big dot)\n");
    FS("-rv    Reverse video on black and white displays\n");
    FS("-tk    Draw tick marks instead of full grid\n");
    
    exit(1);
}

#define ARG(opt, name) \
if (strcmp(argv[idx], opt) == 0) { \
    if (do_it) param_set(name, BOOL, "on"); \
    idx++; continue; \
}

#define ARG2(opt, name, type, missing) \
if (strcmp(argv[idx], opt) == 0) { \
   if (idx+1 >= argc) argerror(missing, argv[idx]); \
   if (do_it) param_set(name, type, argv[idx+1]); \
   idx += 2; continue;\
}

#define MAXLO	30

int ParseArgs(argc, argv, do_it)
int argc;
char *argv[];
int do_it;
/*
 * This routine parses the argument list for xgraph.  There are too
 * many to mention here so I won't.  If `do_it' is non-zero, options
 * are actually changed.  If `do_it' is zero, the argument list
 * is parsed but the options aren't set.  The routine is called
 * once to obtain the input files then again after the data is
 * read to set the options.
 */
{
    int idx, set;
    char *hi;

    idx = 1;
    while (idx < argc) {
	if (argv[idx][0] == '-') {
	    /* Check to see if its a data set name */
	    if (sscanf(argv[idx], "-%d", &set) == 1) {
		/* The next string is a set name */
		if (idx+1 >= argc) argerror("missing set name", argv[idx]);
		if (do_it) {
		    PlotData[set].setName = argv[idx+1];
		}
		idx += 2;
	    } else {
		/* Some non-dataset option */
		ARG2("-x", "XUnitText", STR, "missing axis name");
		ARG2("-y", "YUnitText", STR, "missing axis name");
		ARG2("-t", "TitleText", STR, "missing plot title");
		ARG2("-fg", "Foreground", PIXEL, "missing color name");
		ARG2("-bg", "Background", PIXEL, "missing color name");
		ARG2("-bd", "Border", PIXEL, "missing color name");
		ARG2("-bw", "BorderSize", INT, "missing border size");
		ARG2("-zg", "ZeroColor", PIXEL, "missing color name");
		ARG2("-zw", "ZeroWidth", INT, "missing width");
		ARG2("-tf", "TitleFont", FONT, "missing font name");
		ARG2("-lf", "LabelFont", FONT, "missing font name");
		ARG("-rv", "ReverseVideo");
		ARG("-tk", "Ticks");
		ARG("-bb", "BoundBox");
		if (strcmp(argv[idx], "-lx") == 0) {
		    /* Limit the X coordinates */
		    if (idx+1 >= argc) argerror("missing coordinate(s)",
						argv[idx]);
		    if (hi = index(argv[idx+1], ',')) {
			char low[MAXLO];
		    
			(void) strncpy(low, argv[idx+1], hi-argv[idx+1]);
			low[hi-argv[idx+1]] = '\0';
			hi++;
			if (do_it) {
			    param_set("XLowLimit", DBL, argv[idx+1]);
			    param_set("XHighLimit", DBL, hi);
			}
		    } else {
			argerror("limit coordinates not specified right",
				 argv[idx]);
		    }
		    idx += 2;
		    continue;
		}
		if (strcmp(argv[idx], "-ly") == 0) {
		    /* Limit the Y coordinates */
		    if (idx+1 >= argc) argerror("missing coordinate(s)",
						  argv[idx]);
		    if (hi = index(argv[idx+1], ',')) {
			char low[MAXLO];

			(void) strncpy(low, argv[idx+1], hi-argv[idx+1]);
			low[hi-argv[idx+1]] = '\0';
			hi++;
			if (do_it) {
			    param_set("YLowLimit", DBL, argv[idx+1]);
			    param_set("YHighLimit", DBL, hi);
			}
		    } else {
			argerror("limit coordinates not specified right",
				 argv[idx]);
		    }
		    idx += 2;
		    continue;
		} 
		ARG2("-lw", "LineWidth", INT, "missing line width");
		ARG("-nl", "NoLines");
		ARG("-m", "Markers");
		ARG("-M", "StyleMarkers");
		ARG("-p", "PixelMarkers");
		ARG("-P", "LargePixels");
		ARG("-lnx", "LogX");
		ARG("-lny", "LogY");
		ARG("-bar", "BarGraph");
		ARG2("-brw", "BarWidth", DBL, "missing width");
		ARG2("-brb", "BarBase", DBL, "missing base");
		ARG("-db", "Debug");
		ARG2("-gw", "GridSize", INT, "missing grid size");
		ARG2("-gs", "GridStyle", STYLE, "missing grid style");
		if (strcmp(argv[idx], "-display") == 0) {
		    /* Harmless display specification */
		    idx += 2;
		    continue;
		}
		argerror("unknown option", argv[idx]);
	    }
	} else if (argv[idx][0] == '=') {
	    /* Its a geometry specification */
	    if (do_it) param_set("Geometry", STR, argv[idx]+1);
	    idx++;
	} else {
	    /* It might be the host:display string */
	    if (rindex(argv[idx], ':') == (char *) 0) {
		/* Should be an input file */
		inFileNames[numFiles] = argv[idx];
		numFiles++;
	    }
	    idx++;
	}
    }
}

/*
 * New dataset reading code
 */

static int setNumber = 0;
static PointList **curSpot = (PointList **) 0;
static PointList *curList = (PointList *) 0;
static int newGroup = 0;
static int redundant_set = 0;

static int rdSet(fn)
char *fn;			/* Reading from file `fn' */
/*
 * Set up new dataset.  Will return zero if there are too many data sets.
 */
{
    char setname[100];

    if (!redundant_set) {
	if (setNumber < MAXSETS) {
	    (void) sprintf(setname, "Set %d", setNumber);
	    if ((strcmp(PlotData[setNumber].setName, setname) == 0) && fn) {
		PlotData[setNumber].setName = fn;
	    }
	    curSpot = &(PlotData[setNumber].list);
	    PlotData[setNumber].list = (PointList *) 0;
	    newGroup = 1;
	    setNumber++;
	    redundant_set = 1;
	    return 1;
	} else {
	    return 0;
	}
    } else {
	return 1;
    }
}

static void rdSetName(name)
char *name;			/* New set name */
/*
 * Sets the name of a data set.  Automatically makes a copy.
 */
{
    PlotData[setNumber-1].setName = STRDUP(name);
}

static void rdGroup()
/*
 * Set up for reading new group of points within a dataset.
 */
{
    newGroup = 1;
}

static void rdPoint(xval, yval)
double xval, yval;		/* New point         */
/*
 * Adds a new point to the current group of the current
 * data set.
 */
{
    if (newGroup) {
	*curSpot = (PointList *) malloc(sizeof(PointList));
	curList = *curSpot;
	curSpot = &(curList->next);
	curList->numPoints = 0;
	curList->allocSize = INITSIZE;
	curList->xvec = (double *) malloc((unsigned)(INITSIZE * sizeof(double)));
	curList->yvec = (double *) malloc((unsigned)(INITSIZE * sizeof(double)));
	curList->next = (PointList *) 0;
	newGroup = 0;
    }
    if (curList->numPoints >= curList->allocSize) {
	curList->allocSize *= 2;
	curList->xvec = (double *) realloc((char *) curList->xvec,
					   (unsigned) (curList->allocSize *
						       sizeof(double)));
	curList->yvec = (double *) realloc((char *) curList->yvec,
					   (unsigned) (curList->allocSize *
						       sizeof(double)));
    }

    curList->xvec[curList->numPoints] = xval;
    curList->yvec[curList->numPoints] = yval;

    (curList->numPoints)++;
    redundant_set = 0;
}

static int rdFindMax()
/* 
 * Returns the maximum number of items in any one group of any
 * data set.
 */
{
    int i;
    PointList *list;
    int max = -1;

    for (i = 0;  i < setNumber;  i++) {
	for (list = PlotData[i].list;  list;  list = list->next) {
	    if (list->numPoints > max) max = list->numPoints;
	}
    }
    return max;
}


typedef enum line_type {
    EMPTY, COMMENT, SETNAME, DRAWPNT, MOVEPNT, SETPARAM, ERROR
} LineType;

typedef struct point_defn {
    double xval, yval;
} Point;

typedef struct parmval_defn {
    char *name, *value;
} ParmVals;

typedef struct line_info {
    LineType type;
    union val_defn {
	char *str;		/* SETNAME, ERROR   */
	Point pnt;		/* DRAWPNT, MOVEPNT */
	ParmVals parm;		/* SETPARAM         */
    } val;
} LineInfo;

static LineType parse_line(line, result)
char *line;			/* Line to parse   */
LineInfo *result;		/* Returned result */
/*
 * Parses `line' into one of the types given in the definition
 * of LineInfo.  The appropriate values are filled into `result'.
 * Below are the current formats for each type:
 *   EMPTY:	All white space
 *   COMMENT:	Starts with "#"
 *   SETNAME:	A name enclosed in double quotes
 *   DRAWPNT:	Two numbers optionally preceded by keyword "draw"
 *   MOVEPNT:	Two numbers preceded by keyword "move"
 *   SETPARAM:  Two non-null strings separated by ":"
 *   ERROR:	Not any of the above (an error message is returned)
 * Note that often the values are pointers into the line itself
 * and should be copied if they are to be used over a long period.
 */
{
    char *first;

    /* Find first non-space character */
    while (*line && isspace(*line)) line++;
    if (*line) {
	if (*line == '#') {
	    /* comment */
	    result->type = COMMENT;
	} else if (*line == '"') {
	    /* setname */
	    result->type = SETNAME;
	    line++;
	    result->val.str = line;
	    while (*line && (*line != '\n') && (*line != '"')) line++;
	    if (*line) *line = '\0';
	} else {
	    first = line;
	    while (*line && !isspace(*line)) line++;
	    if (*line) {
		*line = '\0';
		if (stricmp(first, "move") == 0) {
		    /* MOVEPNT */
		    if (sscanf(line+1, "%lf %lf",
			       &result->val.pnt.xval,
			       &result->val.pnt.yval) == 2) {
			result->type = MOVEPNT;
		    } else {
			result->type = ERROR;
			result->val.str = "Cannot read move coordinates";
		    }
		} else if (stricmp(first, "draw") == 0) {
		    /* DRAWPNT */
		    if (sscanf(line+1, "%lf %lf",
			       &result->val.pnt.xval,
			       &result->val.pnt.yval) == 2) {
			result->type = DRAWPNT;
		    } else {
			result->type = ERROR;
			result->val.str = "Cannot read draw coordinates";
		    }
		} else if (first[strlen(first)-1] == ':') {
		    /* SETPARAM */
		    first[strlen(first)-1] = '\0';
		    result->val.parm.name = first;
		    line++;
		    while (*line && isspace(*line)) line++;
		    /* may be a \n at end of it */
		    if (line[strlen(line)-1] == '\n') {
			line[strlen(line)-1] = '\0';
		    }
		    result->val.parm.value = line;
		    result->type = SETPARAM;
		} else if (sscanf(first, "%lf", &result->val.pnt.xval) == 1) {
		    /* DRAWPNT */
		    if (sscanf(line+1, "%lf", &result->val.pnt.yval) == 1) {
			result->type = DRAWPNT;
		    } else {
			result->type = ERROR;
			result->val.str = "Cannot read second coordinate";
		    }
		} else {
		    /* ERROR */
		    result->type = ERROR;
		    result->val.str = "Unknown line type";
		}
	    } else {
		/* ERROR */
		result->type = ERROR;
		result->val.str = "Premature end of line";
	    }
	}
    } else {
	/* empty */
	result->type = EMPTY;
    }
    return result->type;
}


int ReadData(stream, filename)
FILE *stream;
char *filename;
/*
 * Reads in the data sets from the supplied stream.  If the format
 * is correct,  it returns the current maximum number of points across
 * all data sets.  If there is an error,  it returns -1.
 */
{
    char buffer[MAXBUFSIZE];
    LineInfo info;
    int line_count = 0;
    int errors = 0;
    
    if (!rdSet(filename)) {
	(void) fprintf(stderr, "Error in file `%s' at line %d:\n  %s\n",
		       filename, line_count,
		       "Too many data sets - extra data ignored");
	return -1;
    }
    while (fgets(buffer, MAXBUFSIZE, stream)) {
	line_count++;
	switch (parse_line(buffer, &info)) {
	case EMPTY:
	    if (!rdSet(filename)) {
		(void) fprintf(stderr, "Error in file `%s' at line %d:\n  %s\n",
			       filename, line_count,
			       "Too many data sets - extra data ignored");
		return -1;
	    }
	    break;
	case COMMENT:
	    /* nothing */
	    break;
	case SETNAME:
	    rdSetName(info.val.str);
	    break;
	case DRAWPNT:
	    rdPoint(info.val.pnt.xval, info.val.pnt.yval);
	    break;
	case MOVEPNT:
	    rdGroup();
	    rdPoint(info.val.pnt.xval, info.val.pnt.yval);
	    break;
	case SETPARAM:
	    param_reset(info.val.parm.name, info.val.parm.value);
	    break;
	default:
	    if (filename) {
		(void) fprintf(stderr, "Error in file `%s' at line %d:\n  %s\n",
			       filename, line_count, info.val.str);
		errors++;
	    }
	    break;
	}
    }
    if (errors) return -1; else return rdFindMax();
}




int DrawWindow(win_info)
LocalWin *win_info;		/* Window information */
/*
 * Draws the data in the window.  Does not clear the window.
 * The data is scaled so that all of the data will fit.
 * Grid lines are drawn at the nearest power of 10 in engineering
 * notation.  Draws axis numbers along bottom and left hand edges.
 * Centers title at top of window.
 */
{
    /* Figure out the transformation constants */
    if (TransformCompute(win_info)) {
	
	/* Draw the title */
	DrawTitle(win_info);

	/* Draw the legend */
	DrawLegend(win_info);

	/* Draw the axis unit labels,  grid lines,  and grid labels */
	DrawGridAndAxis(win_info);

	/* Draw the data sets themselves */
	DrawData(win_info);
    }
}




DrawTitle(wi)
LocalWin *wi;		/* Window information    */
/*
 * This routine draws the title of the graph centered in
 * the window.  It is spaced down from the top by an amount
 * specified by the constant PADDING.  The font must be
 * fixed width.  The routine returns the height of the
 * title in pixels.
 */
{
    wi->dev_info.xg_text(wi->dev_info.user_state,
			 wi->dev_info.area_w/2,
			 wi->dev_info.axis_pad,
			 PM_STR("TitleText"), T_TOP, T_TITLE);
}




int TransformCompute(wi)
LocalWin *wi;			/* Window information          */
/*
 * This routine figures out how to draw the axis labels and grid lines.
 * Both linear and logarithmic axes are supported.  Axis labels are
 * drawn in engineering notation.  The power of the axes are labeled
 * in the normal axis labeling spots.  The routine also figures
 * out the necessary transformation information for the display
 * of the points (it touches XOrgX, XOrgY, UsrOrgX, UsrOrgY, and
 * UnitsPerPixel).
 */
{
    double bbCenX, bbCenY, bbHalfWidth, bbHalfHeight;
    int idx, maxName, leftWidth;
    char err[MAXBUFSIZE];
    char *XUnitText = PM_STR("XUnitText");

    /*
     * First,  we figure out the origin in the X window.  Above
     * the space we have the title and the Y axis unit label.
     * To the left of the space we have the Y axis grid labels.
     */

    wi->XOrgX = wi->dev_info.bdr_pad + (7 * wi->dev_info.axis_width)
      + wi->dev_info.bdr_pad;
    wi->XOrgY = wi->dev_info.bdr_pad + wi->dev_info.title_height
      + wi->dev_info.bdr_pad + wi->dev_info.axis_height
	+ wi->dev_info.axis_height/2 + wi->dev_info.bdr_pad;

    /*
     * Now we find the lower right corner.  Below the space we
     * have the X axis grid labels.  To the right of the space we
     * have the X axis unit label and the legend.  We assume the 
     * worst case size for the unit label.
     */

    maxName = 0;
    for (idx = 0;  idx < MAXSETS;  idx++) {
	if (PlotData[idx].list) {
	    int tempSize;

	    tempSize = strlen(PlotData[idx].setName);
	    if (tempSize > maxName) maxName = tempSize;
	}
    }
    /* Worst case size of the X axis label: */
    leftWidth = (strlen(XUnitText)) * wi->dev_info.axis_width;
    if ((maxName*wi->dev_info.axis_width)+wi->dev_info.bdr_pad > leftWidth)
      leftWidth = maxName * wi->dev_info.axis_width + wi->dev_info.bdr_pad;
    
    wi->XOppX = wi->dev_info.area_w - wi->dev_info.bdr_pad - leftWidth;
    wi->XOppY = wi->dev_info.area_h - wi->dev_info.bdr_pad
      - wi->dev_info.axis_height - wi->dev_info.bdr_pad;

    if ((wi->XOrgX >= wi->XOppX) || (wi->XOrgY >= wi->XOppY)) {
	do_error(strcpy(err, "Drawing area is too small\n"));
	return 0;
    }

    /* 
     * We now have a bounding box for the drawing region.
     * Figure out the units per pixel using the data set bounding box.
     */
    wi->XUnitsPerPixel = (wi->hiX - wi->loX)/((double) (wi->XOppX - wi->XOrgX));
    wi->YUnitsPerPixel = (wi->hiY - wi->loY)/((double) (wi->XOppY - wi->XOrgY));

    /*
     * Find origin in user coordinate space.  We keep the center of
     * the original bounding box in the same place.
     */
    bbCenX = (wi->loX + wi->hiX) / 2.0;
    bbCenY = (wi->loY + wi->hiY) / 2.0;
    bbHalfWidth = ((double) (wi->XOppX - wi->XOrgX))/2.0 * wi->XUnitsPerPixel;
    bbHalfHeight = ((double) (wi->XOppY - wi->XOrgY))/2.0 * wi->YUnitsPerPixel;
    wi->UsrOrgX = bbCenX - bbHalfWidth;
    wi->UsrOrgY = bbCenY - bbHalfHeight;
    wi->UsrOppX = bbCenX + bbHalfWidth;
    wi->UsrOppY = bbCenY + bbHalfHeight;

    /*
     * Everything is defined so we can now use the SCREENX and SCREENY
     * transformations.
     */
    return 1;
}

int DrawGridAndAxis(wi)
LocalWin *wi;			/* Window information         */
/*
 * This routine draws grid line labels in engineering notation,
 * the grid lines themselves,  and unit labels on the axes.
 */
{
    int expX, expY;		/* Engineering powers */
    int startX;
    int Yspot, Xspot;
    char power[10], value[10], final[MAXBUFSIZE+10];
    double Xincr, Yincr, Xstart, Ystart, Yindex, Xindex, larger;
    XSegment segs[2];
    double initGrid(), stepGrid();
    int tickFlag = PM_BOOL("Ticks");
    int logXFlag = PM_BOOL("LogX");
    int logYFlag = PM_BOOL("LogY");
    char *XUnitText = PM_STR("XUnitText");
    char *YUnitText = PM_STR("YUnitText");

    /*
     * Grid display powers are computed by taking the log of
     * the largest numbers and rounding down to the nearest
     * multiple of 3.
     */
    if (logXFlag) {
	expX = 0;
    } else {
	if (fabs(wi->UsrOrgX) > fabs(wi->UsrOppX)) {
	    larger = fabs(wi->UsrOrgX);
	} else {
	    larger = fabs(wi->UsrOppX);
	}
	expX = ((int) floor(nlog10(larger)/3.0)) * 3;
    }
    if (logYFlag) {
	expY = 0;
    } else {
	if (fabs(wi->UsrOrgY) > fabs(wi->UsrOppY)) {
	    larger = fabs(wi->UsrOrgY);
	} else {
	    larger = fabs(wi->UsrOppY);
	}
	expY = ((int) floor(nlog10(larger)/3.0)) * 3;
    }

    /*
     * With the powers computed,  we can draw the axis labels.
     */
    if (expY != 0) {
	(void) strcpy(final, YUnitText);
	(void) strcat(final, " x 10");
	Xspot = wi->dev_info.bdr_pad +
	  ((strlen(YUnitText)+5) * wi->dev_info.axis_width);
	Yspot = wi->dev_info.bdr_pad * 2 + wi->dev_info.title_height +
	  wi->dev_info.axis_height/2;
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     Xspot, Yspot, final, T_RIGHT, T_AXIS);
	(void) sprintf(power, "%d", expY);
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     Xspot, Yspot, power, T_LOWERLEFT, T_AXIS);
    } else {
	Yspot = wi->dev_info.bdr_pad * 2 + wi->dev_info.title_height;
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     wi->dev_info.bdr_pad, Yspot, YUnitText,
			     T_UPPERLEFT, T_AXIS);
    }
	
    startX = wi->dev_info.area_w - wi->dev_info.bdr_pad;
    if (expX != 0) {
	(void) sprintf(power, "%d", expX);
	startX -= (strlen(power) * wi->dev_info.axis_width);
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     startX, wi->XOppY, power, T_LOWERLEFT, T_AXIS);
	(void) strcpy(final, XUnitText);
	(void) strcat(final, " x 10");
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     startX, wi->XOppY, final, T_RIGHT, T_AXIS);
    } else {
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     startX, wi->XOppY, XUnitText, T_RIGHT, T_AXIS);
    }

    /* 
     * First,  the grid line labels
     */
    Yincr = (wi->dev_info.axis_pad + wi->dev_info.axis_height) * wi->YUnitsPerPixel;
    Ystart = initGrid(wi->UsrOrgY, Yincr, logYFlag);
    for (Yindex = Ystart;  Yindex < wi->UsrOppY;  Yindex = stepGrid()) {
	Yspot = SCREENY(wi, Yindex);
	/* Write the axis label */
	WriteValue(value, Yindex, expY, logYFlag);
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     wi->dev_info.bdr_pad +
				     (7 * wi->dev_info.axis_width),
			     Yspot, value, T_RIGHT, T_AXIS);
    }

    Xincr = (wi->dev_info.axis_pad + (wi->dev_info.axis_width * 7)) * wi->XUnitsPerPixel;
    Xstart = initGrid(wi->UsrOrgX, Xincr, logXFlag);
    for (Xindex = Xstart;  Xindex < wi->UsrOppX;  Xindex = stepGrid()) {
	Xspot = SCREENX(wi, Xindex);
	/* Write the axis label */
	WriteValue(value, Xindex, expX, logXFlag);
	wi->dev_info.xg_text(wi->dev_info.user_state,
			     Xspot,
			     wi->dev_info.area_h - wi->dev_info.bdr_pad,
			     value, T_BOTTOM, T_AXIS);
    }

    /*
     * Now,  the grid lines or tick marks
     */
    Yincr = (wi->dev_info.axis_pad + wi->dev_info.axis_height) * wi->YUnitsPerPixel;
    Ystart = initGrid(wi->UsrOrgY, Yincr, logYFlag);
    for (Yindex = Ystart;  Yindex < wi->UsrOppY;  Yindex = stepGrid()) {
	Yspot = SCREENY(wi, Yindex);
	/* Draw the grid line or tick mark */
	if (tickFlag) {
	    segs[0].x1 = wi->XOrgX;
	    segs[0].x2 = wi->XOrgX + wi->dev_info.tick_len;
	    segs[1].x1 = wi->XOppX - wi->dev_info.tick_len;
	    segs[1].x2 = wi->XOppX;
	    segs[0].y1 = segs[0].y2 = segs[1].y1 = segs[1].y2 = Yspot;
	} else {
	    segs[0].x1 = wi->XOrgX;  segs[0].x2 = wi->XOppX;
	    segs[0].y1 = segs[0].y2 = Yspot;
	}
	if ((ABS(Yindex) < ZERO_THRES) && !logYFlag) {
	    wi->dev_info.xg_seg(wi->dev_info.user_state,
				 1, segs, PM_INT("ZeroWidth"),
				 L_ZERO, 0, 0);
	    if (tickFlag) {
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				     1, &(segs[1]), PM_INT("ZeroWidth"),
				     L_ZERO, 0, 0);
	    }
	} else {
	    wi->dev_info.xg_seg(wi->dev_info.user_state,
				 1, segs, PM_INT("GridSize"),
				 L_AXIS, 0, 0);
	    if (tickFlag) {
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				     1, &(segs[1]), PM_INT("GridSize"),
				     L_AXIS, 0, 0);
	    }
	}
    }

    Xincr = (wi->dev_info.axis_pad + (wi->dev_info.axis_width * 7)) * wi->XUnitsPerPixel;
    Xstart = initGrid(wi->UsrOrgX, Xincr, logXFlag);
    for (Xindex = Xstart;  Xindex < wi->UsrOppX;  Xindex = stepGrid()) {
	Xspot = SCREENX(wi, Xindex);
	/* Draw the grid line or tick marks */
	if (tickFlag) {
	    segs[0].x1 = segs[0].x2 = segs[1].x1 = segs[1].x2 = Xspot;
	    segs[0].y1 = wi->XOrgY;
	    segs[0].y2 = wi->XOrgY + wi->dev_info.tick_len;
	    segs[1].y1 = wi->XOppY - wi->dev_info.tick_len;
	    segs[1].y2 = wi->XOppY;
	} else {
	    segs[0].x1 = segs[0].x2 = Xspot;
	    segs[0].y1 = wi->XOrgY; segs[0].y2 = wi->XOppY;
	}
	if ((ABS(Xindex) < ZERO_THRES) && !logXFlag) {
	    wi->dev_info.xg_seg(wi->dev_info.user_state,
				 1, segs, PM_INT("ZeroWidth"), L_ZERO, 0, 0);
	    if (tickFlag) {
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				    1, &(segs[1]), PM_INT("ZeroWidth"),
				    L_ZERO, 0, 0);

	    }
	} else {
	    wi->dev_info.xg_seg(wi->dev_info.user_state,
				 1, segs, PM_INT("GridSize"), L_AXIS, 0, 0);
	    if (tickFlag) {
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				     1, &(segs[1]), PM_INT("GridSize"), L_AXIS, 0, 0);
	    }
	}
    }
    /* Check to see if he wants a bounding box */
    if (PM_BOOL("BoundBox")) {
	XSegment bb[4];

	/* Draw bounding box */
	bb[0].x1 = bb[0].x2 = bb[1].x1 = bb[3].x2 = wi->XOrgX;
	bb[0].y1 = bb[2].y2 = bb[3].y1 = bb[3].y2 = wi->XOrgY;
	bb[1].x2 = bb[2].x1 = bb[2].x2 = bb[3].x1 = wi->XOppX;
	bb[0].y2 = bb[1].y1 = bb[1].y2 = bb[2].y1 = wi->XOppY;
	wi->dev_info.xg_seg(wi->dev_info.user_state,
			     4, bb, PM_INT("GridSize"), L_AXIS, 0, 0);
    }
}

static double gridBase, gridStep, gridJuke[101];
static int gridNJuke, gridCurJuke;

#define ADD_GRID(val)	(gridJuke[gridNJuke++] = log10(val))

double initGrid(low, step, logFlag)
double low;			/* desired low value          */
double step;			/* desired step (user coords) */
int logFlag;			/* is axis logarithmic?       */
{
    double ratio, x;
    double RoundUp(), stepGrid();

    gridNJuke = gridCurJuke = 0;
    gridJuke[gridNJuke++] = 0.0;

    if (logFlag) {
	ratio = pow(10.0, step);
	gridBase = floor(low);
	gridStep = ceil(step);
	if (ratio <= 3.0) {
	    if (ratio > 2.0) {
		ADD_GRID(3.0);
	    } else if (ratio > 1.333) {
		ADD_GRID(2.0);	ADD_GRID(5.0);
	    } else if (ratio > 1.25) {
		ADD_GRID(1.5);	ADD_GRID(2.0);	ADD_GRID(3.0);
		ADD_GRID(5.0);	ADD_GRID(7.0);
	    } else {
		for (x = 1.0; x < 10.0 && (x+.5)/(x+.4) >= ratio; x += .5) {
		    ADD_GRID(x + .1);	ADD_GRID(x + .2);
		    ADD_GRID(x + .3);	ADD_GRID(x + .4);
		    ADD_GRID(x + .5);
		}
		if (floor(x) != x) ADD_GRID(x += .5);
		for ( ; x < 10.0 && (x+1.0)/(x+.5) >= ratio; x += 1.0) {
		    ADD_GRID(x + .5);	ADD_GRID(x + 1.0);
		}
		for ( ; x < 10.0 && (x+1.0)/x >= ratio; x += 1.0) {
		    ADD_GRID(x + 1.0);
		}
		if (x == 7.0) {
		    gridNJuke--;
		    x = 6.0;
		}
		if (x < 7.0) {
		    ADD_GRID(x + 2.0);
		}
		if (x == 10.0) gridNJuke--;
	    }
	    x = low - gridBase;
	    for (gridCurJuke = -1; x >= gridJuke[gridCurJuke+1]; gridCurJuke++){
	    }
	}
    } else {
	gridStep = RoundUp(step);
	gridBase = floor(low / gridStep) * gridStep;
    }
    return(stepGrid());
}

double stepGrid()
{
    if (++gridCurJuke >= gridNJuke) {
	gridCurJuke = 0;
	gridBase += gridStep;
    }
    return(gridBase + gridJuke[gridCurJuke]);
}

double RoundUp(val)
double val;			/* Value */
/*
 * This routine rounds up the given positive number such that
 * it is some power of ten times either 1, 2, or 5.  It is
 * used to find increments for grid lines.
 */
{
    int exponent, idx;

    exponent = (int) floor(nlog10(val));
    if (exponent < 0) {
	for (idx = exponent;  idx < 0; idx++) {
	    val *= 10.0;
	}
    } else {
	for (idx = 0;  idx < exponent; idx++) {
	    val /= 10.0;
	}
    }
    if (val > 5.0) val = 10.0;
    else if (val > 2.0) val = 5.0;
    else if (val > 1.0) val = 2.0;
    else val = 1.0;
    if (exponent < 0) {
	for (idx = exponent;  idx < 0;  idx++) {
	    val /= 10.0;
	}
    } else {
	for (idx = 0;  idx < exponent;  idx++) {
	    val *= 10.0;
	}
    }
    return val;
}

int WriteValue(str, val, expv, logFlag)
char *str;			/* String to write into */
double val;			/* Value to print       */
int expv;			/* Exponent             */
int logFlag;			/* Is this a log axis?  */
/*
 * Writes the value provided into the string in a fixed format
 * consisting of seven characters.  The format is:
 *   -ddd.dd
 */
{
    int idx;

    if (logFlag) {
	if (val == floor(val)) {
	    (void) sprintf(str, "%.0e", pow(10.0, val));
	} else {
	    (void) sprintf(str, "%.2g", pow(10.0, val - floor(val)));
	}
    } else {
	if (expv < 0) {
	    for (idx = expv;  idx < 0;  idx++) {
		val *= 10.0;
	    }
	} else {
	    for (idx = 0;  idx < expv;  idx++) {
		val /= 10.0;
	    }
	}
	(void) sprintf(str, "%.2f", val);
    }
}
	    
    
#define LEFT_CODE	0x01
#define RIGHT_CODE	0x02
#define BOTTOM_CODE	0x04
#define TOP_CODE	0x08

/* Clipping algorithm from Neumann and Sproull by Cohen and Sutherland */
#define C_CODE(xval, yval, rtn) \
rtn = 0; \
if ((xval) < wi->UsrOrgX) rtn = LEFT_CODE; \
else if ((xval) > wi->UsrOppX) rtn = RIGHT_CODE; \
if ((yval) < wi->UsrOrgY) rtn |= BOTTOM_CODE; \
else if ((yval) > wi->UsrOppY) rtn |= TOP_CODE

int DrawData(wi)
LocalWin *wi;
/*
 * This routine draws the data sets themselves using the macros
 * for translating coordinates.
 */
{
    double sx1, sy1, sx2, sy2, tx, ty;
    int idx, subindex;
    int code1, code2, cd, mark_inside;
    int X_idx;
    XSegment *ptr;
    PointList *thisList;
    int markFlag, pixelMarks, bigPixel, colorMark;
    int noLines = PM_BOOL("NoLines");
    int lineWidth = PM_INT("LineWidth");

    set_mark_flags(&markFlag, &pixelMarks, &bigPixel, &colorMark);
    for (idx = 0;  idx < MAXSETS;  idx++) {
	thisList = PlotData[idx].list;
	while (thisList) {
	    X_idx = 0;
	    for (subindex = 0;  subindex < thisList->numPoints-1;  subindex++) {
		/* Put segment in (sx1,sy1) (sx2,sy2) */
		sx1 = thisList->xvec[subindex];
		sy1 = thisList->yvec[subindex];
		sx2 = thisList->xvec[subindex+1];
		sy2 = thisList->yvec[subindex+1];
		/* Now clip to current window boundary */
		C_CODE(sx1, sy1, code1);
		C_CODE(sx2, sy2, code2);
		mark_inside = (code1 == 0);
		while (code1 || code2) {
		    if (code1 & code2) break;
		    cd = (code1 ? code1 : code2);
		    if (cd & LEFT_CODE) {	/* Crosses left edge */
			ty = sy1 + (sy2 - sy1) * (wi->UsrOrgX - sx1) / (sx2 - sx1);
			tx = wi->UsrOrgX;
		    } else if (cd & RIGHT_CODE) { /* Crosses right edge */
			ty = sy1 + (sy2 - sy1) * (wi->UsrOppX - sx1) / (sx2 - sx1);
			tx = wi->UsrOppX;
		    } else if (cd & BOTTOM_CODE) { /* Crosses bottom edge */
			tx = sx1 + (sx2 - sx1) * (wi->UsrOrgY - sy1) / (sy2 - sy1);
			ty = wi->UsrOrgY;
		    } else if (cd & TOP_CODE) { /* Crosses top edge */
			tx = sx1 + (sx2 - sx1) * (wi->UsrOppY - sy1) / (sy2 - sy1);
			ty = wi->UsrOppY;
		    }
		    if (cd == code1) {
			sx1 = tx;  sy1 = ty;
			C_CODE(sx1, sy1, code1);
		    } else {
			sx2 = tx;  sy2 = ty;
			C_CODE(sx2, sy2, code2);
		    }
		}
		if (!code1 && !code2) {
		    /* Add segment to list */
		    Xsegs[X_idx].x1 = SCREENX(wi, sx1);
		    Xsegs[X_idx].y1 = SCREENY(wi, sy1);
		    Xsegs[X_idx].x2 = SCREENX(wi, sx2);
		    Xsegs[X_idx].y2 = SCREENY(wi, sy2);
		    X_idx++;
		}

		/* Draw markers if requested and they are in drawing region */
		if (markFlag && mark_inside) {
		    if (pixelMarks) {
			if (bigPixel) {
			    wi->dev_info.xg_dot(wi->dev_info.user_state,
						Xsegs[X_idx-1].x1, Xsegs[X_idx-1].y1,
						P_DOT, 0, idx % MAXATTR);
			} else {
			    wi->dev_info.xg_dot(wi->dev_info.user_state,
						Xsegs[X_idx-1].x1, Xsegs[X_idx-1].y1,
						P_PIXEL, 0, PIXVALUE(idx));
			}
		    } else {
			/* Distinctive markers */
			wi->dev_info.xg_dot(wi->dev_info.user_state,
					    Xsegs[X_idx-1].x1, Xsegs[X_idx-1].y1,
					    P_MARK, MARKSTYLE(idx),
					    PIXVALUE(idx));
		    }
		}

		/* Draw bar elements if requested */
		if (PM_BOOL("BarGraph")) {
		    int barPixels, baseSpot;
		    XSegment line;

		    barPixels = (int) ((PM_DBL("BarWidth")/wi->XUnitsPerPixel) + 0.5);
		    if (barPixels <= 0) barPixels = 1;
		    baseSpot = SCREENY(wi, PM_DBL("BarBase"));
		    line.x1 = line.x2 = Xsegs[X_idx-1].x1;
		    line.y1 = baseSpot;  line.y2 = Xsegs[X_idx-1].y1;
		    wi->dev_info.xg_seg(wi->dev_info.user_state,
					1, &line, barPixels, L_VAR,
					LINESTYLE(idx), PIXVALUE(idx));
		}
	    }
	    /* Handle last marker */
	    if (markFlag && (thisList->numPoints > 0)) {
		C_CODE(thisList->xvec[thisList->numPoints-1],
		       thisList->yvec[thisList->numPoints-1],
		       mark_inside);
		if (mark_inside == 0) {
		    if (pixelMarks) {
			if (bigPixel) {
			    wi->dev_info.xg_dot(wi->dev_info.user_state,
						Xsegs[X_idx-1].x2, Xsegs[X_idx-1].y2,
						P_DOT, 0, idx % MAXATTR);
			} else {
			    wi->dev_info.xg_dot(wi->dev_info.user_state,
						Xsegs[X_idx-1].x2, Xsegs[X_idx-1].y2,
						P_PIXEL, 0, PIXVALUE(idx));
			}
		    } else {
			/* Distinctive markers */
			wi->dev_info.xg_dot(wi->dev_info.user_state,
					    Xsegs[X_idx-1].x2, Xsegs[X_idx-1].y2,
					    P_MARK, MARKSTYLE(idx),
					    PIXVALUE(idx));
		    }
		}
	    }
	    /* Handle last bar */
	    if ((thisList->numPoints > 0) && PM_BOOL("BarGraph")) {
		int barPixels, baseSpot;
		XSegment line;

		barPixels = (int) ((PM_DBL("BarWidth")/wi->XUnitsPerPixel) + 0.5);
		if (barPixels <= 0) barPixels = 1;
		baseSpot = SCREENY(wi, PM_DBL("BarBase"));
		line.x1 = line.x2 = Xsegs[X_idx-1].x2;
		line.y1 = baseSpot;  line.y2 = Xsegs[X_idx-1].y2;
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				    1, &line, barPixels, L_VAR,
				    LINESTYLE(idx), PIXVALUE(idx));
	    }

	    /* Draw segments */
	    if (thisList->numPoints > 0 && (!noLines) && (X_idx > 0)) {
		ptr = Xsegs;
		while (X_idx > wi->dev_info.max_segs) {
		    wi->dev_info.xg_seg(wi->dev_info.user_state,
					wi->dev_info.max_segs, ptr,
					lineWidth, L_VAR,
					LINESTYLE(idx), PIXVALUE(idx));
		    ptr += wi->dev_info.max_segs;
		    X_idx -= wi->dev_info.max_segs;
		}
		wi->dev_info.xg_seg(wi->dev_info.user_state,
				    X_idx, ptr,
				    lineWidth, L_VAR,
				    LINESTYLE(idx), PIXVALUE(idx));
	    }
	    /* Next subset */
	    thisList = thisList->next;
	}
    }
}



int DrawLegend(wi)
LocalWin *wi;
/*
 * This draws a legend of the data sets displayed.  Only those that
 * will fit are drawn.
 */
{
    int idx, spot, lineLen, oneLen;
    XSegment leg_line;
    int markFlag, pixelMarks, bigPixel, colorMark;

    set_mark_flags(&markFlag, &pixelMarks, &bigPixel, &colorMark);
    spot = wi->XOrgY;
    lineLen = 0;
    /* First pass draws the text */
    for (idx = 0;  idx < MAXSETS;  idx++) {
	if ((PlotData[idx].list) &&
	    (spot + wi->dev_info.axis_height + 2 < wi->XOppY))
	  {
	      /* Meets the criteria */
	      oneLen = strlen(PlotData[idx].setName);
	      if (oneLen > lineLen) lineLen = oneLen;
	      wi->dev_info.xg_text(wi->dev_info.user_state,
				   wi->XOppX + wi->dev_info.bdr_pad,
				   spot+2,
				   PlotData[idx].setName,
				   T_UPPERLEFT, T_AXIS);
	      spot += 2 + wi->dev_info.axis_height + wi->dev_info.bdr_pad;
	  }
    }
    lineLen = lineLen * wi->dev_info.axis_width;
    leg_line.x1 = wi->XOppX + wi->dev_info.bdr_pad;
    leg_line.x2 = leg_line.x1 + lineLen;
    spot = wi->XOrgY;
    /* second pass draws the lines */
    for (idx = 0;  idx < MAXSETS;  idx++) {
	if ((PlotData[idx].list) &&
	    (spot + wi->dev_info.axis_height + 2 < wi->XOppY))
	  {
	      leg_line.y1 = leg_line.y2 = spot - wi->dev_info.legend_pad;
	      wi->dev_info.xg_seg(wi->dev_info.user_state,
				  1, &leg_line, 1, L_VAR,
				  LINESTYLE(idx), PIXVALUE(idx));
	      if (markFlag && !pixelMarks) {
		  wi->dev_info.xg_dot(wi->dev_info.user_state,
				      leg_line.x1, leg_line.y1,
				      P_MARK, MARKSTYLE(idx), PIXVALUE(idx));
		  
	      }
	      spot += 2 + wi->dev_info.axis_height + wi->dev_info.bdr_pad;
	  }
    }
}



static void set_mark_flags(markFlag, pixelMarks, bigPixel, colorMark)
int *markFlag;
int *pixelMarks;
int *bigPixel;
int *colorMark;
/*
 * Determines the values of the old boolean flags based on the
 * new values in the parameters database.
 */
{
    *markFlag = 0;  *pixelMarks = 0;  *colorMark = 0;  *bigPixel = 0;
    if (PM_BOOL("Markers")) {
	*markFlag = 1;  *pixelMarks = 0;  *colorMark = 0;
    }
    if (PM_BOOL("PixelMarkers")) {
	*markFlag = 1;  *pixelMarks = 1;  *bigPixel = 0;
    }
    if (PM_BOOL("LargePixels")) {
	*markFlag = 1;  *pixelMarks = 1;  *bigPixel = 1;
    }
    if (PM_BOOL("StyleMarkers")) {
	*markFlag = 1;  *pixelMarks = 0;  *colorMark = 1;
    }
}



#define RND(val)	((int) ((val) + 0.5))

/*ARGSUSED*/
void do_hardcopy(prog, info, init_fun, dev_spec, file_or_dev, maxdim,
		 ti_fam, ti_size, ax_fam, ax_size, doc_p)
char *prog;			/* Program name for Xdefaults    */
char *info;			/* Some state information        */
int (*init_fun)();		/* Hardcopy init function        */
char *dev_spec;			/* Device specification (if any) */
char *file_or_dev;		/* Filename or device spec       */
double maxdim;			/* Maximum dimension in cm       */
char *ti_fam, *ax_fam;		/* Font family names             */
double ti_size, ax_size;	/* Font sizes in points          */
int doc_p;			/* Documentation predicate       */
/*
 * This routine resets the function pointers to those specified
 * by `init_fun' and causes a screen redisplay.  If `dev_spec'
 * is non-zero,  it will be considered a sprintf string with
 * one %s which will be filled in with `file_or_dev' and fed
 * to popen(3) to obtain a stream.  Otherwise,  `file_or_dev'
 * is considered to be a file and is opened for writing.  The
 * resulting stream is fed to the initialization routine for
 * the device.
 */
{
    LocalWin *curWin = (LocalWin *) info;
    LocalWin thisWin;
    FILE *out_stream;
    char buf[MAXBUFSIZE], err[MAXBUFSIZE], ierr[ERRBUFSIZE];
    char tilde[MAXBUFSIZE*10];
    int final_w, final_h, flags;
    double ratio;

    if (dev_spec) {
	(void) sprintf(buf, dev_spec, file_or_dev);
	out_stream = popen(buf, "w");
	if (!out_stream) {
	    do_error(sprintf(err, "Unable to issue command:\n  %s\n", buf));
	    return;
	}
    } else {
	tildeExpand(tilde, file_or_dev);
	out_stream = fopen(tilde, "w");
	if (!out_stream) {
	    do_error(sprintf(err, "Unable to open file `%s'\n", tilde));
	    return;
	}
    }
    thisWin = *curWin;
    ratio = ((double) thisWin.dev_info.area_w) /
      ((double) thisWin.dev_info.area_h);
    if (thisWin.dev_info.area_w > thisWin.dev_info.area_h) {
	final_w = RND(maxdim * 10000.0);
	final_h = RND(maxdim/ratio * 10000.0);
    } else {
	final_w = RND(maxdim * ratio * 10000.0);
	final_h = RND(maxdim * 10000.0);
    }
    ierr[0] = '\0';
    flags = 0;
    if (doc_p) flags |= D_DOCU;
    if ((*init_fun)(out_stream, final_w, final_h, ti_fam, ti_size,
		    ax_fam, ax_size, flags, &(thisWin.dev_info), ierr)) {
	DrawWindow(&thisWin);
	if (thisWin.dev_info.xg_end) {
	    thisWin.dev_info.xg_end(thisWin.dev_info.user_state);
	}
    } else {
	do_error(ierr);
    }
    if (dev_spec) {
	(void) pclose(out_stream);
    } else {
	(void) fclose(out_stream);
    }
}


static char *tildeExpand(out, in)
char *out;			/* Output space for expanded file name */
char *in;			/* Filename with tilde                 */
/*
 * This routine expands out a file name passed in `in' and places
 * the expanded version in `out'.  It returns `out'.
 */
{
    char username[50], *userPntr;
    struct passwd *userRecord;

    out[0] = '\0';

    /* Skip over the white space in the initial path */
    while ((*in == ' ') || (*in == '\t')) in++;

    /* Tilde? */
    if (in[0] == TILDE) {
	/* Copy user name into 'username' */
	in++;  userPntr = &(username[0]);
	while ((*in != '\0') && (*in != '/')) {
	    *(userPntr++) = *(in++);
	}
	*(userPntr) = '\0';
	/* See if we have to fill in the user name ourselves */
	if (strlen(username) == 0) {
	    userRecord = getpwuid(getuid());
	} else {
	    userRecord = getpwnam(username);
	}
	if (userRecord) {
	    /* Found user in passwd file.  Concatenate user directory */
	    strcat(out, userRecord->pw_dir);
	}
    }

    /* Concantenate remaining portion of file name */
    strcat(out, in);
    return out;
}



#define ERR_MSG_SIZE	2048

/*ARGSUSED*/
static int XErrHandler(disp_ptr, evt)
Display *disp_ptr;
XErrorEvent *evt;
/*
 * Displays a nicely formatted message and core dumps.
 */
{
    char err_buf[ERR_MSG_SIZE], mesg[ERR_MSG_SIZE], number[ERR_MSG_SIZE];
    char *mtype = "XlibMessage";

    XGetErrorText(disp_ptr, evt->error_code, err_buf, ERR_MSG_SIZE);
    (void) fprintf(stderr, "X Error: %s\n", err_buf);
    XGetErrorDatabaseText(disp_ptr, mtype, "MajorCode",
			  "Request Major code %d", mesg, ERR_MSG_SIZE);
    (void) fprintf(stderr, mesg, evt->request_code);
    (void) sprintf(number, "%d", evt->request_code);
    XGetErrorDatabaseText(disp_ptr, "XRequest", number, "", err_buf,
			  ERR_MSG_SIZE);
    (void) fprintf(stderr, " (%s)\n", err_buf);

    abort();
}

