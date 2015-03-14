/*
 * multi_display.c -- display a control panel which contains buttons
 * which allow a base frame to be created on either screen controlled
 * by the same X11 server.  In order for this program to work, you
 * must have two screens.
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <xview/xv_xrect.h>

#define WIDTH		448
#define HEIGHT		500

Frame      baseFr;
Panel	   panel;
Display    *display;
Xv_Window  win_0, win_1;
Xv_Screen  scrn_0, scrn_1;
GC         gc_0, gc_1;

void      Display0(), Display1(), QuitTest();

main(argc,argv)
int	argc;
char	*argv[];
{
    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    /* Get the default servers's 0, and 1th screens */
    scrn_0 = (Xv_Screen) xv_get( xv_default_server, SERVER_NTH_SCREEN, 0 );
    scrn_1 = (Xv_Screen) xv_get( xv_default_server, SERVER_NTH_SCREEN, 1 );

    /* Get the root window for the screen 0 and screen 1 */
    win_0 = (Xv_Window) xv_get( scrn_0, XV_ROOT );
    win_1 = (Xv_Window) xv_get( scrn_1, XV_ROOT );

    baseFr = xv_create( win_0, FRAME,
		       FRAME_LABEL,	"Control Panel",
		       XV_WIDTH, 250,
		       XV_HEIGHT, 150,
		       0);

    display = (Display *) xv_get( baseFr, XV_DISPLAY );    

    panel = xv_create( baseFr, PANEL, 
		      XV_X, 0,
		      XV_Y, 0,
		      XV_WIDTH, WIN_EXTEND_TO_EDGE,
		      XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		      OPENWIN_SHOW_BORDERS, FALSE,
		      0);

    xv_create(panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Quit", 
	      PANEL_NOTIFY_PROC, QuitTest, 0);

    xv_create(panel, PANEL_BUTTON,
	      XV_X, 5,
	      XV_Y, 50,
	      PANEL_LABEL_STRING, "Display On unix:0.0", 
	      PANEL_NOTIFY_PROC, Display0, 0);

    xv_create(panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Display On unix:0.1", 
	      PANEL_NOTIFY_PROC, Display1, 0);

    xv_main_loop(baseFr);
    return 0;
}

void QuitTest(item, event)
  Panel_item item;
  Event *event;
{
  xv_destroy(panel);
  xv_destroy(baseFr);
  exit(0);
}

void Display0(item, event)
  Panel_item item;
  Event *event;
{
    Canvas    canvas;
    void      can_0_repaint_proc();
    Pixfont   *demoFont;
    Frame     frame;
    
    frame = xv_create(win_0, FRAME,
		      FRAME_LABEL,	"unix:0.0",
		      XV_WIDTH,	WIDTH,
		      XV_HEIGHT,	HEIGHT,
		      XV_SHOW, TRUE,
		      0);

    canvas = xv_create(frame, CANVAS, 
		       CANVAS_X_PAINT_WINDOW,   TRUE,
		       CANVAS_REPAINT_PROC,     can_0_repaint_proc,
		       0); 
    gc_0 = DefaultGC( display, 0 );
    demoFont = (Pixfont *) xv_create( 0, FONT,FONT_NAME,
				"lucidasans-BoldItalic-18", 
				0 );
    if ( !demoFont )  {
	fprintf( stderr,"Can't find lucidasans-14.\n" );
	exit(1);
    }
    XSetFont( display, gc_0, xv_get( demoFont, XV_XID ) );
    XSetLineAttributes( display, gc_0, 6, LineSolid, CapRound, JoinRound );
}


void Display1(item, event)
  Panel_item item;
  Event *event;
{
    Canvas    canvas;
    void      can_1_repaint_proc();
    Pixfont   *demoFont;
    Frame     frame;
    
    frame = xv_create(win_1, FRAME,
		      FRAME_LABEL,	"unix:0.1",
		      XV_WIDTH,	550,
		      XV_HEIGHT, 500,
		      XV_SHOW, TRUE,
		      0);

    canvas = xv_create(frame, CANVAS, 
		       CANVAS_X_PAINT_WINDOW,   TRUE,
		       CANVAS_REPAINT_PROC,     can_1_repaint_proc,
		       0); 
    gc_1 = DefaultGC( display, 1 );
    demoFont = (Pixfont *) xv_create( 0,FONT,FONT_NAME,"lucidasans-BoldItalic-14",0);
    if ( !demoFont )  {
	fprintf( stderr,"Can't find lucidasans-BoldItalic-14.\n" );
	exit(1);
    }
    XSetFont( display, gc_1, xv_get( demoFont, XV_XID ) );
    XSetLineAttributes( display, gc_1, 6, LineSolid, CapRound, JoinRound );
}


void
can_0_repaint_proc(canvas, pw, display, xid, xrects)
    Canvas      canvas;
    Xv_Window   pw;
    Display     *display;
    Window      xid;
    Xv_xrectlist *xrects;
{
    int           width, height;
    XPoint        pts[3];
    XGCValues     gcValue;
    unsigned long gcValMask;
    
    /* Set clip rects, if any */
    if (xrects)  {
	XSetClipRectangles(display,gc_0,0,0,xrects->rect_array, xrects->count,Unsorted);
    }
    else {
	gcValue.clip_mask = None;
	XChangeGC( display, gc_0, GCClipMask, &gcValue );
    }
    
    width = (int) xv_get( pw, XV_WIDTH );
    height = (int) xv_get( pw, XV_HEIGHT );

    XDrawString( display, xid, gc_0, width/4, height/2, "Multi Headed XView!!!", 21 );
    
    XDrawLine( display, xid, gc_0, 0, height/2, width/2, 0 );
    XDrawLine( display, xid, gc_0, 0, height/2, width/2, height );
    XDrawLine( display, xid, gc_0, width/2, 0, width, height/2 );
    XDrawLine( display, xid, gc_0, width, height/2, width/2, height );

    pts[0].x = 0;          pts[0].y = 0;
    pts[1].x = 0;          pts[1].y = height/2;
    pts[2].x = width/2;    pts[2].y = 0;
    XFillPolygon( display, xid, gc_0, pts, 3, Convex, CoordModeOrigin );


    pts[0].x = height;     pts[0].y = 0;
    pts[1].x = width/2;    pts[1].y = 0;
    pts[2].x = width;      pts[2].y = height/2;
    XFillPolygon( display, xid, gc_0, pts, 3, Convex, CoordModeOrigin );


    pts[0].x = height;     pts[0].y = width;
    pts[1].x = width;      pts[1].y = height/2;
    pts[2].x = width/2;    pts[2].y = height;
    XFillPolygon( display, xid, gc_0, pts, 3, Convex, CoordModeOrigin );

    gcValue.fill_style = FillTiled;
    gcValue.function = GXcopy;
    gcValMask = GCFillStyle | GCFunction;
    XChangeGC( display, gc_0, gcValMask, &gcValue );

    pts[0].x = 0;          pts[0].y = height;
    pts[1].x = 0;          pts[1].y = height/2;
    pts[2].x = width/2;    pts[2].y = height;
    XFillPolygon( display, xid, gc_0, pts, 3, Convex, CoordModeOrigin );
}


void
can_1_repaint_proc(canvas, pw, display, xid, xrects)
    Canvas      canvas;
    Xv_Window   pw;
    Display     *display;
    Window      xid;
    Xv_xrectlist *xrects;
{
    int           width, height;
    XPoint        pts[4];
    XGCValues     gcValue;
    unsigned long gcValMask;
    
    /* Set clip rects, if any */
    if (xrects)  {
	XSetClipRectangles(display,gc_1,0,0,xrects->rect_array, xrects->count,Unsorted);
    }
    else {
	gcValue.clip_mask = None;
	XChangeGC( display, gc_1, GCClipMask, &gcValue );
    }
    
    width = (int) xv_get( pw, XV_WIDTH );
    height = (int) xv_get( pw, XV_HEIGHT );

    XDrawString(display,xid,gc_1,(5*width)/8,(5*height)/8,"Multi Headed XView!!!", 21 );
    
    XDrawLine( display, xid, gc_1, (3 * width )/4, height/4, width/2, (5*height)/8 );
    XDrawLine( display, xid, gc_1, width/2, (5*height)/8, (3 * width)/4, height );
    XDrawLine( display, xid, gc_1, (3*width)/4, height, width, (5*height)/8 );
    XDrawLine( display, xid, gc_1, width, (5*height)/8, (3*width)/4, height/4 );

    pts[0].x = 0;            pts[0].y = 0;
    pts[1].x = (3*width)/4;  pts[1].y = height/4;
    pts[2].x = width/2;      pts[2].y = (5*height)/8;
    pts[3].x = (3*width)/4;  pts[3].y = height;
    XFillPolygon( display, xid, gc_1, pts, 4, Convex, CoordModeOrigin );
}


