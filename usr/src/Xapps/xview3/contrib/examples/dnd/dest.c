/* 
 * dest.c - Example of how to register interest in receiving drag and drop
 *          events and how to complete a drag and drop operation.
 *	
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/dragdrop.h>
#include <xview/xv_xrect.h>
#include <X11/Xlib.h>

#define  DROP_WIDTH	65
#define  DROP_HEIGHT	65

#define  BULLSEYE_SITE	1

Frame 		frame;
Canvas  	canvas;
Panel  		panel;
Xv_drop_site 	drop_site;
Server_image	drop_image;
Server_image	drop_image_inv;
Panel_item	p_string,
		p_length,
		p_host;

Selection_requestor  sel;

int		inverted;

main(argc, argv)
    int argc;
    char **argv;
{
    void	EventProc(),
		PaintCanvas(),
		ResizeCanvas();
    Xv_Font	font;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    frame = xv_create((Window)NULL, FRAME,
				XV_X, 			330,
				XV_Y, 			10,
				XV_WIDTH, 		10,
				XV_LABEL, 	      "Drag & Drop Destination",
				0);
    font = (Xv_Font)xv_find(frame, FONT, FONT_NAME, "lucida-12", NULL);
    if (!font) {
	fprintf(stderr, "Cannot use font: lucida-12.\n");
	font = (Xv_Font)xv_get(frame, XV_FONT);
    }


    panel = xv_create(frame, PANEL,
				PANEL_LAYOUT,		PANEL_VERTICAL,
				XV_FONT,		font,
			 	0);

    p_string = xv_create(panel, PANEL_TEXT,
				PANEL_LABEL_STRING, 	"Dropped Text:",
				PANEL_VALUE_DISPLAY_LENGTH, 50,
				0);
    p_host = xv_create(panel, PANEL_TEXT,
				PANEL_LABEL_STRING, 	"From host:",
				PANEL_VALUE_DISPLAY_LENGTH, 15,
				0);
    p_length = xv_create(panel, PANEL_TEXT,
				PANEL_LABEL_STRING, 	"Length:",
				PANEL_VALUE_DISPLAY_LENGTH, 6,
				0);

    window_fit(panel);

    canvas = xv_create(frame, CANVAS,
				XV_HEIGHT, 		100,
				XV_WIDTH, 		WIN_EXTEND_TO_EDGE,
				XV_X,			0,
				WIN_BELOW, 		panel,
				CANVAS_REPAINT_PROC, 	PaintCanvas,
				CANVAS_RESIZE_PROC, 	ResizeCanvas,
				CANVAS_X_PAINT_WINDOW, 	TRUE,
			        0);

    xv_set(canvas_paint_window(canvas),
				WIN_BIT_GRAVITY, 	ForgetGravity,
				WIN_CONSUME_EVENTS,
						WIN_RESIZE,
						0,
				WIN_EVENT_PROC, 	EventProc,
				0);

    drop_image = xv_create(NULL, SERVER_IMAGE, 
				SERVER_IMAGE_BITMAP_FILE, "./bullseye.bm",
				0);

    drop_image_inv = xv_create(NULL, SERVER_IMAGE, 
				SERVER_IMAGE_BITMAP_FILE, "./bullseyeI.bm",
				0);

				/* Selection requestor object that will be 
				 * passed into dnd_decode_drop() and later used
				 * to make requests to the source of the
				 * drop.
				 */
    sel = xv_create(canvas, SELECTION_REQUESTOR, 0);

				/* This application has one drop site with
				 * site id BULLSEYE_SITE and whose shape will
				 * be described by a rectangle.  If 
				 * animation is supported, it would like to
				 * receive LOC_DRAG, LOC_WINENTER and
				 * LOC_WINEXIT events.
				 */
    drop_site = xv_create(canvas_paint_window(canvas), DROP_SITE_ITEM,
				DROP_SITE_ID,   	BULLSEYE_SITE,
				DROP_SITE_EVENT_MASK,   DND_ENTERLEAVE,
				0);

    inverted = False;

    window_fit(frame);
    xv_main_loop(frame);
    exit(0);
}

void
EventProc(window, event)
    Xv_Window       window;
    Event           *event;
{
    switch (event_action(event)) {
				/* When drop previewing is available, if
				 * the drop site has selected for previewing
				 * events (DROP_SITE_EVENT_MASK) then it will
				 * receive ACTION_DRAG_PREVIEW events from
				 * the source as requested.
				 */
      case ACTION_DRAG_PREVIEW:
	switch(event_id(event)) {
	  case LOC_WINENTER:
	    inverted = True; 
	    break;
	  case LOC_WINEXIT:
	    inverted = False; 
	    break;
	  case LOC_DRAG:
	    break;
	}
	PaintCanvas(NULL, window, XV_DISPLAY_FROM_WINDOW(window),
		    xv_get(window, XV_XID), NULL);
	break;
      case ACTION_DRAG_COPY:
      case ACTION_DRAG_MOVE: {
        Xv_drop_site    ds;
	Xv_Server	server = XV_SERVER_FROM_WINDOW(event_window(event));

				/* If the user dropped over an acceptable
				 * drop site, the owner of the drop site will
				 * be sent an ACTION_DROP_{COPY, MOVE} event.
				 */
				/* To acknowledge the drop and to associate the
				 * rank of the source's selection to our
				 * requestor selection object, we call
				 * dnd_decode_drop().
				 */
	if ((ds = dnd_decode_drop(sel, event)) != XV_ERROR) {
				/* We can use the macro dnd_site_id() to access
				 * the site id of the drop site that was
				 * dropped on.
				 */
	    if (xv_get(ds, DROP_SITE_ID) == BULLSEYE_SITE)
		UpdatePanel(server, sel);

				/* If this is a move operation, we must ask
				 * the source to delete the selection object.
				 * We should only do this if the transfer of
				 * data was successful.
				 */
	    if (event_action(event) == ACTION_DRAG_MOVE) {
		int length, format;

                xv_set(sel, SEL_TYPE_NAME, "DELETE", 0);
                (void)xv_get(sel, SEL_DATA, &length, &format);
	    }

				/* To complete the drag and drop operation,
				 * we tell the source that we are all done.
				 */
	    dnd_done(sel);
	    inverted = False;
	    PaintCanvas(NULL, window, XV_DISPLAY_FROM_WINDOW(window),
		        xv_get(window, XV_XID), NULL);

	} else
	    printf ("drop error\n");
	break;
      }
      default:
        break;
    }
}


UpdatePanel(server, sel)
    Xv_Server		server;
    Selection_requestor	sel;
{
    int 	 length,
		 format,
		*string_length;
    char 	 buf[7],
		*string,
		*hostname;

    xv_set(sel, SEL_TYPE, XA_STRING, 0);
    string = (char *)xv_get(sel, SEL_DATA, &length, &format);
    if (length != SEL_ERROR) {
    	xv_set(p_string, PANEL_VALUE, string, 0);
    	free (string);
    }

    xv_set(sel, SEL_TYPE, xv_get(server, SERVER_ATOM, "LENGTH"), 0);
    string_length = (int *)xv_get(sel, SEL_DATA, &length, &format);
    if (length != SEL_ERROR) {
	sprintf(buf, "%d", *string_length);
        xv_set(p_length, PANEL_VALUE, buf, 0);
        free ((char *)string_length);
    }

    xv_set(sel, SEL_TYPE, xv_get(server, SERVER_ATOM, "HOST_NAME"), 0);
    hostname = (char *)xv_get(sel, SEL_DATA, &length, &format);
    if (length != SEL_ERROR) {
    	xv_set(p_host, PANEL_VALUE, hostname, 0);
    	free (hostname);
    }
    xv_set(sel, SEL_TYPE_NAME, "_SUN_SELECTION_END", 0);
    (void)xv_get(sel, SEL_DATA, &length, &format);
}

void
PaintCanvas(canvas, paint_window, dpy, xwin, xrects)
    Canvas        canvas;         /* unused */
    Xv_Window     paint_window;   /* unused */
    Display      *dpy;
    Window        xwin;
    Xv_xrectlist *xrects;         /* unused */
{
    static GC   gc;
    static int  gcCreated = False;
    static int  lastMode = False;
    int         width, height;  
    int         x, y;  
    Rect	*r;

    if (!gcCreated) {
        XGCValues gcv;
        gcv.stipple = (Pixmap) xv_get(drop_image, XV_XID);
        gcv.foreground = BlackPixel(dpy, XDefaultScreen(dpy));
        gcv.background = WhitePixel(dpy, XDefaultScreen(dpy));
        gcv.fill_style = FillStippled;
        gc = XCreateGC(dpy, xwin, GCStipple|GCForeground|GCBackground|
                                                             GCFillStyle, &gcv);
    }

    if (lastMode != inverted) {
	if (!inverted)
	    XSetStipple(dpy, gc, (Pixmap) xv_get(drop_image, XV_XID));
	else 
	    XSetStipple(dpy, gc, (Pixmap) xv_get(drop_image_inv, XV_XID));
	lastMode = inverted;
    }

    width = xv_get(paint_window, XV_WIDTH);
    height = xv_get(paint_window, XV_HEIGHT);

    x = (width/2)-(DROP_WIDTH/2);
    y = (height/2)-(DROP_HEIGHT/2);

    XClearArea(dpy, xwin, x, y, DROP_WIDTH, DROP_HEIGHT, False);
    XSetTSOrigin(dpy, gc, x, y);
    XFillRectangle(dpy, xwin, gc, x, y, DROP_WIDTH, DROP_HEIGHT);
}

void 
ResizeCanvas(canvas, width, height)
    Canvas 	canvas;
    int 	width;
    int 	height;
{
    int         x, y;  
    Rect	rect;

    x = (width/2)-(DROP_WIDTH/2);
    y = (height/2)-(DROP_HEIGHT/2);

    rect.r_left = x;
    rect.r_top = y;
    rect.r_width = DROP_WIDTH;
    rect.r_height = DROP_HEIGHT;

			/* Update the drop site information. */
    xv_set(drop_site, DROP_SITE_DELETE_REGION_PTR, NULL,
		      DROP_SITE_REGION, &rect,
		      0);
}
