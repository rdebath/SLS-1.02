/*
 * source1.c - Example of how to source a drag and drop operation.
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/cursor.h>
#include <xview/dragdrop.h>
#include <xview/sel_pkg.h>
#include <xview/xv_xrect.h>
#include <xview/svrimage.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>

short drop_icon[] = {
#include "./drop.icon"
};

#define  POINT_IN_RECT(px, py, rx, ry, rw, rh) \
			((px) >= rx && (py) >= ry && \
			(px) < rx+rw && (py) < ry+rh)

#define  STRING_MSG	"chromosome: DNA-containing body of the cell nucleus."

#define  HOST  	0
#define  STRING 1
#define  LENGTH 2

Frame 		frame;
Canvas  	canvas;
Dnd		dnd;
Cursor		arrow_cursor;
Server_image	arrow_image;
Server_image	arrow_image_mask;
Server_image	box_image;
Server_image	drop_here_image;
Cursor		drop_here_cursor;
Selection_owner sel;
Selection_item  selItem[5];
Atom		selAtom[5];

int  SelectionConvert();
extern int sel_convert_proc();

static XColor   fg = {0L, 65535, 65535, 65535};
static XColor   bg = {0L, 0, 0, 0};

typedef struct _DragObject {
    Server_image  image;
    int		  x, y;
    unsigned int  w, h;
    int		  inverted;
} DragObject;

DragObject  dO;

main(argc, argv)
    int    argc;
    char **argv;
{
    void   	EventProc(),
	   	SelectionLose(),
	   	PaintCanvas();
    Xv_Server 	server;
    Cursor	arror_cursor;

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    frame = xv_create((Window)NULL, FRAME,
			XV_LABEL, 		"Drag & Drop Source",
			XV_X, 			10,
			XV_Y, 			10,
			FRAME_SHOW_FOOTER,	True,
			0);

    canvas = xv_create(frame, CANVAS,
                        XV_HEIGHT, 		100,
                        XV_WIDTH, 		300,
                        CANVAS_REPAINT_PROC, 	PaintCanvas,
                        CANVAS_X_PAINT_WINDOW, 	TRUE,
                        0);
 
    xv_set(canvas_paint_window(canvas),
                        WIN_BIT_GRAVITY, 	ForgetGravity,
    			WIN_CONSUME_EVENTS,
			    	WIN_MOUSE_BUTTONS,
			    	LOC_DRAG,
                            	WIN_RESIZE,
                        	0,
                        WIN_EVENT_PROC, 	EventProc,
                        0);

			/* Create the drag cursor images */
    arrow_image = xv_create(NULL, SERVER_IMAGE, 
			SERVER_IMAGE_BITMAP_FILE, "arrow.bm", NULL);
    arrow_image_mask = xv_create(NULL, SERVER_IMAGE, 
			SERVER_IMAGE_BITMAP_FILE, "arrow_mask.bm", NULL);
			/* Create a cursor to use in dnd ops. */
    arrow_cursor = XCreatePixmapCursor(XV_DISPLAY_FROM_WINDOW(canvas),
			(XID)xv_get(arrow_image, XV_XID),
			(XID)xv_get(arrow_image_mask, XV_XID),
			&fg, &bg, 61, 3);

    drop_here_image = xv_create(NULL, SERVER_IMAGE, 
			XV_WIDTH,		64,
			XV_HEIGHT,		64,
			SERVER_IMAGE_BITS,	drop_icon,
			0);

    drop_here_cursor = XCreatePixmapCursor(XV_DISPLAY_FROM_WINDOW(canvas),
			(XID)xv_get(drop_here_image, XV_XID),
			(XID)xv_get(drop_here_image, XV_XID),
			&fg, &bg, 32, 32);

    dO.image = xv_create(NULL, SERVER_IMAGE, 
			SERVER_IMAGE_BITMAP_FILE, "arrowb.bm", NULL);
    dO.w = xv_get(dO.image, XV_WIDTH);
    dO.h = xv_get(dO.image, XV_HEIGHT);
    dO.inverted = False;

    CreateSelection(server, canvas_paint_window(canvas));

    window_fit(frame);

    xv_main_loop(frame);
    exit(0);

}

CreateSelection(server, window)
    Xv_Server	server;
    Xv_object   window;
{
    char   name[15];
    int	   len;

			/* Primary selection, acquired whenever the arrow
			 * bitmap is selected by the user.
			 * This is not a requirement for dnd to work.
			 */
    sel = xv_create(window, SELECTION_OWNER,
			SEL_RANK,		XA_PRIMARY,
			SEL_LOSE_PROC,  	SelectionLose,
			0);

		
			/* Create the drag and drop object.  */
    dnd = xv_create(window, DRAGDROP,
   			DND_TYPE, 		DND_COPY, 
			DND_X_CURSOR,		arrow_cursor,
			DND_ACCEPT_X_CURSOR,	drop_here_cursor,
			SEL_CONVERT_PROC,	SelectionConvert,
			0);

			/* Associate some selection items with the dnd object.*/
    (void) gethostname(name, 15);
    selAtom[HOST] = (Atom)xv_get(server, SERVER_ATOM, "HOST_NAME");
    selItem[HOST] = xv_create(dnd, SELECTION_ITEM,
			SEL_TYPE, 		selAtom[HOST],
			SEL_DATA, 		(Xv_opaque)name,
			0);

    selAtom[STRING] = (Atom)XA_STRING;
    selItem[STRING] = xv_create(dnd, SELECTION_ITEM,
			SEL_TYPE, 		selAtom[STRING],
			SEL_DATA, 		(Xv_opaque)STRING_MSG,
			0);

    len = strlen(STRING_MSG);
    selAtom[LENGTH] = (Atom)xv_get(server, SERVER_ATOM, "LENGTH");
    selItem[LENGTH] = xv_create(dnd, SELECTION_ITEM,
			SEL_TYPE, 		selAtom[LENGTH],
			SEL_FORMAT,		sizeof(int)*NBBY,
			SEL_LENGTH,		1,
			SEL_DATA, 		(Xv_opaque)&len,
			0);
}

void
EventProc(window, event)
Xv_Window       window;
Event           *event;
{
    static int drag_pixels = 0;
    static int dragging = False;

    switch (event_action(event)) {
      case ACTION_SELECT:
	if (event_is_down(event)) {
	    dragging = False;
			/* If the user selected our dnd object, highlight
			 * the box and acquire the primary selection. 
			 */
	    if (POINT_IN_RECT(event_x(event), event_y(event), 
				     dO.x, dO.y, dO.w, dO.h)) {
		xv_set(sel, SEL_OWN, True, 0);
		dO.inverted = True;
		PaintObject(dO, xv_get(window, XV_XID),
					        XV_DISPLAY_FROM_WINDOW(window));
	    } else
			/* If the user selected outside of the dnd object,
			 * de-highlight the object. And release the primary
			 * selection.
			 */
		xv_set(sel, SEL_OWN, False, 0);
	} else
	    drag_pixels = 0;
        break;
      case LOC_DRAG:
			/* If the user dragged at least five pixel over our
			 * dnd object, begin the dnd operation.
			 */
        if (event_left_is_down(event)) {
	    if (POINT_IN_RECT(event_x(event),
			      event_y(event),dO.x,dO.y,dO.w,dO.h))
		dragging = True;
	    
            if (dragging && drag_pixels++ == 5) {
	        xv_set(frame, FRAME_LEFT_FOOTER, "Drag and Drop:", 0);
	        switch (dnd_send_drop(dnd)) {
	    	  case XV_OK:
		      xv_set(frame, FRAME_LEFT_FOOTER,
				  	"Drag and Drop: Began", 0);
		      break;
	          case DND_TIMEOUT:
		      xv_set(frame, FRAME_LEFT_FOOTER,
					"Drag and Drop: Timed Out",0);
		      break;
	          case DND_ILLEGAL_TARGET:
		      xv_set(frame, FRAME_LEFT_FOOTER,
					"Drag and Drop: Illegal Target",0);
		      break;
	          case DND_SELECTION:
		      xv_set(frame, FRAME_LEFT_FOOTER,
					"Drag and Drop: Bad Selection",0);
		      break;
	          case DND_ROOT:
		      xv_set(frame, FRAME_LEFT_FOOTER,
					"Drag and Drop: Root Window",0);
		      break;
	          case XV_ERROR:
		      xv_set(frame, FRAME_LEFT_FOOTER,
					"Drag and Drop: Failed",0);
		      break;
	        }
            drag_pixels = 0;
            }
	}
        break;
    }
}

PaintObject(object, win, dpy)
    DragObject   object;
    Window	 win;
    Display     *dpy;
{
    static GC   gc;
    static int  gcCreated = False;

    if (!gcCreated) {
        XGCValues gcv;
        gcv.stipple = (Pixmap) xv_get(object.image, XV_XID);
        gcv.fill_style = FillStippled;
        gc = XCreateGC(dpy, win, GCStipple|GCForeground|GCBackground|
                                                             GCFillStyle, &gcv);
        XSetForeground(dpy, gc, BlackPixel(dpy, XDefaultScreen(dpy)));
        XSetBackground(dpy, gc, WhitePixel(dpy, XDefaultScreen(dpy)));
    }

    if (object.inverted) {
	XSetFillStyle(dpy, gc, FillSolid);
	XDrawRectangle(dpy, win, gc, object.x-1, object.y-1, 66, 66);
	XSetFillStyle(dpy, gc, FillStippled);
    } else
	XClearWindow(dpy, win);

    XSetTSOrigin(dpy, gc, object.x, object.y);
    XFillRectangle(dpy, win, gc, object.x, object.y, 65, 65);
}

void
PaintCanvas(canvas, paint_window, dpy, xwin, xrects)
    Canvas        canvas;         /* unused */
    Xv_Window     paint_window;   /* unused */
    Display      *dpy;
    Window        xwin;
    Xv_xrectlist *xrects;         /* unused */
{
    unsigned    width, height;  
    int         x, y;  

    width = xv_get(paint_window, XV_WIDTH);
    height = xv_get(paint_window, XV_HEIGHT);

    x = (width/2)-(dO.w/2);
    y = (height/2)-(dO.h/2);

    dO.x = x;
    dO.y = y;

    PaintObject(dO, xwin, dpy);
}

/* The convert proc is called whenever someone makes a request to the dnd
 * selection.  Two cases we handle within the convert proc: DELETE and
 * _SUN_DRAGDROP_DONE.  Everything else we pass on to the default convert
 * proc which knows about our selection items.
 */
int
SelectionConvert(seln, type, data, length, format)
    Selection_owner	 seln;
    Atom		*type;
    Xv_opaque		*data;
    long		*length;
    int			*format;
{
    Xv_Server 		 server = XV_SERVER_FROM_WINDOW(xv_get(seln, XV_OWNER));

    if (*type == (Atom)xv_get(server, SERVER_ATOM, "_SUN_SELECTION_END")) {
			/* Destination has told us it has completed the drag
			 * and drop transaction.  We should respond with a
			 * zero-length NULL reply.
			 */
	xv_set(dnd, SEL_OWN, False, 0);
	xv_set(frame, FRAME_LEFT_FOOTER, "Drag and Drop: Completed",0);
	*format = 32;
	*length = 0;
	*data = NULL;
	*type = (Atom)xv_get(server, SERVER_ATOM, "NULL");
	return(True);
    } else if (*type == (Atom)xv_get(server, SERVER_ATOM, "DELETE")) {
			/* Destination asked us to delete the selection.
			 * If it is appropriate to do so, we should.
			 */
	*format = 32;
	*length = 0;
	*data = NULL;
	*type = (Atom)xv_get(server, SERVER_ATOM, "NULL");
	return(True);
    } else
			/* Let the default convert procedure deal with the
			 * request.
			 */
	return(sel_convert_proc(seln, type, data, length, format));
}

/* When we lose the primary selection, this procedure is called.  We dehigh-
 * light our selection.
 */
void
SelectionLose(seln)
    Selection_owner seln;
{
    Xv_Window	owner = xv_get(seln, XV_OWNER);

    if (xv_get(seln, SEL_RANK) == XA_PRIMARY) {
        dO.inverted = False;
        PaintObject(dO, xv_get(owner, XV_XID), XV_DISPLAY_FROM_WINDOW(owner));
    }
}
