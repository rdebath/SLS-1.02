/*
 * hot_spot.c -- create a cursor and query it's position on the 
 * screen and in the panel's window.
 * Our own function, create_cursor(), attaches a new cursor to the
 * window parameter passed into the function.
 */
#include <X11/X.h>
#include <X11/Xlib.h>           /* for the xlib graphics */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/svrimage.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame       frame;
    Panel       panel;
    void        do_it();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /*
     * Create a base frame, a panel, and a panel button.
     */
    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    create_cursor(xv_get(panel, CANVAS_NTH_PAINT_WINDOW, 0));
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Push Me",
        PANEL_NOTIFY_PROC,      do_it,
        NULL);

    window_fit(panel);
    window_fit(frame);
    window_main_loop(frame);
}

/*
 * when user selects the panel button, the current mouse location is
 * printed relative to the panel's window and to the screen.
 * This location is governed by the hot spot on the cursor.
 */
void
do_it(item, event)
{
    Rect *r;
    Panel panel = (Panel)xv_get(item, PANEL_PARENT_PANEL);

    r = (Rect *)xv_get(xv_get(panel, XV_ROOT), WIN_MOUSE_XY);
    printf("Root window: %d %d\n", r->r_left, r->r_top);
    r = (Rect *)xv_get(xv_get(panel, CANVAS_NTH_PAINT_WINDOW, 0), WIN_MOUSE_XY);
    printf("Panel window: %d %d\n", r->r_left, r->r_top);
}

/*
 * create_cursor() creates a bull's eye cursor and assigns it
 * to the window (parameter).
 */
create_cursor(window)
Xv_Window window;
{
    Xv_Cursor      cursor;
    Server_image   image;
    Pixmap         pixmap;
    Display        *dpy = (Display *)xv_get(window, XV_DISPLAY);
    GC             gc;
    XGCValues      gcvalues;

    image = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
        XV_WIDTH,       16,
        XV_HEIGHT,      16,
        NULL);
    pixmap = (Pixmap)xv_get(image, XV_XID);
    /* Create GC with reversed foreground and background colors to
     * clear pixmap first.  Use 1 and 0 because pixmap is 1-bit deep.
     */
    gcvalues.foreground = 0;
    gcvalues.background = 1;
    gc = XCreateGC(dpy, pixmap, GCForeground|GCBackground, &gcvalues);
    XFillRectangle(dpy, pixmap, gc, 0, 0, 16, 16);
    /*
     * Reset foreground and background values for XDrawArc() routines.
     */
    gcvalues.foreground = 1;
    gcvalues.background = 0;
    XChangeGC(dpy, gc, GCForeground | GCBackground, &gcvalues);
    XDrawArc(dpy, pixmap, gc, 2, 2, 12, 12, 0, 360 * 64);
    XDrawArc(dpy, pixmap, gc, 6, 6, 4, 4, 0, 360 * 64);

    /* Creaste cursor and assign it to the window (parameter) */
    cursor = xv_create(XV_NULL, CURSOR,
        CURSOR_IMAGE,   image,
        CURSOR_XHOT,    7,
        CURSOR_YHOT,    7,
        NULL);
    xv_set(window, WIN_CURSOR, cursor, NULL);

    /* free the GC -- the cursor and the image must not be freed. */
    XFreeGC(dpy, gc);
}
