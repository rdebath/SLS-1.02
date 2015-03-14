/*
 * scroll_view.c
 * Dan Heller <argv@sun.com> 1989
 *
 * Display a canvas in a frame.  The canvas displays a window that has
 * lines drawn from the opposite corners and draws a black box in the
 * top left corner.  This canvas may be split in many ways (vertically
 * and/or horizontally), but the repaint routine makes sure that each
 * paint window displays the same thing.

 * This program also demonstrates how to handle splitting views
 * programmatically.  Using the left mouse button splits a view
 * horizontally at the Y location of the mouse.  Using the middle
 * mouse button splits the view vertically at the X location of
 * the mouse.
 */
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>   /* Using Xlib graphicsas well */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/scrollbar.h>
#include <xview/rectlist.h>

Canvas  canvas;
void    events(), repaint_proc(), init_split(), join_split();

main(argc, argv)
int argc;
char *argv[];
{
    Frame       frame;
    Xv_Window   view;
    Rect        *rect;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /*
     * Create a frame that's 300 wide by 150 high -- give it a titlebar
     */
    frame = (Frame)xv_create(XV_NULL, FRAME,
        XV_WIDTH,               300,
        XV_HEIGHT,              150,
        FRAME_LABEL,            argv[0],
        NULL);

    /*
     * Create a canvas that's 500 by 500.  This canvas should not adjust
     * its size if resized.  Install the repaint callback: repaint_proc()
     */
    canvas = (Canvas)xv_create(frame, CANVAS,
        CANVAS_WIDTH,           500,
        CANVAS_HEIGHT,          500,
        CANVAS_AUTO_SHRINK,     FALSE,
        CANVAS_AUTO_EXPAND,     FALSE,
        CANVAS_REPAINT_PROC,    repaint_proc,
        NULL);

    /* Install the callback for events on the first (and only, so far)
     * paint window.  We'll use the default events provided by the canvas.
     */
    xv_set(canvas_paint_window(canvas),
        WIN_EVENT_PROC,         events,
        WIN_CONSUME_EVENTS,     ACTION_SELECT, ACTION_ADJUST, NULL,
        NULL);

    /*
     * There's only one viewport since multi-views cannot be created
     * when creating a canvas.  Install "init" and "destroy" callbacks
     * in the canvas object.  See the corresponding routines for specifics.
     */
    xv_set(canvas,
        OPENWIN_SPLIT,
            OPENWIN_SPLIT_INIT_PROC,    init_split,
            OPENWIN_SPLIT_DESTROY_PROC, join_split,
            NULL,
        NULL);

    /*
     * Attach scrollbars to the canvas.
     */
    xv_create(canvas, SCROLLBAR,
        SCROLLBAR_SPLITTABLE,   TRUE,
        SCROLLBAR_DIRECTION,    SCROLLBAR_VERTICAL,
        NULL);
    xv_create(canvas, SCROLLBAR,
        SCROLLBAR_SPLITTABLE,   TRUE,
        SCROLLBAR_DIRECTION,    SCROLLBAR_HORIZONTAL,
        NULL);

    xv_main_loop(frame);
    exit(0);
}

/*
 * The repaint procedure is called whenever repainting is needed in a
 * paint window.  If the canvas has been split into several views and
 * repainting is necessary, then this repaint procedure is called for
 * each paint window in the canvas.
 */
void
repaint_proc(canvas, paint_window, repaint_area)
Canvas          canvas;
Xv_Window       paint_window;
Rectlist        repaint_area;
{
    Display     *dpy;
    Window      win;
    Xv_Window   pw;
    Rect        *rect;

    /* Get the size of the entire paint window */
    rect = (Rect *)xv_get(paint_window, XV_RECT);

    /* Use Xview graphics to draw lines from opposite corners. */
    xv_vector(paint_window, 0, 0, rect->r_width, rect->r_height, PIX_SET, 1);
    xv_vector(paint_window, rect->r_width, 0, 0, rect->r_height, PIX_SET, 1);

    /* Use Xlib calls to draw a black square in the top corner of the pw */
    dpy = (Display *)XV_DISPLAY_FROM_WINDOW(paint_window);
    win = (Window)xv_get(paint_window, XV_XID);
    XFillRectangle(dpy, win, DefaultGC(dpy, DefaultScreen(dpy)), 10,10, 50,50);
}

/*
 * This routine is installed as the callback for events for the paint
 * window.  If more paint windows are created as a result of a view
 * split, then this routine must be reinstalled in a new view.
 */
void
events(pw, event)
Xv_Window pw;
Event *event;
{
    int code = event_action(event);
    Xv_Window view;
    int i = (int)xv_get(canvas, OPENWIN_NVIEWS);

    /* Not interested in button up events */
    if (win_inputnegevent(event))
        return;

    /* Determine which paint window this event happened in. */
    while (pw != xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, --i) && i > 0)
        ;
    /* The paint window number is "i" -- get the "i"th view window */
    view = xv_get(canvas, OPENWIN_NTH_VIEW, i);

    /* determine which event was passed and deal with it. */
    switch (code) {
        case ACTION_SELECT : case ACTION_ADJUST :
            /*
             * split the view at the appropriate position -- this call
             * will generate a call to init_split below since a new view
             * will have been created.
             */
            printf("split at %d,%d\n", event_x(event), event_y(event));
            xv_set(canvas,
                OPENWIN_SPLIT, /* takes a null-terminated attr-value list */
                    OPENWIN_SPLIT_VIEW,         view,
                    OPENWIN_SPLIT_DIRECTION,    code == ACTION_ADJUST?
                                                    OPENWIN_SPLIT_VERTICAL :
                                                    OPENWIN_SPLIT_HORIZONTAL,
                    OPENWIN_SPLIT_POSITION,     code == ACTION_ADJUST?
                                                    event_x(event) :
                                                    event_y(event),
                    NULL,
                NULL);
            break;
        default:
            return;
    }
    /* indicate which paint window and view window ID's */
    printf("win %x, view: %x\n", pw, view);
}

/*
 * notify this routine whenever two views are joined.
 */
void
join_split(view)
Xv_Window view;
{
    puts("joined view");
}

/*
 * Notify this routine whenever a view is split.  The new view is
 * created and its position is indicated.  This is the first time
 * the new view can be accessed by the program.  Immediately install
 * the callback for events for the new paint window.
 */
void
init_split(oldview, newview, pos)
Xv_Window oldview, newview;
int pos;
{
    xv_set(xv_get(newview, CANVAS_VIEW_PAINT_WINDOW),
        WIN_EVENT_PROC,         events,
        WIN_CONSUME_EVENT,      ACTION_SELECT, ACTION_ADJUST, NULL,
        NULL);
}
