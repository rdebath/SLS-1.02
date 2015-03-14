/*
 * line.c -- demonstrates installing a repaint routine in a canvas.
 * The routine is called whenever the canvas needs to be repainted.
 * This usually occurs when the canvas is exposed or resized.
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame frame;
    void  canvas_repaint_proc();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);

    (void) xv_create(frame, CANVAS,
        CANVAS_REPAINT_PROC,    canvas_repaint_proc,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        NULL);

    xv_main_loop(frame);
}

/*
 * repaint routine draws a line from the top left to the bottom right
 * corners of the window
 */
void
canvas_repaint_proc(canvas, paint_window, dpy, xwin, xrects)
Canvas        canvas;         /* unused */
Xv_Window     paint_window;   /* unused */
Display      *dpy;
Window        xwin;
Xv_xrectlist *xrects;         /* unused */
{
    GC gc;
    int width, height;

    gc = DefaultGC(dpy, DefaultScreen(dpy));
    width = (int)xv_get(paint_window, XV_WIDTH);
    height = (int)xv_get(paint_window, XV_HEIGHT);

    XDrawLine(dpy, xwin, gc, 0, 0, width, height);
}
