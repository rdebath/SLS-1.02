/*
 * simple_font.c -- very simple program showing how to render text
 * using a font gotten from xv_find().  Hello World is printed in
 * the top-left corner of a canvas window.
 */
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>   /* X.h and Xlib.h used for Xlib graphics */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <xview/xv_xrect.h>

#define GC_KEY  10 /* any arbitrary number -- used for XV_KEY_DATA */

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Canvas      canvas;
    XGCValues   gcvalues;
    Xv_Font     font;
    void        my_repaint_proc();
    Display     *dpy;
    GC          gc;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               400,
        XV_HEIGHT,              200,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_REPAINT_PROC,    my_repaint_proc,
        NULL);
    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    font = (Xv_Font)xv_find(frame, FONT, FONT_NAME, "courier", NULL);
    if (!font) {
        fprintf(stderr, "%s: cannot use font: courier.\n", argv[0]);
        font = (Xv_Font)xv_get(frame, XV_FONT);
    }

    /* Create a GC to use with Xlib graphics -- set the fg/bg colors
     * and set the Font, which is the XV_XID of the XView font object.
     */
    gcvalues.font = (Font)xv_get(font, XV_XID);
    gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
    gcvalues.graphics_exposures = False;
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCForeground | GCBackground | GCFont | GCGraphicsExposures,
        &gcvalues);

    /* Assign the gc to the canvas object so we can use the same gc
     * each time we draw into the canvas.  Also avoids a global
     * variable to store the GC.
     */
    xv_set(canvas, XV_KEY_DATA, GC_KEY, gc, NULL);
    xv_main_loop(frame);
}

/*
 * Called every time the window needs repainting.
 */
void
my_repaint_proc(canvas, pw, dpy, xwin, xrects)
Canvas          canvas;
Xv_Window       pw;
Display         *dpy;
Window          xwin;
Xv_xrectlist    *xrects;
{
    GC gc = (GC)xv_get(canvas, XV_KEY_DATA, GC_KEY);

    XDrawString(dpy, xwin, gc, 10, 20,
        "Hello World", 11); /* 11 = strlen("Hello World") */
}
