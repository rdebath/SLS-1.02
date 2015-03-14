/* color_logo.c --
 *  This program demonstrates the combined use of the XView color
 *  model/API and Xlib graphics calls. The program uses XView to
 *  create and manage its colormap segment while doing its actual
 *  drawing using Xlib routines.
 *  The program draws the X logo in red, green and blue in a canvas.
 */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include <xview/xv_xrect.h>
#include <X11/bitmaps/xlogo64>

/* Color indices */
#define WHITE           0
#define RED             1
#define GREEN           2
#define BLUE            3
#define NUM_COLORS      4

GC gc;                      /* used for rendering logos */
unsigned long *pixel_table; /* pixel values for colors */
Pixmap        xlogo;        /* the xlogo */

/* Create a frame, canvas, and a colormap segment and assign the
 * cms to the canvas.  CMS_INDEX_TABLE returns the actual colormap
 * indices and are used to set the gc's foreground for XCopyPlane
 * calls.
 */
main(argc,argv)
int     argc;
char    *argv[];
{
    Frame         frame;
    XGCValues     gc_val;
    XGCValues     gcvalues;
    void          canvas_repaint_proc();
    Cms           cms;
    static Xv_singlecolor colors[] = {
        { 255, 255, 255 }, /* white */
        { 255,   0,   0 }, /* red */
        { 0,   255,   0 }, /* green */
        { 0,     0, 255 }, /* blue */
    };

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    cms = (Cms) xv_create(NULL, CMS,
        CMS_SIZE, 4,
        CMS_COLORS, colors,
        NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,    argv[0],
        XV_WIDTH,       448,
        XV_HEIGHT,      192,
        NULL);

    (void) xv_create(frame, CANVAS,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_REPAINT_PROC,    canvas_repaint_proc,
        WIN_CMS,                cms,
        NULL);

    /* Get the actual indices into the colormap */
    pixel_table = (unsigned long *)xv_get(cms, CMS_INDEX_TABLE);

    /* create the xlogo -- get display/window from the frame obj */
    xlogo = XCreateBitmapFromData(
        xv_get(frame, XV_DISPLAY), xv_get(frame, XV_XID),
        xlogo64_bits, xlogo64_width, xlogo64_height);

    /* setup gc for rendering logos to screen */
    gcvalues.graphics_exposures = False;
    gcvalues.background = pixel_table[WHITE];
    gc = XCreateGC(xv_get(frame, XV_DISPLAY), xv_get(frame, XV_XID),
        GCBackground | GCGraphicsExposures, &gcvalues);

    xv_main_loop(frame);
}

/* Draws onto the canvas using Xlib drawing functions.
 * Draw the X logo into the window in three colors. In each case,
 * change the GC's foreground color to the pixel value specified.
 */
void
canvas_repaint_proc(canvas, pw, display, win, xrects)
Canvas      canvas;   /* unused */
Xv_Window   pw;       /* unused */
Display     *display;
Window      win;
Xv_xrectlist *xrects; /* unused */
{
    /* Use XCopyPlane because the logo is a 1-bit deep pixmap. */
    XSetForeground(display, gc, pixel_table[RED]);
    XCopyPlane(display, xlogo, win, gc, 0, 0,
        xlogo64_width, xlogo64_height, 64, 64, 1);

    XSetForeground(display, gc, pixel_table[GREEN]);
    XCopyPlane(display, xlogo, win, gc, 0, 0,
        xlogo64_width, xlogo64_height, 192, 64, 1);

    XSetForeground(display, gc, pixel_table[BLUE]);
    XCopyPlane(display, xlogo, win, gc, 0, 0,
        xlogo64_width, xlogo64_height, 320, 64, 1);
}
