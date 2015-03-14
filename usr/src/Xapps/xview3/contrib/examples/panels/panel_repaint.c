/*
 * panel_repaint.c -- repaint a panel background without disturbing
 * the repainting of panel items.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/bitmaps/gray1>

#define PANEL_GC_KEY    101  /* any arbitrary number */

main(argc, argv)
int argc;
char *argv[];
{
    Display     *display;
    Frame        frame;
    Panel        panel;
    int          quit();
    void         panel_repaint();
    XGCValues    gcvalues;
    Server_image grey;

    Mask        gcmask = 0L;
    GC          gc;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_REPAINT_PROC,     panel_repaint,
        NULL);

    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      quit,
        PANEL_CLIENT_DATA,      frame,
        NULL);

    window_fit(frame);

    grey = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               gray1_width,
        XV_HEIGHT,              gray1_height,
        SERVER_IMAGE_DEPTH,     1, /* clarify for completeness*/
        SERVER_IMAGE_X_BITS,    gray1_bits,
        NULL);

    display = (Display *)xv_get(panel, XV_DISPLAY);
    gcvalues.stipple = (Pixmap) xv_get(grey, XV_XID);
    gcvalues.fill_style = FillOpaqueStippled;
    gcvalues.plane_mask = AllPlanes;
    gcvalues.graphics_exposures = False;
    gcvalues.foreground = BlackPixel(display, DefaultScreen(display));
    gcvalues.background = WhitePixel(display, DefaultScreen(display));
    gcmask = GCStipple | GCFillStyle | GCPlaneMask |
        GCGraphicsExposures | GCForeground | GCBackground;
    gc = XCreateGC(display, xv_get(panel, XV_XID), gcmask, &gcvalues);

    /* attach the GC to the panel for use by the repaint proc above */
    xv_set(panel, XV_KEY_DATA, PANEL_GC_KEY, gc, NULL);

    xv_main_loop(frame);
    exit(0);
}

/*
 * repaint procedure for the panel paints a gray pattern over the
 * entire panel.  Use the GC attached to the panel via XV_KEY_DATA.
 */
void
panel_repaint(panel, pw)
Panel panel;
Xv_Window pw;
{
    /* get the GC attached to the panel in main() */
    GC gc = (GC)xv_get(panel, XV_KEY_DATA, PANEL_GC_KEY);

    /* call XFillRectangle on the entire size of the panel window */
    XFillRectangle((Display *)xv_get(panel, XV_DISPLAY), xv_get(pw, XV_XID), gc,
        0, 0, xv_get(pw, XV_WIDTH), xv_get(pw, XV_HEIGHT));
}

quit(item)
Panel_item item;
{
    Frame frame = (Frame)xv_get(item, PANEL_CLIENT_DATA);
    xv_destroy_safe(frame);
}
