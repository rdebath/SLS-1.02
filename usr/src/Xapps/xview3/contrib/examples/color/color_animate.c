/*
 * color_animate.c -- Do frame-by-frame animation using colors.
 */
#include <stdio.h>
#include <ctype.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/notify.h>
#include <xview/cms.h>

Frame             frame;
Canvas            canvas;
Display          *dpy;
Cms               cms;
GC                gc;
Window            canvas_win;
Notify_value      animate();
struct itimerval  timer;

/* indices into color table renders specified colors. */
#define WHITE   0
#define RED     1
#define GREEN   2
#define BLUE    3
#define ORANGE  4
#define AQUA    5
#define PINK    6
#define BLACK   7
#define RANDOM_COLOR   8

int max_images = 9;
int cnt = 0;

main(argc, argv)
int     argc;
char    *argv[];
{
    Panel       panel;
    XGCValues   gcvalues;
    Xv_Font     _font;
    XFontStruct *font;
    void        start_stop(), adjust_speed() /*, change_glyph() */;
    extern void exit();

    static Xv_singlecolor cms_colors[] = {
        { 255,    255,    255 },
        { 255,      0,      0 },
        { 0,      255,     50 },
        { 10,      50,    255 },
        { 250,    130,     80 },
        { 30,     230,    250 },
        { 230,     30,    250 },
        { 0,     0,    0 }, /* black */
    };

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    cms = xv_create(NULL, CMS,
        /* CMS_TYPE,                XV_DYNAMIC_CMS, */
        CMS_SIZE,               8,
        CMS_COLOR_COUNT,        7,
        CMS_COLORS,    cms_colors,
        NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        NULL);

    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,           PANEL_VERTICAL,
        NULL);
    xv_create(panel, PANEL_SLIDER,
	XV_X,			0,
	XV_Y,			35,
        PANEL_LABEL_STRING,     "Millisecs Between Frames",
        PANEL_VALUE,            0,
        PANEL_MAX_VALUE,        400,
        PANEL_NOTIFY_PROC,      adjust_speed,
        NULL);
    window_fit(panel);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        WIN_CMS,                cms,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_RETAINED,        FALSE,
        NULL);
    canvas_win = (Window)xv_get(canvas_paint_window(canvas), XV_XID);

    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    if (!(_font = (Xv_Font)xv_find(frame, FONT,
        FONT_NAME,      "icon",
        NULL)))
        _font = xv_find(frame, FONT, FONT_NAME, "9x15", NULL);
    font = (XFontStruct *)xv_get(_font, FONT_INFO);

    gcvalues.font = font->fid;
    gcvalues.graphics_exposures = False;
    gcvalues.background = xv_get(cms, CMS_BACKGROUND_PIXEL);
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCBackground | GCFont | GCGraphicsExposures, &gcvalues);

    xv_main_loop(frame);
}

Notify_value
animate()
{
    static short red, green, blue;
    Xv_singlecolor color;
    int val;

    red = (red+1) % 255;
    green = (green+2) % 255;
    blue = (blue+3) % 255;
    color.red = red;
    color.green = green;
    color.blue = blue;

    val = xv_set(cms,
        CMS_COLOR_COUNT,        1,
        CMS_INDEX,              0,
        CMS_COLORS,             &color,
        NULL);
    XSetForeground(dpy, gc, xv_get(cms, CMS_PIXEL, red % 8));

    /* XDrawImageString(dpy, canvas_win, gc, 5, 40, images[cnt], 1); */
    XFillRectangle(dpy, canvas_win, gc, 5, 5, 39, 39);
    cnt = (cnt + 1) % max_images;

    return NOTIFY_DONE;
}

void
adjust_speed(item, value)
Panel_item item;
int value;
{
    if (value > 0) {
        timer.it_value.tv_usec = (value + 20) * 1000;
        timer.it_interval.tv_usec = (value + 20) * 1000;
        notify_set_itimer_func(frame, animate, ITIMER_REAL, &timer, NULL);
    } else
        /* turn it off */
        notify_set_itimer_func(frame, NULL, ITIMER_REAL, NULL, NULL);
}
