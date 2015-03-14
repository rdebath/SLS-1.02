/*
 * animate.c -- use glyphs from the "icon" font distributed with XView
 * to do frame-by-frame animation.
 */
#include <stdio.h>
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>            /* for <sys/time.h> */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/notify.h>

Frame            frame;
Display          *dpy;
GC               gc;
Window           canvas_win;
Notify_value     animate();
struct itimerval timer;

#define ArraySize(x)  (sizeof(x)/sizeof(x[0]))
char *horses[] = { "N", "O", "P", "Q", "R" };
char *boys[] = { "\007", "\005", "\007", "\010" };
char *men[] = { "\\", "]", "Y", "Z", "[" };
char *eyes[] = {
    "2", "5", "4", "3", "4", "5",
    "2", "1", "0", "/", "0", "1"
};

int max_images = ArraySize(horses);
char **images = horses;
int cnt;

main(argc, argv)
int     argc;
char    *argv[];
{
    Panel       panel;
    Canvas      canvas;
    XGCValues   gcvalues;
    Xv_Font     _font;
    XFontStruct *font;
    void        adjust_speed(), change_glyph();
    extern void exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        NULL);

    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,           PANEL_VERTICAL,
        NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    xv_create(panel, PANEL_SLIDER,
        PANEL_LABEL_STRING,     "Millisecs Between Frames",
        PANEL_VALUE,            0,
        PANEL_MAX_VALUE,        120,
        PANEL_NOTIFY_PROC,      adjust_speed,
        NULL);
    xv_create(panel, PANEL_CHOICE,
        PANEL_LABEL_STRING,     "Glyphs",
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_DISPLAY_LEVEL,    PANEL_ALL,
        PANEL_CHOICE_STRINGS,   "Horse", "Man", "Boy", "Eye", NULL,
        PANEL_NOTIFY_PROC,      change_glyph,
        NULL);
    window_fit(panel);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        NULL);
    canvas_win = (Window)xv_get(canvas_paint_window(canvas), XV_XID);

    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    _font = (Xv_Font)xv_find(frame, FONT,
        FONT_NAME,      "icon",
        NULL);
    font = (XFontStruct *)xv_get(_font, FONT_INFO);

    gcvalues.font = font->fid;
    gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
    gcvalues.graphics_exposures = False;
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCForeground | GCBackground | GCFont | GCGraphicsExposures,
        &gcvalues);

    xv_main_loop(frame);
}

void
change_glyph(item, value)
Panel_item item;
int value;
{
    cnt = 0;
    if (value == 0) {
        max_images = ArraySize(horses);
        images = horses;
    } else if (value == 1) {
        max_images = ArraySize(men);
        images = men;
    } else if (value == 2) {
        max_images = ArraySize(boys);
        images = boys;
    } else if (value == 3) {
        max_images = ArraySize(eyes);
        images = eyes;
    }
    XClearWindow(dpy, canvas_win);
}

/*ARGSUSED*/
Notify_value
animate()
{
    XDrawImageString(dpy, canvas_win, gc, 5, 40, images[cnt], 1);
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
        notify_set_itimer_func(frame, animate,
            ITIMER_REAL, &timer, NULL);
    } else
        /* turn it off */
        notify_set_itimer_func(frame, NOTIFY_FUNC_NULL,
            ITIMER_REAL, NULL, NULL);
}
