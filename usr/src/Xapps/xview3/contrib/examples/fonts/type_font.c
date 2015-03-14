/*
 * simple_font.c -- very simple program showing how to render text
 * using fonts loaded by XView.
 */
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/font.h>

Display *dpy;
GC      gc;
XFontStruct *font_info;

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Canvas      canvas;
    XGCValues   gcvalues;
    Xv_Font     font;
    void        my_event_proc();
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
    window_fit(panel);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               400,
        XV_HEIGHT,              200,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        NULL);
    xv_set(canvas_paint_window(canvas),
        WIN_EVENT_PROC,         my_event_proc,
        NULL);

    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    font = (Xv_Font)xv_get(frame, XV_FONT);
    font_info = (XFontStruct *)xv_get(font, FONT_INFO);

    gcvalues.font = (Font)xv_get(font, XV_XID);
    gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
    gcvalues.graphics_exposures = False;
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCForeground | GCBackground | GCFont | GCGraphicsExposures, &gcvalues);

    xv_main_loop(frame);
}

void
my_event_proc(win, event)
Xv_Window win;
Event *event;
{
    static int x = 10, y = 10;
    Window xwin = (Window)xv_get(win, XV_XID);
    char c;

    if (event_is_up(event))
        return;

    if (event_is_ascii(event)) {
        c = (char)event_id(event);
        if (c == '\n' || c == '\r') {
            y += font_info->max_bounds.ascent +
                        font_info->max_bounds.descent;
            x = 10;
        } else if (c == 7 || c == 127) { /* backspace or delete */
            if (x > 10)
                x -= XTextWidth(font_info, "m", 1);
            /* use XDrawImageString to overwrite previous text */
            XDrawImageString(dpy, xwin, gc, x, y, "  ", 2);
        } else {
            XDrawString(dpy, xwin, gc, x, y, &c, 1);
            x += XTextWidth(font_info, &c, 1);
        }
    } else if (event_action(event) == ACTION_SELECT) {
        x = event_x(event);
        y = event_y(event);
    }
}
