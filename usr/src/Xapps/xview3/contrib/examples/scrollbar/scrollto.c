/* scroll_to.c -- demonstrate how to monitor the scrolling
 * requests invoked by the user.  Requests can be monitored,
 * ignored or changed programmatically.  This program creates
 * a canvas window by default or a textsw with the -textsw
 * command line option.  Both contain a scrollbar.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/canvas.h>
#include <xview/scrollbar.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame           frame;
    Textsw          textsw;
    Canvas          canvas;
    Scrollbar       sbar;
    Notify_value    monitor_scroll();

    (void) xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = xv_create(NULL, FRAME, NULL);

    if (argc > 1 && !strcmp(argv[1], "-textsw")) {
        textsw = xv_create(frame, TEXTSW,
            TEXTSW_FILE_CONTENTS, "/etc/termcap",
            NULL);
        sbar = xv_get(textsw, TEXTSW_SCROLLBAR);
    } else {
        canvas = xv_create(frame, CANVAS,
            CANVAS_WIDTH, 1000,
            CANVAS_HEIGHT, 1000,
            CANVAS_AUTO_SHRINK, FALSE,
            CANVAS_AUTO_EXPAND, FALSE,
            NULL);
        sbar = xv_create(canvas, SCROLLBAR,
            SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
            SCROLLBAR_PIXELS_PER_UNIT, 10,
            NULL);
    }
    notify_interpose_event_func(xv_get(sbar, SCROLLBAR_NOTIFY_CLIENT),
        monitor_scroll, NOTIFY_SAFE);

    xv_main_loop(frame);
}

/*
 * To change the behavior of the scrolling of the canvas, do not pass
 * on the event via notify_next_event_func() when the event type is
 * SCROLLBAR_REQUEST.
 */
Notify_value
monitor_scroll(client, event, sbar, type)
Notify_client     client;
Event            *event;
Scrollbar         sbar;
Notify_event_type type;
{
    int     view_start, last_view_start, pixels_per, is_neg = 0, total;

    if (event_id(event) == SCROLLBAR_REQUEST) {
        view_start = (int)xv_get(sbar, SCROLLBAR_VIEW_START);
        last_view_start = (int)xv_get(sbar, SCROLLBAR_LAST_VIEW_START);
        pixels_per = (int)xv_get(sbar, SCROLLBAR_PIXELS_PER_UNIT);
        if ((total = view_start - last_view_start) < 0)
            total = -total, is_neg = 1;
        printf("scrolled from %d to %d: %d pixels (%d units) %s\n",
            last_view_start, view_start, pixels_per * total, total,
            is_neg? "up" : "down");
    }
    return notify_next_event_func(client, event, sbar, type);
}
