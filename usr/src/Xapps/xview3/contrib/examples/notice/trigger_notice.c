/*
 * trigger_notice.c -- Demonstrate the use of triggers in notices.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>

main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Xv_opaque   my_notify_proc();
    extern void exit();

    /*
     * Initialize XView, create a frame, a panel and one panel button.
     */
    xv_init(XV_INIT_ARGS, argc, argv, NULL);
    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Move",
        PANEL_NOTIFY_PROC,      my_notify_proc,
        NULL);

    /* make sure everything looks good */
    window_fit(panel);
    window_fit(frame);

    /* start window event processing */
    xv_main_loop(frame);
}

/*
 * my_notify_proc() -- called when the user selects the "Move"
 * panel button.  Put up a notice_prompt to get new coordinates
 * to move the main window.
 */
Xv_opaque
my_notify_proc(item, event)
Panel_item  item;
Event      *event;
{
    int         result, x, y;
    Panel       panel = (Panel)xv_get(item, PANEL_PARENT_PANEL);
    Frame       frame = (Frame)xv_get(panel, XV_OWNER);

    x = event_x(event), y = event_y(event);
    printf("original click relative to panel: %d, %d\n", x, y);
    result = notice_prompt(panel, event,
        NOTICE_FOCUS_XY,        x, y,
        NOTICE_MESSAGE_STRINGS,
            "You may move the window to a new location specified by",
            "clicking the Left Mouse Button somewhere on the screen",
            "or cancel this operation by selecting \"cancel\".",
            NULL,
        NOTICE_BUTTON_YES,      "cancel",
        NOTICE_TRIGGER,         MS_LEFT,
        NOTICE_NO_BEEPING,      TRUE,
        NULL);

    if (result == NOTICE_TRIGGERED) {
        x = event_x(event) + (int)xv_get(frame, XV_X);
        y = event_y(event) + (int)xv_get(frame, XV_Y);
        printf("screen x,y: %d, %d\n", x, y);
        xv_set(frame, XV_X, x, XV_Y, y, NULL);
    }
}
