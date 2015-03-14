/*
 * fullscreen.c
 * Demonstrate the fullscreen package.  Create a panel button that
 * creates a fullscreen instance, thus grabbing the X server.  User
 * presses a mouse button to release the server.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/fullscreen.h>

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Panel       panel;
    void        exit(), grab();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Fullscreen",
        PANEL_NOTIFY_PROC,      grab,
        NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}

/*
 * Notify procedure for when the "Fullscreen" button is pushed.
 * Create a fullscreen instance, scan for a button event, then
 * destroy it.
 */
void
grab(item, event)
Panel_item item;
Event *event;
{
    Panel       panel = (Panel)xv_get(item, PANEL_PARENT_PANEL);
    Frame       frame = (Frame)xv_get(panel, XV_OWNER);
    Fullscreen  fs;
    Inputmask   im;

    /* set up an input mask for the call to xv_input_readevent(). */
    win_setinputcodebit(&im, MS_LEFT);
    win_setinputcodebit(&im, MS_MIDDLE);
    win_setinputcodebit(&im, MS_RIGHT);
    win_setinputcodebit(&im, LOC_MOVE);

    /*
     * Create a fullscreen object (initialize X server grab).
     * Specify which events should be allowed to pass through.
     * These events should match the input mask coded above.
     */
    fs = xv_create(panel, FULLSCREEN,
        WIN_CONSUME_EVENTS,
            WIN_MOUSE_BUTTONS, LOC_MOVE, NULL,
        NULL);

    /* Loop till user generates a button event */
    while (xv_input_readevent(panel, event, TRUE, TRUE, &im) != -1)
        if (event_is_button(event))
            break;

    /* Destroy the fullscreen (release the X server grab) */
    xv_destroy(fs);

    /* Report which button was pushed. */
    printf("event was button %d (%d, %d)\n",
        event_id(event) - BUT_FIRST+1,
        event_x(event) + (int)xv_get(frame, XV_X),
        event_y(event) + (int)xv_get(frame, XV_Y));
}
