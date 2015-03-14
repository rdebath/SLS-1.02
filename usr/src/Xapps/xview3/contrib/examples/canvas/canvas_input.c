/*
 * canvas_input.c --
 * Display a canvas whose views may be split repeatedly.  The event
 * handler is installed for each view, so events are displayed in
 * each paint window.
 */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/scrollbar.h>
#include <xview/xv_xrect.h>

Canvas  canvas;
Frame   frame;
char    msg[128];
void    init_split(), my_event_proc(), my_repaint_proc();

main(argc,argv)
int     argc;
char    *argv[];
{
    /*
     * Initialize, create base frame (with footers) and create canvas.
     */
    xv_init(XV_INIT_ARGS, argc, argv, NULL);
    frame = (Frame)xv_create(NULL,FRAME,
        FRAME_LABEL,            "Split View Windows.",
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);
    canvas = (Canvas)xv_create(frame,CANVAS,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        OPENWIN_SPLIT,
            OPENWIN_SPLIT_INIT_PROC,    init_split,
            NULL,
        CANVAS_REPAINT_PROC,    my_repaint_proc,
        NULL);

    (void) xv_create(canvas, SCROLLBAR,
        SCROLLBAR_SPLITTABLE,   TRUE,
        SCROLLBAR_DIRECTION,    SCROLLBAR_VERTICAL,
        NULL);
    (void) xv_create(canvas, SCROLLBAR,
        SCROLLBAR_SPLITTABLE,   TRUE,
        SCROLLBAR_DIRECTION,    SCROLLBAR_HORIZONTAL,
        NULL);

    /*
     *  Set input mask
     */
    xv_set(canvas_paint_window(canvas),
        WIN_CONSUME_EVENTS,
            WIN_NO_EVENTS,
            WIN_ASCII_EVENTS, KBD_USE, KBD_DONE,
            LOC_DRAG, LOC_WINENTER, LOC_WINEXIT, WIN_MOUSE_BUTTONS,
            NULL,
        WIN_EVENT_PROC, my_event_proc,
        NULL);

    xv_main_loop(frame);
}

/*
 * when a viewport is split, this routine is called.
 */
void
init_split(splitview, newview, pos)
Xv_Window splitview, newview;
int pos;
{
    Xv_Window   view;
    int         i = 0;

    /*
     * Determine view # from the new view and set its scrollbar to 0,0
     */
    OPENWIN_EACH_VIEW(canvas, view)
        if (view == splitview) {
            /* identify the view # of the view the user just split. */
            sprintf(msg, "Split view #%d", i+1);
            xv_set(frame, FRAME_LEFT_FOOTER, msg, NULL);
        } else if (view == newview) {
            xv_set(xv_get(canvas, OPENWIN_VERTICAL_SCROLLBAR, view),
                SCROLLBAR_VIEW_START, 0,
                NULL);
            xv_set(xv_get(canvas, OPENWIN_HORIZONTAL_SCROLLBAR, view),
                SCROLLBAR_VIEW_START, 0,
                NULL);
        }
        i++;
    OPENWIN_END_EACH
    sprintf(msg, "Total views: %d", i);
    xv_set(frame, FRAME_RIGHT_FOOTER, msg, NULL);
}

/*
 * Called when an event is received in an arbitrary paint window.
 */
void
my_event_proc(window, event, arg)
Xv_Window       window;
Event           *event;
Notify_arg      arg;
{
    register char *p = msg;

    *p = 0;

    /* test to see if a function key has been hit */
    if (event_is_key_left(event))
        sprintf(p, "(L%d) ", event_id(event) - KEY_LEFTFIRST + 1);
    else if (event_is_key_top(event))
        sprintf(p, "(T%d) ", event_id(event) - KEY_TOPFIRST + 1);
    else if (event_is_key_right(event))
        sprintf(p, "(R%d) ", event_id(event) - KEY_RIGHTFIRST + 1);
    else if (event_id(event) == KEY_BOTTOMLEFT)
        strcpy(p, "bottom left ");
    else if (event_id(event) == KEY_BOTTOMRIGHT)
        strcpy(p, "bottom right ");
    p += strlen(p);


    if (event_is_ascii(event)) {
        /*
         * note that shift modifier is reflected in the event code by
         * virtue of the char printed is upper/lower case.
         */
        sprintf(p, "Keyboard: key '%c' (%d) %s at %d,%d",
            event_action(event), event_action(event),
            event_is_down(event)? "pressed" : "released",
            event_x(event), event_y(event));
    } else switch (event_action(event)) {
        case ACTION_CLOSE :
            xv_set(frame, FRAME_CLOSED, TRUE, NULL);
            break;
        case ACTION_OPEN :
            strcpy(p, "frame opened up");
            break;
        case ACTION_HELP :
            strcpy(p, "Help (action ignored)");
            break;
        case ACTION_SELECT :
            sprintf(p, "Button: Select (Left) %s at %d,%d",
                event_is_down(event)? "pressed" : "released",
                event_x(event), event_y(event));
            break;
        case ACTION_ADJUST :
            sprintf(p, "Button: Adjust (Middle) %s at %d,%d",
                event_is_down(event)? "pressed" : "released",
                event_x(event), event_y(event));
            break;
        case ACTION_MENU :
            sprintf(p, "Button: Menu (Right) %s at %d,%d",
                event_is_down(event)? "pressed" : "released",
                event_x(event), event_y(event));
            break;
        case SHIFT_RIGHT :
            sprintf(p, "Keyboard: right shift %s",
                event_is_down(event)? "pressed" : "released");
            break;
        case SHIFT_LEFT :
            sprintf(p, "Keyboard: left shift %s",
                event_is_down(event)? "pressed" : "released");
            break;
        case SHIFT_LEFTCTRL : case SHIFT_RIGHTCTRL :
            sprintf(p, "Keyboard: control key %s",
                event_is_down(event)? "pressed" : "released");
            break;
        case SHIFT_META :
            sprintf(p, "Keyboard: meta key %s",
                event_is_down(event)? "pressed" : "released");
            break;
        case SHIFT_ALT :
            sprintf(p, "Keyboard: alt key %s",
                event_is_down(event)? "pressed" : "released");
            break;
        case KBD_USE:
            sprintf(p, "Keyboard: got keyboard focus");
            break;
        case KBD_DONE:
            sprintf(p, "Keyboard: lost keyboard focus");
            break;
        case LOC_MOVE:
            sprintf(p, "Pointer: moved to %d,%d",
                    event_x(event),event_y(event));
            break;
        case LOC_DRAG:
            sprintf(p, "Pointer: dragged to %d,%d",
                    event_x(event), event_y(event));
            break;
        case LOC_WINENTER:
            win_set_kbd_focus(window, xv_get(window, XV_XID));
            sprintf(p, "Pointer: entered window at %d,%d",
                    event_x(event), event_y(event));
            break;
        case LOC_WINEXIT:
            sprintf(p, "Pointer: exited window at %d,%d",
                    event_x(event), event_y(event));
            break;
        case WIN_RESIZE :
        case WIN_REPAINT :
            return;
        default :
            /* There are too many ACTION events to trap -- ignore the
             * ones we're not interested in.
             */
            return;
    }

    my_repaint_proc(canvas, window,
        xv_get(canvas, XV_DISPLAY), xv_get(window, XV_XID), NULL);
}

/*
 * my_repaint_proc()
 *      Called to repaint the canvas in response to damage events
 *      and the initial painting of the canvas window.
 *      Displays the keyboard, pointer and button message strings
 *      after erasing the previous messages.
 */
void
my_repaint_proc(canvas, pw, dpy, xwin, xrects)
Canvas          canvas;
Xv_Window       pw;
Display         *dpy;
Window          xwin;
Xv_xrectlist    *xrects;
{
    char        win_num[16];
    Xv_Window   w;
    int         i = 0;
    GC          gc = DefaultGC(dpy, DefaultScreen(dpy));

    /*
     * Determine which paint window we're writing in.
     */
    CANVAS_EACH_PAINT_WINDOW(canvas, w)
        if (w == pw)
            break;
        i++;
    CANVAS_END_EACH
    sprintf(win_num, "(Window #%d) ", i+1);

    XClearWindow(dpy, xwin);
    XDrawString(dpy, xwin, gc, 25, 25, win_num, strlen(win_num));
    XDrawString(dpy, xwin, gc, 25, 45, msg, strlen(msg));
}
