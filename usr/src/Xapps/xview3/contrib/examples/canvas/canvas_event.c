/*
 *  canvas_event.c
 *  Demonstrates how to get keyboard and mouse events in an canvas
 *  window.  Looks for keyboards, pointer movement and button
 *  events and displays the info in the canvas.
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>

void    event_proc(), repaint_proc();
char    kbd_msg[128], ptr_msg[128], but_msg[128];

/*
 * main()
 *      Create a canvas specifying a repaint procedure.
 *      Get the paint window for the canvas and set the input
 *      mask and the event procedure.
 */
main(argc, argv)
int argc;
char *argv[];
{
    Frame       frame;
    Canvas      canvas;

    /* Initialize XView */
    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /* Create windows -- base frame and canvas. */
    frame = (Frame)xv_create(NULL, FRAME, NULL);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               300,
        XV_HEIGHT,              110,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_REPAINT_PROC,    repaint_proc,
        NULL);
    window_fit(frame);

    /* Set input mask */
    xv_set(canvas_paint_window(canvas),
        WIN_EVENT_PROC,         event_proc,
        WIN_CONSUME_EVENTS,
            KBD_DONE, KBD_USE, LOC_DRAG, LOC_MOVE, LOC_WINENTER,
            LOC_WINEXIT, WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS,
            NULL,
        NULL);

    /* Initial messages */
    strcpy(kbd_msg, "Keyboard: key press events");
    strcpy(ptr_msg, "Pointer: pointer movement events");
    strcpy(but_msg, "Button: button press events");

    /* Start event loop */
    xv_main_loop(frame);
}

/*
 * event_proc()
 *      Called when an event is received in the canvas window.
 *      Updates the keyboard, pointer and button message strings
 *      and then calls repaint_proc() to paint them to the window.
 */
void
event_proc(window, event)
Xv_Window window;
Event    *event;
{
    if (event_is_ascii(event))
        sprintf(kbd_msg, "Keyboard: key '%c' %d pressed at %d,%d",
                event_action(event), event_action(event),
                event_x(event), event_y(event));
    else
        switch (event_action(event)) {
            case KBD_USE:
                sprintf(kbd_msg, "Keyboard: got keyboard focus");
                break;
            case KBD_DONE:
                sprintf(kbd_msg, "Keyboard: lost keyboard focus");
                break;
            case LOC_MOVE:
                sprintf(ptr_msg, "Pointer: moved to %d,%d",
                    event_x(event), event_y(event));
                break;
            case LOC_DRAG:
                sprintf(ptr_msg, "Pointer: dragged to %d,%d",
                    event_x(event), event_y(event));
                break;
            case LOC_WINENTER:
                sprintf(ptr_msg, "Pointer: entered window at %d,%d",
                    event_x(event), event_y(event));
                break;
            case LOC_WINEXIT:
                sprintf(ptr_msg, "Pointer: exited window at %d,%d",
                    event_x(event), event_y(event));
                break;
            case ACTION_SELECT:
            case MS_LEFT:
                sprintf(but_msg, "Button: Select (Left) at %d,%d",
                    event_x(event), event_y(event));
                break;
            case ACTION_ADJUST:
            case MS_MIDDLE:
                sprintf(but_msg, "Button: Adjust (Middle) at %d,%d",
                    event_x(event), event_y(event));
                break;
            case ACTION_MENU:
            case MS_RIGHT:
                sprintf(but_msg, "Button: Menu (Right) at %d,%d",
                    event_x(event), event_y(event));
                break;
            default:
                return;
        }

    /* call repaint proc directly to update messages */
    repaint_proc((Canvas)NULL, window,
        (Display *)xv_get(window, XV_DISPLAY),
        xv_get(window, XV_XID), (Xv_xrectlist *) NULL);
}

/*
 * repaint_proc()
 *      Called to repaint the canvas in response to damage events
 *      and the initial painting of the canvas window.
 *      Displays the keyboard, pointer and button message strings
 *      after erasing the previous messages.
 */
void
repaint_proc(canvas, paint_window, dpy, xwin, xrects)
Canvas        canvas;           /* Ignored */
Xv_Window     paint_window;     /* Ignored */
Display      *dpy;
Window        xwin;
Xv_xrectlist *xrects;           /* Ignored */
{
    GC gc = DefaultGC(dpy, DefaultScreen(dpy));

    XClearWindow(dpy, xwin);
    XDrawString(dpy, xwin, gc, 25, 25, kbd_msg, strlen(kbd_msg));
    XDrawString(dpy, xwin, gc, 25, 50, ptr_msg, strlen(ptr_msg));
    XDrawString(dpy, xwin, gc, 25, 75, but_msg, strlen(but_msg));
}
