/*
 *  notify.c -- Demonstrates how to bypass xv_main_loop() by creating your
 *  own event loop.
 */
#include <X11/Xlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>

void    event_proc(), repaint_proc();
char    kbd_msg[128], ptr_msg[128], but_msg[128];
int	RUN = TRUE;
XFontStruct *font;
			/* Time out for select. */
struct timeval timeout = { 0, 250000 };

main(argc, argv)
int argc;
char *argv[];
{
    Display	*dpy;
    Frame       frame;
    Textsw      textsw;
    Canvas      canvas;
    fd_set	readfds;
    int		fd;
    Notify_func my_destroy_func();

    /* Initialize XView */
    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /* Create windows -- base frame, canvas and a textsw. */
    frame = (Frame)xv_create(NULL, FRAME,
	XV_X,			10,
	XV_Y,			10,
	XV_LABEL,		"Notifier",
        NULL);

    /* Interpose on the frame's destroy proc so I know when the user has
     * selected quit from the menu.  I don't want to get into a position
     * where all my windows are gone, but my main loop is still spinning
     * away.
     */
    notify_interpose_destroy_func(frame, my_destroy_func);

    canvas = xv_create(frame, CANVAS,
        XV_WIDTH,               520,
        XV_HEIGHT,              130,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_REPAINT_PROC,    repaint_proc,
	/* Set attrs on the paint window */
	CANVAS_PAINTWINDOW_ATTRS,
            WIN_EVENT_PROC,     event_proc,
            WIN_CONSUME_EVENTS,
                KBD_DONE, KBD_USE, LOC_DRAG, LOC_MOVE, LOC_WINENTER,
                LOC_WINEXIT, WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS,
                NULL,
	    NULL,
        NULL);

    textsw = xv_create(frame, TEXTSW,
	XV_X, 		0,
	WIN_BELOW, 	canvas,
	XV_WIDTH,	520,
	XV_HEIGHT,	500,
	NULL);

    window_fit(frame);

    /* Initial messages */
    strcpy(kbd_msg, "Keyboard: key press events");
    strcpy(ptr_msg, "Pointer: pointer movement events");
    strcpy(but_msg, "Button: button press events");

    /* Get a nice big font */
    font = XLoadQueryFont(dpy = (Display *) XV_DISPLAY_FROM_WINDOW(frame), 
		"-b&h-lucida-medium-r-normal-sans-24-240-*-*-p-*-iso8859-1");

    /* Map the frame.  This is normally done for us by xv_main_loop(). */
    xv_set(frame, XV_SHOW, TRUE);

    /* Get the server connection number. */
    fd = XConnectionNumber(dpy);

    /* Make sure everything gets over to the server. */
    XFlush(dpy);

    /* Create my own loop, only dispatching events when there is something
     * to dispatch.  When I'm not dispatching events, I'm free to do
     * as I please.
     */
    while (RUN) {
        FD_SET(fd, &readfds);

	/* Check to see if the server has written to us. */
	if (select(FD_SETSIZE, &readfds, NULL, NULL, &timeout)) {

	    /* Read and dispatch events from the server. */
	    notify_dispatch();

	} else {
	    static int toggle = 0;

            /* Do my own thing here.  Just remember not to neglect incomming
	     * events.  (*Ping the notifier as often as possible*)
	     */

	    /* Erase contents of textsw. */
	    textsw_erase(textsw, 0, TEXTSW_INFINITY);

	    /* Load textsw with one of two files. */
	    xv_set(textsw,
	        TEXTSW_FILE_CONTENTS, toggle ? "/etc/fstab" : "/etc/passwd",
		TEXTSW_FIRST,	  0,
		NULL);

	    toggle = ~toggle;

	    /* If I've used any X routines, be sure to flush the requests
	     * through to the server.
	     */
	    XFlush(dpy);
	}
    }
    /* All done. */
}

/* This is called before the frame's destroy func (ie. I've interposed in
 * front of it.
 */
Notify_func
my_destroy_func(frame, status)
Frame 		frame;
Destroy_status  status;
{
    if (status == DESTROY_PROCESS_DEATH || status == DESTROY_CLEANUP)
	RUN = FALSE;

    /* Now call the frame's destroy func. */
    notify_next_destroy_func(frame, status);
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
    XSetFont(dpy, gc, font->fid);

    XClearWindow(dpy, xwin);
    XDrawString(dpy, xwin, gc, 25, 35, kbd_msg, strlen(kbd_msg));
    XDrawString(dpy, xwin, gc, 25, 65, ptr_msg, strlen(ptr_msg));
    XDrawString(dpy, xwin, gc, 25, 95, but_msg, strlen(but_msg));
}
