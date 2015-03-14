/*
 * split_views.c -- run this program and then split the views using the
 * scrollbars.  The new view should be scrolled to 0, 0 (click on the
 * left and top elevator anchors to reset both scrollbars on the new view).
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/scrollbar.h>

Canvas	canvas;
Frame	frame;
char	msg[128];
void	init_split(), my_event_proc(), my_repaint_proc();

main(argc,argv)
int	argc;
char	*argv[];
{
    /*
     * Initialize, create base frame (with footers) and * create canvas.
     */
    xv_init(XV_INIT_ARGS, argc,argv, 0);
    frame = xv_create(NULL,FRAME,
	FRAME_LABEL,		"Try Splitting views.",
	FRAME_SHOW_FOOTER,	TRUE,
	NULL);
    canvas = xv_create(frame,CANVAS,
	OPENWIN_SPLIT,
	    OPENWIN_SPLIT_INIT_PROC,	init_split,
	    NULL,
	CANVAS_REPAINT_PROC,	my_repaint_proc,
	NULL);

    xv_create(canvas, SCROLLBAR,
	SCROLLBAR_SPLITTABLE,	TRUE,
	SCROLLBAR_DIRECTION,	SCROLLBAR_VERTICAL,
	NULL);
    xv_create(canvas, SCROLLBAR,
	SCROLLBAR_SPLITTABLE,	TRUE,
	SCROLLBAR_DIRECTION,	SCROLLBAR_HORIZONTAL,
	NULL);

    /*
     *	Set input mask
     */
    xv_set(canvas_paint_window(canvas),
	    WIN_CONSUME_EVENTS,
		WIN_NO_EVENTS, WIN_ASCII_EVENTS, KBD_USE, KBD_DONE,
		LOC_DRAG, LOC_WINENTER, LOC_WINEXIT, WIN_MOUSE_BUTTONS,
		NULL,
	    WIN_EVENT_PROC,	my_event_proc,
	    NULL);

    xv_main_loop(frame);
    return 0;
}

/*
 * when a viewport is split, this routine is called.
 */
void
init_split(splitview, newview, pos)
Xv_Window splitview, newview;
int pos;
{
    Xv_Window	view, win;
    int		i = 0;

    /*
     * Determine which view # is the new view and which is the original view
     */
    OPENWIN_EACH_VIEW(canvas, view)
	if (view == splitview) {
	    /* identify the view # of the view the user just split. */
	    sprintf(msg, "Split view #%d", i+1);
	    xv_set(frame, FRAME_LEFT_FOOTER, msg, NULL);
	} else if (view == newview) {
	    /*
	     * install the same event handling mask and event callback
	     * for the newview's paint window.
	     */
	    xv_set(win = xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, i),
		WIN_CONSUME_EVENTS,
		    WIN_NO_EVENTS, WIN_ASCII_EVENTS, KBD_USE, KBD_DONE,
		    LOC_DRAG, LOC_WINENTER, LOC_WINEXIT, WIN_MOUSE_BUTTONS,
		    NULL,
		WIN_EVENT_PROC,	my_event_proc,
		NULL);
	}
	i++;
    OPENWIN_END_EACH
    printf("win = %x, CANVAS_VIEW_PAINT_WINDOW = %x\n",
	win, xv_get(newview, CANVAS_VIEW_PAINT_WINDOW));
    sprintf(msg, "Total views: %d", i);
    xv_set(frame, FRAME_RIGHT_FOOTER, msg, NULL);
}

/*
 * Called when an event is received in an arbitrary paint window.
 */
void
my_event_proc(window, event, arg)
Xv_Window	window;
Event		*event;
Notify_arg	arg;
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
	strcpy(p, "bottom left ");
    p += strlen(p);

    /* Test to see if event is a special "mnemonic" action */
    if (event_action(event) != event_id(event)) {
	switch (event_action(event)) {
	    case ACTION_CLOSE :
		strcpy(p, "close (action ignored)");
		break;
	    case ACTION_OPEN :
		strcpy(p, "open (action ignored)");
		break;
	    case ACTION_HELP :
		strcpy(p, "Help (action ignored)");
		break;
	    case ACTION_SELECT : /* the action */
	    case MS_LEFT :	 /* the actual (literal) event */
		sprintf(p, "Button: Select (Left) %s at %d,%d",
		    event_is_down(event)? "pressed" : "released",
		    event_x(event), event_y(event));
		break;
	    case ACTION_ADJUST :
	    case MS_MIDDLE :
		sprintf(p, "Button: Adjust (Middle) %s at %d,%d",
		    event_is_down(event)? "pressed" : "released",
		    event_x(event), event_y(event));
		break;
	    case ACTION_MENU :
	    case MS_RIGHT :
		sprintf(p, "Button: Menu (Right) %s at %d,%d",
		    event_is_down(event)? "pressed" : "released",
		    event_x(event), event_y(event));
		break;
	    default : ;
		/* There are too many ACTION events to trap -- ignore the
		 * ones we're not interested in.
		 */
	}
    } else if (event_is_ascii(event))
	/*
	 * note that shift modifier is reflected in the event code by
	 * virtue of the char printed is upper/lower case.
	 */
	sprintf(p, "Keyboard: key '%c' (%d) %s at %d,%d",
	    event_action(event), event_action(event),
	    event_is_down(event)? "pressed" : "released",
	    event_x(event), event_y(event));
    else switch (event_id(event)) {
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
		strcpy(msg, "resize");
		break;
	case WIN_REPAINT :
		strcpy(msg, "repaint");
		break;
	default:
		if (msg[0])
		    printf("unknown event: %d\n", event_id(event));
    }
    my_repaint_proc(canvas, window, NULL);
}

/*
 * my_repaint_proc()
 *	Called to repaint the canvas in response to damage events
 *	and the initial painting of the canvas window.
 *	Displays the keyboard, pointer and button message strings
 *	after erasing the previous messages.
 */
void
my_repaint_proc(canvas, pw, repaint_area)
Canvas		canvas;
Xv_Window	pw;
Rectlist	*repaint_area;
{
    static char buf[] =
	"                                                                    ";
    char	win_num[16];
    Xv_Window	w;
    int		i = 0;

    /*
     * Determine which # paint window we're writing in.
     */
    CANVAS_EACH_PAINT_WINDOW(canvas, w)
	if (w == pw)
	    break;
	i++;
    CANVAS_END_EACH
    sprintf(win_num, "(Window #%d) ", i+1);
    xv_text(pw, 25, 25, PIX_SRC, NULL, win_num);

    xv_text(pw, 25, 45, PIX_SRC, NULL, buf);
    xv_text(pw, 25, 45, PIX_SRC, NULL, msg);
}
