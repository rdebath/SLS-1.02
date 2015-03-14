/*
 * pin_menu.c -
 *	Demonstrate how to generate your own pinup menu.
 *	Use of MENU_GEN_PIN_WINDOW is for static menus only.
 *	This demo uses menus whose items may change, so we
 *	need to reflect those changes in our own command frame.
 */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>

Frame	frame;

/*
 * main -
 *	Create a frame, canvas and menu.
 *	A canvas receives input in its canvas_paint_window().
 *	Specify creation of an Open Look Menu and transformation of
 *	the menu to a pinned command window.
 *	Each menu item specifies an action proc to be called when the
 *	item is chosen, regardless of whether or not menu is pinned.
 */
main(argc,argv)
int	argc;
char	*argv[];
{
    Canvas	canvas;
    Menu	menu;
    int		my_notify_proc();
    void	my_event_proc(), my_menu_done();
    extern void	exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME,
	FRAME_LABEL,	argv[0],
	NULL);
    canvas = (Canvas)xv_create(frame, CANVAS,
	XV_WIDTH,	300,
	XV_HEIGHT,	200,
	NULL);
    menu = (Menu)xv_create(NULL, MENU,
	MENU_GEN_PIN_WINDOW,	frame, "Junk",
	MENU_DONE_PROC,		my_menu_done,
	/*
	MENU_STRINGS,		"Yes", "No", "Maybe", NULL,
	MENU_NOTIFY_PROC,	my_notify_proc,
	*/
	MENU_ITEM, MENU_STRING,	"No", MENU_NOTIFY_PROC, my_notify_proc, NULL,
	MENU_ITEM, MENU_STRING,	"Yes", MENU_NOTIFY_PROC, my_notify_proc, NULL,
	MENU_ITEM, MENU_STRING,	"Maybe", MENU_NOTIFY_PROC, my_notify_proc, NULL,
	MENU_ITEM,
	    MENU_STRING,	"Save",
	    MENU_NOTIFY_PROC,	my_notify_proc,
	    MENU_PULLRIGHT,
		xv_create(canvas, MENU,
		    MENU_ITEM,
			MENU_STRING,		"Update Changes",
			MENU_NOTIFY_PROC,	my_notify_proc,
			NULL,
		    NULL),
	    NULL,
	MENU_ITEM,
	    MENU_STRING,	"Quit",
	    MENU_NOTIFY_PROC,	exit,
	    NULL,
	NULL);

    xv_set(canvas_paint_window(canvas),
	WIN_CONSUME_EVENTS,	WIN_MOUSE_BUTTONS, NULL,
	WIN_EVENT_PROC,		my_event_proc,
	/* associate the menu to the canvas win so we can retreive it easily */
	WIN_CLIENT_DATA,	menu,
	NULL);

    window_fit(frame);
    window_main_loop(frame);
}

/*
 * my_menu_done - menu has been popped-down.  Make sure the command frame panel
 * matches the menu.
 */
void
my_menu_done(menu, result)
Menu menu;
Xv_opaque result;
{
    int default_item, i;
    Frame pin_frame;
    Panel panel;
    Panel_item pi;

    printf("result = %x\n", result);
    if (!(pin_frame = (Frame)xv_get(menu, MENU_PIN_WINDOW))) {
	puts("menu has no pin frame");
	return;
    }
    panel = (Panel)xv_get(pin_frame, FRAME_CMD_PANEL);
    /* get the ordinal number of the default menu item */
    default_item = (int)xv_get(menu, MENU_DEFAULT);

    /* search for the <default>-th item in the panel and... */
    pi = (Panel_item)xv_get(panel, PANEL_FIRST_ITEM);
    for (i = 1 /*menu items offset at 1*/; i < default_item && pi; i++)
	pi = (Panel_item)xv_get(pi, PANEL_NEXT_ITEM);

    /* set that panel item to be the default item */
    xv_set(panel, PANEL_DEFAULT_ITEM, pi, NULL);
}

/*
 * my_notify_proc - Display menu selection in frame header.
 */
/*ARGSUSED*/
int
my_notify_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
    xv_set(frame,
	FRAME_LABEL,	xv_get(menu_item, MENU_STRING),
	NULL);
    return (XV_OK);
}

/*
 * my_event_proc - Call menu_show() to display menu on right mouse button push.
 */
void
my_event_proc(window, event)
Xv_Window window;
Event *event;
{
    if (event_action(event) == ACTION_MENU && event_is_down(event)) {
	Menu menu = (Menu)xv_get(window, WIN_CLIENT_DATA);
	if (!xv_get(menu, MENU_PIN_WINDOW))
	    create_pin_win(menu);
	menu_show(menu, window, event, NULL);
    }
}

#define MENU_KEY	100
#define MENU_ITEM_KEY	101
#define ACTION_KEY	102

create_pin_win(menu)
Menu menu;
{
    int i;
    void pin_btn_notify();
    Frame cmd_frame = (Frame)xv_create(frame, FRAME_CMD, XV_SHOW, FALSE, NULL);
    Panel panel = (Panel)xv_get(cmd_frame, FRAME_CMD_PANEL);
    Menu_item mi;

    xv_set(panel, PANEL_LAYOUT, PANEL_VERTICAL, 0);
    printf("frame = %x, panel = %x\n", cmd_frame, panel);
    for (i = (int)xv_get(menu, MENU_NITEMS); i > 0; i--) {
	mi = (Menu_item)xv_get(menu, MENU_NTH_ITEM, i);
	printf("adding panel item: %s\n", xv_get(mi, MENU_STRING));
	xv_create(panel, PANEL_BUTTON,
	    /* PANEL_MENU_ITEM,		TRUE, */
	    PANEL_LABEL_STRING,		xv_get(mi, MENU_STRING),
	    PANEL_NOTIFY_PROC,		pin_btn_notify,
	    XV_KEY_DATA, MENU_KEY,	menu,
	    XV_KEY_DATA, MENU_ITEM_KEY,	mi,
	    XV_KEY_DATA, ACTION_KEY,	xv_get(mi, MENU_NOTIFY_PROC),
	    NULL);
    }
    window_fit(panel);
    window_fit(cmd_frame);
    xv_set(menu, MENU_PIN_WINDOW, cmd_frame, NULL);
}

/*ARGSUSED*/
void
pin_btn_notify(item, event)
Panel_item item;
Event *event;
{
    Menu	menu = (Menu)xv_get(item, XV_KEY_DATA, MENU_KEY);
    Menu_item	mi = (Menu)xv_get(item, XV_KEY_DATA, MENU_ITEM_KEY);
    void	(*action)() = (void (*)())xv_get(item, XV_KEY_DATA, ACTION_KEY);

    (*action)(menu, mi);
}
