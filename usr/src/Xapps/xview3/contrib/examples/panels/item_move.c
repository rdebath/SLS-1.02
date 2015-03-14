/*
 * item_move.c
 *    Move items around in a panel using an interpose event handler
 * specific to the panel.  Two panels are created -- the left panel
 * contains panel buttons that allow you to create certain types of
 * items that are put in the second panel.  Use the MENU (right)
 * mouse button to move items around in the second panel.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>

/* We need handles to the base frame and a panel -- instead of
 * using global variables, we're going to attach the objects to
 * the objects which need to reference them.  Attach using
 * XV_KEY_DATA -- here are the keys.
 */
#define PACKAGE_KEY     100
#define FRAME_KEY       101
#define PANEL_KEY       102

main(argc, argv)
int argc;
char *argv[];
{
    Frame       	frame;
    Panel       	panel;
    Xv_Window		window;
    Panel_item  	create_text, item;
    Notify_value	my_event_proc();
    int         	create_item();
    char		buf[64];

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    sprintf(buf, "%s:  Use MENU (Right) Button To Move Items", argv[0]);
    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            buf,
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);
    /*
     * Create panel for known panel items.  Layout panel vertically.
     */
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,           PANEL_VERTICAL,
        NULL);
    /*
     * Create text panel item, attach the frame as client data for
     * use by the notify procedure create_item().  Text items inherit
     * the layout of "label" and "value" from its parent panel.
     * override for the text item by setting PANEL_LAYOUT explicitly.
     */
    create_text = (Panel_item)xv_create(panel, PANEL_TEXT,
	XV_X,			0,
	XV_Y,			20,
        PANEL_LABEL_STRING,     "Create Button:",
        PANEL_NOTIFY_PROC,      create_item,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_VALUE_DISPLAY_LENGTH,     10,
        NULL);
    /*
     * Create panel button to determine which type of button to create -- 
     * a button, message, or text item.  See create_item().
     */
    item = (Panel_item)xv_create(panel, PANEL_CHOICE,
	XV_X,			0,
	XV_Y,			50,
        PANEL_DISPLAY_LEVEL,    PANEL_CURRENT,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_LABEL_STRING,     "Item type",
        PANEL_CHOICE_STRINGS,   "Button", "Message", "Text", NULL,
        NULL);
    window_fit(panel);

    /* Create a new panel to be used for panel creation.  The panel
     * from above is no longer referenced.  The panel created here
     * is the panel used throughout the rest of this program.  To
     * add confusion, "panel" is used as the handle of this panel, too.
     * The panel referenced in WIN_RIGHT_OF and XV_HEIGHT is the old
     * one since the new one hasn't been created yet.
     */
    panel = (Panel)xv_create(frame, PANEL,
        WIN_RIGHT_OF,           canvas_paint_window(panel),
        XV_WIDTH,               300,
        XV_HEIGHT,              xv_get(panel, XV_HEIGHT), 
        WIN_BORDER,   		TRUE,  	
        XV_KEY_DATA,            PANEL_KEY,      panel,
        NULL);

    /* Install event handling routine for the panel.  This must be done 
     *	by an interpose function to make sure that the ACTION_MENU
     *	event is not consumed by the first panel before it has a chance
     *	to get to the second panel's event proc:  my_event_proc.
     */
    notify_interpose_event_func(panel, my_event_proc, NOTIFY_SAFE);
  
    /* attach various items to the text item for text_select() */
    xv_set(create_text,
        XV_KEY_DATA,            FRAME_KEY,      frame,
        XV_KEY_DATA,            PACKAGE_KEY,    item,
        XV_KEY_DATA,            PANEL_KEY,      panel,
        NULL);
    window_fit(frame);
    xv_main_loop(frame);
}

/*
 * Process events for panel's subwindow.  This routine gets -all-
 * events that occur in the panel subwindow but passes them on to
 * the normal event dispatcher when the interposed function has been
 * completed.  The notify function, my_event_proc, is only 
 * interested in MENU button events that happen on top of panel items.  
 * When the user clicks and _drags_ the MENU button on a panel item, 
 * the item is moved to where the mouse moves to.
 */
Notify_value
my_event_proc(panel, event, arg, type)
Panel 			 panel;
Event			*event;
Notify_arg		 arg;
Notify_event_type	 type;
{
    static Panel_item   item;
    static int  x_offset, y_offset;
    Frame       frame = (Frame)xv_get(panel, XV_OWNER);
    Rect        *rect, *item_rect;
    char        buf[64];

    /*
     * If the mouse is dragging an item, reset its new location.
     */
    if (event_action(event) == LOC_DRAG && item) {
        Panel_item pi;
        Rect r;
        /*
         * Get the rect of item, then *copy* it -- never change data
         * returned by xv_get().  Modify the copied rect reflecting
         * new X,Y position of panel item and check to see if it
         * intersects with any existing panel items.
         */
        rect = (Rect *)xv_get(item, XV_RECT);
        rect_construct(&r, /* see <xview/rect.h> for macros */
            rect->r_left, rect->r_top, rect->r_width, rect->r_height);
        r.r_left = event->ie_locx - x_offset;
        r.r_top = event->ie_locy - y_offset;
        PANEL_EACH_ITEM(panel, pi)
            if (pi == item)
                continue;
            /* don't let panel items overlap */
            item_rect = (Rect *)xv_get(pi, XV_RECT);
            if (rect_intersectsrect(item_rect, &r))
                return;
        PANEL_END_EACH
        /* no overlap -- move panel item. */
        xv_set(item,
            PANEL_ITEM_X, r.r_left,
            PANEL_ITEM_Y, r.r_top,
            NULL);
    }

    /* If it's not the MENU button, we're not interested,
     * so allow the event to be passed on to the notifier
     * for normal event handling.
     */
    if (event_action(event) != ACTION_MENU) {  
	notify_next_event_func(panel, (Notify_event) event, arg, type);
	return;
    }

    /*
     * next two cases is MENU button just-down or just-released
     */
    if (event_is_down(event)) {
        /* Right (MENU) button down on an item -- determine panel item */
	if ( (xv_get((panel), PANEL_FIRST_ITEM) ) == NULL ) {
	        sprintf(buf, "No panel items are currently in the panel.");
		xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
	}
        PANEL_EACH_ITEM(panel, item)
            rect = (Rect *)xv_get(item, XV_RECT);
            if (rect_includespoint(rect,
                event->ie_locx, event->ie_locy)) {
                x_offset = event->ie_locx - rect->r_left;
                y_offset = event->ie_locy - rect->r_top;
	        sprintf(buf, "Panel item found.");
		xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
                break;
            }
	    else {
	        sprintf(buf, "The cursor is not over any panel item.");
		xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
	      }
        PANEL_END_EACH
        if (item)
            sprintf(buf, "Moving item: '%s'",
                (char *)xv_get(item, PANEL_LABEL_STRING));
        else
            buf[0] = 0;
    } else if (item) {
        char *name = (char *)xv_get(item, PANEL_LABEL_STRING);

        /* test if item is inside panel by comparing XV_RECTs */
        rect = (Rect *)xv_get(panel, XV_RECT);
        if (!rect_includespoint(rect,
            event->ie_locx + rect->r_left,
            event->ie_locy + rect->r_top)) {
            /* item is outside the panel -- remove item */
            xv_destroy(item);
            sprintf(buf, "Removed '%s' from panel", name);
        } else
            sprintf(buf, "'%s' moved to %d %d", name,
                (int)xv_get(item, XV_X), (int)xv_get(item, XV_Y));
        /* set "item" to null so that new drag
         * events don't attempt to move old item.
         */
        item = NULL;
    }
    xv_set(frame, FRAME_LEFT_FOOTER, buf, NULL);
}

/*
 * Callback routine for all panel buttons.
 * If the panel item is the text item, determine the name of the new
 * panel button the user wishes to create.  Loop through all the
 * existing panel items looking for one with the same label.  If so,
 * return PANEL_NONE and set the frame's footer with an error message.
 * Otherwise, create a new panel item with the label, reset the text
 * item value and return PANEL_NEXT.
 */
int
create_item(item, event)
Panel_item item;
Event *event;
{
    Xv_pkg      *pkg;
    Panel       panel = (Panel)xv_get(item, XV_KEY_DATA, PANEL_KEY);
    Frame       frame = (Frame)xv_get(item, XV_KEY_DATA, FRAME_KEY);
    Panel_item  pi, pkg_item;
    char        buf[64];
    int         selected();

    pkg_item = (Panel_item)xv_get(item, XV_KEY_DATA, PACKAGE_KEY);
    (void) strncpy(buf, (char *)xv_get(item, PANEL_VALUE), sizeof buf);
    if (!buf[0])
        return PANEL_NONE;
    switch((int)xv_get(pkg_item, PANEL_VALUE)) {
        case 1: pkg = PANEL_MESSAGE; break;
        case 2: pkg = PANEL_TEXT; break;
        default: pkg = PANEL_BUTTON;
    }
    /* loop thru all panel items and check for item with same name */
    PANEL_EACH_ITEM(panel, pi)
        if (!strcmp(buf, (char *)xv_get(pi, PANEL_LABEL_STRING))) {
            xv_set(frame, FRAME_LEFT_FOOTER, "Label Taken", NULL);
            return PANEL_NONE;
        }
    PANEL_END_EACH
    (void) xv_create(panel, pkg,
        PANEL_LABEL_STRING,     buf,
        PANEL_NOTIFY_PROC,      selected,
        XV_KEY_DATA,            FRAME_KEY,      frame,
        /* only for text items, but doesn't affect other items */
        PANEL_VALUE_DISPLAY_LENGTH, 10,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        NULL);
    xv_set(item, PANEL_VALUE, "", NULL);
    return PANEL_NEXT;
}

/*
 * For panel buttons. return XV_OK or XV_ERROR if the item was
 * selected using the left mouse button or not.
 */
int
selected(item, event)
Panel_item item;
Event *event;
{
    Frame       frame = (Frame)xv_get(item, XV_KEY_DATA, FRAME_KEY);
    char        buf[64];

    if (event_action(event) == ACTION_SELECT) {
        sprintf(buf, "'%s' selected", xv_get(item, PANEL_LABEL_STRING));
        xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
        return XV_OK;
    }
    return XV_ERROR;
}

