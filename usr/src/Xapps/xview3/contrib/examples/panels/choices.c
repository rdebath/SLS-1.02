/*
 * choices.c -- displays several ways of presenting choices for
 * selection.  
 */

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/openmenu.h>

Panel panel;

main(argc, argv)
int argc;
char *argv[];
{
    Frame	frame;
    Menu	menu;
    void	quit(); 
    int		numeric_text();
    int		selected(), toggle_selected();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = xv_create(XV_NULL, FRAME,
	FRAME_LABEL,		argv[0],
	XV_WIDTH,		450,
	XV_HEIGHT,		250,
	FRAME_SHOW_FOOTER,	TRUE,
	NULL);
    panel = xv_create(frame, PANEL, NULL);
    xv_set(canvas_paint_window(panel), NULL);
    xv_create(panel, PANEL_BUTTON,
	PANEL_LABEL_STRING,     "Quit",
	PANEL_NOTIFY_PROC,      quit,
	PANEL_CLIENT_DATA,	frame,
	NULL);

    xv_create(panel, PANEL_CHOICE,
	PANEL_CHOICE_STRINGS,	"One", "Two", "Three", "Four", NULL,
	PANEL_NOTIFY_PROC,      selected,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_create(panel, PANEL_CHOICE,
	PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
	PANEL_LABEL_STRING,     "Choices",
	PANEL_CHOICE_STRINGS,	"One", "Two", "Three", "Four", NULL,
	PANEL_NOTIFY_PROC,      selected,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_create(panel, PANEL_CHOICE,
	PANEL_LABEL_STRING,     "Choices",
	PANEL_CHOICE_STRINGS,	"One", "Two", "Three", "Four", NULL,
	PANEL_NOTIFY_PROC,      selected,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_create(panel, PANEL_CHOICE,
	PANEL_CHOOSE_ONE,	FALSE,
	PANEL_LABEL_STRING,     "Choices",
	PANEL_VALUE,		5, /* choices 1 and 3 */
	PANEL_CHOICE_STRINGS,	"One", "Two", "Three", "Four", NULL,
	PANEL_NOTIFY_PROC,      toggle_selected,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_create(panel, PANEL_TOGGLE,
	PANEL_FEEDBACK,		PANEL_MARKED,
	PANEL_LABEL_STRING,     "Choices",
	PANEL_VALUE,		5, /* choices 1 and 3 */
	PANEL_CHOICE_STRINGS,	"One", "Two", "Three", "Four", NULL,
	PANEL_NOTIFY_PROC,      toggle_selected,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_create(panel, PANEL_NUMERIC_TEXT,
	PANEL_LABEL_STRING,	"Numbers:",
	PANEL_VALUE,		5,
	PANEL_MAX_VALUE,	1000000,
	PANEL_MIN_VALUE,	-1000000,
	PANEL_NOTIFY_PROC,	numeric_text,
	PANEL_CLIENT_DATA,	frame,
	NULL);
    xv_main_loop(frame);
}

int
toggle_selected(item, value, event)
Panel_item item;
unsigned value;
Event *event;
{
    char buf[32];
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);
    int i;
    buf[0] = 0;
    if (event_id(event) == MS_LEFT) {
	for (i = 0; value; i++, value >>= 1)
	    if (value & 1)
		sprintf(buf+strlen(buf), "%s%c ",
		    xv_get(item, PANEL_CHOICE_STRING, i),
		    (value >> 1)? ',' : ' ');
	xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
	return XV_OK;
    }
    return XV_ERROR;
}

int
selected(item, value, event)
Panel_item item;
int value;
Event *event;
{
    char buf[32];
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);
    if (event_id(event) == MS_LEFT) {
	sprintf(buf, "\"%s\" selected",
	    xv_get(item, PANEL_CHOICE_STRING, panel_get_value(item)));
	xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
	return XV_OK;
    }
    return XV_ERROR;
}


numeric_text(item, event)
Panel_item item;
Event *event;
{
    char buf[32];

    Frame frame = xv_get(item, PANEL_CLIENT_DATA);


    sprintf(buf, "\"%s\" set to %d",
       (char *)xv_get(item, PANEL_LABEL_STRING), (int)xv_get(item, PANEL_VALUE));
    xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
    return PANEL_NEXT;
}

void
quit(item, event)
Panel_item item;
Event *event;
{
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);
    if (event_id(event) == MS_LEFT)
	xv_destroy_safe(frame);
}
