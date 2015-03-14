/* 
 * sel_hold.c: Example of how to acquire and hold a selection.
 *	       Chris Kasso	1/1/91
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/font.h>
#include <xview/sel_pkg.h>

Frame 		frame;
Xv_Server	server;
Panel  		panel;
Panel_item	p_selection,
		p_contents,
		p_own,
		p_lose;

Selection_owner sel;
Selection_item  sel_targets;

#define ATOM(server, name)	(Atom)xv_get(server, SERVER_ATOM, name)

main(argc, argv)
    int    argc;
    char **argv;
{
    Panel_setting	NotifyProc();
    int			SelectionConvertProc();
    void		SelectionDoneProc(),
			SelectionLoseProc();
    Xv_Font		font;
    Atom		targets[5];

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    frame = xv_create((Window)NULL, FRAME,
			XV_X, 		520,
			XV_Y, 		655,
			XV_LABEL, 	"Selection Holder Example",
			FRAME_SHOW_FOOTER,	True,
			NULL);

    font = (Xv_Font)xv_find(frame, FONT, FONT_NAME, "lucida-19", NULL);
    if (!font) {
	fprintf(stderr, "Cannot use font: lucida-19.\n");
	font = (Xv_Font)xv_get(frame, XV_FONT);
    }

    panel = xv_create(frame, PANEL,
			XV_FONT,	font,
		 	NULL);

    p_selection = xv_create(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, 		"Selection:",
			PANEL_VALUE_DISPLAY_LENGTH, 	40,
			PANEL_NOTIFY_PROC,		NotifyProc,
			PANEL_ITEM_X,			xv_col(panel,0),
			PANEL_ITEM_Y,			xv_row(panel,0),
			NULL);
    p_contents = xv_create(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, 		"Contents:",
			PANEL_VALUE_DISPLAY_LENGTH, 	40,
			PANEL_ITEM_X,			xv_col(panel,0),
			PANEL_ITEM_Y,			xv_row(panel,1),
			NULL);
    p_own = xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, 	"Own Selection",
			PANEL_NOTIFY_PROC,	NotifyProc,
			PANEL_ITEM_X,		xv_col(panel,5),
			PANEL_ITEM_Y,		xv_row(panel,2),
			NULL);
    p_lose = xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, 	"Lose Selection",
			PANEL_NOTIFY_PROC,	NotifyProc,
			PANEL_ITEM_X,		xv_col(panel,30),
			PANEL_ITEM_Y,		xv_row(panel,2),
			NULL);

	/* Create a selection owner object */
    sel = xv_create(panel, SELECTION_OWNER,
			SEL_CONVERT_PROC,	SelectionConvertProc,
			SEL_DONE_PROC,		SelectionDoneProc,
			SEL_LOSE_PROC,		SelectionLoseProc,
			NULL);

    targets[0] = (Atom)xv_get(server, SERVER_ATOM, "TARGETS");
    targets[1] = (Atom)xv_get(server, SERVER_ATOM, "TIMESTAMP");
    targets[2] = (Atom)xv_get(server, SERVER_ATOM, "LENGTH");
    targets[3] = (Atom)xv_get(server, SERVER_ATOM, "STRING");
    targets[4] = (Atom)xv_get(server, SERVER_ATOM, "DELETE");

	/* Create a selection item, ownened by the selection owner we just
	 * created.  This pre-registers a conversion, in this case a
	 * conversion for ``TARGETS''.
	 */
    sel_targets = xv_create(sel, SELECTION_ITEM,
			SEL_TYPE_NAME, 		"TARGETS",
			SEL_FORMAT,		32,
			SEL_LENGTH,		5,
			SEL_DATA,		(Xv_opaque)targets,
			NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
    exit(0);
}

Panel_setting
NotifyProc(item, event)
    Panel_item       item;
    Event           *event;
{
    if (item == p_selection) {
        char     *rank;

		/* Get the rank of the selection the user would like to use. */
    	rank = (char *)xv_get(item, PANEL_VALUE);
		/* Set the rank to our selection owner object. */
    	xv_set(sel, SEL_RANK_NAME, rank, NULL);

        return(PANEL_NEXT);
    } else if (item == p_own) {
		/* The user pressed the ``own'' button, so we acquire the
		 * selection.
		 */
	xv_set(sel, SEL_OWN, True,
		    SEL_TIME, event_time(event),
		    NULL);
	xv_set(frame, FRAME_LEFT_FOOTER, "Acquired Selection...", NULL);
    } else if (item == p_lose) {
		/* The user pressed the ``lose'' button, so we lose ownership
		 * of the selection.
		 */
	xv_set(sel, SEL_OWN, False,
		    SEL_TIME, event_time(event),
		    NULL);
	xv_set(frame, FRAME_LEFT_FOOTER, "Lost Selection...", NULL);
    }
    return(PANEL_DONE);
}

	/* The conversion procedure is called whenever some client makes
	 * a request to our selection.  Its purpose is to respond to the
	 * request.
	 */
int
SelectionConvertProc(sel, target, data, length, format)
    Selection_owner	 sel;
    Atom		*target;	/* Input/Output */
    Xv_opaque		*data;		/* Output */
    unsigned long	*length;	/* Output */
    int			*format;	/* Output */
{
	/* Request for the length of the selection. */
    if (*target == ATOM(server, "LENGTH")) {
	static unsigned long	 len;
	char			*contents;

	contents = (char *)xv_get(p_contents, PANEL_VALUE);
	len = strlen(contents);

	*target = ATOM(server, "INTEGER");
	*format = 32;
	*length = 1;
	*data = (Xv_opaque)&len; 
	return(True);
	/* Request for the string contents of the selection. */
    } else if (*target == ATOM(server, "STRING")) {
	char 			*contents;

	contents = (char *)xv_get(p_contents, PANEL_VALUE);

	*target = ATOM(server, "STRING");
	*format = 8;
	*length = strlen(contents);
	*data = (Xv_opaque)strdup(contents); 
	return(True);
	/* Request to delete the selection. */
    } else if (*target == ATOM(server, "DELETE")) {
	xv_set(p_contents, PANEL_VALUE, "", NULL);
	*target = ATOM(server, "NULL");
	*format = 32;
	*length = 0;
	*data = (Xv_opaque)NULL;
	return(True);
    } else
	/* Call the default selection conversion procedure.  Will handle
	 * requests for any pre-registered conversions.
	 */
	return(sel_convert_proc(sel, target, data, length, format));
}

	/* The selection done procedure is called after each conversion has
	 * happened.  This gives the application a chance to free up
	 * memory.
	 */
void
SelectionDoneProc(sel, data, target)
    Selection_owner	 sel;
    Xv_opaque		*data;
    Atom		 target;
{
    if (target == ATOM(server, "STRING"))
	free((char *)data);
}

	/* If some other client acquires the selection we hold, the
	 * selection lose procedure is called.  This tells us that we
	 * no longer hold that selection.
	 */
void
SelectionLoseProc(sel)
    Selection_owner	sel;
{
    xv_set(frame, FRAME_LEFT_FOOTER, "Lost Selection...", NULL);
}
