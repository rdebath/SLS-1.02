/* 
 * sel_req.c: Example of how to make requests to a selection owner for 
 * 	      the selection contents.
 *	      Chris Kasso	1/1/91
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
Textsw  	textsw;
Xv_Server	server;
Panel  		panel;
Panel_item	p_selection,
		p_target,
		p_request;

Selection_requestor  sel;

#define TARGETS		1<<0
#define TIMESTAMP	1<<1
#define LENGTH		1<<2
#define STRING		1<<3
#define DELETE		1<<4
#define LINE 		"------------------------------------------------------"

#define ATOM(server, name)	(Atom)xv_get(server, SERVER_ATOM, name)

main(argc, argv)
    int    argc;
    char **argv;
{
    void	MakeRequest(),
		SelectionReplyProc(),
		RequestChoice();
    Xv_Font	font;

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    frame = xv_create((Window)NULL, FRAME,
			XV_X, 		10,
			XV_Y, 		10,
			XV_LABEL, 	"Selection Requestor Example",
			NULL);

    font = (Xv_Font)xv_find(frame, FONT, FONT_NAME, "lucida-19", NULL);
    if (!font) {
	fprintf(stderr, "Cannot use font: lucida-19.\n");
	font = (Xv_Font)xv_get(frame, XV_FONT);
    }

    panel = xv_create(frame, PANEL,
			PANEL_LAYOUT,	PANEL_VERTICAL,
			XV_FONT,	font,
		 	NULL);

    p_selection = xv_create(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, 		"Selection:",
			PANEL_VALUE_DISPLAY_LENGTH, 	40,
			PANEL_NOTIFY_PROC,		MakeRequest,
			NULL);
    p_target = xv_create(panel, PANEL_TOGGLE,
			PANEL_LABEL_STRING, 		"Request:",
			PANEL_NOTIFY_PROC,		RequestChoice,
			PANEL_CHOICE_STRINGS,		"TARGETS",
							"TIMESTAMP",
							"LENGTH",
							"STRING",
							"DELETE",
							NULL,
			NULL);
    p_request = xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, 	"Make Request",
			PANEL_NOTIFY_PROC,	MakeRequest,
			PANEL_ITEM_X,		xv_cols(panel, 20),
			0);

    window_fit(panel);

    textsw = xv_create(frame, TEXTSW,
			XV_X,		0,
			XV_FONT,	font,
			WIN_BELOW,	panel,
			NULL);

	/* Create a selection requestor object. */
    sel = xv_create(panel, SELECTION_REQUESTOR,
			SEL_REPLY_PROC,	SelectionReplyProc,
			NULL);

    window_fit(frame);
    xv_main_loop(frame);
    exit(0);
}

void
RequestChoice(item, value, event)
    Panel_item       item;
    unsigned int     value;
    Event           *event;
{
    int   set = False;

    /* Build the request based on the toggle items the user has selected. */

    if (value & TARGETS) {
	if (set)
	    xv_set(sel, SEL_APPEND_TYPE_NAMES, "TARGETS", NULL, NULL);
	else 
	    xv_set(sel, SEL_TYPE_NAME, "TARGETS", NULL);
	set = True;
    }
    if (value & TIMESTAMP) {
	if (set)
	    xv_set(sel, SEL_APPEND_TYPE_NAMES, "TIMESTAMP", NULL, NULL);
	else 
	    xv_set(sel, SEL_TYPE_NAME, "TIMESTAMP", NULL);
	set = True;
    } 
    if (value & LENGTH) {
	if (set)
	    xv_set(sel, SEL_APPEND_TYPE_NAMES, "LENGTH", NULL, NULL);
	else 
	    xv_set(sel, SEL_TYPE_NAME, "LENGTH", NULL);
	set = True;
    } 
    if (value & STRING) {
	if (set)
	    xv_set(sel, SEL_APPEND_TYPE_NAMES, "STRING", NULL, NULL);
	else 
	    xv_set(sel, SEL_TYPE_NAME, "STRING", NULL);
	set = True;
    } 
    if (value & DELETE) {
	if (set)
	    xv_set(sel, SEL_APPEND_TYPE_NAMES, "DELETE", NULL, NULL);
	else 
	    xv_set(sel, SEL_TYPE_NAME, "DELETE", NULL);
	set = True;
    }
}

void
MakeRequest(item, event)
    Panel_item       item;
    Event           *event;
{
    if (item == p_selection) {
	char *rank = NULL;

	/* Set the rank of the selection we are going to make requests to. */
	rank = (char *)xv_get(item, PANEL_VALUE);
	xv_set(sel, SEL_RANK_NAME, 	rank,
		    NULL);
    } else {
	/* Post a non-blocking request to the selection holder. */
	sel_post_req(sel);
        textsw_erase(textsw, 0, TEXTSW_INFINITY);
    }
}

	/* When the selection holder replies to our request(s), the
	 * Selection reply procedure is called with the information we
	 * requested.
	 */
void
SelectionReplyProc(sel, target, type, value, length, format)
    Selection_requestor	sel;
    Xv_opaque		value;
    Atom		target;
    Atom		type;
    unsigned long	length;
    int			format;
{
    if (length == SEL_ERROR) {
	SelectionError(sel, target, *(int *)value);
	return;
    }

    if (target == ATOM(server, "TARGETS")) {
	textsw_insert(textsw, "Holder will convert the following targets:\n",
		      43);
	do {
	    Atom *targets = (Atom *)value;
	    char *target_name;

	    if (targets[--length]) {
	        target_name = (char *)xv_get(server, SERVER_ATOM_NAME,
					     targets[length]);

	        textsw_insert(textsw, "\t", 1);
	        textsw_insert(textsw, target_name, strlen(target_name));
	        textsw_insert(textsw, "\n", 1);
	    }
	} while(length);
    } else if (target == ATOM(server, "TIMESTAMP")) {
	char buf[10];

	textsw_insert(textsw, "TIMESTAMP of acquisition: ", 26); 
	sprintf(buf, "%U\n", *(unsigned long *)value);
	textsw_insert(textsw, buf, strlen(buf));
    } else if (target == ATOM(server, "LENGTH")) {
	char buf[10];

	textsw_insert(textsw, "Length of selection: ", 21); 
	sprintf(buf, "%d\n", *(int *)value);
	textsw_insert(textsw, buf, strlen(buf));
    } else if (target == ATOM(server, "STRING")) {
	static int	incr = False;

	if (type == ATOM(server, "INCR")) {
	    textsw_insert(textsw, "Contents of the selection:\n", 27); 
	    incr = True;
	} else if (length) {
	    if (!incr)
	        textsw_insert(textsw, "Contents of the selection:\n", 27); 
	    textsw_insert(textsw, (char *)value, length); 
	    textsw_insert(textsw, "\n", 1); 
	} else
	    incr = False;
    } else if (target == ATOM(server, "DELETE")) {
	textsw_insert(textsw, "The Selection has been deleted\n", 31);
    }
    textsw_insert(textsw, LINE, strlen(LINE));
}

SelectionError(sel, target, errorCode)
    Selection_requestor	 sel;
    Atom	 	 target;
    int		 	 errorCode;
{
    Atom     rank;
    char    *rank_string;
    char    *target_string = (char *)xv_get(server, SERVER_ATOM_NAME, target); 
    char     msg[100];

    rank = (Atom)xv_get(sel, SEL_RANK);
    rank_string = (char *)xv_get(server, SERVER_ATOM_NAME, rank); 

    sprintf(msg, "Selection failed for rank ``%s'' on target ``%s'': ",
					           rank_string, target_string);
    textsw_insert(textsw, msg, strlen(msg));
    
    switch(errorCode) {
      case SEL_BAD_PROPERTY :
	textsw_insert(textsw, "Bad Property", strlen("Bad Property"));
        break;
      case SEL_BAD_CONVERSION :
	textsw_insert(textsw, "Conversion Rejected",
		      strlen("Conversion Rejected"));
        break;
      case SEL_BAD_TIME:
	textsw_insert(textsw, "Bad Time Match", strlen("Bad Time Match"));
        break;
      case SEL_BAD_WIN_ID:
	textsw_insert(textsw, "Bad Window Match", strlen("Bad Window Match"));
        break;
      case SEL_TIMEDOUT:
	textsw_insert(textsw, "Timeout", strlen("Timeout"));
        break;
    }
}
