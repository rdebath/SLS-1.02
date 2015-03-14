/*
 * $XConsortium: actions.c,v 1.12 91/07/09 12:09:07 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>

#include <X11/Xaw/Cardinals.h>	

#include "editresP.h"

/*
 * External Functions.
 */

extern void SetMessage(), _TreeSelect(), _TreeSelectNode(), _FindWidget();
extern void _TreeActivateNode(), _TreeRelabel(), _TreeRelabelNode();
extern void PrepareToLayoutTree(), LayoutTree(), _PopdownFileDialog();

/*
 * Private data.
 */

struct ActionValues {
    String name;
    int type;
};

static struct ActionValues select_values[] = {
    { "widget", (int) SelectWidget },
    { "all", (int) SelectAll },
    { "nothing", (int) SelectNone },
    { "invert", (int) SelectInvert },
    { "children", (int) SelectChildren },
    { "descendants", (int) SelectDescendants },
    { "parent", (int) SelectParent },
    { "ancestors", (int) SelectAncestors }
};

static struct  ActionValues label_values[] = {
    { "name", (int) NameLabel },
    { "class", (int) ClassLabel },
    { "id", (int) IDLabel },
    { "window", (int) WindowLabel },
    { "toggle", (int) ToggleLabel }
};

static WNode * FindTreeNodeFromWidget();
static Boolean CheckAndFindEntry();

/*	Function Name: SelectAction
 *	Description: 
 *      Arguments: w - any widget in the widget tree.
 *                 event - NOT USED.
 *                 params, num_params - the parameters paseed to the action
 *                                      routine. 
 *
 * params[0] - One of "nothing", "parent", "children", "ancestors",
 *                    "descendants", "invert", "all"
 * num_params - must be one.
 */

/* ARGSUSED */
static void
SelectAction(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    WNode * node;
    int type;

    if (!CheckAndFindEntry("Select", params, *num_params, 
			   select_values, XtNumber(select_values), &type))
	return;

    switch(type) {
    case SelectAll:
    case SelectNone:
    case SelectInvert:
	_TreeSelect(global_tree_info, type);
	break;
    case SelectWidget:
	_FindWidget(XtParent(w));
	break;
    default:
	node = FindTreeNodeFromWidget(w);
	if (node)
	    _TreeActivateNode(node, type);	
	else
	    _TreeSelect(global_tree_info, type);
	break;
    }
}

/*	Function Name: RelabelAction
 *	Description: 
 *      Arguments: w - any widget in the widget tree.
 *                 event - NOT USED.
 *                 params, num_params - the parameters paseed to the action
 *                                      routine. 
 *
 * params[0] - One of "name", "class", "id"
 * num_params - must be one.
 */

/* ARGSUSED */
static void
RelabelAction(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    WNode * node;
    int type;

    if (!CheckAndFindEntry("Relabel", params, *num_params, 
			   label_values, XtNumber(label_values), &type))
	return;

    if ((node = FindTreeNodeFromWidget(w)) == NULL) 
	_TreeRelabel(global_tree_info, type);
    else {
	PrepareToLayoutTree(global_tree_info->tree_widget); 
	_TreeRelabelNode(node, type, FALSE);
	LayoutTree(global_tree_info->tree_widget); 
    }
}

/*	Function Name: PopdownFileDialogAction
 *	Description: Pops down the file dialog widget.
 *                   and calls the approipriate handler.
 *	Arguments: w - any child of the dialog widget.
 *                 event - the event that caused this action.
 *                 params, num_params - params passed to the action routine.
 * RETURNED        none.
 */

/* ARGSUSED */

static void
PopdownFileDialogAction(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    char buf[BUFSIZ];
    Boolean val;

    if (*num_params != 1) {
	sprintf(buf, "Action `%s' must have exactly one argument.", 
		"PopdownFileDialog");

	SetMessage(global_screen_data.info_label, buf);
	return;
    }

    XmuCopyISOLatin1Lowered(buf, params[0]);

    if (streq(buf, "cancel"))
	val = FALSE;
    else if (streq(buf, "okay"))
	val = TRUE;
    else {
	sprintf(buf, "Action %s's argument must be either `cancal' or `okay'.",
		"PopdownFileDialog");

	SetMessage(global_screen_data.info_label, buf);
	return;
    }

    _PopdownFileDialog(w, (XtPointer) val, NULL);
}

/*	Function Name: ActionQuit
 *	Description: This function prints a message to stdout.
 *	Arguments: w - ** UNUSED **
 *                 call_data - ** UNUSED **
 *                 client_data - ** UNUSED **
 *	Returns: none
 */

extern Widget toplevel;
/* ARGSUSED */
static void
ActionQuit(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
  if (w==toplevel) {
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit(0);
  }
  else  {
    if (streq(XtName(w), RESOURCE_BOX))
      global_resource_box_up = FALSE;
    XtPopdown(w);
    XtDestroyWidget(w);
  }
}

/*	Function Name: SetApplicationActions
 *	Description: Sets my application actions.
 *	Arguments: app_con - the application context.
 *	Returns: none.
 */

extern void ModifySVEntry();

static XtActionsRec actions[] = {
  {"Select",            SelectAction},
  {"SVActiveEntry",     ModifySVEntry},
  {"Relabel",      	RelabelAction}, 
  {"PopdownFileDialog", PopdownFileDialogAction},
  {"quit",              ActionQuit}
};

void
SetApplicationActions(app_con)
XtAppContext app_con;
{
    XtAppAddActions(app_con, actions, XtNumber(actions));
    
}


/************************************************************
 *
 * Private functions	
 *
 ************************************************************/

/*	Function Name: CheckAndFindEntry
 *	Description: Checks the args to make sure they are valid,
 *                   then parses the arg list to find the correct action
 *                   to take.
 *	Arguments: action_name - name of the action (for error messages).
 *                 params, num_params - params passed to the action routine.
 *                 table, num_table - table to check the parameters against.
 * RETURNED        type - info about the action to take.
 *	Returns: TRUE if the arguments are okay.
 */

static Boolean
CheckAndFindEntry(action_name, params, num_params, table, num_table, type)
String * params, action_name;
Cardinal num_params, num_table;
struct ActionValues * table;
int * type;
{
    char buf[BUFSIZ];
    int i;

    if (num_params != 1) {
	sprintf(buf, "Action `%s' must have exactly one argument.", 
		action_name);
	SetMessage(global_screen_data.info_label, buf);
	return(FALSE);
    }
	
    XmuCopyISOLatin1Lowered(buf, params[0]);
    for ( i = 0 ; i < num_table; i++ ) 
	if (streq(buf, table[i].name)) {
	    *type = table[i].type;
	    return(TRUE);
	}
    
    sprintf(buf,"Unknown parameter to action `%s' must be one of:\n", 
	    action_name);

    for (i = 0; i < num_table; ) {
	strcat(buf, table[i++].name);
	
	if (i == (num_table - 1))
	    strcat(buf, ", or ");
	else if (i < num_table)
	    strcat(buf, ", ");
    }
    
    SetMessage(global_screen_data.info_label, buf);
    return(FALSE);
}

/*	Function Name: FindTreeNodeFromWidget
 *	Description: finds the tree node associated with a widget.
 *	Arguments: w - widget to check.
 *	Returns: the node associated with this widget, or NULL.
 */

static WNode *
FindTreeNodeFromWidget(w)
Widget w;
{
    int ret_val;
    caddr_t data_return;

    /*
     * Yes, I really am casting a widget to a window  ** TRUST ME ***
     */

    ret_val = XFindContext(XtDisplay(w), (Window) w, NODE_INFO, &data_return);

    if (ret_val == 0) 
	return((WNode *) data_return);
    return(NULL);
}

