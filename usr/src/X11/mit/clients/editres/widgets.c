/*
 * $XConsortium: widgets.c,v 1.20 92/02/11 11:44:24 dave Exp $
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

/*
 * Code for creating all widgets used by EditRes.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>	/* Get standard string definations. */

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>	
#include <X11/Xaw/Cardinals.h>	
#include <X11/Xaw/Label.h>	
#include <X11/Xaw/List.h>	
#include <X11/Xaw/MenuButton.h>	
#include <X11/Xaw/Paned.h>	
#include <X11/Xaw/Panner.h>	
#include <X11/Xaw/Porthole.h>	
#include <X11/Xaw/SmeBSB.h>	
#include <X11/Xaw/SmeLine.h>	
#include <X11/Xaw/SimpleMenu.h>	
#include <X11/Xaw/Toggle.h>	
#include <X11/Xaw/Tree.h>
#include <X11/Xaw/Viewport.h>	

#include "editresP.h"

/*
 * functions.
 */

static void CreateResourceNameForm(), SetToggleGroupLeaders(), CreateLists();
static void CreateCommandMenu(), CreateTreeCommandMenu(), FreeClientData();
static void FreeResBox(), CreateValueWidget(), PopupOnNode();
static Widget CreateTopArea();
static void MakeBoxLookNice();

extern void GetResourceList(), AnyChosen(), SetResourceString();
extern void PannerCallback(), PortholeCallback(), DumpTreeToFile();
extern void Quit(), SendTree(), FlashActiveWidgets();
extern void TreeSelect(), TreeRelabel(), TreeActivate(), FindWidget();
extern void ResourceListCallback(), PopdownResBox(), SaveResource();
extern void GetNamesAndClasses(), ApplyResource(), ActivateResourceWidgets();
extern void ActivateWidgetsAndSetResourceString(), SetFile();

extern void InitSetValues();

/*	Function Name: BuildWidgetTree
 *	Description: Creates all widgets for Editres.
 *	Arguments: parent - the shell to put them into.
 *	Returns: none.
 */

void 
BuildWidgetTree(parent)
Widget parent;
{
    Widget paned, porthole, panner;

    paned = XtCreateManagedWidget("paned", panedWidgetClass, parent,
				  NULL, ZERO);

    panner = CreateTopArea(paned);

    porthole = XtCreateManagedWidget("porthole", portholeWidgetClass,
				     paned, NULL, ZERO);

/*
 * Allow the panner and porthole to talk to each other.
 */

    XtAddCallback(porthole, 
		  XtNreportCallback, PortholeCallback, (XtPointer) panner);
    XtAddCallback(panner, 
		  XtNreportCallback, PannerCallback, (XtPointer) porthole);

    global_tree_parent = porthole;
}

/*	Function Name: CreateTopArea
 *	Description: Creates the top part of the display
 *	Arguments: parent - widget to put this menu bar into.
 *	Returns: none. 
 */

static Widget
CreateTopArea(parent)
Widget parent;
{
    Widget box, panner, pane;

    box = XtCreateManagedWidget("box", boxWidgetClass, parent, NULL, ZERO);

    CreateCommandMenu(box);
    CreateTreeCommandMenu(box);

    pane = XtCreateManagedWidget("hPane", panedWidgetClass, parent, NULL,ZERO);

    {
	panner = XtCreateManagedWidget("panner", pannerWidgetClass, 
				       pane, NULL, ZERO);
	global_screen_data.info_label = XtCreateManagedWidget("userMessage", 
							     labelWidgetClass,
							     pane, NULL, ZERO);
    }
    return(panner);
}

/*	Function Name: CreateCommandMenu
 *	Description: Creats the command menu.
 *	Arguments: parent - widget to put this menu into.
 *	Returns: none.
 */

static void
CreateCommandMenu(parent)
Widget parent;
{
    Widget menu, entry, button;

    button = XtCreateManagedWidget("commands", menuButtonWidgetClass, parent,
				   NULL, ZERO);

    menu = XtCreatePopupShell("menu", simpleMenuWidgetClass, button,
			      NULL, ZERO);
    
    entry = XtCreateManagedWidget("sendTree", smeBSBObjectClass, menu,
				    NULL, ZERO);
    XtAddCallback(entry, XtNcallback, SendTree, (XtPointer) TRUE);

    entry = XtCreateManagedWidget("refreshTree", smeBSBObjectClass, menu,
				  NULL, ZERO);
    XtAddCallback(entry, XtNcallback, SendTree, (XtPointer) FALSE);

    entry = XtCreateManagedWidget("dumpTreeToFile", smeBSBObjectClass, menu,
				    NULL, ZERO);
    XtAddCallback(entry, XtNcallback, DumpTreeToFile, NULL);

    entry = XtCreateManagedWidget("line", smeLineObjectClass, menu,
				  NULL, ZERO);
    entry= XtCreateManagedWidget("getResourceList", smeBSBObjectClass, menu,
				 NULL, ZERO);
    XtAddCallback(entry, XtNcallback, GetResourceList, NULL);

    entry = XtCreateManagedWidget("setValues", smeBSBObjectClass, menu,
				    NULL, ZERO);
    XtAddCallback(entry, XtNcallback, InitSetValues, NULL);

    entry = XtCreateManagedWidget("line", smeLineObjectClass, menu,
				  NULL, ZERO);

    entry = XtCreateManagedWidget("quit", smeBSBObjectClass, menu,
				    NULL, ZERO);
    XtAddCallback(entry, XtNcallback, Quit, NULL);

}

/*	Function Name: CreateTreeCommandMenu
 *	Description: Creats the command menu.
 *	Arguments: parent - widget to put this menu into.
 *	Returns: none.
 */

#define SELECT 0
#define ACTIVATE 1
#define LABEL 2
#define LINE 3
#define FIND 4
#define FLASH 5

struct tree_ops_menu {
    char * name;
    int type;
    XtPointer data;
};

static void
CreateTreeCommandMenu(parent)
Widget parent;
{
    Widget menu, button, entry;
    int i, number;
    static struct tree_ops_menu tree_menu[] = {
	{ "showClientWidget", FIND, (XtPointer) NULL },
        { "selectAll", SELECT, (XtPointer) SelectAll },
	{ "unselectAll", SELECT, (XtPointer) SelectNone },
	{ "invertAll", SELECT, (XtPointer) SelectInvert },
	{ "line", LINE, (XtPointer) NULL },
	{ "selectChildren", SELECT, (XtPointer) SelectChildren },
        { "selectParent", SELECT, (XtPointer) SelectParent },
	{ "selectDescendants", SELECT, (XtPointer) SelectDescendants },
        { "selectAncestors", SELECT, (XtPointer) SelectAncestors },
        { "line", LINE, (XtPointer) NULL },
        { "showWidgetNames", LABEL, (XtPointer) NameLabel },
        { "showClassNames", LABEL, (XtPointer) ClassLabel },
        { "showWidgetIDs", LABEL, (XtPointer) IDLabel},
        { "showWidgetWindows", LABEL, (XtPointer) WindowLabel },
        { "line", LINE, (XtPointer) NULL },
	{ "flashActiveWidgets", FLASH, (XtPointer) NULL }
    };

    button = XtCreateManagedWidget("treeCommands", menuButtonWidgetClass,
				   parent, NULL, ZERO);

    menu = XtCreatePopupShell("menu", simpleMenuWidgetClass, button,
			      NULL, ZERO);

    for ( i = 0, number = XtNumber(tree_menu) ; i < number ; i++) {
	void (*func)();
	WidgetClass class = smeBSBObjectClass;

	switch (tree_menu[i].type) {
	case SELECT:
	    func = TreeSelect;
	    break;
	case LABEL:
	    func = TreeRelabel;
	    break;
	case LINE:
	    func = NULL;
	    class = smeLineObjectClass;
	    break;
	case FIND:
	    func = FindWidget;
	    break;
	case FLASH:
	    func = FlashActiveWidgets;
	    break;
	default:
	    continue;
	}

	entry = XtCreateManagedWidget(tree_menu[i].name, class, menu,
				      NULL, ZERO);
	if (func != NULL) 
	    XtAddCallback(entry, XtNcallback, func, tree_menu[i].data);
    }
}

static Pixmap old_pixmap;

/*	Function Name: PrepareToLayoutTree
 *	Description: prepares the Tree widget to be layed out.
 *	Arguments: tree - the Tree widget.
 *	Returns: none
 */

void
PrepareToLayoutTree(tree)
Widget tree;
{
    Arg args[1];

    XtSetArg(args[0], XtNbackgroundPixmap, &old_pixmap);
    XtGetValues(XtParent(tree), args, ONE);

    XtSetArg(args[0], XtNbackgroundPixmap, None);
    XtSetValues(XtParent(tree), args, ONE);

    XtUnmapWidget(tree);
}

/*	Function Name: LayoutTree
 *	Description: Laysout the tree widget.
 *	Arguments: tree - the widget tree.
 *	Returns: none.
 */

void
LayoutTree(tree)
Widget tree;
{
    Arg args[1];
    
    XawTreeForceLayout(tree);
    XtMapWidget(tree); 

    XtSetArg(args[0], XtNbackgroundPixmap, old_pixmap);
    XtSetValues(XtParent(tree), args, ONE);
}

/************************************************************
 *
 * Functions for creating the Resource Box.
 *
 ************************************************************/

/*	Function Name: CreateResourceBoxWidgets
 *	Description: Creates the widgets that make up the resource box.
 *	Arguments: node - the widget node.
 *                 names - the list of names that make up the normal resources.
 *                 cons_names - the list of names that make up 
 *                              the constraint resources. 
 *	Returns: none.
 */

void
CreateResourceBoxWidgets(node, names, cons_names)
WNode * node;
char **names, **cons_names;
{
    Widget pane, box, button;
    ResourceBoxInfo * res_box;

    res_box = (ResourceBoxInfo *) XtMalloc(sizeof(ResourceBoxInfo));
    node->resources->res_box = res_box;

    res_box->shell = XtCreatePopupShell(RESOURCE_BOX,
					transientShellWidgetClass,
					node->widget, NULL, ZERO);
    XtAddCallback(res_box->shell, XtNdestroyCallback,
		  FreeResBox, (XtPointer) node);

    pane = XtCreateManagedWidget("pane", panedWidgetClass, 
				 res_box->shell, NULL, ZERO);

    res_box->res_label = XtCreateManagedWidget("resourceLabel", 
					       labelWidgetClass, 
					       pane, NULL, ZERO);

    CreateResourceNameForm(pane, node);
    CreateLists(pane, node, names, cons_names);
    CreateValueWidget(pane, node);

    XtSetKeyboardFocus(pane, res_box->value_wid); /* send keyboard to value. */

    box = XtCreateManagedWidget("commandBox", boxWidgetClass,
				 pane, NULL, ZERO);

    button = XtCreateManagedWidget("setFile", commandWidgetClass,
				   box, NULL, ZERO);
    XtAddCallback(button, XtNcallback, SetFile, NULL);

    button = XtCreateManagedWidget("save", commandWidgetClass,
				   box, NULL, ZERO);
    XtAddCallback(button, XtNcallback, SaveResource,(XtPointer) res_box);

    button = XtCreateManagedWidget("apply", commandWidgetClass,
				   box, NULL, ZERO);
    XtAddCallback(button, XtNcallback, ApplyResource,(XtPointer) node);

    button = XtCreateManagedWidget("saveAndApply", commandWidgetClass,
				   box, NULL, ZERO);
    XtAddCallback(button, XtNcallback, SaveResource,(XtPointer) res_box);
    XtAddCallback(button, XtNcallback, ApplyResource,(XtPointer) node);

    button = XtCreateManagedWidget("cancel", commandWidgetClass,
				   box, NULL, ZERO);
    XtAddCallback(button,XtNcallback,PopdownResBox,(XtPointer)res_box->shell);

    SetToggleGroupLeaders(node);
    PopupOnNode(node, res_box->shell);
}

/*	Function Name: CreateResourceNameForm
 *	Description: Creates the Form widget with children that represent
 *                   the full resource name for this object.
 *	Arguments: parent - parent of the form.
 *                 node - the node corrosponding to this object.
 *	Returns: none
 */

static void
CreateResourceNameForm(parent, node)
Widget parent;
WNode * node;
{
    ResourceBoxInfo * res_box = node->resources->res_box;
    AnyInfo *new_info, *old_info;
    char **names, **classes;
    Widget form;
    NameInfo * name_info = NULL;
    Cardinal num_args;
    Arg args[10];
    int i;
    Widget dot, star, name, class, single, any;

    GetNamesAndClasses(node, &names, &classes);

    form = XtCreateManagedWidget("namesAndClasses", formWidgetClass,
				 parent, NULL, ZERO);

    name = class = any = NULL;
    i = 0;
    old_info = NULL;
    while (TRUE) {

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, name); num_args++;
	XtSetArg(args[num_args], XtNradioData, "."); num_args++;
	dot = XtCreateManagedWidget("dot", toggleWidgetClass, 
				    form, args, num_args);
	XtAddCallback(dot, XtNcallback, 
		      ActivateWidgetsAndSetResourceString,(XtPointer) node);

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, class); num_args++;
	XtSetArg(args[num_args], XtNfromVert, dot); num_args++;
	XtSetArg(args[num_args], XtNradioGroup, dot); num_args++;
	XtSetArg(args[num_args], XtNradioData, "*"); num_args++;
	star = XtCreateManagedWidget("star", toggleWidgetClass, 
				     form, args, num_args);
	XtAddCallback(star,XtNcallback, 
		      ActivateWidgetsAndSetResourceString, (XtPointer) node);

	if (name_info != NULL) {
	    name_info->next = (NameInfo *) XtMalloc(sizeof(NameInfo));
	    name_info = name_info->next;
	}
	else
	    res_box->name_info = 
		     name_info = (NameInfo *) XtMalloc(sizeof(NameInfo));

	name_info->sep_leader = dot;
	name_info->name_leader = NULL;

	if (names[i] != NULL) {
	    new_info = (AnyInfo *) XtMalloc(sizeof(AnyInfo));
	    new_info->node = node;
	    new_info->left_dot = dot;
	    new_info->left_star = star;
	    new_info->left_count = 0;
	    if (old_info != NULL) 
		old_info->right_count = &(new_info->left_count);
	}
	else if (old_info != NULL) 
	    old_info->right_count = NULL;

	if (old_info != NULL) {
	    old_info->right_dot = dot;
	    old_info->right_star = star;

	    XtAddCallback(any, XtNcallback, AnyChosen, (XtPointer) old_info);
	    XtAddCallback(any, XtNdestroyCallback, 
			  FreeClientData, (XtPointer) old_info);
	}

	if ( names[i] == NULL) /* no more name and class boxes. */
	    break;

	old_info = new_info;

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, dot); num_args++;
	XtSetArg(args[num_args], XtNlabel, names[i]); num_args++;
	XtSetArg(args[num_args], XtNradioData, names[i]); num_args++;
	name = XtCreateManagedWidget("name", toggleWidgetClass, 
				     form, args, num_args);
	XtAddCallback(name,XtNcallback,
		      ActivateWidgetsAndSetResourceString,(XtPointer) node);

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, star); num_args++;
	XtSetArg(args[num_args], XtNfromVert, name); num_args++;
	XtSetArg(args[num_args], XtNlabel, classes[i]); num_args++;
	XtSetArg(args[num_args], XtNradioGroup, name); num_args++;
	XtSetArg(args[num_args], XtNradioData, classes[i]); num_args++;
	class = XtCreateManagedWidget("class", toggleWidgetClass, 
				      form,args,num_args);
	XtAddCallback(class, XtNcallback,
		      ActivateWidgetsAndSetResourceString,(XtPointer) node);

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, star); num_args++;
	XtSetArg(args[num_args], XtNfromVert, class); num_args++;
	XtSetArg(args[num_args], XtNradioData, "?"); num_args++;
	XtSetArg(args[num_args], XtNradioGroup, name); num_args++;
	single = XtCreateManagedWidget("single", toggleWidgetClass, 
				       form, args, num_args);
	XtAddCallback(single,XtNcallback,
		      ActivateWidgetsAndSetResourceString,(XtPointer) node);

	num_args = 0;
	XtSetArg(args[num_args], XtNfromHoriz, any); num_args++;
	XtSetArg(args[num_args], XtNfromVert, single); num_args++;
	XtSetArg(args[num_args], XtNradioGroup, name); num_args++;
	XtSetArg(args[num_args], XtNradioData, ANY_RADIO_DATA); num_args++;
	any = XtCreateManagedWidget("any", toggleWidgetClass, 
				    form, args, num_args);

	name_info->name_leader = name;

	MakeBoxLookNice(dot, star, any, single, name, class,
			(i == 0 ? -1 : (names[i + 1] ? 0 : 1)));

	i++;
    }

    name_info->next = NULL;
    XtFree((char *)names);		/* Free what you allocate... */
    XtFree((char *)classes);
}

/*	Function Name: SetToggleGroupLeaders
 *	Description: Sets the leaders of each toggle group.
 *                 node - The widget node containing this res box.
 *	Returns: none
 */

static void
SetToggleGroupLeaders(node)
WNode * node;
{
    NameInfo *name;
    ResourceBoxInfo * res_box = node->resources->res_box;
    static Arg args[] = {
	{XtNstate, (XtArgVal) TRUE}
    };

    for (name  = res_box->name_info; name != NULL; name = name->next) {
	XtSetValues(name->sep_leader, args, XtNumber(args));
	if (name->name_leader != NULL)
	    XtSetValues(name->name_leader, args, XtNumber(args));
    }
    SetResourceString(NULL, (XtPointer) node, NULL);
}

/*	Function Name: MakeBoxLookNice
 *	Description: Resizes the box that contains the resource names
 *                   to look a bit nicer.
 *	Arguments: dot, star - the widgets containing the separator types.
 *                 any, single, name, class - the widgets that contain the
 *                                     name and class of this object.
 *	Returns: none.
 */
 
static void
MakeBoxLookNice(dot, star, any, single, name, class, endbox)
Widget dot, star, any, single, name, class;
int endbox;
{

#define MAX_HDIST 3

    Arg args[10];
    Cardinal num_args;
    Dimension any_width, name_class_width, dot_star_width;
    Dimension width_1, width_2;
    int h_dist[MAX_HDIST];
    int i;

    /*
     * Make sure that the dot and star widgets are the same size.
     */

    num_args = 0;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[0])); num_args++;
    XtSetArg(args[num_args], XtNwidth, &width_1); num_args++;
    XtGetValues(dot, args, num_args);

    num_args = 0;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[1])); num_args++;
    XtSetArg(args[num_args], XtNwidth, &width_2); num_args++;
    XtGetValues(star, args, num_args);

    num_args = 0;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[2])); num_args++;
    XtSetArg(args[num_args], XtNwidth, &any_width); num_args++;
    XtGetValues(any, args, num_args);
    
    dot_star_width = (width_1 > width_2) ? width_1 : width_2;
    for (i = 1 ; i < MAX_HDIST; i++) {
	if (h_dist[i] > h_dist[0]) h_dist[0] = h_dist[i];
    }

    num_args = 0;
    XtSetArg(args[num_args], XtNhorizDistance, h_dist[0]); num_args++;
    XtSetValues(any, args, num_args);
    
    /*
     * Add a new arg, and continue...
     */
    XtSetArg(args[num_args], XtNwidth, dot_star_width); num_args++; 
    XtSetValues(star, args, num_args);
    XtSetValues(dot, args, num_args);


    /*
     * Now make sure that the Any Widget is as wide as the longest
     * of the name and class widgets, plus space for the dot and star widgets.
     * Don't forget the Form widget's internal space.
     */

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width_1); num_args++;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[0])); num_args++;
    XtGetValues(name, args, num_args);

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width_2); num_args++;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[1])); num_args++;
    XtGetValues(class, args, num_args);

    if (width_2 > width_1) width_1 = width_2;
    if (h_dist[1] > h_dist[0]) h_dist[0] = h_dist[1];

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width_2); num_args++;
    XtSetArg(args[num_args], XtNhorizDistance, &(h_dist[1])); num_args++;
    XtGetValues(single, args, num_args);

    name_class_width = (width_1 > width_2) ? width_1 : width_2;
    if (h_dist[1] > h_dist[0]) h_dist[0] = h_dist[1];
    if (any_width > name_class_width)
	name_class_width = any_width;
    any_width = dot_star_width + h_dist[0] + name_class_width;
    if (endbox < 0)
	any_width += dot_star_width / 2;
    else if (endbox > 0)
	any_width += (dot_star_width - dot_star_width / 2);

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, any_width); num_args++;
    XtSetValues(any, args, num_args);	

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, name_class_width); num_args++;
    XtSetArg(args[num_args], XtNhorizDistance, h_dist[0]); num_args++;
    XtSetValues(name, args, num_args);	
    XtSetValues(class, args, num_args);	
    XtSetValues(single, args, num_args);	
}

/*	Function Name: CreateLists
 *	Description: Creates the list widgets for the normal and constraint 
 *                   resources
 *	Arguments: parent - parent of the lists.
 *                 node - The widget node containing this res box.
 *                 names, cons_names - lists for norm and cons resource boxes.
 *	Returns: none
 */

static void
CreateLists(parent, node, names, cons_names) 
Widget parent;
WNode * node;
char **names, **cons_names;
{
    Cardinal num_args;
    ResourceBoxInfo * res_box = node->resources->res_box;
    Arg args[1];

    (void) XtCreateManagedWidget("namesLabel", labelWidgetClass, 
				 parent, NULL, ZERO);
    
    num_args = 0;
    XtSetArg(args[num_args], XtNlist, names); num_args++;	
    res_box->norm_list = XtCreateManagedWidget("namesList", listWidgetClass, 
				      parent, args, num_args);
    XtAddCallback(res_box->norm_list, XtNcallback, 
		  ResourceListCallback, (XtPointer) node);
    XtAddCallback(res_box->norm_list, XtNdestroyCallback, 
		  FreeClientData, (XtPointer) names);

    if (cons_names != NULL) {
	(void) XtCreateManagedWidget("constraintLabel", labelWidgetClass, 
				     parent, NULL, ZERO);
	
	num_args = 0;
	XtSetArg(args[num_args], XtNlist, cons_names); num_args++;	
	res_box->cons_list = XtCreateManagedWidget("constraintList", 
						   listWidgetClass, 
						   parent, args, num_args);
	XtAddCallback(res_box->cons_list, XtNcallback, 
		      ResourceListCallback, (XtPointer) node);
	XtAddCallback(res_box->cons_list, XtNdestroyCallback, 
		      FreeClientData, (XtPointer) cons_names);
    }
    else 
	res_box->cons_list = NULL;
}

/*	Function Name: CreateValueWidget
 *	Description: Creates the value widget for entering the resources value.
 *	Arguments: parent - parent of this widget.
 *                 res_box - the resource box info.
 *	Returns: none.
 */

static void
CreateValueWidget(parent, node)
Widget parent;
WNode * node;
{
    Widget form, label;
    Cardinal num_args;
    Arg args[10];
    ResourceBoxInfo * res_box = node->resources->res_box;
    
    form = XtCreateManagedWidget("valueForm", formWidgetClass,
				 parent, NULL, ZERO);

    num_args = 0;
    XtSetArg(args[num_args], XtNleft, XawChainLeft); num_args++;
    XtSetArg(args[num_args], XtNright, XawChainLeft); num_args++;
    XtSetArg(args[num_args], XtNtop, XawChainTop); num_args++;
    XtSetArg(args[num_args], XtNbottom, XawChainBottom); num_args++;
    label = XtCreateManagedWidget("valueLabel", labelWidgetClass, 
				 form, args, num_args);

    num_args = 0;
    XtSetArg(args[num_args], XtNfromHoriz, label); num_args++;
    XtSetArg(args[num_args], XtNleft, XawChainLeft); num_args++;
    XtSetArg(args[num_args], XtNright, XawChainRight); num_args++;
    XtSetArg(args[num_args], XtNtop, XawChainTop); num_args++;
    XtSetArg(args[num_args], XtNbottom, XawChainBottom); num_args++;
    res_box->value_wid = XtCreateManagedWidget("valueText", 
					       asciiTextWidgetClass, 
					       form, args, num_args);
#ifdef notdef
    XtAddCallback(XawTextGetSource(res_box->value_wid), XtNcallback,
		  SetResourceString, (XtPointer) node);
#endif
}

/*	Function Name: PopupOnNode
 *	Description: Pops a shell widget up centered on the node specified.
 *	Arguments: node - the node.
 *                 shell - the shell to popup.
 *	Returns: none.
 */

extern Atom wm_delete_window;

static void
PopupOnNode(node, shell)
WNode * node;
Widget shell;
{
    Arg args[3];
    Cardinal num_args;
    Position x, y;
    Dimension width, height, bw;

    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width); num_args++;
    XtSetArg(args[num_args], XtNheight, &height); num_args++;
    XtSetArg(args[num_args], XtNborderWidth, &bw); num_args++;
    XtGetValues(node->widget, args, num_args);
    XtTranslateCoords(node->widget, 
		      (Position) (width/2 + bw), (Position) (height/2 + bw),
		      &x, &y);
    
    XtOverrideTranslations
      (shell, XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));
    XtRealizeWidget(shell);
    wm_delete_window = XInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW",
				   False);
    (void) XSetWMProtocols (XtDisplay(shell), XtWindow(shell),
                            &wm_delete_window, 1);
    XtGetValues(shell, args, num_args);	/* use same arg_list. */

    x -= (Position) (width/2 + bw);
    y -= (Position) (height/2 + bw);

    if (x < 0)
	x = 0;
    else {
	Position max_loc = WidthOfScreen(XtScreen(shell)) - 
	                     (Position) (width + 2 * bw);
	if (x > max_loc)
	    x = max_loc;
    }

    if (y < 0) 
	y = 0;
    else {
	Position max_loc = HeightOfScreen(XtScreen(shell)) - 
	                     (Position) (height + 2 * bw);
	if (y > max_loc)
	    y = max_loc;
    }

    num_args = 0;
    XtSetArg(args[num_args], XtNx, x); num_args++;
    XtSetArg(args[num_args], XtNy, y); num_args++;
    XtSetValues(shell, args, num_args);

    XtPopup(shell, XtGrabNone);
}

/*	Function Name: FreeClientData
 *	Description: Frees the client data passed to this function.
 *	Arguments: w - UNUSED.
 *                 list_ptr - pointer to the list to check.
 *                 junk - UNUSED.
 *	Returns: none
 */

/* ARGSUSED */
static void
FreeClientData(w, ptr, junk)
Widget w;
XtPointer ptr, junk;
{
    XtFree(ptr);
}

/*	Function Name: FreeResBox.
 *	Description: Frees resource box allocated memory.
 *	Arguments: w - UNUSED.
 *                 ptr - pointer to the node that has this resources box.
 *                 junk - UNUSED.
 *	Returns: none
 */

/* ARGSUSED */
static void
FreeResBox(w, ptr, junk)
Widget w;
XtPointer ptr, junk;
{
    WNode * node = (WNode *) ptr;
    NameInfo *old_name, *name = node->resources->res_box->name_info;
    
    global_resource_box_up = FALSE;

    XtFree((XtPointer) node->resources->res_box);
    node->resources->res_box = NULL;

    while (name != NULL) {
	old_name = name;
	name = name->next;
	XtFree((XtPointer) old_name);
    } 
}


    
