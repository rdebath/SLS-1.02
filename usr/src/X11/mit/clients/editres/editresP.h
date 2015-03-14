/*
 * $XConsortium: editresP.h,v 1.12 91/07/30 15:30:47 rws Exp $
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
 *
 * Author:  Chris D. Peterson, MIT X Consortium
 */

#include <X11/Xmu/EditresP.h>
#include <X11/Xresource.h>

#define DEBUG

#ifdef DEBUG
#  define CLIENT_TIME_OUT 60000	/* wait sixty seconds for the client. */
#else
#  define CLIENT_TIME_OUT 5000	/* wait five seconds for the client. */
#endif /* DEBUG */

#define CURRENT_PROTOCOL_VERSION 4

#define FLASH_TIME  1000	/* Default flash time in microseconds */
#define NUM_FLASHES 3		/* Default number of flashes. */

#define NO_IDENT 0		/* an ident that will match nothing. */

#define NUM_INC 10		/* amount to increment allocators. */

#define ANY_RADIO_DATA ("the any widget")
#define RESOURCE_BOX ("resourceBox")

extern void exit();

/*
 * Retrieving ResType and Boolean is the same as retrieving a Card8.
 */

#define _XEditResGetBoolean _XEditResGet8
#define _XEditResGetResType _XEditResGet8

/*
 * Contexts to use with the X Context Manager.
 */

#define NODE_INFO ((XContext) 42)

/*
 * Error codes for X Server errors. 
 */

#define NO_ERROR 0
#define NO_WINDOW 1

typedef enum {LocalSendWidgetTree, LocalSetValues, LocalFindChild, 
	      LocalFlashWidget, LocalGetGeometry, LocalGetResources}ResCommand;

typedef enum {ClassLabel, NameLabel, IDLabel, WindowLabel,
	      ToggleLabel} LabelTypes;
typedef enum {SelectWidget, SelectAll, SelectNone, SelectInvert, SelectParent, 
	      SelectChildren,  SelectDescendants, SelectAncestors} SelectTypes;

typedef struct _NameInfo {
    struct _NameInfo * next;	/* Next element in the linked list. */
    Widget sep_leader;		/* The separator toggle group leader. */
    Widget name_leader;		/* The name toggle group leader. */
} NameInfo;

typedef struct _ResourceBoxInfo {
    Widget value_wid;		/* The string containing the value. */
    Widget res_label;		/* The label containing current resoruce. */
    Widget shell;		/* Shell widget containing resource box. */
    Widget norm_list;		/* The List widget for the normal list. */
    Widget cons_list;		/* The List widget for the 
				   Constriaint Resources */
    NameInfo * name_info;	/* The info about the widgets for each
				   name and class in the instance heirarchy. */
} ResourceBoxInfo;
    
typedef struct _WidgetResourceInfo {
    char * name, * class, *type; /* Name, Class and Type of each resource. */
} WidgetResourceInfo;
    
typedef struct _WidgetResources {
    int num_normal, num_constraint;
    WidgetResourceInfo *normal, *constraint;
    ResourceBoxInfo * res_box;
} WidgetResources;

typedef struct _WNode {
    char * name;
    char * class;
    unsigned long id, window;
    struct _WNode * parent;
    struct _WNode ** children;
    struct _TreeInfo * tree_info;
    Cardinal num_children, alloc_children;
    Widget widget;
    WidgetResources * resources;
} WNode;

/*
 * Information for the Select any widget, toggle buttons in the resource
 * boxes.
 */

typedef struct _AnyInfo {
    WNode * node;		/* A Pointer off to the node corrsponding to
				   this resource box. */
    Widget left_dot, left_star;	/* The dot and star widgets to our left. */
    Widget right_dot, right_star; /* The dot and star widgets to our right. */
    int left_count, *right_count; /* If count > 0 then desensitize the left or
				    right dot and star widgets. */
} AnyInfo;

/*
 * Information about the client we are currently working with.
 */

typedef struct _CurrentClient {
    ResCommand command;		/* the command sent. */
    ResIdent ident;
    ProtocolStream stream;	/* protocol stream for this client. */
    XtIntervalId timeout;	/* timeout set in case he doesn't answer. */
    Window window;		/* window to communicate with. */
    Atom atom;			/* Atom used to communicate with this client.*/
} CurrentClient;

/*
 * Information about a tree we can display.
 */

typedef struct _TreeInfo {
    Widget tree_widget;		/* The Tree widget that contains all nodes */
    WNode * top_node;		/* The top node in the tree. */
    WNode ** active_nodes;	/* The currently active nodes. */
    Cardinal num_nodes, alloc_nodes; /* number of active nodes, and space */
    Widget * flash_widgets;	/* list of widgets to flash on and off. */
    Cardinal num_flash_widgets, alloc_flash_widgets; /* number of flash wids.*/
} TreeInfo;

/*
 * Information specific to a give APPLICATION screen.
 */

typedef struct _ScreenData {
    Widget set_values_popup;	/* The SetValues popup. */
    Widget res_text;		/* SetValues resource text widget. */
    Widget val_text;		/* SetValues value text widget. */
    Widget info_label;	        /* The information label. */
} ScreenData;

typedef struct _AppResources {
    Boolean debug;		/* Is debugging on? */
    int num_flashes, flash_time; /* Number and duration of flashes. */
    Pixel flash_color;		/* Color of flash window. */
    char * save_resources_file;	/* File to save the resources into. */

    /* Private state */
    Boolean allocated_save_resources_file;
} AppResources;

/*
 * Information needed to apply the resource string to all widgets.
 */

typedef struct _ApplyResourcesInfo {
    char * name, *class;	/* name and class  of this resource. */
    unsigned short count;
    ProtocolStream * stream;
    XrmDatabase database;
} ApplyResourcesInfo;
    

/************************************************************
 *
 * The Event Structures.
 *
 ************************************************************/

typedef struct _AnyEvent {
    EditresCommand type;
} AnyEvent;

typedef struct _WidgetTreeInfo {
    WidgetInfo widgets;
    char * name;
    char * class;
    unsigned long window;
} WidgetTreeInfo;

typedef struct _SendWidgetTreeEvent {
    EditresCommand type;
    unsigned short num_entries;
    WidgetTreeInfo * info;
} SendWidgetTreeEvent;

typedef struct _SetValuesInfo {
    WidgetInfo widgets;
    char * message;
} SetValuesInfo;
    
typedef struct _SetValuesEvent {
    EditresCommand type;
    unsigned short num_entries;
    SetValuesInfo * info;
} SetValuesEvent;

typedef struct _ResourceInfo {
    ResourceType res_type;
    char * name, *class, *type;
} ResourceInfo;

typedef struct _GetResourcesInfo {
    WidgetInfo widgets;
    Boolean error;
    char * message;
    unsigned short num_resources;
    ResourceInfo * res_info;
} GetResourcesInfo;

typedef struct _GetResourcesEvent {
    EditresCommand type;
    unsigned short num_entries;
    GetResourcesInfo * info;
} GetResourcesEvent;

typedef struct _GetGeomInfo {
    EditresCommand type;
    WidgetInfo widgets;
    Boolean error;
    char * message;
    Boolean visable;
    short x, y;
    unsigned short width, height, border_width;
} GetGeomInfo;

typedef struct _GetGeomEvent {
    EditresCommand type;
    unsigned short num_entries;
    GetGeomInfo * info;
} GetGeomEvent;

typedef struct _FindChildEvent {
    EditresCommand type;
    WidgetInfo widgets;
} FindChildEvent;

typedef union _Event {
    AnyEvent any_event;
    SendWidgetTreeEvent send_widget_tree_event;
    SetValuesEvent set_values_event;
    GetResourcesEvent get_resources_event;
    GetGeomEvent get_geom_event;
    FindChildEvent find_child_event;
} Event;
    
/*
 * Global variables. 
 */

#ifndef THIS_IS_MAIN
    extern int global_error_code;
    extern unsigned long global_serial_num;
    extern int (*global_old_error_handler)();
    extern Boolean global_resource_box_up;

    extern TreeInfo *global_tree_info;
    extern CurrentClient global_client;
    extern ScreenData global_screen_data;
    extern Widget global_tree_parent;
    extern AppResources global_resources;
#endif

/*
 * Macros.
 */

#define streq(a, b)        ( strcmp((a), (b)) == 0 )
