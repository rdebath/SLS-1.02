/*=================================================================
 * visual.c - 
 *    Test XView's ability to handle different visuals and displays
 *
 * dmaustin Sat Oct 20 16:07:48 1990
 *=================================================================
 */

#include <xview/xview.h>
#include <xview/server.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/canvas.h>
#include <xview/scrollbar.h>
#include <xview/notice.h>
#include <X11/Xutil.h>
#include "visual.h"

static char *visual_names[] = 
{
    "StaticGray",
    "GrayScale",
    "StaticColor",
    "PseudoColor",
    "TrueColor",
    "DirectColor"
};

static void           parse_command_line();
static Server_Info   *query_server_info();
static void	      init_panel();
static Panel_setting  display_name_change();
static void           screen_choice_change();
static void           adjust_screen_choice();
static void	      adjust_visual_choices();
static Panel_item     adjust_visual_choice();
static Panel_item     recreate_choice_stack();
static void	      check_box_notify();
static void	      create_test_window();
static Frame          create_test_frame();
static Panel	      create_test_panel();
static Textsw	      create_test_textsw();
static Canvas	      create_test_canvas();
static Menu	      create_test_menu();
static void	      quit_test_frame();
static void	      event_proc();
static void	      repaint_proc();

Attr_attribute	      canvas_menu_key;

Frame 		frame;
Server_Info    *servers = NULL;
Server_Info    *current_server = NULL;
Panel_item	display_name;
Panel_item	screen_choice;
Panel_item	api_choice;
Panel_item	test_frame_visual;
Panel_item	create_panel_box;
Panel_item	test_panel_visual;
Panel_item	create_textsw_box;
Panel_item	test_textsw_visual;
Panel_item	create_canvas_box;
Panel_item	test_canvas_visual;
Panel_item	test_menu_visual;

main(argc, argv)
int argc;
char *argv[];
{
    Xv_Server server;

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
    parse_command_line(argc, argv);

    current_server = query_server_info(server);

    frame = (Frame)xv_create(NULL, FRAME,
        XV_LABEL, "Mr. Visual Head",
        FRAME_SHOW_FOOTER, TRUE,
        NULL);

    init_panel(frame, current_server);

    canvas_menu_key = xv_unique_key();
    window_fit(frame);
    xv_main_loop(frame);
}

static void 
parse_command_line(argc, argv)
    int argc;
    char *argv[];
{
    if (argc != 1) {
	fprintf(stderr, "%s: unknown option %s\n", argv[0], argv[1]);
	exit(-1);
    }
}

static Server_Info *
query_server_info(server)
    Xv_Server server;
{
    Server_Info *server_list = servers;
    Server_Info *server_info;
    char *name = (char *)xv_get(server, XV_NAME);
    int  screen;
    XVisualInfo vinfo_template;

    /* Check to see if we have already queried this server */
    while (server_list && strcmp(name, server_list->name))
      server_list = server_list->next;

    if (server_list)
      /* We have already interrogated this server, so return it */
      server_info = server_list;
    else {
	/* Need to create a new Server_Info, and fill in the information */
	server_info = (Server_Info *)malloc(sizeof(Server_Info));
	server_info->server = server;
	server_info->name = name;
	server_info->display = (Display *)xv_get(server, XV_DISPLAY);
	server_info->num_screens = ScreenCount(server_info->display);
	server_info->screens = 
	  (Screen_Info *)malloc(server_info->num_screens * sizeof(Screen_Info));

	/* Fill in all of the screen information */
	for (screen = 0; screen < server_info->num_screens; screen++) {
	    server_info->screens[screen].screen = 
	      (Xv_Screen)xv_get(server, SERVER_NTH_SCREEN, screen);
	    vinfo_template.screen = screen;
	    server_info->screens[screen].visuals = 
	      XGetVisualInfo(server_info->display, VisualScreenMask, &vinfo_template,
			     &(server_info->screens[screen].num_visuals));
	}
	
	/* Add the new server to the global list of servers */
	server_info->next = servers;
	servers = server_info;
    }
    return(server_info);
}

static void
init_panel(frame, server_info)
    Frame        frame;
    Server_Info *server_info;
{
    Panel panel;
    
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT, PANEL_VERTICAL,
        NULL);

    display_name = (Panel_item)xv_create(panel, PANEL_TEXT,
        PANEL_LABEL_STRING, "Display name:",
        PANEL_VALUE, server_info->name,
        PANEL_VALUE_DISPLAY_LENGTH, 20,					 
        PANEL_NOTIFY_PROC, display_name_change,
        NULL);

    screen_choice = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Screen Number:",					  
        PANEL_CHOICE_STRINGS, "0", NULL,					  
        PANEL_NOTIFY_PROC, screen_choice_change,
        NULL);

    api_choice = (Panel_item)xv_create(panel, PANEL_CHOICE,
        PANEL_LABEL_STRING, "Visual specification:",
        PANEL_CHOICE_STRINGS, "Visual", "Class / Depth", NULL,
        NULL);

    test_frame_visual = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Test Frame Visual:",
        NULL);
					      
    create_panel_box = (Panel_item)xv_create(panel, PANEL_CHECK_BOX,
        PANEL_LABEL_STRING, "Test Panel:",					     
        PANEL_VALUE, 1,
        PANEL_NOTIFY_PROC, check_box_notify,
        NULL);
    xv_set(panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
    test_panel_visual = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Visual:",
        NULL);

    xv_set(panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
    create_textsw_box = (Panel_item)xv_create(panel, PANEL_CHECK_BOX,
        PANEL_LABEL_STRING, "Test Textsw:",
        PANEL_VALUE, 0,
        PANEL_NOTIFY_PROC, check_box_notify,
        NULL);
    xv_set(panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
    test_textsw_visual = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Visual:",
        PANEL_INACTIVE, TRUE,					       
        NULL);

    xv_set(panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
    create_canvas_box = (Panel_item)xv_create(panel, PANEL_CHECK_BOX,
        PANEL_LABEL_STRING, "Test Canvas:",
        PANEL_VALUE, 1,
        PANEL_NOTIFY_PROC, check_box_notify,
        NULL);
    xv_set(panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
    test_canvas_visual = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Visual:",
        NULL);

    xv_set(panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

    test_menu_visual = (Panel_item)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Canvas Menu Visual:",
	NULL);
					      
    adjust_screen_choice(server_info);

    (void)xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING, "Test Configuration...",
        PANEL_NOTIFY_PROC, create_test_window,
        NULL);
		    
    window_fit(panel);
}    


/* ARGSUSED */
static Panel_setting
display_name_change(item, event)
    Panel_item item;
    Event     *event;
{
    Xv_Server server;
    char *name = (char *)xv_get(item, PANEL_VALUE);
    char error_message[1000];

    xv_set(frame, FRAME_BUSY, TRUE, NULL);
    server = (Xv_Server)xv_create(NULL, SERVER,
        XV_NAME, name,
	NULL);
    if (!server || server == XV_ERROR) {
	sprintf(error_message, "Unable to connect to display %s\n", name);
	xv_set(frame, FRAME_LEFT_FOOTER, error_message, NULL);
    } else {
	current_server = query_server_info(server);
	adjust_screen_choice(current_server);
	xv_set(frame, FRAME_LEFT_FOOTER, "", NULL);
    }	
    xv_set(frame, FRAME_BUSY, FALSE, NULL);
    return(PANEL_NONE);
}

/* ARGSUSED */
static void
screen_choice_change(item, value, event)
    Panel_item item;
    int        value;
    Event     *event;
{
    adjust_visual_choices(current_server, value);
}

static void
adjust_screen_choice(server_info)
    Server_Info *server_info;
{
    char label[10];
    int  num_choices = (int)xv_get(screen_choice, PANEL_NCHOICES);
    int  screen;
    
    if (num_choices > server_info->num_screens) {
      screen_choice = recreate_choice_stack(screen_choice);
      num_choices = 0;
    }
    
    if (num_choices < server_info->num_screens)
    for (screen = num_choices; screen < server_info->num_screens; screen++) {
	sprintf(label, "%d", screen);
	xv_set(screen_choice, PANEL_CHOICE_STRING, screen, label, NULL);
    }
    
    screen = DefaultScreen(server_info->display);
    xv_set(screen_choice, 
	   PANEL_DEFAULT_VALUE, screen,
	   PANEL_VALUE, screen,
	   PANEL_INACTIVE, (server_info->num_screens <= 1) ? TRUE : FALSE,
	   NULL);

    adjust_visual_choices(server_info, screen);
}

static void
adjust_visual_choices(server_info, screen_num)
    Server_Info *server_info;
    int screen_num;
{
    test_frame_visual = adjust_visual_choice(test_frame_visual, server_info, screen_num);
    test_panel_visual = adjust_visual_choice(test_panel_visual, server_info, screen_num);
    test_textsw_visual = adjust_visual_choice(test_textsw_visual, server_info, screen_num);
    test_canvas_visual = adjust_visual_choice(test_canvas_visual, server_info, screen_num);
    test_menu_visual = adjust_visual_choice(test_menu_visual, server_info, screen_num);
}

static Panel_item
adjust_visual_choice(choice_stack, server_info, screen_num)
    Panel_item   choice_stack;
    Server_Info *server_info;
    int          screen_num;
{
    Screen_Info *screen_info = &(server_info->screens[screen_num]);
    XVisualInfo *vinfo;
    VisualID default_visual_id = 
      XVisualIDFromVisual(DefaultVisual(server_info->display, screen_num));
    int visual;
    char label[100];
    
    if ((int)xv_get(choice_stack, PANEL_NCHOICES) > screen_info->num_visuals + 1) {
	choice_stack = recreate_choice_stack(choice_stack);
    }

    for (visual = 0; visual < screen_info->num_visuals; visual++) {
	vinfo = &(screen_info->visuals[visual]);
	if (vinfo->class < StaticGray || vinfo->class > DirectColor)
	  sprintf(label, "Unknown (%d bit)", vinfo->depth);
	else
	  sprintf(label, "%s (%d bit)", visual_names[vinfo->class], vinfo->depth);
	xv_set(choice_stack, 
	    PANEL_CHOICE_STRING, visual, label,
	    NULL);
	if (default_visual_id == vinfo->visualid)
	  xv_set(choice_stack, PANEL_DEFAULT_VALUE, visual, NULL);
    }

    xv_set(choice_stack, 
        PANEL_CHOICE_STRING, visual, "Default",
	PANEL_VALUE, visual,
        NULL);

    return(choice_stack);
}

/* 
 * This brain death is due to the lack of the ability to change the number
 * of choices in a choice item
 */
static Panel_item
recreate_choice_stack(old_choice)
    Panel_item old_choice;
{
    Panel_item new_choice;

    new_choice = (Panel_item)xv_create(xv_get(old_choice, XV_OWNER), PANEL_CHOICE_STACK,
        XV_X, (int)xv_get(old_choice, XV_X),
        XV_Y, (int)xv_get(old_choice, XV_Y),
        PANEL_LABEL_STRING, (char *)xv_get(old_choice, PANEL_LABEL_STRING),
        PANEL_NOTIFY_PROC, xv_get(old_choice, PANEL_NOTIFY_PROC),
	NULL);
    xv_destroy(old_choice);
    return(new_choice);
}


/* ARGSUSED */
static void
check_box_notify(item, value, event)
Panel_item item;
int value;
Event *event;
{
    Panel_item choice;
    
    if (item == create_panel_box)
      choice = test_panel_visual;
    else if (item == create_textsw_box)
      choice = test_textsw_visual;
    else 
      choice = test_canvas_visual;
    
    xv_set(choice, PANEL_INACTIVE, !value, NULL);
}    


/* ARGSUSED */
static void
create_test_window(item, event)
   Panel_item item;
   Event *event;
{
    Screen_Info *screen_info;
    int screen_num;
    Xv_Window root;
    Frame test_frame;
    int use_depth = !(xv_get(api_choice, PANEL_VALUE));
    int visual;
    XVisualInfo *vinfo;

    /* Get root window of the screen */
    screen_num = (int)xv_get(screen_choice, PANEL_VALUE);
    screen_info = &(current_server->screens[screen_num]);
    root = (Xv_Window)xv_get(screen_info->screen, XV_ROOT);
    
    /* Create the frame */
    visual = (int)xv_get(test_frame_visual, PANEL_VALUE);
    if (visual >= screen_info->num_visuals)
      vinfo = NULL;
    else
      vinfo = &screen_info->visuals[visual];
    test_frame = create_test_frame(root, frame, vinfo, use_depth);

    /* Create the panel */
    if (xv_get(create_panel_box, PANEL_VALUE)) {
	visual = (int)xv_get(test_panel_visual, PANEL_VALUE);
	if (visual >= screen_info->num_visuals)
	  vinfo = NULL;
	else
	  vinfo = &screen_info->visuals[visual];
	(void)create_test_panel(test_frame, vinfo, use_depth);
    }
    
    /* Create the textsw */
    if (xv_get(create_textsw_box, PANEL_VALUE)) {
	visual = (int)xv_get(test_textsw_visual, PANEL_VALUE);
	if (visual >= screen_info->num_visuals)
	  vinfo = NULL;
	else
	  vinfo = &screen_info->visuals[visual];
	(void)create_test_textsw(test_frame, vinfo, use_depth);
    }
    
    /* Create test canvas */
    if (xv_get(create_canvas_box, PANEL_VALUE)) {
	Canvas canvas;

	visual = (int)xv_get(test_canvas_visual, PANEL_VALUE);
	if (visual >= screen_info->num_visuals)
	  vinfo = NULL;
	else
	  vinfo = &screen_info->visuals[visual];
	canvas = create_test_canvas(test_frame, vinfo, use_depth);

	visual = (int)xv_get(test_menu_visual, PANEL_VALUE);
	if (visual >= screen_info->num_visuals)
	  vinfo = NULL;
	else
	  vinfo = &screen_info->visuals[visual];
	xv_set(canvas, XV_KEY_DATA, canvas_menu_key, 
	       create_test_menu(test_frame, vinfo, use_depth),
	       NULL);
    }	

    window_fit(test_frame);

    /* Map the frame */
    xv_set(test_frame, XV_SHOW, TRUE, NULL);
}


static Frame
create_test_frame(parent, owner, vinfo, use_depth)
Xv_Window parent;
Xv_opaque owner;
XVisualInfo *vinfo;
int use_depth;
{
    Frame frame;

    if (!vinfo)
      frame = (Frame)xv_create(parent, FRAME,
          XV_LABEL, "Most Excellent Test Window",
          XV_OWNER, owner,
	  NULL);
    else if (use_depth)
      frame = (Frame)xv_create(parent, FRAME,
          XV_LABEL, "Most Excellent Test Window",
          XV_OWNER, owner,
          XV_VISUAL_CLASS, vinfo->class,
          WIN_DEPTH, vinfo->depth,
          NULL);
    else
      frame = (Frame)xv_create(parent, FRAME,
          XV_LABEL, "Most Excellent Test Window",
          XV_OWNER, owner,
	  XV_VISUAL, vinfo->visual,
          NULL);

    return frame;
}

static Panel
create_test_panel(owner, vinfo, use_depth)
Xv_opaque owner;
XVisualInfo *vinfo;
int use_depth;
{
    Panel panel;
    
    if (!vinfo) 
      panel = (Panel)xv_create(owner, PANEL,
          PANEL_LAYOUT, PANEL_VERTICAL,
	  NULL);
    else if (use_depth)
      panel = (Panel)xv_create(owner, PANEL,
	  PANEL_LAYOUT, PANEL_VERTICAL,
          XV_VISUAL_CLASS, vinfo->class,
          WIN_DEPTH, vinfo->depth,
          NULL);				    
    else
      panel = (Panel)xv_create(owner, PANEL,
	  PANEL_LAYOUT, PANEL_VERTICAL,
          XV_VISUAL, vinfo->visual,			       
          NULL);				    
    
    (void)xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING, "Quit",
        PANEL_NOTIFY_PROC, quit_test_frame,		    
	NULL);
    
    (void)xv_create(panel, PANEL_TOGGLE,
        PANEL_LABEL_STRING, "Pizza Ingredients:",

        PANEL_CHOICE_STRINGS, "Cheese", "Pepperoni", "Pez", 
		              "Sausage", "Peppers", "Peanut Butter",
		              "Olives", "Beer", "Ice Cream", NULL,
        PANEL_CHOICE_NCOLS, 3,		    
        PANEL_VALUE, 4 | 8 | 32 | 128,		    
        NULL);

    (void)xv_create(panel, PANEL_CHOICE_STACK,
        PANEL_LABEL_STRING, "Choice stack:",
        PANEL_CHOICE_STRINGS, "Totally", "Not Even", NULL,
	NULL);
    
    (void)xv_create(panel, PANEL_LIST,
        PANEL_LABEL_STRING, "Happening:",
        PANEL_LIST_STRINGS, "Malibu", "La Jolla", "Redondo", "Zuma", NULL,
	NULL);
    
    (void)xv_create(panel, PANEL_SLIDER,
		    PANEL_LABEL_STRING, "Rudeness Factor:",
		    PANEL_SHOW_VALUE, FALSE,
		    PANEL_MIN_VALUE, 0,
		    PANEL_MAX_VALUE, 100,
		    PANEL_SLIDER_END_BOXES, TRUE,
		    NULL);
    
    (void)xv_create(panel, PANEL_GAUGE,
		    PANEL_LABEL_STRING, "Ocean Temp:",
		    PANEL_MIN_VALUE, 30,
		    PANEL_MAX_VALUE, 200,
		    PANEL_VALUE, 70,
		    PANEL_TICKS, 17,
		    NULL);

    (void)xv_create(panel, PANEL_TEXT,
		    PANEL_LABEL_STRING, "Philosophy:",
		    PANEL_VALUE, "Live like James Bond, Die like James Dean!",
		    PANEL_VALUE_DISPLAY_LENGTH, 50,
		    NULL);
    
    window_fit_height(panel);
    
    return panel;
}

static Textsw
create_test_textsw(owner, vinfo, use_depth)
Xv_opaque owner;
XVisualInfo *vinfo;
int use_depth;
{
    Textsw textsw;
    
    if (!vinfo) 
      textsw = (Textsw)xv_create(owner, TEXTSW,
          TEXTSW_FILE, "/etc/motd",
          WIN_ROWS, 10,
          NULL);
    else if (use_depth)
      textsw = (Textsw)xv_create(owner, TEXTSW,
          TEXTSW_FILE, "/etc/motd",
          WIN_ROWS, 10,
          XV_VISUAL_CLASS, vinfo->class,
          WIN_DEPTH, vinfo->depth,				 
          NULL);
    else 
      textsw = (Textsw)xv_create(owner, TEXTSW,
          TEXTSW_FILE, "/etc/motd",
          WIN_ROWS, 10,
          XV_VISUAL, vinfo->visual,				 
	  NULL);
    
    return textsw;
}

static Canvas
create_test_canvas(owner, vinfo, use_depth)
Xv_opaque owner;
XVisualInfo *vinfo;
int use_depth;
{
    Canvas canvas;
    
    if (!vinfo) 
      canvas = (Canvas)xv_create(owner, CANVAS, NULL);
    else if (use_depth)
      canvas = (Canvas)xv_create(owner, CANVAS,
         XV_VISUAL_CLASS, vinfo->class,
         WIN_DEPTH, vinfo->depth,				 
	 NULL);
    else
      canvas = (Canvas)xv_create(owner, CANVAS,
         XV_VISUAL, vinfo->visual,
	 NULL);

    xv_set(canvas,
	   XV_HEIGHT, 200,
	   CANVAS_HEIGHT, 600,
	   CANVAS_WIDTH, 600,
	   CANVAS_AUTO_EXPAND, FALSE,
	   CANVAS_AUTO_SHRINK, FALSE,
	   CANVAS_REPAINT_PROC, repaint_proc,
	   CANVAS_RETAINED, TRUE,
	   CANVAS_PAINTWINDOW_ATTRS, 
               WIN_EVENT_PROC, event_proc,
               NULL,
	   NULL);
    
    (void)xv_create(canvas, SCROLLBAR,
	SCROLLBAR_SPLITTABLE, TRUE,
	NULL);
    (void)xv_create(canvas, SCROLLBAR,
	SCROLLBAR_SPLITTABLE, TRUE,
	SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
	NULL);

    xv_set(canvas, CANVAS_RETAINED, FALSE, NULL );

    return canvas;
}


static Menu
create_test_menu(owner, vinfo, use_depth)
Xv_opaque owner;
XVisualInfo *vinfo;
int use_depth;
{
    Menu menu, sub_menu;
    Xv_Server server;
    
    server = xv_get(xv_get(owner, XV_SCREEN), SCREEN_SERVER);
    
    sub_menu = (Menu)xv_create(server, MENU,
          MENU_GEN_PIN_WINDOW, owner, "Winter Activities",
          MENU_STRINGS, "Ski", "Snowboard", "Sleep", "Eat", NULL,
          NULL);
			       
    if (!vinfo) 
      menu = (Menu)xv_create(server, MENU,
          MENU_GEN_PIN_WINDOW, owner, "Activities",
          MENU_STRINGS, "Surf", "Skate", "Sleep", "Eat", "Surf", NULL,
          MENU_ITEM,
              MENU_STRING, "Winter",
              MENU_PULLRIGHT, sub_menu,
              NULL,			     
	  NULL);
    else if (use_depth)
      menu = xv_create(server, MENU,
          XV_VISUAL_CLASS, vinfo->class,
          XV_DEPTH, vinfo->depth,		       
          MENU_GEN_PIN_WINDOW, owner, "Menu",
          MENU_STRINGS, "Surf", "Skate", "Sleep", "Eat", "Surf", NULL,
          MENU_ITEM,
              MENU_STRING, "Winter",
              MENU_PULLRIGHT, sub_menu,
              NULL,			     
	  NULL);
    else
      menu = xv_create(server, MENU,
          XV_VISUAL, vinfo->visual,
          MENU_GEN_PIN_WINDOW, owner, "Menu",
          MENU_STRINGS, "Surf", "Skate", "Sleep", "Eat", "Surf", NULL,
          MENU_ITEM,
              MENU_STRING, "Winter",
              MENU_PULLRIGHT, sub_menu,
              NULL,			     
	  NULL);
    
    return menu;
}


static void
quit_test_frame(item, event)
Panel_item item;
Event *event;
{
    Panel panel = (Panel)xv_get(item, XV_OWNER);
    int result;

    result = notice_prompt(panel, NULL,
        NOTICE_FOCUS_XY, event_x(event), event_y(event),
        NOTICE_MESSAGE_STRINGS,
		  "Do you really want to quit this",
		  "most triumphant test window?", NULL,
        NOTICE_BUTTON_YES, "Yes",
        NOTICE_BUTTON_NO,  "No",
	NULL);
    
    if (result == NOTICE_YES) {
	xv_destroy_safe(xv_get(panel, XV_OWNER));
    }
}


static void
event_proc(window, event, arg)
Xv_Window window;
Event *event;
Notify_arg arg;
{

    if ((event_action(event) == ACTION_MENU) && event_is_down(event)) {
	Canvas canvas = xv_get(window, CANVAS_PAINT_CANVAS_WINDOW);

	menu_show(xv_get(canvas, XV_KEY_DATA, canvas_menu_key), 
		  window, event, NULL);
    }
}


static void
repaint_proc()
{
    printf("Now repainting canvas\n");
}


