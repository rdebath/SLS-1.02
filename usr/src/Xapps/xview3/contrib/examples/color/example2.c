/*
 * xv_color.c
 *    This program demonstrates the use of color in XView. It allows the
 *    user to choose the foreground and background colors of the various
 *    objects in an interactive manner.
 *
 */
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/icon.h>
#include <xview/cms.h>
#include <xview/svrimage.h>

#define WHITE           0
#define RED             1
#define GREEN           2
#define BLUE            3
#define YELLOW          4
#define PINK            5
#define GRAY            6
#define BLACK           7
#define NUM_COLORS      8

#define HEIGHT          500
#define WIDTH           500

#define CHIP_HEIGHT     16
#define CHIP_WIDTH      16

#define SELECT_TEXTSW           0
#define SELECT_TEXTSW_VIEW      1
#define SELECT_ICON             2

/* Icon data */
static unsigned short icon_bits[]={
#include <images/cardback.icon>
};
mpr_static(icon_image, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT, 1,
icon_bits);

static unsigned short black_data[] = {
#include <images/black.cursor>
};

/* object currently selected for color change */
int             current_selection = SELECT_TEXTSW;

/* flag to indicate if foreground or background is to be changed */
int             fg = TRUE;
Textsw          textsw;
Panel     	panel;
Icon            icon;

/*
 *              main()
 *    Create a panel and panel items. The application uses panel items
 *    to choose a particular object and change its foreground and background
 *    colors in an interactive manner. Create a textsw. Create an icon.
 *    All the objects share the same colormap segment.
 */
main(argc,argv)
    int         argc;
    char        *argv[];
{
	Frame          	frame;
	Cms		control_cms, plain_cms;
	Server_image	choice_image;
	Xv_Singlecolor	cms_colors[NUM_COLORS];
	Panel_item	panel_palette, panel_fg_bg, panel_object;
	int            	textsw_width;
	void            color_notify(), fg_bg_notify(), object_notify();

	xv_init(XV_INIT_ARGS, argc, argv, 0);

	frame = xv_create(XV_NULL, FRAME,
			  FRAME_LABEL,"xv_color",
			  0);

	/* 
	 * Create a colormap segment with the required colors for 
	 * the color palette. Set it to be a control colormap segment
	 * to enable the panel to be painted using the 3D look.
	 */
	initialize_cms_colors(cms_colors);
	control_cms = xv_create(XV_NULL, CMS,
			CMS_SIZE, NUM_COLORS + CMS_CONTROL_COLORS,
			CMS_COLORS, cms_colors,
			CMS_CONTROL_CMS, TRUE,
			0);

	plain_cms = xv_create(XV_NULL, CMS,
			CMS_SIZE, NUM_COLORS,
			CMS_COLORS, cms_colors,
			0);

	/* Create a server image to use for colored panel choice images */
	choice_image = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
                                          XV_WIDTH, CHIP_WIDTH,
                                          XV_HEIGHT, CHIP_HEIGHT,
                                          SERVER_IMAGE_DEPTH, 1,
                                          SERVER_IMAGE_BITS, black_data,
                                          0);

	/* Create panel and set the colormap segment on the panel */
	panel = xv_create(frame, PANEL,
			  PANEL_LAYOUT, PANEL_HORIZONTAL,
			  WIN_CMS, control_cms,	
			  0);

	/* Create panel items */
	panel_object = xv_create(panel, PANEL_CHOICE_STACK,
				PANEL_LABEL_STRING, "Object",
				PANEL_LABEL_BOLD, TRUE,
				PANEL_CHOICE_STRINGS,
				    "Textsw",
				    "Textsw View",
				    "Icon",
				    0,
				PANEL_NOTIFY_PROC, object_notify,
				0);

	panel_fg_bg = xv_create(panel, PANEL_CHOICE,
				PANEL_LABEL_STRING, "Fg/Bg",
				PANEL_LABEL_BOLD, TRUE,
				PANEL_CHOICE_STRINGS,
				    "Foreground",
				    "Background",
				    0,
				PANEL_NOTIFY_PROC, fg_bg_notify,
				0);


	panel_palette = xv_create(panel, PANEL_CHOICE,
				  PANEL_LABEL_STRING, "Colors",
				  PANEL_LABEL_BOLD, TRUE,
				  XV_X, (int)xv_get(panel_fg_bg, XV_X),
				  PANEL_NEXT_ROW, 15,
				  PANEL_CHOICE_IMAGES,
					choice_image,
					choice_image,
					choice_image,
					choice_image,
					choice_image,
					choice_image,
					choice_image,
					choice_image,
				      0,
				PANEL_CHOICE_COLOR, 0, CMS_CONTROL_COLORS + WHITE,
           			PANEL_CHOICE_COLOR, 1, CMS_CONTROL_COLORS + RED,
           			PANEL_CHOICE_COLOR, 2, CMS_CONTROL_COLORS + GREEN,
           			PANEL_CHOICE_COLOR, 3, CMS_CONTROL_COLORS + BLUE,
           			PANEL_CHOICE_COLOR, 4, CMS_CONTROL_COLORS + YELLOW,
           			PANEL_CHOICE_COLOR, 5, CMS_CONTROL_COLORS + PINK,
           			PANEL_CHOICE_COLOR, 6, CMS_CONTROL_COLORS + GRAY,
           			PANEL_CHOICE_COLOR, 7, CMS_CONTROL_COLORS + BLACK,
				PANEL_NOTIFY_PROC, color_notify,
				0);

	(void)window_fit_height(panel);

	/* create textsw and set the colormap segment for it */
	textsw = xv_create(frame, TEXTSW,
			   WIN_CMS, plain_cms,	
			   WIN_BELOW, panel,
			   WIN_ROWS, 45,
			   WIN_COLUMNS, 80,
			   0);

	/* adjust panel dimensions */
	textsw_width = (int)xv_get(textsw, WIN_WIDTH);
	(void)xv_set(panel, WIN_WIDTH, textsw_width, 0);

	/* associate icon with the base frame */
	icon = xv_create(XV_NULL, ICON,
			 ICON_IMAGE, &icon_image,
			 WIN_CMS, plain_cms,
			 0);
	xv_set(frame, FRAME_ICON, icon, 0);

	window_fit(frame);

	/* Start event loop */
	xv_main_loop(frame);
	return(0);
}

/*
 *		initialize_cms_colors()
 *  Initialize the required RGB values.
 */
initialize_cms_colors(colors)
    Xv_Singlecolor	*colors;
{
    colors[WHITE].red = 255;       
    colors[WHITE].green = 255;       
    colors[WHITE].blue = 255;

    colors[RED].red = 255;         
    colors[RED].green = 0;
    colors[RED].blue = 0;

    colors[GREEN].red = 0;
    colors[GREEN].green = 255; 
    colors[GREEN].blue = 0;

    colors[BLUE].red = 0;
    colors[BLUE].green = 0;
    colors[BLUE].blue = 255;

    colors[YELLOW].red = 255;      
    colors[YELLOW].green = 255;
    colors[YELLOW].blue = 0;

    colors[PINK].red = 188; 
    colors[PINK].green = 143;
    colors[PINK].blue = 143;

    colors[GRAY].red = 220;
    colors[GRAY].green = 220;
    colors[GRAY].blue = 220;

    colors[BLACK].red = 0;
    colors[BLACK].green = 0;
    colors[BLACK].blue = 0;
}


/*
 *              color_notify()
 *    This routine gets called when a color selection is made.
 *    Set the foreground or background on the currently selected object.
 *    WIN_FOREGROUND_COLOR & WIN_BACKGROUND_COLOR allow the application
 *    to specify indices into the colormap segment as the foreground and
 *    background values.
 */
void
color_notify(panel_item, event)
    Panel_item      panel_item;
    Event           *event;
{
    int         choice;
    Xv_opaque   current_object, get_current_object();

    current_object = (Xv_opaque)get_current_object();

    choice = (int) xv_get(panel_item, PANEL_VALUE);
    if (fg) {
	xv_set(current_object, WIN_FOREGROUND_COLOR, choice, 0);
    } else {
	xv_set(current_object, WIN_BACKGROUND_COLOR, choice, 0);
    }
}

/*
 *              fg_bg_notify()
 *    This routine gets called when a foreground/background selection
 *    is made.
 */
void
fg_bg_notify(panel_item, event)
    Panel_item      panel_item;
    Event           *event;
{
    int         choice;

    choice = (int) xv_get(panel_item, PANEL_VALUE);
    if (choice == 0) {
	fg = TRUE;
    } else {
	fg = FALSE;
    }
}

/*
 *              object_notify()
 *    This routine gets called when an object selction is made.
 *    Store this selection as the current object.
 */


void
object_notify(panel_item, event)
    Panel_item      panel_item;
    Event           *event;
{
    current_selection = (int) xv_get(panel_item, PANEL_VALUE);
}

/*
 *              get_current_object()
 *    This routine returns the XView handle to the currently selected
 *    object.
 */
Xv_opaque
get_current_object()
{
    Xv_opaque   current_object;

    switch(current_selection) {
	case SELECT_TEXTSW:
	    current_object = (Xv_opaque)textsw;
	    break;

	case SELECT_TEXTSW_VIEW:
	    current_object =
		(Xv_opaque)xv_get(textsw, OPENWIN_NTH_VIEW, 0);
	    break;

	case SELECT_ICON:
	    current_object = (Xv_opaque)icon;
	    break;

	default:
	    current_object = (Xv_opaque)textsw;
	    break;
    }
    return(current_object);
}
