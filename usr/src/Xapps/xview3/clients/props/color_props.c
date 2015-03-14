#ifndef lint
static char sccsid[] = "@(#)color_props.c 1.11 91/09/14";
#endif

/*
 * color_props.c - Color property sheet for the OpenWindows Properties program.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "props.h"
#include "color.h"
#include "image.h"

#include <xview/cms.h>
#include <xview/svrimage.h>

#define	WORKSPACE_STRING	"Workspace"
#define	WINDOW_STRING		"Windows"
#define	PALETTE_STRING		"Palette"
#define	CUSTOM_STRING		"Custom"
#define WORKSPACE_CLASS_RES	"OpenWindows.WorkspaceColor"
#define WORKSPACE_RES		"openwindows.workspacecolor"
#define WORKSPACE_DEFAULT	"#40a0c0"
#define WINDOW_CLASS_RES	"OpenWindows.WindowColor"
#define WINDOW_RES		"openwindows.windowcolor"
#define WINDOW_DEFAULT		"#cccccc"
#define BACK_CLASS_RES		"Window.Color.Background"
#define BACK_RES		"window.color.background"
#define BACK_DEFAULT		"#ffffff"
#define FRAME_STYLE_CLASS_RES	"OpenWindows.Use3DFrames"
#define FRAME_STYLE_RES		"OpenWindows.use3dframes"

static int  hues[] = {0, 43, 63, 135, 180, 225, 280, 315};	/* 0 - 360 */
static int  saturations[] = {200, 400, 600, 800};		/* 0 - 1000 */
static int  brightnesses[] = {750, 900};			/* 0 - 1000 */
static int  grays[] = {300, 400, 500, 600, 700, 800};		/* 0 - 1000 */

#define NUM_BRIGHTNESSES	(sizeof(brightnesses)/sizeof(int))
#define NUM_HUES		(sizeof(hues)/sizeof(int))
#define NUM_SATURATIONS		(sizeof(saturations)/sizeof(int))
#define NUM_GRAYS		(sizeof(grays)/sizeof(int))
#define NUM_COLUMNS		(NUM_HUES+1)
#define NUM_CUSTOM		2

#define COLOR_CHOICES (NUM_HUES*NUM_SATURATIONS*NUM_BRIGHTNESSES+NUM_GRAYS+NUM_CUSTOM)
#define NUM_RWCOLORS (BlackIndex + 1)

#define COLORMAP_SIZE (COLOR_CHOICES + NUM_RWCOLORS)

#define CUSTOM_INDEX		0
#define WORK_INDEX		0
#define WIN_INDEX		1


static XColor xcolors[COLORMAP_SIZE];
static HSV  colors[COLOR_CHOICES];
static Cms  cms = XV_NULL;
static char *image_string;

static Panel_item h_slider;
static Panel_item s_slider;
static Panel_item v_slider;
static Panel_item h_ticks;
static Panel_item s_ticks;
static Panel_item v_ticks;
static Panel_item palette;
static Panel_item which;
static Panel_item custom;
static int  palette_x;

static int  backup_work_index;
static int  backup_win_index;
static HSV  backup_xact_win;
static HSV  backup_xact_work;

static int  slidermode;		/* palette == 0, sliders == 1 */
static int  windowmode;		/* workspace == 0, window == 1 */
static int  work_index;
static int  win_index;
static HSV  xact_win;
static HSV  xact_work;
static HSV  xact_slider;

#define CHIP_HEIGHT	16
#define CHIP_WIDTH	16
static unsigned short chip_data[] = {
    0x7FFE, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
    0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0x7FFE
};
static Server_image color_chip;

#define IMAGE_WIDTH 64
#define IMAGE_HEIGHT 64
static char image_data[IMAGE_WIDTH * IMAGE_HEIGHT];

#define FIRST_SLIDER_Y		40
#define SLIDER_GAP		40
#define SLIDER_TICK_GAP		4
#define SLIDER_WIDTH		(NUM_COLUMNS*(CHIP_WIDTH+8))
#define SLIDER_MAX		MAXSV
#define SLIDER_MAX_H		MAXH

#define SIMAGE_WIDTH (SLIDER_WIDTH - 44)
#define SIMAGE_HEIGHT 16
static char hue_data[SIMAGE_WIDTH * SIMAGE_HEIGHT];
static char sat_data[SIMAGE_WIDTH * SIMAGE_HEIGHT];
static char val_data[SIMAGE_WIDTH * SIMAGE_HEIGHT];
static Server_image preview;
static Server_image hue_image;
static Server_image sat_image;
static Server_image val_image;
static int  huepix[NUM_HUES];
static int  satpix[NUM_SATURATIONS];
static int  valpix[NUM_GRAYS];

/*
 * string_to_xcolor()
 *   string may be:
 *     color name:   "Blue", "Pink", etc.
 *     hex color:   #a0bf1e
 *     sunview decimal color:   100 220 255
 *
 * if it is not one of these formats (or if the name cannot be found),
 * then string_to_xcolor() returns 1, otherwise it returns 0.
 */
static int
string_to_xcolor(s, xcolor)
    char       *s;
    XColor     *xcolor;
{
    if (!XParseColor(dsp, DefaultColormap(dsp, 0), s, xcolor)) {
	int         red,
	            green,
	            blue;
	if (sscanf(s, "%d %d %d", &red, &green, &blue) == 3) {
	    xcolor->red = red << 8;
	    xcolor->green = green << 8;
	    xcolor->blue = blue << 8;
	    xcolor->flags = DoRed | DoGreen | DoBlue;
	} else
	    return 1;
    }
    /*
     * XParseColor may not have multiplied by 257...
     */
    xcolor->red &= 0xff00;
    xcolor->green &= 0xff00;
    xcolor->blue &= 0xff00;
    return 0;
}

static void
update_colors()
{
    XColor      xcolors[5];

    hsv_to_xcolor(&xact_win, &xcolors[BG1Index]);
    olgx_hsv_to_3D(&xact_win, &xcolors[BG2Index],
		   &xcolors[BG3Index], &xcolors[WhiteIndex]);
    hsv_to_xcolor(&xact_work, &xcolors[WorkspaceIndex]);
    xv_set(cms,
	   CMS_COLOR_COUNT, 5,
	   CMS_INDEX, CMS_CONTROL_COLORS + COLOR_CHOICES,
	   CMS_X_COLORS, xcolors,
	   NULL);
}

static void
update_choices()
{
    if (slidermode) {
	if (windowmode) {
	    xact_slider = xact_win;
	} else {
	    xact_slider = xact_work;
	}
	xv_set(h_slider,
	       PANEL_VALUE, xact_slider.h,
	       NULL);
	xv_set(s_slider,
	       PANEL_VALUE, xact_slider.s,
	       NULL);
	xv_set(v_slider,
	       PANEL_VALUE, xact_slider.v,
	       NULL);
    } else {
	int         the_index;

	the_index = (windowmode) ? win_index : work_index;

	xv_set(palette, PANEL_VALUE, the_index, NULL);
    }
}

static char *
hsv_to_string(hsv)
    HSV        *hsv;
{
    XColor      xcolor;
    static char s[8];

    hsv_to_xcolor(hsv, &xcolor);
    sprintf(s, "#%02x%02x%02x",
	    xcolor.red >> 8, xcolor.green >> 8, xcolor.blue >> 8);
    return strdup(s);
}

static void
store_custom_colors()
{
    if (work_index == WORK_INDEX) {
	colors[WORK_INDEX] = xact_work;
	hsv_to_xcolor(&xact_work, &xcolors[WORK_INDEX]);
    }
    if (win_index == WIN_INDEX) {
	colors[WIN_INDEX] = xact_win;
	hsv_to_xcolor(&xact_win, &xcolors[WIN_INDEX]);
    }
    xv_set(cms,
	   CMS_COLOR_COUNT, NUM_CUSTOM,
	   CMS_INDEX, CMS_CONTROL_COLORS + CUSTOM_INDEX,
	   CMS_X_COLORS, xcolors,
	   NULL);
}

static void
backup_colors()
{
    backup_work_index = work_index;
    backup_win_index = win_index;
    backup_xact_win = xact_win;
    backup_xact_work = xact_work;
}

static void
restore_colors()
{
    work_index = backup_work_index;
    win_index = backup_win_index;
    xact_win = backup_xact_win;
    xact_work = backup_xact_work;
}

void
apply_colors()
{
    defaults_set_string(WORKSPACE_CLASS_RES, hsv_to_string(&xact_work));
    defaults_set_string(WINDOW_CLASS_RES, hsv_to_string(&xact_win));
    backup_colors();
    store_custom_colors();
}

void
reset_colors()
{
    restore_colors();
    update_choices();
    update_colors();
}

static void
which_notify(panel_item, choice, event)
    Panel_item  panel_item;
    int         choice;
    Event      *event;
{
    windowmode = choice;
    update_choices();
}

static void
color_notify(panel_item, choice, event)
    Panel_item  panel_item;
    int         choice;
    Event      *event;
{
    if (windowmode) {
	win_index = choice;
	xact_win = colors[win_index];
    } else {
	work_index = choice;
	xact_work = colors[work_index];
    }
    update_colors();
}

static void
slider_notify(panel_item, value, event)
    Panel_item  panel_item;
    int         value;
    Event      *event;
{
    switch (xv_get(panel_item, PANEL_CLIENT_DATA)) {
    case 1:
	xact_slider.h = value;
	break;
    case 2:
	xact_slider.s = value;
	break;
    case 3:
	xact_slider.v = value;
	break;
    }
    if (windowmode) {
	xact_win = xact_slider;
	win_index = WIN_INDEX;
    } else {
	xact_work = xact_slider;
	work_index = WORK_INDEX;
    }
    update_colors();
}

static void
switch_modes()
{
    if (slidermode) {
	xv_set(palette, XV_SHOW, FALSE, NULL);
	update_choices();
	xv_set(h_slider, XV_SHOW, TRUE, NULL);
	xv_set(s_slider, XV_SHOW, TRUE, NULL);
	xv_set(v_slider, XV_SHOW, TRUE, NULL);
	xv_set(h_ticks, XV_SHOW, TRUE, NULL);
	xv_set(s_ticks, XV_SHOW, TRUE, NULL);
	xv_set(v_ticks, XV_SHOW, TRUE, NULL);
    } else {
	xv_set(h_slider, XV_SHOW, FALSE, NULL);
	xv_set(s_slider, XV_SHOW, FALSE, NULL);
	xv_set(v_slider, XV_SHOW, FALSE, NULL);
	xv_set(h_ticks, XV_SHOW, FALSE, NULL);
	xv_set(s_ticks, XV_SHOW, FALSE, NULL);
	xv_set(v_ticks, XV_SHOW, FALSE, NULL);
	update_choices();
	xv_set(palette, XV_SHOW, TRUE, NULL);
    }
}


static void
custom_notify(panel_item, choice, event)
    Panel_item  panel_item;
    int         choice;
    Event      *event;
{
    if (slidermode && !choice) {
	store_custom_colors();
    }
    slidermode = choice;
    switch_modes();
}

Cms 
create_palette()
{
    int         i,
                h,
                s,
                v;
    XColor      winxcolor;
    XColor      workxcolor;
    HSV         background;

    i = NUM_CUSTOM;		/* leave room for the user's current defaults */

    /* make the gray ramp. */

    for (v = 0; v < NUM_GRAYS; v++) {
	valpix[v] = i;
	colors[i].h = 0;
	colors[i].s = 0;
	colors[i].v = grays[v] * MAXSV / 1000;
	hsv_to_xcolor(&colors[i], &xcolors[i]);
	i++;
    }

    /* make the color cube. */

    for (h = 0; h < NUM_HUES; h++) {
	for (v = 0; v < NUM_BRIGHTNESSES; v++) {
	    for (s = 0; s < NUM_SATURATIONS; s++) {
		if (v == 1 && h == 6)
		    satpix[s] = i;
		if (v == 1 && s == 1)
		    huepix[h] = i;
		colors[i].h = hues[h];
		colors[i].s = saturations[s] * MAXSV / 1000;
		colors[i].v = brightnesses[v] * MAXSV / 1000;
		hsv_to_xcolor(&colors[i], &xcolors[i]);
		i++;
	    }
	}
    }

    /* Create black. */

    xcolors[COLOR_CHOICES + BlackIndex].red = 0;
    xcolors[COLOR_CHOICES + BlackIndex].green = 0;
    xcolors[COLOR_CHOICES + BlackIndex].blue = 0;
    xcolors[COLOR_CHOICES + BlackIndex].flags = DoRed | DoGreen | DoBlue;

    /* The default custom colors will be the OpenLook default colors. */

    string_to_xcolor(WORKSPACE_DEFAULT, &xcolors[WORK_INDEX]);
    xcolor_to_hsv(&xcolors[WORK_INDEX], &colors[WORK_INDEX]);
    string_to_xcolor(WINDOW_DEFAULT, &xcolors[WIN_INDEX]);
    xcolor_to_hsv(&xcolors[WIN_INDEX], &colors[WIN_INDEX]);

    /* Create background (window.color.background) of cmdtool preview... */

    string_to_xcolor(defaults_get_string(BACK_RES, BACK_CLASS_RES,
					 BACK_DEFAULT),
		     &xcolors[COLOR_CHOICES + BackIndex]);

    cms = xv_create(0, CMS,
		    XV_VISUAL, xv_get(frame, XV_VISUAL),
		    CMS_TYPE, XV_DYNAMIC_CMS,
		    CMS_CONTROL_CMS, TRUE,
		    CMS_NAME, "palette",
		    CMS_SIZE, CMS_CONTROL_COLORS + COLORMAP_SIZE,
		    CMS_X_COLORS, xcolors,
		    NULL);

    /* look in resources for window and workspace defaults */

    string_to_xcolor(defaults_get_string(WORKSPACE_RES, WORKSPACE_CLASS_RES,
					 WORKSPACE_DEFAULT),
		     &workxcolor);
    xcolor_to_hsv(&workxcolor, &xact_work);

    string_to_xcolor(defaults_get_string(WINDOW_RES, WINDOW_CLASS_RES,
					 WINDOW_DEFAULT),
		     &winxcolor);
    xcolor_to_hsv(&winxcolor, &xact_win);

    /* find out if any palette entries match the resource colors exactly */

#ifdef DEBUG
    printf("workcolor is %04x %04x %04x\n",
	   workxcolor.red, workxcolor.green, workxcolor.blue);
    printf("wincolor  is %04x %04x %04x\n",
	   winxcolor.red, winxcolor.green, winxcolor.blue);
#endif

    work_index = WORK_INDEX;
    win_index = WIN_INDEX;
    for (i = NUM_CUSTOM; i < COLOR_CHOICES; i++) {
	int         r = xcolors[i].red & 0xff00;
	int         g = xcolors[i].green & 0xff00;
	int         b = xcolors[i].blue & 0xff00;
	if (workxcolor.red == r &&
		workxcolor.green == g &&
		workxcolor.blue == b)
	    work_index = i;
	if (winxcolor.red == r &&
		winxcolor.green == g &&
		winxcolor.blue == b)
	    win_index = i;
    }

    store_custom_colors();
    return cms;
}


void
create_color_panel()
{
    char       *colorstr;
    int         i,
                j;

    /* build the preview image... */

    if (defaults_get_boolean(FRAME_STYLE_RES, FRAME_STYLE_CLASS_RES, FALSE))
	image_string = image_string_3dframes;
    else
	image_string = image_string_2dframes;

    for (i = 0; i < IMAGE_WIDTH * IMAGE_HEIGHT; i++) {
	j = image_string[i] - '0';
	/* less than 7 is one of my colors, 7 & 8 are CMS_CONTROL colors */
	switch (j) {
	case 7:
	    j = 2;		/* shadow color */
	    break;
	case 8:
	    j = 3;		/* hilight color */
	    break;
	default:
	    j += CMS_CONTROL_COLORS + COLOR_CHOICES;
	    break;
	}
	image_data[i] = j;
    }
    preview = xv_create(panel_group[COLOR_PANEL], PANEL_MESSAGE,
			PANEL_LABEL_IMAGE,
			xv_create(XV_NULL, SERVER_IMAGE,
				  SERVER_IMAGE_COLORMAP, "palette",
				  XV_WIDTH, IMAGE_WIDTH,
				  XV_HEIGHT, IMAGE_HEIGHT,
				  SERVER_IMAGE_DEPTH, 8,
				  SERVER_IMAGE_X_BITS, image_data,
				  NULL),
			XV_Y, 20,
			XV_HELP_DATA, "props:AppImageInfo",
			NULL);

    which = xv_create(panel_group[COLOR_PANEL], PANEL_CHOICE,
		      PANEL_CHOICE_STRINGS,
		      LOCALIZE(WORKSPACE_STRING), LOCALIZE(WINDOW_STRING),
		      NULL,
		      PANEL_NOTIFY_PROC, which_notify,
		      PANEL_NEXT_ROW, -1,
		      PANEL_CHOICE_NCOLS, 1,
		      PANEL_VALUE, 0,
		      XV_HELP_DATA, "props:ColorChoiceInfo",
		      NULL);

    custom = xv_create(panel_group[COLOR_PANEL], PANEL_CHOICE,
		       PANEL_CHOICE_STRINGS,
		       LOCALIZE(PALETTE_STRING), LOCALIZE(CUSTOM_STRING),
		       NULL,
		       PANEL_NOTIFY_PROC, custom_notify,
		       PANEL_NEXT_ROW, -1,
		       PANEL_CHOICE_NCOLS, 1,
		       PANEL_VALUE, 0,
		       XV_HELP_DATA, "props:CustomColorInfo",
		       NULL);

    { int a,b,c;

	a = xv_get(preview, XV_WIDTH);
	b = xv_get(which, XV_WIDTH);
	c = xv_get(custom, XV_WIDTH);

	palette_x = MAX_VALUE(MAX_VALUE(a,b),c) + 40;
    }	

    xv_set(preview,
	   XV_X, (palette_x - IMAGE_WIDTH) / 2,
	   NULL);
    xv_set(which,
	   XV_X, (palette_x - xv_get(which, XV_WIDTH)) / 2,
	   NULL);
    xv_set(custom,
	   XV_X, (palette_x - xv_get(custom, XV_WIDTH)) / 2,
	   NULL);

    palette = xv_create(panel_group[COLOR_PANEL], PANEL_CHOICE,
			XV_SHOW, FALSE,
			PANEL_CHOICE_NCOLS, NUM_COLUMNS,
			PANEL_LAYOUT, PANEL_VERTICAL,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			PANEL_NOTIFY_PROC, color_notify,
			XV_HELP_DATA, "props:PaletteInfo",
			XV_X, palette_x,
			XV_Y, 10,
			NULL);

    h_slider = xv_create(panel_group[COLOR_PANEL], PANEL_SLIDER,
			 XV_SHOW, FALSE,
			 PANEL_LABEL_STRING, LOCALIZE("Hue:"),
			 PANEL_CLIENT_DATA, 1,
			 PANEL_NOTIFY_LEVEL, PANEL_ALL,
			 PANEL_SHOW_RANGE, FALSE,
			 PANEL_SHOW_VALUE, FALSE,
			 PANEL_VALUE, 0,
			 PANEL_MIN_VALUE, 0,
			 PANEL_MAX_VALUE, SLIDER_MAX_H,
			 PANEL_NOTIFY_PROC, slider_notify,
			 XV_HELP_DATA, "props:HueSlider",
			 NULL);
    for (j = 0; j < SIMAGE_HEIGHT; j++)
	for (i = 0; i < SIMAGE_WIDTH; i++)
	    hue_data[j * SIMAGE_WIDTH + i] = CMS_CONTROL_COLORS +
		huepix[i * NUM_HUES / SIMAGE_WIDTH];
    hue_image = xv_create(XV_NULL, SERVER_IMAGE,
			  SERVER_IMAGE_COLORMAP, "palette",
			  XV_WIDTH, SIMAGE_WIDTH,
			  XV_HEIGHT, SIMAGE_HEIGHT,
			  SERVER_IMAGE_DEPTH, 8,
			  SERVER_IMAGE_X_BITS, hue_data,
			  NULL);
    h_ticks = xv_create(panel_group[COLOR_PANEL], PANEL_MESSAGE,
			PANEL_LABEL_IMAGE, hue_image,
			NULL);


    s_slider = xv_create(panel_group[COLOR_PANEL], PANEL_SLIDER,
			 XV_SHOW, FALSE,
			 PANEL_LABEL_STRING, LOCALIZE("Saturation:"),
			 PANEL_CLIENT_DATA, 2,
			 PANEL_NOTIFY_LEVEL, PANEL_ALL,
			 PANEL_SHOW_RANGE, FALSE,
			 PANEL_SHOW_VALUE, FALSE,
			 PANEL_VALUE, 0,
			 PANEL_MIN_VALUE, 0,
			 PANEL_MAX_VALUE, SLIDER_MAX,
			 PANEL_NOTIFY_PROC, slider_notify,
			 XV_HELP_DATA, "props:SaturationSlider",
			 NULL);
    for (j = 0; j < SIMAGE_HEIGHT; j++)
	for (i = 0; i < SIMAGE_WIDTH; i++)
	    sat_data[j * SIMAGE_WIDTH + i] = CMS_CONTROL_COLORS +
		satpix[i * NUM_SATURATIONS / SIMAGE_WIDTH];
    sat_image = xv_create(XV_NULL, SERVER_IMAGE,
			  SERVER_IMAGE_COLORMAP, "palette",
			  XV_WIDTH, SIMAGE_WIDTH,
			  XV_HEIGHT, SIMAGE_HEIGHT,
			  SERVER_IMAGE_DEPTH, 8,
			  SERVER_IMAGE_X_BITS, sat_data,
			  NULL);
    s_ticks = xv_create(panel_group[COLOR_PANEL], PANEL_MESSAGE,
			PANEL_LABEL_IMAGE, sat_image,
			NULL);


    v_slider = xv_create(panel_group[COLOR_PANEL], PANEL_SLIDER,
			 XV_SHOW, FALSE,
			 PANEL_LABEL_STRING, LOCALIZE("Brightness:"),
			 PANEL_CLIENT_DATA, 3,
			 PANEL_NOTIFY_LEVEL, PANEL_ALL,
			 PANEL_SHOW_RANGE, FALSE,
			 PANEL_SHOW_VALUE, FALSE,
			 PANEL_VALUE, 0,
			 PANEL_MIN_VALUE, 0,
			 PANEL_MAX_VALUE, SLIDER_MAX,
			 PANEL_NOTIFY_PROC, slider_notify,
			 XV_HELP_DATA, "props:BrightnessSlider",
			 NULL);
    for (j = 0; j < SIMAGE_HEIGHT; j++)
	for (i = 0; i < SIMAGE_WIDTH; i++)
	    val_data[j * SIMAGE_WIDTH + i] = CMS_CONTROL_COLORS +
		valpix[i * NUM_GRAYS / SIMAGE_WIDTH];
    val_image = xv_create(XV_NULL, SERVER_IMAGE,
			  SERVER_IMAGE_COLORMAP, "palette",
			  XV_WIDTH, SIMAGE_WIDTH,
			  XV_HEIGHT, SIMAGE_HEIGHT,
			  SERVER_IMAGE_DEPTH, 8,
			  SERVER_IMAGE_X_BITS, val_data,
			  NULL);
    v_ticks = xv_create(panel_group[COLOR_PANEL], PANEL_MESSAGE,
			PANEL_LABEL_IMAGE, val_image,
			NULL);


    { int a,b,c;

	a = xv_get(h_slider, PANEL_LABEL_WIDTH);
	b = xv_get(s_slider, PANEL_LABEL_WIDTH);
	c = xv_get(v_slider, PANEL_LABEL_WIDTH);

	i = MAX_VALUE(MAX_VALUE(a,b),c);
    }
    j = SLIDER_GAP + xv_get(h_slider, XV_HEIGHT);
    xv_set(h_slider,
	   PANEL_VALUE_X, palette_x + i,
	   PANEL_VALUE_Y, FIRST_SLIDER_Y,
	   PANEL_SLIDER_WIDTH, SLIDER_WIDTH - j,
	   NULL);
    xv_set(s_slider,
	   PANEL_VALUE_X, palette_x + i,
	   PANEL_VALUE_Y, xv_get(h_slider, PANEL_VALUE_Y) + j,
	   PANEL_SLIDER_WIDTH, SLIDER_WIDTH - j,
	   NULL);
    xv_set(v_slider,
	   PANEL_VALUE_X, palette_x + i,
	   PANEL_VALUE_Y, xv_get(s_slider, PANEL_VALUE_Y) + j,
	   PANEL_SLIDER_WIDTH, SLIDER_WIDTH - j,
	   NULL);

    xv_set(h_ticks,
	   XV_X, palette_x + i,
	   XV_Y, xv_get(h_slider, PANEL_VALUE_Y)
	   + xv_get(h_slider, XV_HEIGHT)
	   + SLIDER_TICK_GAP,
	   NULL);
    xv_set(s_ticks,
	   XV_X, palette_x + i,
	   XV_Y, xv_get(h_ticks, XV_Y) + j,
	   NULL);
    xv_set(v_ticks,
	   XV_X, palette_x + i,
	   XV_Y, xv_get(s_ticks, XV_Y) + j,
	   NULL);

    color_chip = xv_create(XV_NULL, SERVER_IMAGE,
			   XV_WIDTH, CHIP_WIDTH,
			   XV_HEIGHT, CHIP_HEIGHT,
			   SERVER_IMAGE_DEPTH, 1,
			   SERVER_IMAGE_BITS, chip_data,
			   NULL);
    for (i = 0; i < COLOR_CHOICES; i++)
	xv_set(palette,
	       PANEL_CHOICE_IMAGE, i, color_chip,
	       PANEL_CHOICE_COLOR, i, CMS_CONTROL_COLORS + i,
	       NULL);

    xv_set(custom, PANEL_VALUE, slidermode, NULL);
    update_colors();
    switch_modes();
    backup_colors();
    window_fit_width(panel_group[COLOR_PANEL]);
}
