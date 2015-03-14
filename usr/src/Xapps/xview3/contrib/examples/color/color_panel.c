/* color_panel.c --
 * This program demonstrates how to set panel items to different
 * colors using the XView API for color.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cms.h>

/* Color indices */
#define WHITE           0
#define RED             1
#define GREEN           2
#define BLUE            3
#define NUM_COLORS      4

/* Create a frame, panel, and a colormap segment and assign the
 * cms to the panel.
 */
main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Cms         cms;
    extern void exit(), pressed();
    static Xv_singlecolor colors[] = {
        { 255, 255, 255 }, /* white */
        { 255,   0,   0 }, /* red */
        { 0,   255,   0 }, /* green */
        { 0,     0, 255 }, /* blue */
    };

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    cms = (Cms) xv_create(NULL, CMS,
        CMS_CONTROL_CMS,        TRUE,
        CMS_SIZE,               CMS_CONTROL_COLORS + 4,
        CMS_COLORS,             colors,
        NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);

    panel = xv_create(frame, PANEL,
        WIN_CMS,        cms,
        NULL);

    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Red",
        PANEL_ITEM_COLOR,       CMS_CONTROL_COLORS + RED,
        PANEL_NOTIFY_PROC,      pressed,
        NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Green",
        PANEL_ITEM_COLOR,       CMS_CONTROL_COLORS + GREEN,
        PANEL_NOTIFY_PROC,      pressed,
        NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Blue",
        PANEL_ITEM_COLOR,       CMS_CONTROL_COLORS + BLUE,
        PANEL_NOTIFY_PROC,      pressed,
        NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_ITEM_COLOR,       CMS_CONTROL_COLORS + WHITE,
        PANEL_NOTIFY_PROC,      exit,
        NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}

void
pressed(item, event)
Panel_item item;
Event *event;
{
    char *name = (char *)xv_get(item, PANEL_LABEL_STRING);
    Frame frame = xv_get(xv_get(item, PANEL_PARENT_PANEL), XV_OWNER);

    xv_set(frame, FRAME_LEFT_FOOTER, name, NULL);
}
