/*
 * color_objs.c --
 *    This program demonstrates the use of color in XView. It allows
 *    the user to choose the foreground and background colors of the
 *    various objects in an interactive manner.
 */
#include <xview/xview.h>
#include <xview/svrimage.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <xview/notice.h>

#define SELECT_TEXTSW           0
#define SELECT_TEXTSW_VIEW      1
#define SELECT_PANEL            2
#define SELECT_ICON             3

#define NUM_COLORS              8

/* Icon data */
static unsigned short icon_bits[] = {
#include "cardback.icon"
};

/* solid black square */
static unsigned short black_bits[] = {
    0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
    0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
};

Panel_item      objects;
Textsw          textsw;
Panel           panel;
Icon            icon;

/*
 * main()
 *    Create a panel and panel items. The application uses panel items
 *    to choose a particular object and change its foreground and
 *    background colors in an interactive manner. Create a textsw.
 *    Create an icon. All the objects share the same colormap segment.
 */
main(argc,argv)
int     argc;
char    *argv[];
{
    Frame         frame;
    Panel_item    color_choices, panel_fg_bg;
    Cms           cms;
    int           i;
    Server_image  chip, icon_image;
    void          color_notify();
    extern void   exit();
    static Xv_singlecolor cms_colors[] = {
        { 255, 255, 255 }, /* white  */
        { 255, 0, 0 },     /* red    */
        { 0, 255, 0 },     /* green  */
        { 0, 0, 255 },     /* blue   */
        { 255, 255, 0 },   /* yellow */
        { 188, 143, 143 }, /* brown  */
        { 220, 220, 220 }, /* gray   */
        { 0, 0, 0 },       /* black  */
    };

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME,
        FRAME_LABEL,    argv[0],
        NULL);

    cms = (Cms)xv_create(NULL, CMS,
        CMS_NAME,           "palette",
        CMS_CONTROL_CMS,    TRUE,
        CMS_TYPE,           XV_STATIC_CMS,
        CMS_SIZE,           CMS_CONTROL_COLORS + NUM_COLORS,
        CMS_COLORS,         cms_colors,
        NULL);

    /* Create panel and set the colormap segment on the panel */
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,       PANEL_VERTICAL,
        WIN_CMS,            cms,
        NULL);

    /* Create panel items */
    objects = (Panel_item)xv_create(panel, PANEL_TOGGLE,
        PANEL_LABEL_STRING,     "Objects",
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_CHOICE_STRINGS,   "Textsw", "Textsw View",
                                "Panel", "Icon", NULL,
        NULL);

    panel_fg_bg = (Panel_item)xv_create(panel, PANEL_CHECK_BOX,
        PANEL_LABEL_STRING,     "Fg/Bg",
        PANEL_CHOOSE_ONE,       TRUE,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_CHOICE_STRINGS,   "Background", "Foreground", NULL,
        NULL);

    chip = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
        XV_WIDTH,           16,
        XV_HEIGHT,          16,
        SERVER_IMAGE_DEPTH, 1,
        SERVER_IMAGE_BITS,  black_bits,
        NULL);
    color_choices = (Panel_item)xv_create(panel, PANEL_CHOICE,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        PANEL_LABEL_STRING,     "Colors",
        PANEL_CLIENT_DATA,      panel_fg_bg,
        XV_X,                   (int)xv_get(panel_fg_bg, XV_X),
        PANEL_NEXT_ROW,         15,
        PANEL_CHOICE_IMAGES,
            chip, chip, chip, chip, chip, chip, chip, chip, NULL,
        PANEL_CHOICE_COLOR,     0, CMS_CONTROL_COLORS + 0,
        PANEL_CHOICE_COLOR,     1, CMS_CONTROL_COLORS + 1,
        PANEL_CHOICE_COLOR,     2, CMS_CONTROL_COLORS + 2,
        PANEL_CHOICE_COLOR,     3, CMS_CONTROL_COLORS + 3,
        PANEL_CHOICE_COLOR,     4, CMS_CONTROL_COLORS + 4,
        PANEL_CHOICE_COLOR,     5, CMS_CONTROL_COLORS + 5,
        PANEL_CHOICE_COLOR,     6, CMS_CONTROL_COLORS + 6,
        PANEL_CHOICE_COLOR,     7, CMS_CONTROL_COLORS + 7,
        PANEL_NOTIFY_PROC,      color_notify,
        NULL);

    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void)window_fit_height(panel);

    /* create textsw and set the colormap segment for it */
    textsw = (Textsw)xv_create(frame, TEXTSW,
        WIN_CMS,              cms,
        WIN_BELOW,            panel,
        WIN_ROWS,             15,
        WIN_COLUMNS,          80,
        TEXTSW_FILE_CONTENTS, "/etc/motd",
        WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS + 0,
        NULL);

    /* adjust panel dimensions */
    (void)xv_set(panel, WIN_WIDTH, xv_get(textsw, WIN_WIDTH), NULL);

    icon_image = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,             64,
        XV_HEIGHT,            64,
        SERVER_IMAGE_DEPTH,   1,
        SERVER_IMAGE_BITS,    icon_bits,
        NULL);
    /* associate icon with the base frame */
    icon = (Icon)xv_create(XV_NULL, ICON,
        ICON_IMAGE,           icon_image,
        WIN_CMS,              cms,
        WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS + 0,
        NULL);
    xv_set(frame, FRAME_ICON, icon, NULL);

    window_fit(frame);

    xv_main_loop(frame);
}

/*
 * This routine gets called when a color selection is made.
 * Set the foreground or background on the currently selected object.
 * WIN_FOREGROUND_COLOR & WIN_BACKGROUND_COLOR allow the application
 * to specify indices into the colormap segment as the foreground
 * and background values.
 */
void
color_notify(panel_item, choice, event)
Panel_item      panel_item;
int             choice;
Event           *event;
{
    int cnt;
    Xv_opaque object, get_object();
    unsigned objs = (unsigned)xv_get(objects, PANEL_VALUE);
    int fg = (int)xv_get(xv_get(panel_item, PANEL_CLIENT_DATA),
                     PANEL_VALUE);

    /* the value of the objects panel item is a bit mask ... "on" bits
     * mean that the choice is selected.  Get the object associated
     * with the choice and set it's color.  "&" tests bits in a mask.
     */
    for (cnt = 0; objs; cnt++, objs >>= 1)
        if (objs & 1)
            if ((object = get_object(cnt)) != panel)
                xv_set(object,
                    fg? WIN_FOREGROUND_COLOR : WIN_BACKGROUND_COLOR,
                    CMS_CONTROL_COLORS + choice, NULL);
            else if (fg)
                PANEL_EACH_ITEM(panel, panel_item)
                    xv_set(panel_item,
                        PANEL_ITEM_COLOR, CMS_CONTROL_COLORS + choice,
                        NULL);
                PANEL_END_EACH
            else
                notice_prompt(panel, NULL,
                    NOTICE_FOCUS_XY, event_x(event), event_y(event),
                    NOTICE_MESSAGE_STRINGS,
                        "You can't set the color of a panel.", NULL,
                    NOTICE_BUTTON_YES, "Ok",
                    NULL);
}

/*
 *    Return the XView handle to nth object.
 */
Xv_opaque
get_object(n)
int n;
{
    switch (n) {
        case SELECT_TEXTSW:
            return textsw;
        case SELECT_TEXTSW_VIEW:
            return xv_get(textsw, OPENWIN_NTH_VIEW, 0);
        case SELECT_PANEL:
            return panel;
        case SELECT_ICON:
            return icon;
        default:
            return textsw;
    }
}
