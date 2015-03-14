/*
 * btn_menu.c -- display a panel that has an OPEN LOOK menu button.
 * The choices displayed are Yes, No and Quit.  If Quit is selected
 * in the menu, the program exits.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/openmenu.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame       frame;
    Panel       panel;
    Menu        menu;
    int         selected();
    void        menu_proc();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);

    /* Create the menu _before_ the panel button */
    menu = (Menu)xv_create(NULL, MENU,
        MENU_NOTIFY_PROC,       menu_proc,
        MENU_STRINGS,           "Yes", "No", "Quit", NULL,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Y/N/Q",
        PANEL_NOTIFY_PROC,      selected,
        PANEL_ITEM_MENU,        menu, /* attach menu to button */
        NULL);
    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}

int
selected(item, event)
Panel_item item;
Event *event;
{
    printf("%s selected...\n", xv_get(item, PANEL_LABEL_STRING));
    return XV_OK;
}

void
menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
    printf("Menu Item: %s\n", xv_get(menu_item, MENU_STRING));
    if (!strcmp((char *)xv_get(menu_item, MENU_STRING), "Quit"))
        exit(0);
}
