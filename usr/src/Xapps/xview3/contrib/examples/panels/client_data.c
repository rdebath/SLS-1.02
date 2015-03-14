/*
 * client_data.c -- demonstrate the use of PANEL_CLIENT_DATA attached
 * to panel items.  Attach the base frame to the "Quit" panel item so
 * that the notify procedure can call xv_destroy_safe() on the frame.
 */
#include <xview/xview.h>
#include <xview/panel.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame  frame;
    Panel  panel;
    int    quit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      quit,
        PANEL_CLIENT_DATA,      frame,
        NULL);

    xv_main_loop(frame);
    puts("The program is now done.");
    exit(0);
}

quit(item)
Panel_item item;
{
    Frame frame = (Frame)xv_get(item, PANEL_CLIENT_DATA);
    xv_destroy_safe(frame);
}
