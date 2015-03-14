/*
 * multiscreen.c -- display a base frame on two different screens
 * attached to the same X11 server.  In order for this program to
 * work, you must have two screens.
 */
#include <xview/xview.h>

main(argc,argv)
int     argc;
char    *argv[];
{
    Xv_Server  server;
    Xv_Screen  screen_0, screen_1;
    Xv_Window  root_0, root_1;
    Frame      frame_0, frame_1;

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    screen_0 = (Xv_Screen) xv_get(server, SERVER_NTH_SCREEN, 0);
    root_0 = (Xv_Window) xv_get(screen_0, XV_ROOT);

    screen_1 = (Xv_Screen) xv_get(server, SERVER_NTH_SCREEN, 1);
    root_1 = (Xv_Window) xv_get(screen_1, XV_ROOT);

    frame_0 = (Frame) xv_create(root_0, FRAME,
        FRAME_LABEL,    "SCREEN 0",
        NULL);

    frame_1 = (Frame) xv_create(root_1, FRAME,
        FRAME_LABEL,     "SCREEN 1",
        NULL);

    win_insert(frame_1);
    xv_main_loop(frame_0);
}
