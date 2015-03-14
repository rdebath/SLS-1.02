/*
 * screen.c -- get some simple info about the current screen:
 * width, height, depth.
 */
#include <xview/xview.h>
#include <xview/screen.h>

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame               frame;
    Xv_Screen           screen;
    Display             *dpy;
    int                 screen_no;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    printf("Server display = '%s'\n", dpy->vendor);
    screen = (Xv_Screen)xv_get(frame, XV_SCREEN);

    screen_no = (int)xv_get(screen, SCREEN_NUMBER);
    printf("Screen #%d: width: %d, height: %d, depth: %d\n",
        screen_no,
        DisplayWidth(dpy, screen_no),
        DisplayHeight(dpy, screen_no),
        DefaultDepth(dpy, screen_no));
}
