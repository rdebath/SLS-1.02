/*
 * subframe.c -- display a subframe from a base frame.
 */
#include <xview/xview.h>

main(argc, argv)
int argc;
char *argv[];
{
    Frame frame, subframe;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME,
        XV_WIDTH,       100,
        XV_HEIGHT,      100,
        FRAME_LABEL,    "Base Frame",
        NULL);

    subframe = (Frame)xv_create(frame, FRAME_CMD,
        XV_WIDTH,       100,
        XV_HEIGHT,      100,
        FRAME_LABEL,    "Popup",
        NULL);

    xv_set(subframe, XV_SHOW, TRUE, NULL);

    xv_main_loop(frame);
}
