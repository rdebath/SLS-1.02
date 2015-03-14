#include <xview/xview.h>
#include <xview/frame.h>

/*
 * subframe.c -- create a base frame that has an associated subframe.
 * Pull the pin out of the subframe and its FRAME_DONE_PROC procedure
 * gets called.
 */
main(argc, argv)
int argc;
char *argv[];
{
    Frame frame, subframe;
    int done_proc();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);

    subframe = (Frame)xv_create(frame, FRAME_CMD,
        FRAME_DONE_PROC, done_proc,
        XV_SHOW,         TRUE,
        NULL);
    xv_main_loop(frame);
}

/*
 * when the pushpin is pulled out, this routine is called
 */
done_proc(subframe)
Frame subframe;
{
    /* we have the choice of vetoing or granting the user's
     * request to dismiss the frame -- if we choose to dismiss
     * the frame, we must do it manually.  Like so:
     */
    xv_set(subframe, XV_SHOW, FALSE, NULL);
    /* otherwise, we should push the pin back in */
}
