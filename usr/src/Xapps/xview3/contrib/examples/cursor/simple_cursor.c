/*
 * simple_cursor.c -- create a cursor (looks like an hourglass) and
 * assign it to a canvas window.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/svrimage.h>

/* data that describes the cursor's image -- see SERVER_IMAGE below */
short cursor_bits[] = {
/* Width=16, Height=16, Depth=1, */
    0x7FFE,0x4002,0x200C,0x1A38,0x0FF0,0x07E0,0x03C0,0x0180,
    0x0180,0x0240,0x0520,0x0810,0x1108,0x23C4,0x47E2,0x7FFE
};

main(argc, argv)
int argc;
char *argv[];
{
    Frame        frame;
    Canvas       canvas;
    Xv_Cursor    cursor;
    Server_image svr_image;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /*
     * create a server image to use as the cursor's image.
     */
    svr_image = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
        XV_WIDTH,               16,
        XV_HEIGHT,              16,
        SERVER_IMAGE_BITS,      cursor_bits,
        NULL);
    /*
     * create a cursor based on the image just created
     */
    cursor = (Xv_Cursor)xv_create(XV_NULL, CURSOR,
        CURSOR_IMAGE,           svr_image,
        NULL);

    /*
     * Create a base frame and a canvas
     */
    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               100,
        XV_HEIGHT,              100,
        NULL);
    /*
     * set the cursor to the paint window for the canvas
     * Do not set it for the canvas itself.
     */
    xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
        WIN_CURSOR,             cursor,
        NULL);

    window_fit(frame);
    window_main_loop(frame);
}
