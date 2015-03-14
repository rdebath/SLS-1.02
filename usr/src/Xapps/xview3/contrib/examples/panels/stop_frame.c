/*
 * stop.c -- Use a server image as a panel message item.
 */
#include <xview/xview.h>
#include <xview/svrimage.h>
#include <xview/panel.h>

static short stop_bits[] = {
    0x3E00, 0x7F00, 0xFF80, 0xFF80, 0xFF80, 0xFF80, 0xFF80, 0x7F00,
    0x3E00, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x7F00, 0x0000
};

main(argc, argv)
char *argv[];
{
    Frame frame;
    Panel panel;
    Server_image stopsign;

    xv_init();

    frame = (Frame)xv_create(NULL, FRAME,
        FRAME_SHOW_HEADER,  FALSE,
        NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);

    stopsign = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,           16,
        XV_HEIGHT,          16,
        SERVER_IMAGE_DEPTH, 1,
        SERVER_IMAGE_BITS,  stop_bits,
        NULL);

    (void) xv_create(panel, PANEL_MESSAGE,
        PANEL_LABEL_IMAGE,  stopsign,
        NULL);
    (void) xv_create(panel, PANEL_MESSAGE,
        PANEL_LABEL_STRING,
            "This action will cause unsaved edits to be lost.",
        NULL);
    (void)xv_create(panel, PANEL_BUTTON,
        PANEL_NEXT_ROW,     -1,
        XV_X,               110,
        PANEL_LABEL_STRING, "Ok",
        NULL);
    (void)xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING, "Cancel",
        NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}
