/*
 * svrimage.c -- demonstrate how a server image can be created and
 * used.  The "bits" used to create the image are taken arbitrarily
 * from <images/trash.icon>
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include <X11/Xlib.h>

short image_bits[] =  {
    0x0000,0x0000, 0x0000,0x0000, 0x0000,0x0000, 0x0000,0x0000,
    0x0007,0xE000, 0x0004,0x2000, 0x03FF,0xFFC0, 0x0200,0x0040,
    0x02FF,0xFF40, 0x0080,0x0100, 0x00AA,0xAB00, 0x00AA,0xAB00,
    0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00,
    0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00,
    0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00,
    0x00AA,0xAB00, 0x00AA,0xAB00, 0x00AA,0xAB00, 0x0091,0x1300,
    0x00C0,0x0200, 0x003F,0xFC00
};

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame               frame;
    Server_image        image;
    Panel               panel;
    void                exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    image = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               32,
        XV_HEIGHT,              30,
        SERVER_IMAGE_BITS,      image_bits,
        NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_MESSAGE,
        PANEL_LABEL_IMAGE,      image,
        PANEL_NOTIFY_PROC,      exit,
        NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}
