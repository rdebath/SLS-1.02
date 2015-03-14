/*
 * icon_demo.c -- demonstrate how an icon is used.  Create a server
 * image and create an icon object with the image as the ICON_IMAGE.
 * Use the icon as the frame's icon.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include <xview/icon.h>

short open_bits[] =  {
#include "open.icon"
};

short closed_bits[] =  {
#include "closed.icon"
};

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame               frame;
    Panel               panel;
    Server_image        open_image, closed_image;
    Icon                icon;
    void                close_frame();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);

    open_image = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        SERVER_IMAGE_BITS,      open_bits,
        NULL);

    closed_image = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        SERVER_IMAGE_BITS,      closed_bits,
        NULL);

    (void) xv_create(panel, PANEL_MESSAGE,
        PANEL_LABEL_IMAGE,      open_image,
        PANEL_NOTIFY_PROC,      close_frame,
        NULL);

    icon = (Icon)xv_create(frame, ICON,
        ICON_IMAGE,             closed_image,
        XV_X,                   100,
        XV_Y,                   100,
        NULL);
    xv_set(frame, FRAME_ICON, icon, NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}

void
close_frame(item, event)
Panel_item item;
Event *event;
{
    Frame frame = (Frame)xv_get(xv_get(item, 
        PANEL_PARENT_PANEL), XV_OWNER);
    xv_set(frame, FRAME_CLOSED, TRUE, NULL);
}
