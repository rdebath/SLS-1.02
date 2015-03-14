/*
 * list.c -- show a scrolling list with three items in it.
 * Each item is an icon (a pattern) and a string.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/svrimage.h>

#define gray1_width 16
#define gray1_height 16
static char gray1_bits[] = {
    0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
    0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
    0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
    0xaa, 0xaa
};

#define gray2_width 16
#define gray2_height 16
static char gray2_bits[] = {
    0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11,
    0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x00, 0x00,
    0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44,
    0x00, 0x00
};

#define gray3_width 16
#define gray3_height 16
static char gray3_bits[] = {
    0x22, 0x22, 0xee, 0xee, 0x33, 0x33, 0xee, 0xee, 0x22, 0x22,
    0xee, 0xee, 0x33, 0x33, 0xee, 0xee, 0x22, 0x22, 0xee, 0xee,
    0x33, 0x33, 0xee, 0xee, 0x22, 0x22, 0xee, 0xee, 0x33, 0x33,
    0xee, 0xee
};

main(argc, argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Server_image gray1, gray2, gray3;
    extern void exit(), which_glyph();

    xv_init(XV_INIT_ARGS, argc, argv, NULL);

    gray1 = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               gray1_width,
        XV_HEIGHT,              gray1_height,
        SERVER_IMAGE_BITS,      gray1_bits,
        NULL);
    gray2 = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               gray2_width,
        XV_HEIGHT,              gray2_height,
        SERVER_IMAGE_BITS,      gray2_bits,
        NULL);
    gray3 = (Server_image)xv_create(NULL, SERVER_IMAGE,
        XV_WIDTH,               gray3_width,
        XV_HEIGHT,              gray3_height,
        SERVER_IMAGE_BITS,      gray3_bits,
        NULL);
    frame = (Frame)xv_create(NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);

    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);

    (void) xv_create(panel, PANEL_LIST,
        PANEL_LIST_ROW_HEIGHT,   16,
        PANEL_LIST_GLYPHS,       gray1, gray2, gray3, NULL,
        PANEL_LIST_STRINGS,      "Pattern1", "Pattern2", "Pattern3", NULL,
        PANEL_LIST_CLIENT_DATAS, 1, 2, 3, NULL,
        PANEL_NOTIFY_PROC,       which_glyph,
        NULL);

    window_fit(panel);
    window_fit(frame);

    xv_main_loop(frame);
}

void
which_glyph(item, string, client_data, op, event)
Panel_item     item; /* panel list item */
char          *string;
caddr_t        client_data;
Panel_list_op  op;
Event         *event;
{
    printf("item = %s (#%d), op = %d\n", string, client_data, op);
}
