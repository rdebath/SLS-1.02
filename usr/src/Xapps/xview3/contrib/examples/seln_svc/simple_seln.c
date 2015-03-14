/*
 * simple_seln.c -- print the primary selection by pressing the panel 
 * button.  The selection may have originated from any window or
 * application on the server.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/server.h>
#include <xview/seln.h>

Xv_Server server;

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Panel       panel;
    void        exit();
    int         print_seln();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame) xv_create(NULL, FRAME,
        FRAME_LABEL,            argv[0],
        NULL);
    panel = (Panel)xv_create(frame, PANEL,
        WIN_WIDTH,              WIN_EXTEND_TO_EDGE,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Print Selection",
        PANEL_NOTIFY_PROC,      print_seln,
        NULL);
    window_fit(panel);
    window_fit(frame);

    server = (Xv_Server)xv_get(xv_get(frame, XV_SCREEN), SCREEN_SERVER);

    xv_main_loop(frame);
}

/*
 * Get the selection using selection_ask().  Note that if the 
 * selection is bigger than about 2K, the whole selection will
 * not be gotten with one call, thus this method of getting
 * the selection may not be sufficient for all situations.
 */
int
print_seln(item, event)
Panel_item item;
Event *event;
{
    Seln_holder         holder;
    Seln_request        *response;
    char                text[BUFSIZ];

    /* get the holder of the primary selection */
    holder = selection_inquire(server, SELN_PRIMARY);
    response = selection_ask(server, &holder,
        SELN_REQ_CONTENTS_ASCII, NULL,
        NULL);

    strcpy(text, response->data + sizeof (SELN_REQ_CONTENTS_ASCII));
    printf("---selection---\n%s\n---end seln---\n", text);

    return XV_OK;
}
