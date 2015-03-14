/*
 * seln_line.c -- demonstrate how to use the selection service to get
 * the line number of the primary selection in a textsw.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/seln.h>

Textsw  textsw;

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Panel       panel;
    void        exit();
    int         seln_proc();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME,
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);

    panel = (Panel)xv_create(frame, PANEL,
        WIN_WIDTH,              WIN_EXTEND_TO_EDGE,
        NULL);

    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Get Selection",
        PANEL_NOTIFY_PROC,      seln_proc,
        PANEL_CLIENT_DATA,      frame,
        NULL);
    (void) xv_create(panel, PANEL_TEXT,
        PANEL_LABEL_STRING,             "No-op:",
        PANEL_VALUE_DISPLAY_LENGTH,     30,
        NULL);

    window_fit(panel);

    textsw = (Textsw)xv_create(frame, TEXTSW,
        WIN_X,                  0,
        WIN_BELOW,              panel,
        WIN_ROWS,               10,
        WIN_COLUMNS,            80,
        TEXTSW_FILE_CONTENTS,   "/etc/passwd",
        NULL);
    window_fit(frame);
    xv_main_loop(frame);
}

int
seln_proc(item, event)
Panel_item item;
Event *event; /* unused */
{
    Frame         frame = (Frame)xv_get(item, PANEL_CLIENT_DATA);
    Seln_holder   holder;
    Seln_request *buffer;
    int           line_number;
    char         *msg[32];

    /* 
     * get primary selection 
     */
    holder = seln_inquire(SELN_PRIMARY);
    /* 
     * ask for the data containing line number of the first
     * character of the selection
     */
    buffer = seln_ask(&holder,
        SELN_REQ_FAKE_LEVEL, SELN_LEVEL_LINE,
        SELN_REQ_FIRST_UNIT, 0,
        NULL);
    /*
     * determine the window that contains the selection
     */
    if (seln_holder_same_client(&holder, textsw)) {
        xv_set(frame,
            FRAME_LEFT_FOOTER, "selection in textsw",
            NULL);
        /* 
         * convert data into the line number 
         */
        sprintf(msg, "Selection: line %ld",
            *(long *)(buffer->data + 3 * sizeof(Seln_attribute)));
        xv_set(frame,
            FRAME_RIGHT_FOOTER, msg,
            NULL);
    } else
        xv_set(frame,
            FRAME_LEFT_FOOTER, "selection elsewhere",
            NULL);

    return XV_OK;
}
