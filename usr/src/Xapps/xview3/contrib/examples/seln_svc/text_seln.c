/*
 * seln.c -- print the primary selection from the server.  If the 
 * selection is in a text subwindow, also print information about 
 * the line number(s) the selection spans and the indexes of 
 * the bytes within the textsw's buffer.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/server.h>
#include <xview/seln.h>

Xv_Server       server;
Textsw          textsw;

char *get_selection();

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Panel       panel;
    void        exit();
    int         print_seln();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
    frame = (Frame)xv_create(NULL, FRAME,
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
        PANEL_LABEL_STRING,     "Get Selection",
        PANEL_NOTIFY_PROC,      print_seln,
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

    server = (Xv_Server)xv_get(xv_get(frame, XV_SCREEN), SCREEN_SERVER);

    xv_main_loop(frame);
}

int
print_seln()
{
    char *text = get_selection();

    if (text)
        printf("---selection---\n%s\n---end seln---\n", text);

    return XV_OK;
}

/*
 * Get the selection using selection_ask().  Note that if the 
 * selection is bigger than about 2K, the whole selection will 
 * not be gotten with one call, thus this method of getting the 
 * selection may not be sufficient.
 */
char *
get_selection()
{
    long                sel_lin_num, lines_selected;
    Textsw_index        first, last;
    Seln_holder         holder;
    Seln_result         result;
    int                 len;
    Seln_request        *response;
    static char         selection_buf[BUFSIZ];
    register char       *ptr;

    /* get the holder of the primary selection */
    holder = selection_inquire(server, SELN_PRIMARY);

    /* If the selection occurs in the text subwindow, print lots of
     * info about the selection.
     */
    if (seln_holder_same_client(&holder, textsw)) {
        /* ask for information from the selection service */
        response = selection_ask(server, &holder,
            /* get index of the first and last chars in the textsw */
            SELN_REQ_FIRST,             NULL,
            SELN_REQ_LAST,              NULL,
            /* get the actual selection bytes */
            SELN_REQ_CONTENTS_ASCII,    NULL,
            /* Now fool the textsw to think entire lines are selected */
            SELN_REQ_FAKE_LEVEL,        SELN_LEVEL_LINE,
            /* Get the line numbers of beginning and ending of the
             * selection */
            SELN_REQ_FIRST_UNIT,        NULL,
            SELN_REQ_LAST_UNIT,         NULL,
            NULL);
        /* set the ptr to beginning of data -- SELN_REQ_FIRST */
        ptr = response->data;
        /* "first" is data succeeding SELN_REQ_FIRST -- skip attr */
        first = *(Textsw_index *)(ptr += sizeof(SELN_REQ_FIRST));
        ptr += sizeof(Textsw_index); /* skip over value of "first" */
        /* "last" is data succeeding SELN_REQ_LAST -- skip attr */
        last  = *(Textsw_index *)(ptr += sizeof(SELN_REQ_LAST));
        ptr += sizeof(Textsw_index); /* skip over value of "last" */

        /* advance pointer past SELN_REQ_CONTENTS_ASCII */
        ptr += sizeof(SELN_REQ_CONTENTS_ASCII);
        len = strlen(ptr); /* length of string in response */
        (void) strcpy(selection_buf, ptr);
        /*
         * advance pointer past length of string.  If the string length
         * isn't aligned to a 4-byte boundary, add the difference in
         * bytes -- then advance pointer passed "value".
         */
        if (len % 4)
            len = len + (4 - (len % 4));
        ptr += len + sizeof(Seln_attribute); /* skip over "value" */

        /* advance pointer past SELN_REQ_FAKE_LEVEL, SELN_LEVEL_LINE */
        ptr += sizeof(SELN_REQ_FAKE_LEVEL) + sizeof(SELN_LEVEL_LINE);

        sel_lin_num = *(long *)(ptr += sizeof(SELN_REQ_FIRST_UNIT));
        ptr += sizeof(long);
        lines_selected = *(long *)(ptr += sizeof(SELN_REQ_LAST_UNIT));
        ptr += sizeof(long);

        /* hack to workaround bug with SELN_REQ_LAST_UNIT always
         * returning -1.  Count the lines explicitly in the selection.
         */
        if (lines_selected < 0) {
            register char *p;
            lines_selected++;
            for (p = selection_buf; *p; p++)
                if (*p == '\n')
                    lines_selected++;
        }
        printf("index in textsw: %d-%d, line number(s) = %d-%d\n",
            first+1, last+1, sel_lin_num+1,
            sel_lin_num + lines_selected + 1);
    } else {
        /* the selection does not lie in our text subwindow */
        response = selection_ask(server, &holder,
            SELN_REQ_CONTENTS_ASCII, NULL,
            NULL);
        if (response->status != SELN_SUCCESS) {
            printf("selection_ask() returns %d\n", response->status);
            return NULL;
        }
        (void) strcpy(selection_buf,
            response->data + sizeof(SELN_REQ_CONTENTS_ASCII));
    }
    return selection_buf;
}
