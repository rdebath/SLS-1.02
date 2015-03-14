/*
 * textsw_to_ttysw.c -- send text from a text subwindow to a 
 * tty subwindow using ttysw_output()
 */
#include <stdio.h>
#include <xview/panel.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/tty.h>

Textsw  textsw;
Tty     ttysw;

main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    void        text_to_tty(), exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL, argv[0],
        NULL);
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT, PANEL_VERTICAL,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Text To Tty",
        PANEL_NOTIFY_PROC,      text_to_tty,
        NULL);
    window_fit(panel);

    textsw = (Textsw)xv_create(frame, TEXTSW,
        WIN_ROWS,       10,
        WIN_COLUMNS,    80,
        NULL);
    ttysw = (Tty)xv_create(frame, TTY,
        WIN_BELOW,      textsw,
        WIN_X,          0,
        TTY_ARGV,       TTY_ARGV_DO_NOT_FORK,
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}

/*
 * callback routine for the panel button -- read text from textsw 
 * and send it to the ttysw using ttysw_output()
 */
void
text_to_tty(item, event)
Panel_item item;
Event *event;
{
    char buf[BUFSIZ];

    (void) xv_get(textsw, TEXTSW_CONTENTS, 0, buf, sizeof buf);
    ttysw_output(ttysw, buf, strlen(buf));
}
