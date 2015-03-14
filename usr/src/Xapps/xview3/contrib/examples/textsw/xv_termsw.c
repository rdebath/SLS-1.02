/*
 * xv_termsw.c
 * Demonstrate incorporation of a Term subwindow in an application;
 * keyboard input to the termsw can come either directly to the
 * termsw or from an adjoining panel text item.
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
/* #include <xview/tty.h> */
#include <xview/termsw.h>

Termsw          term;
Panel_item      text_item;

main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    int         notify_proc();

    xv_init(XV_INIT_ARGS, argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    text_item = (Panel_item)xv_create(panel, PANEL_TEXT,
        PANEL_LABEL_STRING,         "Command:",
        PANEL_NOTIFY_PROC,          notify_proc,
        PANEL_VALUE_DISPLAY_LENGTH, 20,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Apply",
        PANEL_NOTIFY_PROC,      notify_proc,
        NULL);
    window_fit_height(panel);

    term = (Termsw)xv_create(frame, TERMSW, NULL);

    window_fit(frame);
    xv_main_loop(frame);
}

/*
 * This procedure is called when the user this return on the
 * panel text item or clicking on the <apply> button.
 * Use ttysw_input() to feed the string to the termal window.
 */
int
notify_proc(item,event)
Panel_item      item;
Event   *event;
{
    char        str[81];
    
    sprintf(str, "%.81s\n", (char *)xv_get(text_item, PANEL_VALUE));
    ttysw_input(term, str, strlen(str));
    xv_set(text_item, PANEL_VALUE, "", NULL);
    return XV_OK;
}
