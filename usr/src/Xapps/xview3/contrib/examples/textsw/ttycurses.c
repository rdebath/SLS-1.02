/*
 * ttycurses.c -- An application that uses a tty subwindow that
 * emulates a tty so well, you can use curses(3x) routines in it.
 * This program does not handle resizes -- resizing the base frame
 * produces unpredictable results.  To handle resizing properly,
 * the application should install a resize event handler and
 * call endwin() followed by initscr() to reinitialize curses
 * to reflect the size of the window.
 *
 * cc ttycurses.c -lxview -lcurses -ltermlib
 */
#include <curses.h>
#undef WINDOW /* defined by curses.h -- needs to be undefined */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/tty.h>

/* panel items contain the x,y info for outputting text to the ttysw */
Panel_item  x, y, text;

main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Tty         ttysw;
    char        buf[16];
    void        output(), exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);

    panel = (Frame)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,             "Quit",
        PANEL_NOTIFY_PROC,              exit,
        NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,             "Print",
        PANEL_NOTIFY_PROC,              output,
        NULL);
    x = (Panel_item)xv_create(panel, PANEL_NUMERIC_TEXT,
        PANEL_LABEL_STRING,             "X:",
        PANEL_VALUE_DISPLAY_LENGTH,     3,
        NULL);
    y = (Panel_item)xv_create(panel, PANEL_NUMERIC_TEXT,
        PANEL_LABEL_STRING,             "Y:",
        PANEL_VALUE_DISPLAY_LENGTH,     3,
        NULL);
    text = (Panel_item)xv_create(panel, PANEL_TEXT,
        PANEL_LABEL_STRING,             "Text:",
        PANEL_VALUE_DISPLAY_LENGTH,     10,
        PANEL_VALUE,                    "X",
        NULL);
    window_fit(panel);

    ttysw = (Tty)xv_create(frame, TTY,
        WIN_BELOW,      panel,
        WIN_X,          0,
        TTY_ARGV,       TTY_ARGV_DO_NOT_FORK,
        NULL);
    window_fit(frame);

    dup2((int)xv_get(ttysw, TTY_TTY_FD), 0); /* dup2 closes 0 first */
    dup2((int)xv_get(ttysw, TTY_TTY_FD), 1); /* dup2 closes 1 first */

    /* initscr() initializes the curses package and determines
     * characteristics about the window as if it were a terminal.
     * The curses specific variables, LINES and COLS are now set
     * to the row and column sizes of the window.
     */
    initscr();

    xv_set(x, PANEL_MAX_VALUE, COLS-1, NULL);
    xv_set(y, PANEL_MAX_VALUE, LINES-1, NULL);
    sprintf(buf, "LINES: %d", LINES-1);
    xv_set(frame, FRAME_LEFT_FOOTER, buf, NULL);
    sprintf(buf, "COLS: %d", COLS-1);
    xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);

    xv_main_loop(frame);
}
/*
 * callback routine for the <print> panel button.  Get the corrdinates
 * and the text to print on the tty subwindow and use curses library
 * routines to render the text.
 */
void
output()
{
    int X = (int)xv_get(x, PANEL_VALUE);
    int Y = (int)xv_get(y, PANEL_VALUE);
    char *Text = (char *)xv_get(text, PANEL_VALUE);
    mvaddstr(Y, X, Text);
    refresh();
}
