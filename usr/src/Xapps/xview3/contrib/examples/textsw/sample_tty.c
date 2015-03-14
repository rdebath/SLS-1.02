/*
 * sample_tty.c -- create a base frame with a tty subwindow.
 * This subwindow runs a UNIX command specified in an argument
 * vector as shown below.  The example does a "man cat".
 */
#include <xview/xview.h>
#include <xview/tty.h>

char *my_argv[] = { "man", "cat",  NULL };

main(argc, argv)
char *argv[];
{
    Tty tty;
    Frame frame;

    xv_init();
    frame = (Frame)xv_create(NULL, FRAME, NULL);
    tty = (Tty)xv_create(frame, TTY,
        WIN_ROWS,       24,
        WIN_COLUMNS,    80,
        TTY_ARGV,       my_argv,
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}
