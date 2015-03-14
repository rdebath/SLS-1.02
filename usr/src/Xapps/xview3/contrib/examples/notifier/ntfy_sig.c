/*
 * ntfy_sig.c -- shows how to catch signals using the notifier
 */
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>

Frame frame;

Notify_value
sigint_handler(client, sig, when)
Notify_client client;
int sig;
{
    printf("Received signal %d\n", sig);
    return NOTIFY_DONE;
}

main (argc, argv)
int argc;
char *argv[];
{
    Panel panel;
    char  buf[BUFSIZ];
    int   n, quit();

    xv_init (XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create (NULL, FRAME,
        FRAME_LABEL,    argv[0],
        XV_WIDTH,       200,
        XV_HEIGHT,      100,
        NULL);

    panel = (Panel)xv_create (frame, PANEL, NULL);

    (void) xv_create (panel, PANEL_BUTTON,
            PANEL_LABEL_STRING,         "Quit",
            PANEL_NOTIFY_PROC,          quit,
            NULL);

    notify_set_signal_func(frame, sigint_handler, SIGINT, NOTIFY_ASYNC);

    xv_main_loop(frame);
}

int
quit()
{
    xv_destroy_safe(frame);
    return XV_OK;
}
