/*
 * ntfy_do_dis.c -- show an example of implicit notifier dispatching
 * by calling notify_do_dispatch().  Create a frame, panel and "Quit"
 * button, and then loop on calls to read() from stdin.  Event
 * processing is still maintained because the Notifier uses it's own
 * non-blocking read().
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>

Frame frame;

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
        XV_SHOW,        TRUE,
        NULL);

    panel = (Panel)xv_create (frame, PANEL, NULL);

    (void) xv_create (panel, PANEL_BUTTON,
            PANEL_LABEL_STRING,         "Quit",
            PANEL_NOTIFY_PROC,          quit,
            NULL);

    /* Force the frame to be displayed by flushing the server */
    XFlush(xv_get(frame, XV_DISPLAY));

    /* tell the Notifier that it should use its own read() so that it
     * can also detect and dispatch events.  This allows us to loop
     * in this code segment and still process events.
     */
    notify_do_dispatch();

    puts("Frame being displayed -- type away.");
    while ((n = read(0, buf, sizeof buf)) >= 0)
        printf("read %d bytes\n", n);

    printf("read() returned %d\n", n);
}

int
quit()
{
    xv_destroy_safe(frame);
    return XV_OK;
}
