#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>

static short grey_bits[] = {
#include <images/grey_stencil_50.pr>
};
mpr_static(grey, 64, 64, 1, grey_bits);

void
panel_repaint(panel, pw)
Panel panel;
Xv_Window pw;
{
    xv_replrop(pw, 0, 0, xv_get(pw, XV_WIDTH), xv_get(pw, XV_HEIGHT),
	PIX_SET, &grey, 0, 0);
}

main(argc, argv)
int argc;
char *argv[];
{
    Frame	frame;
    Panel       panel;
    int         quit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
    frame = xv_create(XV_NULL, FRAME, NULL);
    panel = xv_create(frame, PANEL,
	OPENWIN_SHOW_BORDERS,	TRUE,
	PANEL_REPAINT_PROC,	panel_repaint,
	NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      quit,
	PANEL_CLIENT_DATA,	frame,
        NULL);
    /* window_fit(panel); */
    window_fit(frame);

    xv_main_loop(frame);
    exit(0);
}

quit(item)
Panel_item item;
{
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);
    xv_destroy_safe(frame);
}
