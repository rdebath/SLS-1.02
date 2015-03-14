#include <xview/xview.h>
#include <xview/panel.h>

main(argc, argv)
char *argv[];
{
    Frame frame;
    Panel panel;
    extern void exit();
    int do_gauge;

    (void) xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);
    do_gauge = argc > 1 && !strcmp(argv[1], "-gauge");
    xv_create(panel, do_gauge? PANEL_GAUGE : PANEL_SLIDER,
        PANEL_LABEL_STRING,     "Brightness: ",
        PANEL_VALUE,            75,
        PANEL_MIN_VALUE,        0,
        PANEL_MAX_VALUE,        100,
        PANEL_LAYOUT,           PANEL_VERTICAL,
        PANEL_TICKS,            5,
        NULL);
    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}
