#include <xview/xview.h>

args_proc(prog)
char *prog;
{
    printf("%s: invalid argument.\\n", prog);
    exit(1);
}

main(argc, argv)
int argc;
char *argv[];
{
    Frame frame;

    frame = xv_create(NULL, FRAME,
        FRAME_ARGC_PTR_ARGV,    &argc, argv,
        FRAME_CMDLINE_HELP_PROC,  args_proc,
        FRAME_LABEL,              argv[0],
        FRAME_SHOW_FOOTER,        TRUE,
        FRAME_LEFT_FOOTER,        "left side",
        FRAME_RIGHT_FOOTER,       "right side",
        XV_WIDTH,                 300,
        XV_HEIGHT,                300,
        NULL);

    xv_set(frame, FRAME_SHOW_HEADER, TRUE, NULL);

    xv_main_loop(frame);
}
