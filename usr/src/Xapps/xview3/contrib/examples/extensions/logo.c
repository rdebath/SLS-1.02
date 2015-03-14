/* logo.c -- demonstrate the use of the logo package. */
#include <xview/xview.h>
#include <xview/cms.h>
#include "logo.h"

main(argc, argv)
char *argv[];
{
    Frame frame;
    Cms cms;
    Logo logo;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    cms = xv_create(NULL, CMS,
        CMS_SIZE,       2,
        CMS_NAMED_COLORS, "powder blue", "maroon", NULL,
        NULL);
    logo = xv_create(frame, LOGO,
        XV_WIDTH,       100,
        XV_HEIGHT,      100,
        WIN_CMS,        cms,
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}
