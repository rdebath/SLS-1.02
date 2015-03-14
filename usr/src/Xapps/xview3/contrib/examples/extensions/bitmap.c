/* bitmap.c -- demonstrate the use of the Bitmap package. */
#include <xview/xview.h>
#include <xview/cms.h>
#include "bitmap.h"

main(argc, argv)
char *argv[];
{
    Frame frame;
    Cms cms;
    Bitmap bitmap;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    if (argc <= 1)
	puts("Specify bitmap filename"), exit(1);

    frame = (Frame)xv_create(NULL, FRAME, NULL);
    cms = xv_create(NULL, CMS,
        CMS_SIZE,       2,
        CMS_NAMED_COLORS, "LightBlue", "maroon", NULL,
        NULL);
    bitmap = xv_create(frame, BITMAP,
        XV_WIDTH,       100,
        XV_HEIGHT,      100,
        WIN_CMS,        cms,
	BITMAP_FILE,    argv[1],
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}
