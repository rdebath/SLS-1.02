/*
 * default_scale.c -- demonstrate the use of defaults_get_enum().
 * Specify a table of font scales and query the resource database
 * for legal values.  For example, you may have the following in
 * your .Xdefaults (which must be loaded into the resource database):
 *      font.scale: large
 */
#include <xview/xview.h>
#include <xview/font.h>
#include <xview/defaults.h>
#include <xview/textsw.h>

Defaults_pairs size_pairs[] = {
    "small",            WIN_SCALE_SMALL,
    "medium",           WIN_SCALE_MEDIUM,
    "large",            WIN_SCALE_LARGE,
    "extralarge",       WIN_SCALE_EXTRALARGE,
    /* the NULL entry is the default if Resource not found */
    NULL,               WIN_SCALE_MEDIUM,
};

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Xv_Font     font;
    int         scale;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);

    scale = defaults_get_enum("font.scale", "Font.Scale", size_pairs);
    /* get the default font for the frame, scaled to resource */
    font = xv_find(frame, FONT,
        FONT_RESCALE_OF,        xv_find(frame, FONT, NULL), scale,
        NULL);

    xv_create(frame, TEXTSW,
        XV_FONT,                font,
        WIN_COLUMNS,            80,
        WIN_ROWS,               10,
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}
