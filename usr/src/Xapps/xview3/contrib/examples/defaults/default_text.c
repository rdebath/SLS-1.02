/*
 * default_text.c -- use the defaults package to get a font name from
 * the resource database to set the textsw's font.
 */
#include <xview/xview.h>
#include <xview/font.h>
#include <xview/defaults.h>
#include <xview/textsw.h>

main(argc, argv)
char *argv[];
{
    Frame       frame;
    Xv_Font     font;
    char        *name;

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME, NULL);

    name = defaults_get_string("textsw.font","Textsw.Font", "fixed"),
    font = xv_find(frame, FONT,
        FONT_NAME,      name,
        NULL);

    xv_create(frame, TEXTSW,
        XV_FONT,        font,
        WIN_COLUMNS,    80,
        WIN_ROWS,       10,
        NULL);

    window_fit(frame);
    xv_main_loop(frame);
}
