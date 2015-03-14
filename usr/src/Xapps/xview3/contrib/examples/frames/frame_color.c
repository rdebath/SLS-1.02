/* 
 *    frame_color.c - demonstrates how to set the frame's
 *    foreground and make it propogate to the children of
 *    the frame.
 */

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cms.h>

main(argc, argv)
    int 	  argc;
    char 	**argv;
{
    Frame         frame;
    Panel         panel;
    Cms		  cms;

    (void)xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

    frame = (Frame)xv_create(NULL, FRAME, 
	FRAME_LABEL,		argv[0],
	NULL);

    cms = xv_create(NULL, CMS,
	CMS_SIZE,		CMS_CONTROL_COLORS + 1,
	CMS_CONTROL_CMS,	True,
	CMS_NAMED_COLORS,	"red", NULL,
	NULL);

    xv_set(frame, WIN_CMS, cms, NULL);

    panel = (Panel)xv_create(frame, PANEL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,    "Push Me",
        NULL);

    xv_main_loop(frame);
}


